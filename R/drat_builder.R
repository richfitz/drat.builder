##' @importFrom docopt docopt
main <- function(args=commandArgs(TRUE)) {
  ## also support specific packages only?
  ## also support --recursive or other git options? and ssh download
  ## TODO: Support 'clean' as a command
  ## TODO: Support rebuilding (can just remove the status file)
  'Usage:
  drat.builder [options] [<package_list>]
  drat.builder -h | --help

  Options:
  --install              attempt to install packages before building
  --install-local        as --install, but install in a local library
  --no-fetch             skip fetch
  --no-commit            skip commit
  --no-build-vignettes   skip vignettes (default behaviour, can override per package)
  --no-manual            skip manual (default behaviour, can override per package)
  --drop-history         drop all git history' -> doc
  oo <- options(warnPartialMatchArgs=FALSE)
  if (isTRUE(oo$warnPartialMatchArgs)) {
    on.exit(options(oo))
  }
  opts <- docopt::docopt(doc, args)
  names(opts) <- gsub("-", "_", names(opts))

  if (is.null(opts$package_list)) {
    opts$package_list <- "packages.txt"
  }

  build(opts$package_list,
        install            = opts$install,
        install_local      = opts$install_local,
        no_fetch           = opts$no_fetch,
        no_commit          = opts$no_commit,
        no_build_vignettes = opts$no_build_vignettes,
        no_manual          = opts$no_manual,
        drop_history       = opts$drop_history)
}

##' Build/Update a drat repo
##' @title Build/Update a drat repo
##' @param package_list Filename for the list of packages
##' @param install Install missing packages (may be required for
##'   building vignettes)
##' @param install_local If installing, install into a temporary
##'   library, rather than the main R libraries?
##' @param no_fetch Skip the fetch step
##' @param no_commit Skip the commit when updating drat
##' @param no_build_vignettes Skip building vignettes (which skips
##'   installation which is a potential headache)
##' @param no_manual Skip building PDF versions of the manual (which
##'   requires a fullish LaTeX installation)
##' @param drop_history Drop all git history (note that this is a
##'   destructive operation, though drat.builder will attempt to leave
##'   things somewhat recoverable).
##' @export
build <- function(package_list="packages.txt",
                  install=FALSE, install_local=FALSE,
                  no_fetch=FALSE, no_commit=FALSE,
                  no_build_vignettes=TRUE, no_manual=TRUE,
                  drop_history=FALSE) {
  defaults <- list(vignettes = !(no_build_vignettes), manual = !(no_manual))
  pkgs <- read_packages(package_list)
  if (!no_fetch) {
    fetch_package_sources(pkgs)
  }
  update_package_sources(pkgs)
  if (install || install_local) {
    lib <- if (install_local) tempfile("library") else NULL
    install_deps(pkgs, lib)
  }
  build_packages(pkgs, defaults)
  update_drat(pkgs, !no_commit, drop_history)
}

read_packages <- function(package_list="packages.txt") {
  packages <- readLines(package_list)
  ## This trims blank lines and "comments"
  packages <- sub("\\s*#.*$", "", packages)
  packages <- parse_packages(packages[!grepl("^\\s*$", packages)])

  packages <- cbind(packages, repo_dir=sprintf("%s/%s",
                                               packages[, "user"],
                                               packages[, "repo"]))

  ## TODO: Would be nice to check that there were no duplicate package
  ## names, bit not done for now.
  path <- dirname(package_list)
  packages_path <- file.path(path, "packages", packages[, "repo"])

  ## This bit of horribleness allows multiple packages within one
  ## repository that will be cloned independently (e.g., if requiring
  ## different refrences).
  i <- !is.na(packages[, "subdir"])
  if (any(i)) {
    packages_path[i] <- paste(packages_path[i],
                              packages[i, "subdir"], sep="__")
  }
  packages_path_pkg <- packages_path
  packages_path_pkg[i] <- file.path(packages_path_pkg[i], packages[i, "subdir"])

  packages <- cbind(packages,
                    path=unname(packages_path),
                    path_pkg=unname(packages_path_pkg),
                    path_repo=file.path("packages_src", packages[, "repo_dir"]))

  attr(packages, "path") <- path
  attr(packages, "name") <- package_list

  packages
}

fetch_package_sources <- function(packages) {
  for (p in rownames(packages)) {
    fetch_package(packages[p, ])
  }
}

fetch_package <- function(p) {
  log("fetch", p[["str"]])
  ## TODO: This is possibly something better done with file storr,
  ## once they exist?
  dp <- package_repo_dir(p)
  if (file.exists(dp)) {
    git_fetch_all(package_repo_dir(p))
  } else {
    git_clone(package_github_url(p), dp, "--mirror")
  }
}

update_package_sources <- function(packages) {
  unlink("packages", TRUE)
  for (p in rownames(packages)) {
    update_package(packages[p, ])
  }
}

update_package <- function(p) {
  src  <- package_repo_dir(p)
  dest <- p[["path"]]
  git_clone(src, dest)
  if (!is.na(p[["ref"]])) {
    call_git(c("checkout", p[["ref"]]), workdir=dest)
  }
}

install_deps <- function(packages, lib=NULL) {
  ## TODO: this possibly wants to install *non globally*; support
  ## installation into a .packages directory and configure libPaths to
  ## look there.  That's not hard to achieve, though it's extremely
  ## hard to test.

  ## Get all deps:
  deps <- unique(unlist(lapply(packages[, "path_pkg"], get_deps)))
  new_packages <- setdiff(deps, .packages(TRUE))
  if (length(new_packages) == 0L) {
    return()
  }

  ## TODO: How do we deal with non-CRAN versions here; some are likely
  ## to be found right here in the drat.
  packages_drat <- apply(packages, 1, package_name)
  packages_cran <- rownames(utils::available.packages())

  new_packages_cran <- setdiff(new_packages, packages_drat)
  packages_missing <- setdiff(new_packages_cran, packages_cran)
  new_packages_cran <- intersect(new_packages_cran, packages_cran)
  new_packages_drat <- intersect(new_packages, packages_drat)

  if (is.null(lib)) {
    lib <- .libPaths()[[1]]
  } else {
    init_library(lib)
  }

  if (length(new_packages_cran) > 0L) {
    ## TODO: optional log-on-error here.
    ## NOTE: was using withCallingHandlers to get warn -> error here
    ## but that plays badly with drat (because it generates warnings
    ## when it can't find the binaries list:
    ##   Warning: unable to access index for repository
    ##   http://traitecoevo.github.io/drat/bin/macosx/mavericks/contrib/3.2
    ## That issue might go away if we added an empty binary index; but
    ## looks like that might be supported in more recent drat
    ## versions.
    log("install", paste(new_packages_cran, collapse=" "))
    utils::install.packages(new_packages_cran, lib=lib, quiet=TRUE)
  }

  ## Not sure if this will always find deps
  if (length(new_packages_drat) > 0L) {
    log("install", paste(new_packages_drat, collapse=" "))
    pkgs <- names(packages_drat)[match(new_packages_drat, packages_drat)]
    pkgs_dirs <- packages[pkgs, "path_pkg"]
    utils::install.packages(pkgs_dirs, lib=lib, repos=NULL, type="source")
  }

  ## Missing packages:
  if (length(packages_missing) > 0L) {
    warning(sprintf("%d packages missing: %s",
                    length(packages_missing),
                    paste(packages_missing, collapse=", ")),
            immediate.=TRUE)
  }
}

## should only build if we have a different version to last time.
build_packages <- function(packages, defaults) {
  status <- status_load(packages)
  for (p in rownames(packages)) {
    build_package(packages[p, ], status, defaults)
  }
}

build_package <- function(p, status, defaults) {
  status_p <- status_line(p)
  name <- status_p$package

  build <- !(file.exists(file.path("src/contrib", package_zip(p))) &&
             identical(status[[name]]$sha, status_p$sha))
  if (build) {
    log("build", p[["str"]])
    do_build(p, defaults)
    if (!is.na(p[["subdir"]])) {
      file.rename(file.path(dirname(p[["path_pkg"]]), package_zip(p)),
                  file.path(dirname(p[["path"]]), package_zip(p)))
    }
  }
  build
}

clean_packages <- function(packages) {
  for (p in rownames(packages)) {
    z <- package_zip(packages[p, ])
    if (file.exists(z)) {
      log("clean", packages[p, "str"])
      file.remove(z)
    }
  }
  status <- status_load(packages)
  names <- apply(packages, 1, package_name)
  status <- status[setdiff(names(status), names)]
  status_save(packages, status)
}

## Optionally work on a branch here to make the work easy to roll
## back?
##
## TODO: Version of this for starting from a fresh commit
##   -- http://stackoverflow.com/a/15572071/1798863
##' @importFrom drat insertPackage
update_drat <- function(packages, commit, drop_history) {
  path <- attr(packages, "path")
  if (!file.exists(".git")) {
    ## NOTE: This duplicates some behaviour in drat::initRepo()
    git_init(path)
    call_git(c("checkout", "-b", "gh-pages"), workdir=path)
  }
  init_drat(path)

  if (commit && git_nstaged(path) > 0L) {
    stop("Must have no staged files to commit")
  }

  for (p in rownames(packages)) {
    update_drat1(packages[p, ], commit, packages)
  }
  update_landing_page(packages, commit)

  ## Now that we have everything, we might drop the history.  This is
  ## a highly destructive move and only going to be appropriate for
  ## keeping histories under control when they are created often.  It
  ## will require a force push to deploy.
  ##
  ## There are couple of options:
  ## - create a new orphan branch and move everything there.
  ## - interactively squash everything together
  ## - the approach below which is extremely quick
  ##   via http://stackoverflow.com/a/23486788/1798863
  if (commit && drop_history) {
    ## Keep an old copy of the history around, just in case:
    git <- Sys_which("git")
    br <- call_system(git, c("rev-parse", "--abbrev-ref", "HEAD"))
    br_old <- paste0(br, "_old")
    call_system(git, c("branch", "-f", br_old))
    sha <-
      call_system(git, c("commit-tree", "HEAD^{tree}",
                         "-m", '"destructive update by drat.builder"'))
    call_system(git, c("reset", sha))
    msg <- c(
      paste("Destructively squashed all history into commit:", sha),
      paste("Original history preserved in branch:", br_old),
      paste("Push will require force (git push -f)"))
    message(paste(msg, collapse="\n"))
  }
}

update_drat1 <- function(p, commit, packages) {
  status_p <- status_line(p)
  name <- status_p$package
  status <- status_load(packages)
  update <- !identical(status[[name]]$sha, status_p$sha)

  if (!update) {
    return()
  }
  status[[name]] <- status_p
  path <- attr(packages, "path")

  z <- package_zip(p)
  zz <- file.path("packages", z)
  log("drat", p[["str"]])
  drat::insertPackage(file.path("packages", z), path)
  status_save(packages, status)

  if (commit) {
    git_add(file.path("src/contrib", basename(z)), force=TRUE, repo=path)
    if (git_nstaged(path) > 0L) {
      log("commit", p[["str"]])
      git_add("src/contrib/PACKAGES", force=TRUE, repo=path)
      git_add("src/contrib/PACKAGES.gz", force=TRUE, repo=path)
      if (file.exists("src/contrib/PACKAGES.rds")) {
        git_add("src/contrib/PACKAGES.rds", force=TRUE, repo=path)
      }
      git_add(status_filename(packages), force=TRUE, repo=path)
      msg <- paste(package_name(p),
                   package_version(p),
                   substr(package_sha(p), 1, 7),
                   package_url(p))
      call_git(c("commit", "--no-verify", "-m", shQuote(msg)), workdir=path)
    }
  }
}

##' @importFrom crayon yellow
log <- function(action, package) {
  message(sprintf("*** [%s] %s",
                  crayon::yellow(action), crayon::blue(package)))
}

## All the package_ functions take a string from the packages.txt file
## and do something useful with them.
package_repo_dir <- function(p) {
  p[["path_repo"]]
}

package_zip <- function(p) {
  paste0(package_name(p), "_", package_version(p), ".tar.gz")
}

package_version <- function(p) {
  get_package_version(p[["path_pkg"]])
}

package_name <- function(p) {
  get_package_name(p[["path_pkg"]])
}

package_sha <- function(p) {
  git_sha(p[["path"]])
}

package_github_url <- function(p) {
  prefix <- "git@github.com:"
  prefix <- "https://github.com/"
  paste0(prefix, paste(p[["user"]], p[["repo"]], sep="/"), ".git")
}

## This is the true remote, but using the above might be better?
package_url <- function(p) {
  workdir <- package_repo_dir(p)
  call_git(c("config", "--get", "remote.origin.url"), workdir=workdir)
}

init_library <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, FALSE, TRUE)
  }
  .libPaths(path)
}

init_drat <- function(path) {
  ## TODO: Hit metacran or something here to get the current R version
  ## so that this can be bumped.  3.5 should last for the next year or
  ## so though.
  dir.create(file.path(path, "src", "contrib"), FALSE, TRUE)
  for (platform in c("windows", "macosx", "macosx/mavericks",
                     "macosx/el-capitan")) {
    for (version in c("3.1", "3.2", "3.3", "3.4", "3.5", "3.6", "4.0")) {
      p <- file.path(path, "bin", platform, "contrib", version)
      if (!file.exists(pp)) {
        pp <- file.path(p, "PACKAGES")
        dir.create(p, FALSE, TRUE)
        writeLines(character(0), pp)
        writeLines_gz(character(0), paste0(pp, ".gz"))
      }
    }
  }

  ## gitignore <- file.path(path, "src", ".gitignore")
  ## if (!file.exists(gitignore)) {
  ##   writeLines("!*.gz", gitignore)
  ## }
}

## From: http://stackoverflow.com/a/30225680
get_deps <- function(path) {
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  jj <- intersect(c("Depends", "Imports", "Suggests"), colnames(dcf))
  val <- unlist(strsplit(dcf[, jj], ","), use.names=FALSE)
  val <- gsub("\\s.*", "", trimws(val))
  val[val != "R"]
}

status_filename <- function(packages) {
  paste0(sub("\\.txt$", "", attr(packages, "name")), ".json")
}

status_load <- function(packages) {
  file <- status_filename(packages)
  if (file.exists(file)) {
    jsonlite::fromJSON(readLines(file), TRUE, FALSE)
  } else {
    list()
  }
}

status_save <- function(packages, data) {
  writeLines(jsonlite::toJSON(data), status_filename(packages))
}

status_line <- function(p) {
  list(package=package_name(p),
       version=package_version(p),
       sha=package_sha(p),
       date=Sys.time(),
       str=p[["str"]])
}

do_build <- function(p, defaults) {
  if (!is.na(p[["opts"]])) {
    defaults <- utils::modifyList(defaults, jsonlite::fromJSON(p[["opts"]]))
  }
  args <- c(if (defaults$manual) character(0) else "--no-manual",
            if (defaults$vignettes) character(0) else "--no-build-vignettes")

  path <- p[["path_pkg"]]
  owd <- setwd(dirname(path))
  on.exit(setwd(owd))
  call_system(file.path(R.home("bin"), "R"),
              c("--vanilla",
                "CMD", "build", args, basename(path)))
}

## Utilities:
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

get_package_name <- function(path) {
  read.dcf(paste0(path, "/DESCRIPTION"))[, "Package"][[1]]
}

get_package_version <- function(path) {
  read.dcf(paste0(path, "/DESCRIPTION"))[, "Version"][[1]]
}

##' Installs the "drat.builder" script in the directory \code{path}
##' @title Install drat.builder script
##' @param path Directory to install the script
##' @export
install_script <- function(path) {
  code <- c("#!/usr/bin/env Rscript",
            "library(methods)",
            "drat.builder:::main()")
  dest <- file.path(path, "drat.builder")
  writeLines(code, dest)
  Sys.chmod(dest, "0755")
}

parse_packages <- function(x) {
  ## The format is:
  ##   <username>/<repo>[/subdir][@ref]
  ## I think we're going to support additional things, including
  ## recursive, alt hosters, etc.
  re <- "^([^/]+)/([^/@#[:space:]]+)(.*)$"
  if (!all(grepl(re, x))) {
    stop("Invalid package line")
  }

  user <- sub(re, "\\1", x)
  repo <- sub(re, "\\2", x)
  rest <- sub(re, "\\3", x)

  re_subdir <- "^(/[^@#[:space:]]*)(.*)"
  i <- grepl(re_subdir, rest)
  subdir <- rep(NA_character_, length(x))
  subdir[i] <- sub("^/", "", sub(re_subdir, "\\1", rest[i]))
  rest[i]   <- sub(re_subdir, "\\2", rest[i])

  ## Either pull request or reference allowed, but let's just not
  ## support PRs yet.
  re_ref <- "^(@[^#[:space:]]*)(.*)"
  i <- grepl(re_ref, rest)
  ref <- rep(NA_character_, length(x))
  ref[i]  <- sub("^@", "", sub(re_ref, "\\1", rest[i]))
  rest[i] <- sub(re_ref, "\\2", rest[i])

  subdir[nchar(subdir) == 0L] <- NA_character_
  ref0 <- !nzchar(ref, FALSE)
  if (any(ref0)) {
    stop("Invalid reference: ", paste(x[ref0], collapse=", "))
  }

  rest <- trimws(rest)
  rest[nchar(rest) == 0L] <- NA_character_

  ## Check that everything left over is valid JSON:
  i <- !is.na(rest)
  check_parse <- function(x) {
    res <- tryCatch(jsonlite::fromJSON(x),
                    error=function(e) {
                      e$message <- sprintf("Error processing json '%s'\n%s",
                                           x, e$message)
                      stop(e)
                    })
    valid <- c(manual=logical(1), vignettes=logical(1))
    extra <- setdiff(names(res), names(valid))
    if (length(extra) > 0L) {
      warning("Extra options ignored: ", paste(extra, collapse=", "),
              immediate.=TRUE)
    }
    ok <- vapply(res, function(x) is.logical(x) && length(x) == 1,
                 logical(1))
    if (!all(ok)) {
      stop(sprintf("All options must be logical scalars (error on %s)",
                   paste(names(ok[!ok]), collapse=", ")))
    }
  }
  lapply(rest[i], check_parse)

  ret <- cbind(user=user, repo=repo, subdir=subdir, ref=ref,
               opts=rest, str=x)
  rownames(ret) <- x
  ret
}
