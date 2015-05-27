##' @importFrom docopt docopt
main <- function(args=commandArgs(TRUE)) {
  ## also support specific packages only?
  ## also support --recursive or other git options? and ssh download
  ## TODO: Support 'clean' as a command
  ## TODO: Support rebuilding (can just remove the status file)
  'Usage:
  drat.builder [options]
  drat.builder -h | --help

  Options:
  --install        attempt to install packages before building
  --install-local  as --install, but install in a local library
  --no-fetch       skip fetch' -> doc
  oo <- options(warnPartialMatchArgs=FALSE)
  if (isTRUE(oo$warnPartialMatchArgs)) {
    on.exit(options(oo))
  }
  opts <- docopt::docopt(doc, args)
  names(opts) <- sub("-", "_", names(opts))

  build("packages.txt",
        install       = opts$install,
        install_local = opts$install_local,
        no_fetch      = opts$no_fetch)
}

##' Build/Update a drat repo
##' @title Build/Update a drat repo
##' @param package_list Filename for the list of packages
##' @param install Install missing packages (may be required for
## building vignettes)
##' @param install_local If installing, install into a temporary
##' library, rather than the main R libraries?
##' @param no_fetch Skip the fetch step
##' @param forget Not yet used
##' @export
build <- function(package_list="packages.txt",
                  install=FALSE, install_local=FALSE,
                  no_fetch=FALSE, forget=FALSE) {
  if (!identical(forget, FALSE)) {
    stop("not yet implemented")
  }
  pkgs <- read_packages(package_list)
  if (!no_fetch) {
    fetch_package_sources(pkgs)
  }
  update_package_sources(pkgs)
  if (install || install_local) {
    lib <- if (install_local) tempfile("library") else NULL
    install_deps(pkgs, lib)
  }
  build_packages(pkgs)
  update_drat(pkgs)
}

read_packages <- function(package_list="packages.txt") {
  packages <- readLines(package_list)
  ## This trims blank lines and "comments"
  packages <- sub("\\s*#.*$", "", packages)
  packages <- parse_packages(packages[!grepl("^\\s*$", packages)])
  ## TODO: this is harsh for the rare cases where identically named
  ## repositories have different packages, or where one repository has
  ## multiple packages in different subdirectories.
  if (any(duplicated(packages[, "repo"]))) {
    dups <- packages[duplicated(packages[, "repo"]), "repo"]
    dups <- packages[packages[, "repo"] %in% dups, ]
    err <- paste(c("Duplicate repository names:",
                   paste0(" - ", dups[order(dups[, "repo"]), "str"])),
                 collapse="\n")
    stop(err)
  }
  path <- dirname(package_list)
  packages_path <- file.path(path, "packages", packages[, "repo"])
  i <- !is.na(packages[, "subdir"])
  packages_path[i] <- file.path(packages_path[i], packages[i, "subdir"])
  packages <- cbind(packages, path=unname(packages_path))

  attr(packages, "path") <- path
  attr(packages, "name") <- package_list

  packages
}

fetch_package_sources <- function(packages) {
  for (p in rownames(packages)) {
    fetch_package(packages[p, ])
  }
}

##' @importFrom git2r fetch
fetch_package <- function(p) {
  log("fetch", p[["str"]])
  ## TODO: This is possibly something better done with file storr,
  ## once they exist?
  dp <- package_repo_dir(p)
  if (file.exists(dp)) {
    git_fetch_all(package_repo_dir(p))
  } else {
    git2r::clone(package_github_url(p), dp, TRUE)
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

  git2r::clone(src, dest, progress=FALSE)
  if (!is.na(p[["ref"]])) {
    git2r::checkout(git2r::repository(dest), p[["ref"]])
  }
}

install_deps <- function(packages, lib=NULL) {
  ## TODO: this possibly wants to install *non globally*; support
  ## installation into a .packages directory and configure libPaths to
  ## look there.  That's not hard to achieve, though it's extremely
  ## hard to test.

  ## Get all deps:
  deps <- unique(unlist(lapply(packages[, "path"], get_deps)))
  new_packages <- setdiff(deps, .packages(TRUE))
  if (length(new_packages) == 0L) {
    return()
  }

  ## TODO: How do we deal with non-CRAN versions here; some are likely
  ## to be found right here in the drat.
  packages_drat <- apply(packages, 1, package_name)
  packages_cran <- rownames(available.packages())

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
    install.packages(new_packages_cran, lib=lib, quiet=TRUE)
  }

  ## Not sure if this will always find deps
  if (length(new_packages_drat) > 0L) {
    log("install", paste(new_packages_drat, collapse=" "))
    pkgs <- names(packages_drat)[match(new_packages_drat, packages_drat)]
    pkgs_dirs <- packages[pkgs, "path"]
    install.packages(pkgs_dirs, lib=lib, repos=NULL, type="source")
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
build_packages <- function(packages) {
  status <- status_load(packages)
  for (p in rownames(packages)) {
    build_package(packages[p, ], status)
  }
}

build_package <- function(p, status) {
  status_p <- status_line(p)
  name <- status_p$package

  build <- !(file.exists(file.path("src/contrib", package_zip(p))) &&
             identical(status[[name]]$sha, status_p$sha))
  if (build) {
    log("build", p[["str"]])
    do_build(p)
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
##' @importFrom drat insertPackage
update_drat <- function(packages, commit=TRUE) {
  path <- attr(packages, "path")
  if (file.exists(".git")) {
    repo <- git2r::repository(path)
  } else {
    ## NOTE: This duplicates some behaviour in drat::initRepo()
    ## TODO: possibly make this optional?
    ## TODO: can tweak this behaviour with commit argument?
    repo <- git2r::init(path)
    readme <- file.path(path, "README.md")
    writeLines("# `drat`", readme)
    git2r::add(repo, "README.md") # TODO: not file.path
    cmt <- git2r::commit(repo, "Initial Commit")
    ## NOTE: arguably a git2r bug where checkout does not work if
    ## there has not been a commit
    git2r::checkout(repo, "gh-pages", create = TRUE)
    git2r::branch_delete(git2r::branches(repo)[[2]])
  }
  init_drat(path)

  if (git_nstaged(repo) > 0L) {
    stop("Must have no staged files")
  }
  for (p in rownames(packages)) {
    update_drat1(packages[p, ], commit, packages)
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
    repo <- git2r::repository(path)
    git_add(repo, file.path("src/contrib", basename(z)), force=TRUE)
    if (git_nstaged(repo) > 0L) {
      log("commit", p[["str"]])
      git_add(repo, "src/contrib/PACKAGES", force=TRUE)
      git_add(repo, "src/contrib/PACKAGES.gz", force=TRUE)
      git_add(repo, status_filename(packages), force=TRUE)
      msg <- paste(package_name(p),
                   package_version(p),
                   substr(package_sha(p), 1, 7),
                   package_url(p))
      git2r::commit(repo, msg)
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
  file.path("packages_src", p[["repo"]])
}

package_zip <- function(p) {
  paste0(package_name(p), "_", package_version(p), ".tar.gz")
}

package_version <- function(p) {
  get_package_version(p[["path"]])
}

package_name <- function(p) {
  get_package_name(p[["path"]])
}

package_sha <- function(p) {
  git_sha(p[["path"]])
}

package_repo <- function(p) {
  git2r::repository(p[["path"]])
}

package_github_url <- function(p) {
  prefix <- "git@github.com:"
  prefix <- "https://github.com/"
  paste0(prefix, paste(p[["user"]], p[["repo"]], sep="/"), ".git")
}

## This is the true remote, but using the above migh be better?
package_url <- function(p) {
  git2r::remote_url(git2r::repository(package_repo_dir(p)), "origin")
}

## Couple of useful git commands
git_sha <- function(path) {
  git2r::branch_target(git2r::head(git2r::repository(path)))
}

git_nstaged <- function(repo) {
  st <- git2r::status(repo, unstaged=FALSE, untracked=FALSE)
  length(st$staged)
}

## Need to get git to fetch *all* references; that means updating
## where we are up to exactly; this is not supported in git2r (the
## other option I believe is to clone in mirror mode, but that is
## going to require calling out to the system git too).  A further
## option is to rewrite the config file, but *that* also requires
## calling out to the system git.
git_fetch_all <- function(path) {
  run_system(Sys.which("git"),
             c("-C", path, "fetch", "origin", "*:*"))
}

## Workaround for git2r::add not having a force argument.
## Filed: https://github.com/ropensci/git2r/issues/148
## Fixed since 7ccb202 but no bump to version number so can't depend
## yet.
git_add <- function(repo, path, force) {
  if (force) {
    git <- Sys.which("git")
    args <- c("-C", repo@path, "add", "--force", path)
    ok <- system2(git, args)
    if (ok != 0L) {
      stop("Error adding file")
    }
  } else {
    git2r::add(repo, path)
  }
}

init_library <- function(path) {
  if (!file.exists(path)) {
    dir.create(path, FALSE, TRUE)
  }
  .libPaths(path)
}

init_drat <- function(path) {
  dir.create(file.path(path, "src", "contrib"), FALSE, TRUE)
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

do_build <- function(p, args=NULL) {
  if (is.null(args)) {
    args <- "--no-manual"
  }
  path <- p[["path"]]
  owd <- setwd(dirname(path))
  on.exit(setwd(owd))
  run_system(file.path(R.home("bin"), "R"),
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
  re <- "^([^/]+)/([^/@#]+)(.*)$"
  if (!all(grepl(re, x))) {
    stop("Invalid package line")
  }

  user <- sub(re, "\\1", x)
  repo <- sub(re, "\\2", x)
  rest <- sub(re, "\\3", x)

  re_subdir <- "^(/[^@#]*)(.*)"
  i <- grepl(re_subdir, rest)
  subdir <- rep(NA_character_, length(x))
  subdir[i] <- sub("^/", "", sub(re_subdir, "\\1", rest[i]))
  rest[i]   <- sub(re_subdir, "\\2", rest[i])

  ## Either pull request or reference allowed, but let's just not
  ## support PRs yet.
  re_ref <- "^(@[^#]*)(.*)"
  i <- grepl(re_ref, rest)
  ref <- rep(NA_character_, length(x))
  ref[i]  <- sub("^@", "", sub(re_ref, "\\1", rest[i]))
  rest[i] <- sub(re_ref, "\\2", rest[i])

  subdir[nchar(subdir) == 0L] <- NA_character_
  if (any(nchar(ref) == 0L)) {
    stop("Invalid reference: ",
         paste(x[nchar(ref) == 0L], collapse=", "))
  }

  if (any(nchar(rest) > 0L)) {
    stop("Malformed git repo: ",
         paste(x[nchar(rest) > 0L], collapse=", "))
  }

  ret <- cbind(user=user, repo=repo, subdir=subdir, ref=ref, str=x)
  rownames(ret) <- x
  ret
}
