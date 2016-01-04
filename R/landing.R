## Landing page.  This will be tweakable later.  For now, it's not!
##
## One nicer option would be to allow this to generate full staticdocs
## for all package.  I might actually pursue that at some point but
## that really begs to be done in a container so I'll hold off on it
## for now.
##
## The dcf bit will switch around soon, because I want to let people
## use markdown there and that requires rmarkdown.  Using dcf because
## it's easy to read and write and doesn't pull any additional
## requirements in.

##' @importFrom whisker whisker.render
update_landing_page <- function(packages, commit) {
  if (file.exists("index.dcf")) {
    dat <- read_dcf("index.dcf")

    call_git(c("ls-remote", "--get-url"))

    if (is.null(dat$username)) {
      url <- git_remote_url()
      url_re <- if (grepl("^git@", url)) "^.*?:" else "^.*?/"
      dat$username <- dirname(sub(url_re, "", url))
    }

    ## NOTE: I have a general version of this elsewhere:
    ok <- c("title", "description", "username")
    msg <- setdiff(ok, names(dat))
    if (length(msg) > 0L) {
      stop("Missing values in the landing page data: ",
           paste(msg, collapse=", "))
    }
    log("page", dat$title)

    pkgs <- lapply(file.path(packages[, "path"], "DESCRIPTION"), read_dcf)
    pkgs_name <- vcapply(pkgs, "[[", "package")
    pkgs_title <- vcapply(pkgs, "[[", "title")
    pkgs_version <- vcapply(pkgs, "[[", "version")
    pkgs_gh <- file.path("https://github.com",
                         packages[, "user"], packages[, "repo"])
    fmt <- '<li><code>%s</code> (v %s, <a href="%s" class="text-info"><i class="fa fa-github"></i></a>): %s</li>'
    pkgs_li <- sprintf(fmt, pkgs_name, pkgs_version, pkgs_gh, pkgs_title)
    dat$package_list <-
      paste(c("<ul>", pkgs_li[order(pkgs_name)], "</ul>"),
            collapse="\n")
    dat$package_name <- pkgs_name[[1]]

    template <- readLines(system.file("index.html", package=.packageName))
    writeLines(whisker::whisker.render(template, dat), "index.html")

    if (commit) {
      path <- attr(packages, "path")
      git_add("index.html", force=TRUE, repo=path)
      if (git_nstaged(path) > 0L) {
        log("commit", "index.html")
        msg <- "Update landing page"
        call_git(c("commit", "--no-verify", "-m", shQuote(msg)), workdir=path)
      }
    }
  }
}
