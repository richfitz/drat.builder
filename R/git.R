call_git <- function(args, ..., workdir=NULL) {
  if (!is.null(workdir)) {
    args <- c("-C", workdir, args)
  }
  call_system(Sys_which("git"), args, ...)
}


## Couple of useful git commands
git_sha <- function(path) {
  call_git(c("rev-parse", "HEAD"), workdir=path)
}

git_nstaged <- function(path) {
  res <- call_git(c("status", "--porcelain", "--untracked-files=no"),
                  workdir=path)
  sum(substr(res, 1, 1) != " ")
}

## Need to get git to fetch *all* references; that means updating
## where we are up to exactly; this is not supported in git2r (the
## other option I believe is to clone in mirror mode, but that is
## going to require calling out to the system git too).  A further
## option is to rewrite the config file, but *that* also requires
## calling out to the system git.
git_fetch_all <- function(path) {
  call_git(c("remote", "update"), workdir=path)
}

## None of this is that bad to get rid of, especially if we do a
## shallow clone of the upstream repos.  Better still is if we can ask
## what HEAD of origin would be without actually fetching it...
git_clone <- function(url, dest=NULL, opts=character()) {
  call_git(c("clone", opts, "--", url, dest))
}

git_init <- function(path=".", opts=character()) {
  call_git(c("init", opts, path))
}

## NOTE: this duplicates code elsewhere, with a coliding name, so will
## take a little effort to update.  Moving from git2r to git via shell
## will require rewiring this a bunch.
git_add <- function(path, opts=character(), repo=NULL, force=FALSE) {
  if (force) {
    opts <- c(opts, "--force")
  }
  call_git(c("add", opts, path), workdir=repo)
}
