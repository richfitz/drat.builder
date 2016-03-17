## This is directly from callr; once that's on CRAN I'll set up a
## direct dependency I think.
call_system <- function(command, args, env=character(), max_lines=20,
                        p=0.8) {
  res <- suppressWarnings(system2(command, args,
                                  env=env, stdout=TRUE, stderr=TRUE))
  ok <- attr(res, "status")
  if (!is.null(ok) && ok != 0) {
    max_nc <- getOption("warning.length")

    cmd <- paste(c(env, shQuote(command), args), collapse = " ")
    msg <- sprintf("Running command:\n  %s\nhad status %d", cmd, ok)
    errmsg <- attr(cmd, "errmsg")
    if (!is.null(errmsg)) {
      msg <- c(msg, sprintf("%s\nerrmsg: %s", errmsg))
    }
    sep <- paste(rep("-", getOption("width")), collapse="")

    ## Truncate message:
    if (length(res) > max_lines) {
      n <- ceiling(max_lines * p)
      res <- c(head(res, ceiling(max_lines - n)),
               sprintf("[[... %d lines dropped ...]]", length(res) - max_lines),
               tail(res, ceiling(n)))
    }

    ## compute the number of characters so far, including three new lines:
    nc <- (nchar(msg) + nchar(sep) * 2) + 3
    i <- max(1, which(cumsum(rev(nchar(res) + 1L)) < (max_nc - nc)))
    res <- res[(length(res) - i + 1L):length(res)]
    msg <- c(msg, "Program output:", sep, res, sep)
    stop(paste(msg, collapse="\n"))
  }
  invisible(res)
}

Sys_which <- function(x) {
  ret <- Sys.which(x)
  if (ret == "") {
    stop(sprintf("%s not found in $PATH", x))
  }
  ret
}

## For working around testthat
assert_tests_passed <- function(obj=.Last.value) {
  obj <- as.data.frame(obj)
  if (any(obj$failed > 0) || any(obj$error)) {
    stop("Tests were not successful")
  }
}

read_dcf <- function(x) {
  dat <- read.dcf(x)
  setNames(as.list(dat), tolower(colnames(dat)))
}

writeLines_gz <- function(text, filename, ...) {
  con <- gzfile(filename)
  on.exit(close(con))
  writeLines(text, con, ...)
}
