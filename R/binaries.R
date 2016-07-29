needs_compilation <- function(p) {
  file.exists(file.path(p, "src"))
}
status_bin_load <- function(packages) {
  file <- status_bin_filename(packages)
  if (file.exists(file)) {
    vcapply(jsonlite::fromJSON(readLines(file), TRUE, FALSE), "[[", 1L)
  } else {
    character(0)
  }
}
status_bin_save <- function(packages, data) {
  writeLines(jsonlite::toJSON(data, auto_unbox=TRUE),
             status_bin_filename(packages))
}
status_bin_filename <- function(packages) {
  paste0(sub("\\.txt$", "", attr(packages, "name")), "_bin.json")
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

build_binaries <- function(packages, host, port, timeout=600, commit=TRUE,
                           ...) {
  status <- status_load(packages)
  status_bin <- status_bin_load(packages)

  n <- recycle_len(host, port)
  host <- rep_len(host, n)
  port <- rep_len(port, n)
  root <- "."

  if (commit && git_nstaged(root) > 0L) {
    stop("Must have no staged files to commit")
  }

  check <- packages[vlapply(packages[, "path_pkg"], needs_compilation), ]
  nms <- vcapply(check[, "path_pkg"],
                 function(x) read.dcf(file.path(x, "DESCRIPTION"), "Package"))

  sha_src <- vcapply(status[nms], "[[", "sha")
  sha_bin <- status_bin[nms]

  i <- is.na(sha_bin) | sha_src != sha_bin
  to_build <- setNames(names(nms)[i], nms[i])

  ok <- setNames(rep.int(TRUE, length(to_build)), to_build)
  for (p in to_build) {
    pkg <- pkgs[p, ]
    for (i in seq_len(n)) {
      ok[[p]] <- ok[[p]] &&
        build_binary_package(pkg, root, host[[i]], port[[i]], ...)
    }
  }

  if (any(ok)) {
    built <- names(to_build)[ok]
    status_bin[built] <- sha_src[built]
    status_bin_save(packages, status_bin)
    ## This is quite shit for now, but will work at least.
    git_add(file.path(root, "bin"))
    git_add(status_bin_filename(packages))
    if (git_nstaged(root) > 0L) {
      built_pkgs <- paste(built, collapse=", ")
      log("commit", paste("binaries:", built_pkgs))
      msg <- paste0("Added binaries for ", built_pkgs)
      call_git(c("commit", "--no-verify", "-m", shQuote(msg)), workdir=root)
    }
  }
}

build_binary_package <- function(p, root, host, port) {
  log("binary", sprintf("%s ==> http://%s:%s", p[["str"]], host, port))
  filename <- file.path(root, "src/contrib", package_zip(p))
  res <- tryCatch(buildr::build_binaries(filename, host, port),
                  error=function(e) NULL)
  if (is.null(res)) {
    log("ERROR", p[["str"]])
    FALSE
  } else {
    log("OK", basename(res))
    drat::insertPackage(res, root)
    TRUE
  }
}

recycle_len <- function(a, b) {
  na <- length(a)
  nb <- length(b)
  if (na == nb || nb == 1L) {
    na
  } else if (na == 1L) {
    nb
  } else {
    stop("Incompatible lengths")
  }
}
