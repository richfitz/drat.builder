context("build (slow)")

## This is more a smoke/regression test than anything; it tests that
## the basic form of drat.builder works.  I've picked cases that
## should exercise the most common options, but this is not meant to
## be an exhaustive test of functionality.
##
## The biggest missing thing is mocking up re-fetching a repo that has
## moved on, upstream.
test_that("build (slow)", {
  skip_on_cran()
  Sys.setenv("R_TESTS" = "")

  r <- getOption("repos")
  r["CRAN"] <- "http://cran.rstudio.org"
  oo <- options(repos=r)
  on.exit(options(oo), add=TRUE)

  path <- tempfile()
  dir.create(path)
  file.copy("packages.txt", path)

  owd <- setwd(path)
  on.exit(setwd(owd), add=TRUE)

  drat.builder::build(install_local=TRUE)

  br <- call_git("branch", "--list")
  expect_that(br, equals("* gh-pages"))

  pkgs <- read.dcf("src/contrib/PACKAGES")

  cmp <- read_packages()
  expect_that(sort(pkgs[, "Package"]),
              equals(sort(unname(cmp[, "repo"]))))

  i <- match("testthat", pkgs[, "Package"])
  expect_that(unname(pkgs[i, "Version"]), equals("0.9.1"))

  i <- match("rrdf", cmp[, "repo"])
  zip <- file.path("src/contrib", package_zip(cmp[i, ]))
  path2 <- tempfile()
  untar(zip, exdir=path2)
  expect_that(file.exists(file.path(path2, "rrdf", "DESCRIPTION")),
              is_true())
  ## Didn't build vignettes:
  expect_that(file.exists(file.path(path2, "rrdf", "inst/doc")),
              is_false())

  ## Next, relax the testthat reference:
  tmp <- readLines("packages.txt")
  i <- grep("testthat", tmp)
  tmp[[i]] <- sub("@.+$", "", tmp[[i]])
  writeLines(tmp, "packages.txt")

  build(no_fetch=TRUE)
  pkgs <- read.dcf("src/contrib/PACKAGES")
  i <- match("testthat", pkgs[, "Package"])
  expect_that(numeric_version(pkgs[i, "Version"]) > numeric_version("0.9.1"),
              is_true())
})
