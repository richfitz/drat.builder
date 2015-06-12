context("drat.builder")

test_that("parse package line", {
  ## Simple case:
  res <- parse_packages("richfitz/storr")
  expect_that(res, is_a("matrix"))
  expect_that(nrow(res), equals(1L))
  expect_that(rownames(res), equals("richfitz/storr"))
  expect_that(res[1, "user"],   equals("richfitz"))
  expect_that(res[1, "repo"],   equals("storr"))
  expect_that(res[1, "subdir"], equals(NA_character_))
  expect_that(res[1, "ref"],    equals(NA_character_))

  ## More complicated:
  res <- parse_packages(c("user/repo",
                          "user/repo/subdir",
                          "user/repo@ref",
                          "user/repo/subdir@ref"))
  expect_that(unname(res[, "subdir"]),
              equals(c(NA, "subdir", NA, "subdir")))
  expect_that(unname(res[, "ref"]),
              equals(c(NA, NA, "ref", "ref")))

  ## trailing yaml:

  txt <- c("foo/bar",
           'foo/bar {"vignettes": true}',
           'foo/bar      {"vignettes": false}   ')
  res <- parse_packages(txt)
  expect_that(unname(res[, "opts"]),
              equals(c(NA, '{"vignettes": true}', '{"vignettes": false}')))

  ## Invalid yaml:
  expect_that(parse_packages(c("foo/bar {baz")),
              throws_error("Error processing json '{baz'", fixed=TRUE))
  expect_that(parse_packages(c("foo/bar/subdir {baz")),
              throws_error("Error processing json '{baz'", fixed=TRUE))
  expect_that(parse_packages(c("foo/bar@ref {baz")),
              throws_error("Error processing json '{baz'", fixed=TRUE))
  expect_that(parse_packages(c("foo/bar/subdir@ref {baz")),
              throws_error("Error processing json '{baz'", fixed=TRUE))

  ## Unknown options:
  expect_that(parse_packages('foo/bar {"opt": false}'),
              gives_warning("Extra options ignored: opt"))

  ## Non-scalar arguments:
  expect_that(parse_packages('foo/bar {"vignettes": "sounds great!"}'),
              throws_error("All options must be logical scalars"))
})
