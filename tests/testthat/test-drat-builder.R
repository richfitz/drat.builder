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
})
