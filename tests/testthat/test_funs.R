# library(radiant.data)
# library(testthat)

context("Radiant functions")

test_that("set_attr", {
  foo <- . %>% set_attr("foo", "something")
  expect_equal(3 %>% foo() %>% attr("foo"), "something")
})

test_that("add_class", {
  foo <- . %>% . ^ 2 %>% add_class("foo")
  expect_equal(3 %>% foo() %>% class(), c("foo", "numeric"))
})

test_that("sig_star", {
  sig_stars(c(.0009, .049, .009, .4, .09)) %>%
    expect_equal(c("***", "*", "**", "", "."))
})

test_that("sshh", {
  expect_equal(sshh(c(message("should be null"), test = 3)), NULL)
  expect_equal(sshh(warning("should be null")), NULL)
})

test_that("sshhr", {
  test <- 3 %>% set_names("test")
  expect_equal(sshhr(c(message("should be null"), test = 3)), test)
  expect_equal(sshhr(c(warning("should be null"), test = 3)), c("should be null", test))
})

test_that("getdata", {
  getdata(mtcars, "mpg:disp", filt = "mpg > 20", rows = 1:5) %>%
    expect_equal(., mtcars[mtcars$mpg > 20, c("mpg", "cyl", "disp")][1:5, 1:3] %>% set_rownames(1:5))
})

test_that("getclass", {
  expect_equal(getclass(diamonds), sapply(diamonds, class) %>% tolower())
})

test_that("is_empty", {
  expect_true(is_empty(""))
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_false(is_empty(3))
  expect_true(is_empty(c()))
  expect_true(is_empty("nothing", empty = "nothing"))
})

test_that("select column", {
  dataset <- getdata(diamonds, vars = "price:clarity")
  expect_equal(colnames(dataset), c("price", "carat", "clarity"))
})

test_that("select character vector", {
  dataset <- getdata(diamonds, vars = c("price", "carat", "clarity"))
  expect_equal(colnames(dataset), c("price", "carat", "clarity"))
})

test_that("filter", {
  dataset <- getdata(diamonds, filt = "cut == 'Very Good'")
  expect_equal(nrow(dataset), 677)
})

test_that("filterdata", {
  dataset <- filterdata(diamonds, filt = "cut == 'Very Good' & price > 5000")
  expect_equal(nrow(dataset), 187)
  expect_equal(sum(dataset$price), 1700078)
})

test_that("filterdata factor", {
  dataset <- filterdata(diamonds, filt = "clarity %in% c('SI2','SI1') & price > 18000")
  expect_equal(nrow(dataset), 14)
  expect_equal(sum(dataset$price), 256587)
})

## 'manual' testing of read_files to avoid adding numerous dataset to package
# files <- list.files("tests/testthat/data", full.names = TRUE)
# for (f in files) {
#   radiant.data::read_files(f, type = "rmd", clipboard = FALSE)
#   radiant.data::read_files(f, type = "r", clipboard = FALSE)
# }

## 'manual' testing with Dropbox folder
# files <- list.files("~/Dropbox/radiant.data/data", full.names = TRUE)
# for (f in files) {
#   radiant.data::read_files(f, type = "rmd", clipboard = FALSE)
#   radiant.data::read_files(f, type = "r", clipboard = FALSE)
# }

## 'manual' testing with Google Drive folder
# files <- list.files("~/Google Drive/radiant.data/data", full.names = TRUE)
# for (f in files) {
#   radiant.data::read_files(f, type = "rmd", clipboard = FALSE)
#   radiant.data::read_files(f, type = "r", clipboard = FALSE)
# }

## load code into clipboard
# radiant.data::read_files(type = "r")
# radiant.data::read_files(type = "rmd")
