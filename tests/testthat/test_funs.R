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
  getdata("mtcars", "mpg:disp", filt = "mpg > 20", rows = 1:5) %>%
    expect_equal(., mtcars[mtcars$mpg > 20, c("mpg", "cyl", "disp")][1:5, 1:3] %>% set_rownames(1:5))
})

test_that("changedata", {
  r_data <<- list() %>% {
    .$dat <- data.frame(a = 1:20)
    .
  }
  changedata("dat", 20:1, "b")
  expect_equal(r_data$dat, data.frame(a = 1:20, b = 20:1))
  rm(r_data, envir = .GlobalEnv)
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

test_that("options", {
  options(width = 10)
  options(scipen = 0)
  radiant.data:::.onLoad("", "")
  expect_equal(getOption("width"), 250)
  expect_equal(getOption("scipen"), 100)
})

test_that("select colon", {
  dat <- getdata("diamonds", vars = "price:clarity")
  expect_equal(colnames(dat), c("price", "carat", "clarity"))
})

test_that("select character vector", {
  dat <- getdata("diamonds", vars = c("price", "carat", "clarity"))
  expect_equal(colnames(dat), c("price", "carat", "clarity"))
})

test_that("filter", {
  dat <- getdata("diamonds", filt = "cut == 'Very Good'")
  expect_equal(nrow(dat), 677)
})

test_that("filterdata", {
  dat <- filterdata(diamonds, filt = "cut == 'Very Good' & price > 5000")
  expect_equal(nrow(dat), 187)
  expect_equal(sum(dat$price), 1700078)
})

test_that("filterdata factor", {
  dat <- filterdata(diamonds, filt = "clarity %in% c('SI2','SI1') & price > 18000")
  expect_equal(nrow(dat), 14)
  expect_equal(sum(dat$price), 256587)
})
