# library(radiant.data)
# library(testthat)

context("R deparse")

## See https://stackoverflow.com/questions/50422627/different-results-from-deparse-in-r-3-4-4-and-r-3-5
test_that("deparse R 3.4.4 vs R 3.5", {
  dctrl <- if (getRversion() > "3.4.4") c("keepNA", "niceNames") else "keepNA"
  expect_equal(deparse(list(dec = 4L, b = "a"), control = dctrl), "list(dec = 4, b = \"a\")")
})

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

test_that("get_data", {
  get_data(mtcars, "mpg:disp", filt = "mpg > 20", rows = 1:5) %>%
    expect_equal(., mtcars[mtcars$mpg > 20, c("mpg", "cyl", "disp")][1:5, 1:3] %>% set_rownames(1:5))
})

test_that("get_class", {
  expect_equal(get_class(diamonds), sapply(diamonds, class) %>% tolower())
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
  dataset <- get_data(diamonds, vars = "price:clarity")
  expect_equal(colnames(dataset), c("price", "carat", "clarity"))
})

test_that("select character vector", {
  dataset <- get_data(diamonds, vars = c("price", "carat", "clarity"))
  expect_equal(colnames(dataset), c("price", "carat", "clarity"))
})

test_that("filter", {
  dataset <- get_data(diamonds, filt = "cut == 'Very Good'")
  expect_equal(nrow(dataset), 677)
})

test_that("filter_data", {
  dataset <- filter_data(diamonds, filt = "cut == 'Very Good' & price > 5000")
  expect_equal(nrow(dataset), 187)
  expect_equal(sum(dataset$price), 1700078)
})

test_that("filter_data factor", {
  dataset <- filter_data(diamonds, filt = "clarity %in% c('SI2','SI1') & price > 18000")
  expect_equal(nrow(dataset), 14)
  expect_equal(sum(dataset$price), 256587)
})

context("Explore")

test_that("explore 8 x 2", {
  result <- explore(diamonds, "price:x")
  expect_equal(colnames(result$tab), c("variable", "mean", "sd"))
  expect_equal(result, structure(list(tab = structure(list(variable = structure(1:8,
    .Label = c("price",  "carat", "clarity", "cut", "color", "depth", "table", "x"), class = "factor"),
    mean = c(3907.186, 0.794283333333333, 0.0133333333333333,
    0.0336666666666667, 0.127333333333333, 61.7526666666667,
    57.4653333333333, 5.72182333333333), sd = c(3956.91540005997,
    0.473826329139292, 0.114716791286006, 0.180399751234967,
    0.333401571319236, 1.44602785395269, 2.24110219949434, 1.12405453974662
    )), class = "data.frame", row.names = c(NA, -8L), nrow = 8L),
    df_name = "diamonds", vars = c("price", "carat", "clarity",
    "cut", "color", "depth", "table", "x"), byvar = "", fun = c("mean",
    "sd"), top = "fun", tabfilt = "", tabsort = "", nr = NULL,
    data_filter = ""), class = c("explore", "list"
  )))
})

test_that("explore 1 x 2", {
  result <- explore(diamonds, "price")
  # summary(result)
  expect_equal(result, structure(list(tab = structure(list(variable = structure(1L, .Label = "price", class = "factor"),
    mean = 3907.186, sd = 3956.91540005997), class = "data.frame", row.names = c(NA,
    -1L), nrow = 1L), df_name = "diamonds", vars = "price", byvar = "",
    fun = c("mean", "sd"), top = "fun", tabfilt = "", tabsort = "",
    nr = NULL, data_filter = ""), class = c("explore",
    "list")
  ))
})

test_that("explore 1 x 1", {
  result <- explore(diamonds, "price", fun = "n_obs")
  # summary(result)
  expect_equal(colnames(result$tab), c("variable", "n_obs"))
})

test_that("explore 1 x 1 x 1", {
  result <- explore(diamonds, "price", byvar = "color", fun = "n_obs")
  # summary(result)
  expect_equal(colnames(result$tab), c("color", "variable", "n_obs"))
})

test_that("explore 1 x 1 x 2", {
  result <- explore(diamonds, "price", byvar = c("color", "cut"), fun = "n_obs")
  # summary(result)
  expect_equal(colnames(result$tab), c("color", "cut", "variable", "n_obs"))
  expect_equal(result$tab[1,], structure(list(color = structure(1L, .Label = c("D", "E", "F",
    "G", "H", "I", "J"), class = "factor"), cut = structure(1L, .Label = c("Fair",
    "Good", "Very Good", "Premium", "Ideal"), class = "factor"),
    variable = structure(1L, .Label = "price", class = "factor"),
    n_obs = 15L), nrow = 35L, row.names = 1L, class = "data.frame"
  ))
})

test_that("explore 2 x 2 x 2", {
  result <- explore(diamonds, c("price", "carat"), byvar = c("color", "cut"), fun = c("n_obs", "mean"))
  expect_equal(colnames(result$tab), c("color", "cut", "variable", "n_obs", "mean"))
})

# result <- explore(diamonds, "price:x")
# summary(result)
# result <- explore(diamonds, c("price","carat"), byvar = "cut", fun = c("n_missing", "skew"))
# summary(result)
# diamonds %>% explore("price", byvar = "cut", fun = c("n_obs", "n_distinct"))

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
