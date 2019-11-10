#' Combine datasets using dplyr's bind and join functions
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/combine.html} for an example in Radiant
#'
#' @param x Dataset
#' @param y Dataset to combine with x
#' @param by Variables used to combine `x` and `y`
#' @param add Variables to add from `y`
#' @param type The main bind and join types from the dplyr package are provided. \bold{inner_join} returns all rows from x with matching values in y, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. \bold{left_join} returns all rows from x, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. \bold{right_join} is equivalent to a left join for datasets y and x. \bold{full_join} combines two datasets, keeping rows and columns that appear in either. \bold{semi_join} returns all rows from x with matching values in y, keeping just columns from x. A semi join differs from an inner join because an inner join will return one row of x for each matching row of y, whereas a semi join will never duplicate rows of x. \bold{anti_join} returns all rows from x without matching values in y, keeping only columns from x. \bold{bind_rows} and \bold{bind_cols} are also included, as are \bold{intersect}, \bold{union}, and \bold{setdiff}. See \url{https://radiant-rstats.github.io/docs/data/combine.html} for further details
#' @param data_filter Expression used to filter the dataset. This should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#' @param ... further arguments passed to or from other methods
#'
#' @return Combined dataset
#'
#' @examples
#' avengers %>% combine_data(superheroes, type = "bind_cols")
#' combine_data(avengers, superheroes, type = "bind_cols")
#' avengers %>% combine_data(superheroes, type = "bind_rows")
#' avengers %>% combine_data(superheroes, add = "publisher", type = "bind_rows")
#'
#' @export
combine_data <- function(
  x, y, by = "", add = "",
  type = "inner_join",
  data_filter = "",
  envir = parent.frame(),
  ...
) {

  is_join <- grepl("_join", type)
  if (is_join && is_empty(by)) {
    return(cat("No variables selected to join datasets\n"))
  }

  ## legacy to deal with argument name change
  if (missing(x) || missing(y)) {
    depr <- list(...)
    x <- depr$dataset
    y <- depr$cmb_dataset
  }

  x_name <- ifelse(is_string(x), x, deparse(substitute(x)))
  y_name <- ifelse(is_string(y), y, deparse(substitute(y)))

  x <- get_data(x, filt = data_filter, na.rm = FALSE, envir = envir)
  if (all(add == "")) {
    y <- get_data(y, na.rm = FALSE, envir = envir)
  } else {
    y <- get_data(y, unique(c(by, add)), na.rm = FALSE, envir = envir)
  }

  ## keeping data descriptions
  x_descr <- attr(x, "description")
  y_descr <- attr(y, "description")

  if (is_join) {
    x <- get(type, envir = as.environment("package:dplyr"))(x, y, by = by)
    madd <- paste0("<br>\nBy: ", paste0(by, collapse = ", "))
  } else {
    x <- get(type, envir = as.environment("package:dplyr"))(x, y)
    madd <- ""
  }

  ## return error message as needed
  if (is.character(x)) return(x)

  mess <- paste0(
    "## Combined\n\nDatasets: ", x_name, " and ", y_name,
    " (", type, ")", madd, "</br>\nOn: ", lubridate::now(), "\n\n", x_descr,
    ifelse(data_filter != "", paste0("\n\n**Data filter:** ", data_filter), ""),
    "\n\n", y_descr
  )

  set_attr(x, "description", mess)
}
