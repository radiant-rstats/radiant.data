#' Combine datasets using dplyr's bind and join functions
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/combine.html} for an example in Radiant
#'
#' @param x Dataset (name). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param y Dataset (name) (to combine with `dataset`. This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param by Variables used to combine `dataset` and `cmb_dataset`
#' @param add Variables to add from `cmb_dataset`
#' @param type The main bind and join types from the dplyr package are provided. \bold{inner_join} returns all rows from x with matching values in y, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. \bold{left_join} returns all rows from x, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned. \bold{right_join} is equivalent to a left join for datasets y and x. \bold{full_join} combines two datasets, keeping rows and columns that appear in either. \bold{semi_join} returns all rows from x with matching values in y, keeping just columns from x. A semi join differs from an inner join because an inner join will return one row of x for each matching row of y, whereas a semi join will never duplicate rows of x. \bold{anti_join} returns all rows from x without matching values in y, keeping only columns from x. \bold{bind_rows} and \bold{bind_cols} are also included, as are \bold{intersect}, \bold{union}, and \bold{setdiff}. See \url{https://radiant-rstats.github.io/docs/data/combine.html} for further details
#' @param data_filter Expression used to filter the dataset. This should be a string (e.g., "price > 10000")
#' @param name Name for the combined dataset
#' @param ... further arguments passed to or from other methods
#'
#' @return If list `r_data` exists the combined dataset is added as `name`. Else the combined dataset will be returned as `name`
#'
#' @examples
#' avengers %>% combinedata(superheroes, type = "bind_cols")
#' combinedata("avengers", "superheroes", type = "bind_cols")
#' avengers %>% combinedata(superheroes, type = "bind_rows")
#' avengers %>% combinedata(superheroes, add = "publisher", type = "bind_rows")
#'
#' @export
combinedata <- function(x, y,
                        by = "",
                        add = "",
                        type = "inner_join",
                        name = "",
                        data_filter = "",
                        ...) {

  is_join <- grepl("_join", type)
  if (is_join && by[1] == "") {
    return(cat("No variables selected to join datasets\n"))
  }

  if (missing(x) || missing(y)) {
    depr <- list(...)
    x <- depr$dataset
    y <- depr$cmb_dataset
  }

  if (is_empty(name == "")) {
    name <- if (is_string(x)) paste0("cmb_", x) else "cmb_data"
  }

  x_name <- ifelse(is_string(x), x, deparse(substitute(x)))
  y_name <- ifelse(is_string(y), y, deparse(substitute(y)))

  x <- getdata(x, filt = data_filter, na.rm = FALSE)
  if (all(add == "")) {
    y <- getdata(y, na.rm = FALSE)
  } else {
    y <- getdata(y, unique(c(by, add)), na.rm = FALSE)
  }

  ## keeping data descriptions
  x_descr <- attr(x, "description")
  y_descr <- attr(y, "description")


  if (is_join) {
    x <- get(type)(x, y, by = by)
    madd <- paste0("<br>\nBy: ", paste0(by, collapse = ", "))
  } else {
    x <- get(type)(x, y)
    madd <- ""
  }

  ## return error message as needed
  if (is.character(x)) return(x)

  mess <-
    paste0(
      "## Combined\n\nDatasets: ", x_name, " and ", y_name,
      " (", type, ")", madd, "</br>\nOn: ", lubridate::now(), "\n\n", x_descr,
      ifelse(data_filter != "", paste0("\n\n**Data filter:** ", data_filter), ""),
      "\n\n", y_descr
    )

  x <- set_attr(x, "description", mess)

  if (exists("r_environment")) {
    env <- r_environment
  } else if (exists("r_data")) {
    env <- pryr::where("r_data")
  } else {
    return(x)
  }

  env$r_data[[name]] <- x
  env$r_data[["datasetlist"]] <- c(name, env$r_data[["datasetlist"]]) %>% unique()
  env$r_data[[paste0(name, "_descr")]] <- mess
  message("\nCombined data added as r_data[[\"", name, "\"]]\n")
}
