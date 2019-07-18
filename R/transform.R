#' Center
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return If x is a numeric variable return x - mean(x)
#' @export
center <- function(x, na.rm = TRUE)
  if (is.numeric(x)) {
    x - mean(x, na.rm = na.rm)
  } else {
    x
  }

#' Standardize
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return If x is a numeric variable return center(x) / mean(x)
#' @export
standardize <- function(x, na.rm = TRUE) {
  if (is.numeric(x)) {
    center(x, na.rm = na.rm) / sd(x, na.rm = na.rm)
  } else {
    x
  }
}

#' Calculate square of a variable
#' @param x Input variable
#' @return x^2
#' @export
square <- function(x) x ^ 2

#' Calculate inverse of a variable
#' @param x Input variable
#' @return 1/x
#' @export
inverse <- function(x) {
  if (is.numeric(x)) 1 / x else x
}

#' Normalize a variable x by a variable y
#' @param x Input variable
#' @param y Normalizing variable
#' @return x/y
#' @export
normalize <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) x / y else x
}

#' Convert input in month-day-year format to date
#' @details Use as.character if x is a factor
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_mdy("2-1-2014")
#' \dontrun{
#' as_mdy("2-1-2014") %>% month(label = TRUE)
#' as_mdy("2-1-2014") %>% week()
#' as_mdy("2-1-2014") %>% wday(label = TRUE)
#' }
#' @export
as_mdy <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(mdy(x)) %>% as.Date()
}

#' Convert input in day-month-year format to date
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_dmy("1-2-2014")
#'
#' @export
as_dmy <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(dmy(x)) %>% as.Date()
}

#' Convert input in year-month-day format to date
#' @param x Input variable
#' @return Date variable of class Date
#' @examples
#' as_ymd("2013-1-1")
#'
#' @export
as_ymd <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(ymd(x)) %>% as.Date()
}

# http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html
#' Convert input in year-month-day-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_ymd_hms("2014-1-1 12:15:01")
#' \dontrun{
#' as_ymd_hms("2014-1-1 12:15:01") %>% as.Date
#' as_ymd_hms("2014-1-1 12:15:01") %>% month
#' as_ymd_hms("2014-1-1 12:15:01") %>% hour
#' }
#' @export
as_ymd_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(ymd_hms(x))
}

#' Convert input in year-month-day-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_ymd_hm("2014-1-1 12:15")
#' @export
as_ymd_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(parse_date_time(x, "%Y%m%d %H%M"))
}

#' Convert input in month-day-year-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hms("1-1-2014 12:15:01")
#' @export
as_mdy_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(parse_date_time(x, "%m%d%Y %H%M%S"))
}

#' Convert input in month-day-year-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hm("1-1-2014 12:15")
#' @export
as_mdy_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(parse_date_time(x, "%m%d%Y %H%M"))
}

#' Convert input in day-month-year-hour-minute-second format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hms("1-1-2014 12:15:01")
#' @export
as_dmy_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(parse_date_time(x, "%d%m%Y %H%M%S"))
}

#' Convert input in day-month-year-hour-minute format to date-time
#' @param x Input variable
#' @return Date-time variable of class Date
#' @examples
#' as_mdy_hm("1-1-2014 12:15")
#' @export
as_dmy_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(parse_date_time(x, "%d%m%Y %H%M"))
}

#' Convert input in hour-minute-second format to time
#' @param x Input variable
#' @return Time variable of class Period
#' @examples
#' as_hms("12:45:00")
#' \dontrun{
#' as_hms("12:45:00") %>% hour
#' as_hms("12:45:00") %>% second
#' }
#' @export
as_hms <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(hms(x))
}

#' Convert input in hour-minute format to time
#' @param x Input variable
#' @return Time variable of class Period
#' @examples
#' as_hm("12:45")
#' \dontrun{
#' as_hm("12:45") %>% minute()
#' }
#' @export
as_hm <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  sshhr(hm(x))
}

#' Convert variable to integer avoiding potential issues with factors
#' @param x Input variable
#' @return Integer
#' @examples
#' as_integer(rnorm(10))
#' as_integer(letters)
#' as_integer(as.factor(5:10))
#' as.integer(as.factor(5:10))
#' as_integer(c("a","b"))
#'
#' @export
as_integer <- function(x) {
  if (is.factor(x)) {
    int <- sshhr(levels(x) %>% .[x] %>% as.integer())
    if (length(na.omit(int)) == 0) as.integer(x) else int
  } else if (is.character(x)) {
    int <- sshhr(as.integer(x))
    if (length(na.omit(int)) == 0) as_integer(as.factor(x)) else int
  } else {
    as.integer(x)
  }
}

#' Convert variable to numeric avoiding potential issues with factors
#' @param x Input variable
#' @return Numeric
#' @examples
#' as_numeric(rnorm(10))
#' as_numeric(letters)
#' as_numeric(as.factor(5:10))
#' as.numeric(as.factor(5:10))
#' as_numeric(c("a","b"))
#' as_numeric(c("3","4"))
#'
#' @export
as_numeric <- function(x) {
  if (is.factor(x)) {
    num <- sshhr(levels(x) %>% .[x] %>% as.numeric())
    if (length(na.omit(num)) == 0) as.numeric(x) else num
  } else if (is.character(x)) {
    num <- sshhr(as.numeric(x))
    if (length(na.omit(num)) == 0) as_numeric(as.factor(x)) else num
  } else {
    as.numeric(x)
  }
}

#' Wrapper for factor with ordered = FALSE
#' @param x Input vector
#' @param ordered Order factor levels (TRUE, FALSE)
#' @export
as_factor <- function(x, ordered = FALSE) factor(x, ordered = ordered)

#' Wrapper for as.character
#' @param x Input vector
#' @export
as_character <- function(x) as.character(x)

#' Wrapper for lubridate's as.duration function. Result converted to numeric
#' @param x Time difference
#' @export
as_duration <- function(x) as.numeric(lubridate::as.duration(x))

#' Distance in kilometers or miles between two locations based on lat-long
#' Function based on \url{http://www.movable-type.co.uk/scripts/latlong.html}. Uses the haversine formula
#' @param long1 Longitude of location 1
#' @param lat1 Latitude of location 1
#' @param long2 Longitude of location 2
#' @param lat2 Latitude of location 2
#' @param unit Measure kilometers ("km", default) or miles ("miles")
#' @param R Radius of the earth
#' @return Distance between two points
#' @examples
#' as_distance(32.8245525,-117.0951632, 40.7033127,-73.979681, unit = "km")
#' as_distance(32.8245525,-117.0951632, 40.7033127,-73.979681, unit = "miles")
#'
#' @export
as_distance <- function(
  lat1, long1, lat2, long2,
  unit = "km", R = c("km" = 6371, "miles" = 3959)[[unit]]
) {

  rad <- pi / 180
  d1 <- lat1 * rad
  d2 <- lat2 * rad
  dlat <- (lat2 - lat1) * rad
  dlong <- (long2 - long1) * rad
  a <- sin(dlat / 2) ^ 2 + cos(d1) * cos(d2) * sin(dlong / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

#' Generate a variable used to selected a training sample
#' @param n Number (or fraction) of observations to label as training
#' @param nr Number of rows in the dataset
#' @param seed Random seed
#' @return 0/1 variables for filtering
#' @examples
#' make_train(.5, 10)
#'
#' @export
make_train <- function(n = .7, nr = 100, seed = 1234) {
  seed %>% gsub("[^0-9]", "", .) %>%
    {if (!is_empty(.)) set.seed(seed)}
  if (n < 1) n <- round(n * nr) %>% max(1)
  ind <- seq_len(nr)
  training <- rep_len(0L, nr)
  training[sample(ind, n)] <- 1L
  training
}

#' Add transformed variables to a data frame with the option to include a custom variable name extension
#'
#' @details Wrapper for dplyr::mutate_at that allows custom variable name extensions
#'
#' @param .tbl Data frame to add transformed variables to
#' @param .funs Function(s) to apply (e.g., log)
#' @param ... Variables to transform
#' @param .ext Extension to add for each variable
#' @param .vars A list of columns generated by dplyr::vars(), or a character vector of column names, or a numeric vector of column positions.
#'
#' @examples
#' mutate_ext(mtcars, .funs = log, mpg, cyl, .ext = "_ln")
#' mutate_ext(mtcars, .funs = log, .ext = "_ln")
#' mutate_ext(mtcars, .funs = log)
#' mutate_ext(mtcars, .funs = log, .ext = "_ln", .vars = vars(mpg, cyl))
#'
#' @export
mutate_ext <- function(.tbl, .funs, ..., .ext = "", .vars = c()) {
  if (length(.vars) == 0) {
    ## from https://stackoverflow.com/a/35317870/1974918
    .vars <- sapply(substitute(list(...))[-1], deparse)
    if (length(.vars) == 0) {
      .vars <- colnames(.tbl)
    }
  }

  if (is_empty(.ext)) {
    dplyr::mutate_at(.tbl, .vars = .vars, .funs = .funs) %>%
      set_rownames(rownames(.tbl))
  } else {
    new <- gsub("^~", "", .vars) %>% paste0(., .ext)
    .tbl[, new] <- transmute_at(.tbl, .vars = .vars, .funs = .funs) %>%
      set_colnames(new)
    .tbl
  }
}

#' Create quantiles
#'
#' @details Approach used produces results most similar to Stata
#'
#' @param x Numeric variable
#' @param n number of bins to create
#' @param rev Reverse the order of the xtiles
#'
#' @examples
#' xtile(1:10,5)
#' xtile(1:10,5, rev = TRUE)
#'
#' @export
xtile <- function(x, n = 5, rev = FALSE) {
  if (!is.numeric(x)) {
    stop(paste0("The variable to bin must be of type {numeric} but is of type {", class(x)[1], "}"), call. = FALSE)
  } else if (n < 1) {
    stop(paste0("The number of bins must be > 1 but is ", n), call. = FALSE)
  } else if (length(x) < n) {
    stop(paste("The number of bins to create is larger than\nthe number of data points. Perhaps you grouped the data before\ncalling the xtile function and the number of observations per\ngroup is too small"), call. = FALSE)
  }

  breaks <- quantile(x, prob = seq(0, 1, length = n + 1), type = 2)
  if (length(breaks) < 2) stop(paste("Insufficient variation in x to construct", n, "breaks"), call. = FALSE)
  .bincode(x, breaks, include.lowest = TRUE) %>%
    {if (rev) as.integer((n + 1) - .) else .}
}

#' Show all rows with duplicated values (not just the first or last)
#'
#' @details If an entire row is duplicated use "duplicated" to show only one of the duplicated rows. When using a subset of variables to establish uniqueness it may be of interest to show all rows that have (some) duplicate elements
#'
#' @param .tbl Data frame to add transformed variables to
#' @param ... Variables used to evaluate row uniqueness
#'
#' @examples
#' bind_rows(mtcars, mtcars[c(1,5,7),]) %>%
#'   show_duplicated(mpg, cyl)
#' bind_rows(mtcars, mtcars[c(1,5,7),]) %>%
#'   show_duplicated
#'
#' @export
show_duplicated <- function(.tbl, ...) {
  .vars <- sapply(substitute(list(...))[-1], deparse)
  if (length(.vars) == 0 || length(unique(.vars)) == ncol(.tbl)) {
    filter(.tbl, duplicated(.tbl))
  } else {
    .tbl %>%
      group_by_at(.vars = .vars) %>%
      filter(n() > 1) %>%
      mutate(nr_dup = 1:n()) %>%
      arrange_at(.vars = .vars) %>%
      ungroup()
  }
}

#' Weighted standard deviation
#'
#' @details Calculated a weighted standard deviation
#'
#' @param x Numeric vector
#' @param wt Numeric vector of weights
#' @param na.rm Remove missing values (default is TRUE)
#'
#' @export
weighted.sd <- function(x, wt, na.rm = TRUE) {
  if (na.rm) {
    x <- na.omit(x)
    wt <- na.omit(wt)
  }
  wt <- wt / sum(wt)
  wm <- weighted.mean(x, wt)
  sqrt(sum(wt * (x - wm) ^ 2))
}

#' Create data.frame summary
#'
#' @details Used in Radiant's Data > Transform tab
#'
#' @param dataset Data.frame
#' @param dc Class for each variable
#' @param dec Number of decimals to show
#'
#' @export
get_summary <- function(dataset, dc = get_class(dataset), dec = 3) {
  isFct <- "factor" == dc
  isNum <- dc %in% c("numeric", "integer", "Duration")
  isDate <- "date" == dc
  isChar <- "character" == dc
  isLogic <- "logical" == dc
  isPeriod <- "period" == dc
  isTs <- "ts" == dc

  if (sum(isNum) > 0) {
    cn <- names(dc)[isNum]

    cat("Summarize numeric variables:\n")
    select(dataset, which(isNum)) %>%
      gather("variable", "values", !! cn, factor_key = TRUE) %>%
      group_by_at(.vars = "variable") %>%
      summarise_all(
        list(
          n_obs = n_obs,
          n_missing = n_missing,
          n_distinct = n_distinct,
          mean = mean,
          median = median,
          min = min,
          max = max,
          p25 = p25,
          p75 = p75,
          sd = sd,
          se = se
        ),
        na.rm = TRUE
      ) %>%
      data.frame(check.names = FALSE, stringsAsFactors = FALSE) %>%
      format_df(dec = dec, mark = ",") %>%
      set_colnames(c("", colnames(.)[-1])) %>%
      print(row.names = FALSE)
    cat("\n")
  }

  if (sum(isTs) > 0) {
    cn <- names(dc)[isTs]

    cat("Summarize time-series variables:\n")
    lapply(
      select(dataset, which(isTs)),
      function(x) {
        as.data.frame(x) %>%
          summarise_all(
            list(
              n_obs = n_obs,
              n_missing = n_missing,
              n_distinct = n_distinct,
              mean = mean,
              median = median,
              min = min,
              max = max,
              start = ~ attr(., "tsp")[1] %>% round(dec),
              end = ~ attr(., "tsp")[2] %>% round(dec),
              frequency = ~ attr(., "tsp")[3] %>% as.integer()
            ),
            na.rm = TRUE
          )
      }
    ) %>%
      bind_rows() %>%
      data.frame(check.names = FALSE, stringsAsFactors = FALSE) %>%
      data.frame(.vars = cn, .) %>%
      format_df(dec = 3, mark = ",") %>%
      set_colnames(c("", colnames(.)[-1])) %>%
      print(row.names = FALSE)
    cat("\n")
  }

  if (sum(isFct) > 0) {
    cat("Summarize factors:\n")
    select(dataset, which(isFct)) %>% summary(maxsum = 20) %>% print()
    cat("\n")
  }

  if (sum(isDate) > 0) {
    cat("Earliest dates:\n")
    select(dataset, which(isDate)) %>%
      summarise_all(min) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      print(row.names = FALSE)
    cat("\nFinal dates:\n")
    select(dataset, which(isDate)) %>%
      summarise_all(max) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      print(row.names = FALSE)

    cat("\n")
  }

  if (sum(isPeriod) > 0) {
    max_time <- function(x) sort(x) %>% tail(1)
    min_time <- function(x) sort(x) %>% head(1)

    cat("Earliest time:\n")
    select(dataset, which(isPeriod)) %>%
      summarise_all(min_time) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      print(row.names = FALSE)
    cat("\nFinal time:\n")
    select(dataset, which(isPeriod)) %>%
      summarise_all(max_time) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      print(row.names = FALSE)
    cat("\n")
  }

  if (sum(isChar) > 0) {
    ## finding unique elements can be slow for large files
    if (nrow(dataset) < 10^5) {
      cat("Summarize character variables (< 20 unique values shown):\n")
      select(dataset, which(isChar)) %>% lapply(unique) %>% {
        for (i in names(.)) {
          cat(i, paste0("(n_distinct ", length(.[[i]]), "): "), .[[i]][1:min(20, length(.[[i]]))], "\n")
        }
      }
    } else {
      cat("Summarize character variables (< 20 values shown):\n")
      select(dataset, which(isChar)) %>% {
        for (i in names(.)) {
          cat(i, ":", .[[i]][1:min(20, length(.[[i]]))], "\n")
        }
      }
    }
    cat("\n")
  }
  if (sum(isLogic) > 0) {
    cat("Summarize logical variables:\n")
    select(dataset, which(isLogic)) %>%
      summarise_all(list(x = ~ sum(., na.rm = TRUE), y = ~ mean(., na.rm = TRUE), z = ~ n_missing(.))) %>%
      round(dec) %>%
      matrix(ncol = 3) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(c("# TRUE", "% TRUE", "n_missing")) %>%
      set_rownames(names(dataset)[isLogic]) %>%
      format(big.mark = ",", scientific = FALSE) %>%
      print()
    cat("\n")
  }
}

#' Create data.frame from a table
#'
#' @param dataset Data.frame
#' @param freq Column name with frequency information
#'
#' @examples
#' data.frame(price = c("$200","$300"), sale = c(10, 2)) %>% table2data()
#'
#' @export
table2data <- function(dataset, freq = tail(colnames(dataset), 1)) {

  if (!is.numeric(dataset[[freq]])) stop("The frequency variable must be numeric", call = FALSE)
  blowup <- function(i) {
    if (!is.na(dataset[[freq]][i])) dataset[rep(i, each = dataset[[freq]][i]), ]
  }

  lapply(seq_len(nrow(dataset)), blowup) %>%
    bind_rows() %>%
    select_at(.vars = base::setdiff(colnames(dataset), freq)) %>%
    mutate_all(as.factor)
}

#' Generate list of levels and unique values
#'
#' @param dataset A data.frame
#' @param ... Unquoted variable names to evaluate
#'
#' @examples
#' data.frame(a = c(rep("a",5),rep("b",5)), b = c(rep(1,5),6:10)) %>% level_list
#' level_list(mtcars, mpg, cyl)
#'
#' @export
level_list <- function(dataset, ...) {
  fl <- function(x) {
    if ("factor" %in% class(x)) {
      levels(x)
    } else {
      unique(x)
    }
  }
  .vars <- sapply(substitute(list(...))[-1], deparse)
  if (length(.vars) > 0) {
    lapply(select_at(dataset, .vars = .vars), fl)
  } else {
    lapply(dataset, fl)
  }
}

#' Add ordered argument to lubridate::month
#' @param x Input date vector
#' @param label Month as label (TRUE, FALSE)
#' @param abbr Abbreviate label (TRUE, FALSE)
#' @param ordered Order factor (TRUE, FALSE)
#'
#' @importFrom lubridate month
#'
#' @seealso See the \code{\link[lubridate]{month}} function in the lubridate package for additional details
#'
#' @export
month <- function(x, label = FALSE, abbr = TRUE, ordered = FALSE) {
  x <- lubridate::month(x, label = label, abbr = abbr)
  if (!ordered && label) {
    factor(x, ordered = FALSE)
  } else {
    x
  }
}

#' Add ordered argument to lubridate::wday
#' @param x Input date vector
#' @param label Weekday as label (TRUE, FALSE)
#' @param abbr Abbreviate label (TRUE, FALSE)
#' @param ordered Order factor (TRUE, FALSE)
#'
#' @importFrom lubridate wday
#'
#' @seealso See the \code{\link[lubridate:day]{lubridate::wday()}} function in the lubridate package for additional details
#'
#' @export
wday <- function(x, label = FALSE, abbr = TRUE, ordered = FALSE) {
  x <- lubridate::wday(x, label = label, abbr = abbr)
  if (!ordered && label) {
    factor(x, ordered = FALSE)
  } else {
    x
  }
}

#' Remove/reorder levels
#' @details Keep only a specific set of levels in a factor. By removing levels the base for comparison in, e.g., regression analysis, becomes the first level. To relabel the base use, for example, repl = 'other'
#' @param x Character or Factor
#' @param levs Set of levels to use
#' @param repl String (or NA) used to replace missing levels
#'
#' @examples
#' refactor(diamonds$cut, c("Premium","Ideal")) %>% head()
#' refactor(diamonds$cut, c("Premium","Ideal"), "Other") %>% head()
#'
#' @export
refactor <- function(x, levs = levels(x), repl = NA) {
  if (is.character(x)) {
    lv <- unique(x)
    if (length(levs) == 0) levs <- lv
  } else if (is.factor(x)) {
    lv <- levels(x)
  } else {
    return(x)
  }

  if (length(levs) > 0 && length(lv) > length(levs)) {
    if (!is_empty(repl)) levs <- unique(c(repl, levs))
    x <- as_character(x) %>% ifelse(. %in% base::setdiff(lv, levs), repl, .)
  }

  factor(x, levels = levs)
}

###############################
## function below not exported
###############################
.recode. <- function(x, cmd) car::Recode(x, cmd)
