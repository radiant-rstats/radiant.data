# to avoid 'no visible binding for global variable' NOTE
globalVariables(c("r_environment", "session", "r_data", "r_state", ".",
                  ".rs.restartR", "..density..", "Total", "tfoot", "thead",
                  "tr", "th", "variable", "y", "matches"))

#' radiant.data
#'
#' @name radiant.data
#' @docType package
#' @import ggplot2 shiny dplyr
#' @importFrom rlang parse_exprs
#' @importFrom car Recode
#' @importFrom rstudioapi insertText isAvailable
#' @importFrom knitr knit2html knit knit_print
#' @importFrom markdown markdownToHTML
#' @importFrom rmarkdown render html_dependency_bootstrap pdf_document html_document word_document
#' @importFrom pryr where object_size
#' @importFrom magrittr %<>% %T>% %$% set_rownames set_colnames set_names divide_by add extract2
#' @importFrom lubridate is.Date is.POSIXt now year month wday week hour minute second ymd mdy dmy ymd_hms hms hm as.duration parse_date_time
#' @importFrom tibble rownames_to_column as_data_frame data_frame
#' @importFrom tidyr gather_ gather spread spread_ separate
#' @importFrom grid textGrob gpar
#' @importFrom gridExtra grid.arrange
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom readr read_delim write_csv locale problems
#' @importFrom base64enc dataURI
#' @importFrom methods is
#' @importFrom stats as.formula chisq.test dbinom median na.omit quantile sd setNames var weighted.mean
#' @importFrom utils combn head install.packages read.table tail
#' @importFrom import from
#' @importFrom plotly ggplotly subplot
NULL

#' Exporting knit_print from knitr
#' @importFrom knitr knit_print
#' @name knit_print
#' @rdname knit_print
#' @export
NULL

#' Exporting rownames_to_column from tibble
#' @importFrom tibble rownames_to_column
#' @name rownames_to_column
#' @rdname rownames_to_column
#' @export
NULL

#' Exporting data_frame
#' @importFrom tibble data_frame
#' @name data_frame
#' @rdname data_frame
#' @export
NULL

#' Exporting as_data_frame
#' @importFrom tibble as_data_frame
#' @name as_data_frame
#' @rdname as_data_frame
#' @export
NULL

#' Exporting tibble
#' @importFrom tibble tibble
#' @name tibble
#' @rdname tibble
#' @export
NULL

#' Exporting as_tibble
#' @importFrom tibble as_tibble
#' @name as_tibble
#' @rdname as_tibble
#' @export
NULL

#' Exporting tidy from broom
#' @importFrom broom tidy
#' @name tidy
#' @rdname tidy
#' @export
NULL

#' Exporting glance from broom
#' @importFrom broom glance
#' @name glance
#' @rdname glance
#' @export
NULL

#' Exporting the kurtosi function from the psych package
#' @importFrom psych kurtosi
#' @name kurtosi
#' @rdname kurtosi.re
#' @export
NULL

#' Exporting the skew function from the psych package
#' @importFrom psych skew
#' @name skew
#' @rdname skew.re
#' @export
NULL

#' Exporting the ggplotly function from the plotly package
#' @importFrom plotly ggplotly
#' @name ggplotly
#' @rdname ggplotly
#' @export
NULL

#' Exporting the subplot function from the plotly package
#' @importFrom plotly subplot
#' @name subplot
#' @rdname subplot
#' @export
NULL

#' Diamond prices
#' @details A sample of 3,000 from the diamonds dataset bundeled with ggplot2. Description provided in attr(diamonds,"description")
#' @docType data
#' @keywords datasets
#' @name diamonds
#' @usage data(diamonds)
#' @format A data frame with 3000 rows and 10 variables
NULL

#' Survival data for the Titanic
#' @details Survival data for the Titanic. Description provided in attr(titanic,"description")
#' @docType data
#' @keywords datasets
#' @name titanic
#' @usage data(titanic)
#' @format A data frame with 1043 rows and 10 variables
NULL

#' Comic publishers
#' @details List of comic publishers from \url{http://stat545-ubc.github.io/bit001_dplyr-cheatsheet.html}. The dataset is used to illustrate data merging / joining. Description provided in attr(publishers,"description")
#' @docType data
#' @keywords datasets
#' @name publishers
#' @usage data(publishers)
#' @format A data frame with 3 rows and 2 variables
NULL

#' Super heroes
#' @details List of super heroes from \url{http://stat545-ubc.github.io/bit001_dplyr-cheatsheet.html}. The dataset is used to illustrate data merging / joining. Description provided in attr(superheroes,"description")
#' @docType data
#' @keywords datasets
#' @name superheroes
#' @usage data(superheroes)
#' @format A data frame with 7 rows and 4 variables
NULL

#' Avengers
#' @details List of avengers. The dataset is used to illustrate data merging / joining. Description provided in attr(avengers,"description")
#' @docType data
#' @keywords datasets
#' @name avengers
#' @usage data(avengers)
#' @format A data frame with 7 rows and 4 variables
NULL
