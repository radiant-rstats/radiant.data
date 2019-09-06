# to avoid 'no visible binding for global variable' NOTE
globalVariables(
  c(".", "..density..", "r_data", "tfoot", "th", "thead", "tr", "variable", "r_info")
)

#' radiant.data
#'
#' @name radiant.data
#' @docType package
#' @import ggplot2 shiny dplyr
#' @importFrom rlang parse_exprs
#' @importFrom car Recode
#' @importFrom rstudioapi insertText isAvailable
#' @importFrom knitr knit2html knit
#' @importFrom markdown markdownToHTML
#' @importFrom rmarkdown render html_dependency_bootstrap pdf_document html_document word_document
#' @importFrom magrittr %<>% %T>% %$% set_rownames set_colnames set_names divide_by add extract2
#' @importFrom lubridate is.Date is.POSIXt now year month wday week hour minute second ymd mdy dmy ymd_hms hms hm as.duration parse_date_time
#' @importFrom tidyr gather spread separate extract
#' @importFrom gridExtra grid.arrange
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom readr read_delim read_csv write_csv read_rds write_rds locale problems
#' @importFrom readxl read_excel
#' @importFrom base64enc dataURI
#' @importFrom stats as.formula chisq.test dbinom median na.omit quantile sd setNames var weighted.mean
#' @importFrom utils combn head tail install.packages read.table write.table
#' @importFrom import from
#' @importFrom curl curl_download
#' @importFrom writexl write_xlsx
#' @importFrom shinyFiles getVolumes parseDirPath parseFilePaths parseSavePath shinyFileChoose shinyFileSave shinyFilesButton shinyFilesLink shinySaveButton shinySaveLink
#'
NULL

#' Exporting glue from glue
#' @details See \code{\link[glue]{glue}} in the \code{glue} package for more details
#' @importFrom glue glue
#' @name glue
#' @rdname glue
#' @export
NULL

#' Exporting glue_data from glue
#' @details See \code{\link[glue:glue]{glue::glue_data()}} in the \code{glue} package for more details
#' @importFrom glue glue_data
#' @name glue_data
#' @rdname glue_data
#' @export
NULL

#' Exporting glue_collapse from glue
#' @details See \code{\link[glue:glue]{glue::glue_collapse()}} in the \code{glue} package for more details
#' @importFrom glue glue_collapse
#' @name glue_collapse
#' @rdname glue_collapse
#' @export
NULL

#' Exporting knit_print from knitr
#' @details See \code{\link[knitr]{knit_print}} in the \code{knitr} package for more details
#' @importFrom knitr knit_print
#' @name knit_print
#' @rdname knit_print
#' @export
NULL

#' Exporting rownames_to_column from tibble
#' @details See \code{\link[tibble]{rownames}} in the \code{tibble} package for more details
#' @importFrom tibble rownames_to_column
#' @name rownames_to_column
#' @rdname rownames_to_column
#' @export
NULL

#' Exporting tibble from tibble
#' @details See \code{\link[tibble]{tibble}} in the \code{tibble} package for more details
#' @importFrom tibble tibble
#' @name tibble
#' @rdname tibble
#' @export
NULL

#' Exporting as_tibble from tibble
#' @details See \code{\link[tibble]{as_tibble}} in the \code{tibble} package for more details
#' @importFrom tibble as_tibble
#' @name as_tibble
#' @rdname as_tibble
#' @export
NULL

#' Exporting tidy from broom
#' @details See \code{\link[broom]{tidy}} in the \code{broom} package for more details
#' @importFrom broom tidy
#' @name tidy
#' @rdname tidy
#' @export
NULL

#' Exporting glance from broom
#' @details See \code{\link[broom]{glance}} in the \code{broom} package for more details
#' @importFrom broom glance
#' @name glance
#' @rdname glance
#' @export
NULL

#' Exporting kurtosi from psych
# @details See \code{\link[psych:mardia]{psych::kurtosi()}} in the \code{psych} package for more details
#' @details See \code{\link{kurtosi}} in the \code{psych} package for more details
#' @importFrom psych kurtosi
#' @name kurtosi
#' @rdname kurtosi.re
#' @export
NULL

#' Exporting skew from psych
#' @details See \code{\link{skew}} in the \code{psych} package for more details
# @details See \code{\link[psych:mardia]{psych::skew()}} in the \code{psych} package for more details
#' @importFrom psych skew
#' @name skew
#' @rdname skew.re
#' @export
NULL

#' Diamond prices
#' @details A sample of 3,000 from the diamonds dataset bundled with ggplot2. Description provided in attr(diamonds,"description")
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
