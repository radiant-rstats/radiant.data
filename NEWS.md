# CHANGES IN radiant.data VERSION 0.7.2

## NEW FEATURES

- Specify the maximum number of rows to load for a csv and csv (url) file through _Data > Manage_
- Support for loading and saving feather files, including specifying the maximum number of rows to load through _Data > Manage_
- Added author and year arguments to help modals in inst/app/radiant.R (thanks @kmezhoud)
- Added size argument for scatter plots to create bubble charts (thanks @andrewsali)
- Example and CSS formatting for tables in R > Report
- Added `seed` argument to `make_train`
- Added `prop`, `sdprop`, etc. for working with proportions

## BUG FIXES

- When clicking the `rename` button, without changing the name, the dataset was set to NULL (thanks @kmezhoud, https://github.com/radiant-rstats/radiant/issues/5)
- Replace ext with .ext in `mutate_each` function call
- Variance estimation in Data > Explore would cause an error with unit cell-frequencies (thanks @kmezhoud, https://github.com/radiant-rstats/radiant/issues/6)
- Fix for as_integer when factor levels are characters
- Fix for integer conversion in explore
- Remove \\r and special characters from strings in r_data and r_state 
- Fix sorting in _R > Report_ for tables created using _Data > Pivot_ and _Data > Explore_ when column headers contain symbols or spaces (thanks @4kammer)
- Set `error = TRUE` for rmarkdown for consistency with knitr as used in R > Report
- Correctly handle decimal indicators when loading csv files in _Data > Manage_
- Don't overwrite a dataset to combine if combine generates an error when user sets the the name of the combined data to that of an already selected dataset
- When multiple variables were selected, data were not correctly summarized in Data > Transform
- Add (function) lable to bar plot when x-variable is an integer

## Deprecated
- varp_rm deprecated in favor of varpop 
- sdp_rm deprecated in favor of sdpop 
