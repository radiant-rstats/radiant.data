# CHANGES IN radiant.data 0.9.0.22

* Fix for [#43](https://github.com/radiant-rstats/radiant/issues/43) where scatter plot was not shown for a dataset with less than 1,000 rows
* Updated equation example in _Report > Rmd_
* Fix for _Report > Rmd_ and _Report > R_ when R-code or Rmarkdown is being pulled from the Rstudio editor

# CHANGES IN radiant.data 0.9.0.17

* Use thousand separator for `summary.pivotr` and `summary.explore`
* Fix in code-generation for `table2data`

# CHANGES IN radiant.data 0.9.0.16

* Changed license for help files and images for radiant.data to [CC-BY-SA](https://creativecommons.org/licenses/by-sa/4.0/legalcode)

# CHANGES IN radiant.data 0.9.0.15

* Allow all textarea inputs and multi-select inputs to be resized manually by the user
* Fix for `rounddf` to ignore dates
* Use 200 dpi for plots in _Report > Rmd_ and _Report > R_
* _Data > Vizualize_ now has an option to select a sample of data for scatter plots (e.g., 1K, 5K, 10K, or All)

# CHANGES IN radiant.data 0.9.0.13

* Apply `fixMS` to replace curly quotes, em dash, etc. when using _Data > Transform > Create_
* Option to set number of decimals to show in _Data > View_ 
* Improved number formatting in interactive tables in _Data > View_, _Data > Pivot_, and _Data > Explore_
* Fix for decimals to show in interactive tables _Report > Rmd_ and saved HTML reports
* Option to include an interactive view of a dataset in _Report > Rmd_. By default, the number of rows is set to 100 as, most likely, the user will not want to embed a large dataset in save HTML report
* Better error messages for `xtile` and when binning data with too many groups
* _Data > Transform_ will leave variables selected, unless switching to `Create` or `Spread`
* Fix for variable type warnings in _Data > Pivot_ when filtering the table
* Switch focus to editor in _Report > Rmd_ and _Report > R_ when no other input has focus
* Fix for \ in equations in _Report > Rmd_

# CHANGES IN radiant.data 0.9.0.7

* Allow response variables with NA values in _Model > Logistic regression_ and other classification models
* Support logicals in code generation from _Data > View_
* Track window size using `input$get_screen_width`
* Fix to include `DiagrammeR` based plots in Rmarkdown reports
* Focus on editor when switching to _Report > Rmd_ or _Report > R_ so generated code is shown immediately and the user can navigate and type in the editor without having to click first
* Cleanup now also occurs when the stop button is used in Rstudio to close the app
* Fix in `read_files` for SQLite data names
* De-activate spellcheck autocorrection in `selectizeInput` in Rstudio Viewer [shiny #1916](https://github.com/rstudio/shiny/issues/1916)
* Fix to allow selecting and copying text output from _Report > Rmd_ and _Report > R_
* Remove "fancy" quotes from filters
* Add information about the first level when plotting a bar chart with a categorical variable on the Y-axis (e.g., mean(buyer {yes}))
* Known issue: The Rstudio viewer may not always close the viewer window when trying to stop the application with the `Stop` link in the navbar. As a work-around, use Rstudio's stop buttons instead. 

# CHANGES IN radiant.data 0.9.0

* Show Rstudio project information in navbar if available
* If Rstudio project is used _Report > Rmd_ and _Report > R_ will use the project directory as base. This allows users to use relative paths and making it easier to share (reproducible) code
* Specify options in .Rprofile for upload memory limit and running _Report > Rmd_ on server
* `find_project` function based on `rstudioapi`
* Overflow `pre` and `code` blocks in HTML reports generated in _Report > Rmd_
* Read rdata files through _Data > Manage_
* _Report > Rmd_ option to view Editor, Preview, or Both
* _Report > Rmd_ Read button to generate code to load various types of data (e.g., rda, rds, xls, yaml, feather)
* _Report > Rmd_ Read button to generate code to load various types of files in report (e.g., jpg, png, md, Rmd, R). If Radiant was started from an Rstudio project, the file paths used will be relative to the project root. Paths to files synced to local Dropbox or Google Drive folder will use the `find_dropbox` and `find_gdrive` functions to enhances reproducibility.
* _Report > Rmd_ Load Report button can be used to load Rmarkdown file in the editor. It will also extract the source code from Notebook and HTML files with embedded Rmarkdown
* _Report > Rmd_ will read Rmd directly from Rstudio when "To Rstudio (Rmd)" is selected. This will make it possible to use Rstudio Server Pro's _Share project_ option for realtime collaboration in Radiant
* Long lines of code generated for _Report > Rmd_ will be wrapped to enhance readability 
* _Report > R_ is now equivalent to _Report > Rmd_ but in R-code format
* Enhance keyboard shortcuts
* Enhanced editing features in _Report > Rmd_ and _Report > R_ based on updates to `shinyAce`

# CHANGES IN radiant.data 0.8.7.8

* Added preview options to _Data > Manage_ based on https://github.com/radiant-rstats/radiant/issues/30
* Add selected dataset name as default table download name in _Data > View_, _Data > Pivot_, and _Data > Explore_
* Use "stack" as the default for histograms and frequency charts in _Data > Visualize_
* Fix for large numbers in _Data > Explore_ that could cause an integer overflow
* Cleanup `Stop & Report` option in navbar

# CHANGES IN radiant.data 0.8.7.4

* Upgraded tidyr dependency to 0.7

# CHANGES IN radiant.data 0.8.7.1

* Upgraded dplyr dependency to 0.7.1

# CHANGES IN radiant.data 0.8.6

## NEW FEATURES

* Export `ggplotly` from `plotly` for interactive plots in _Report > Rmd_
* Export `subplot` from `plotly` for grids of interactive plots in _Report > Rmd_
* Set default `res = 96` for `renderPlot` and `dpi = 96` for `knitr::opts_chunk`
* Add `fillcol`, `linecol`, and `pointcol` to `visualize` to set plot colors when no `fill` or `color` variable has been selected
* Reverse legend ordering in _Data > Visualize_ when axes are flipped using `coor_flip()`
* Added functions to choose.files and choose.dir. Uses JavaScript on Mac, utils::choose.files and utils::choose.dir on Windows, and reverts to file.choose on Linux
* Added `find_gdrive` to determine the path to a user's local Google Drive folder if available
* `fixMs` for encoding in reports on Windows

## BUG FIXES

* Chi-sqaure results were not displayed correctly in _Data > Pivot_
* Fix for `state_multiple`

# CHANGES IN radiant.data 0.8.1

## NEW FEATURES

- Specify the maximum number of rows to load for a csv and csv (url) file through _Data > Manage_
- Support for loading and saving feather files, including specifying the maximum number of rows to load through _Data > Manage_
- Added author and year arguments to help modals in inst/app/radiant.R (thanks @kmezhoud)
- Added size argument for scatter plots to create bubble charts (thanks @andrewsali)
- Example and CSS formatting for tables in _Report > Rmd_
- Added `seed` argument to `make_train`
- Added `prop`, `sdprop`, etc. for working with proportions
- Set `ylim` in `visualize` for multiple plots
- Show progress indicator when saving reports from _Report > Rmd_
- `copy_attr` convenience function
- `refactor` function to keep only a subset of levels in a factor and recode the remaining (and first) level to, for example, other
- `register` function to add a (transformed) dataset to the dataset dropdown
- Remember name of state files loaded and suggest that name when re-saving the state
- Show dataset name in output if dataframe passed directly to analysis function
- R-notebooks are now the default option for output saved from _Report > Rmd_ and _Report > R_
- Improved documentation on how to customize plots in _Report > Rmd_
- Keyboard short-cut to put code into _Report > Rmd_ (ALT-enter)

## BUG FIXES

- When clicking the `rename` button, without changing the name, the dataset was set to NULL (thanks @kmezhoud, https://github.com/radiant-rstats/radiant/issues/5)
- Replace ext with .ext in `mutate_each` function call
- Variance estimation in Data > Explore would cause an error with unit cell-frequencies (thanks @kmezhoud, https://github.com/radiant-rstats/radiant/issues/6)
- Fix for as_integer when factor levels are characters
- Fix for integer conversion in explore
- Remove \\r and special characters from strings in r_data and r_state 
- Fix sorting in _Report > Rmd_ for tables created using _Data > Pivot_ and _Data > Explore_ when column headers contain symbols or spaces (thanks @4kammer)
- Set `error = TRUE` for rmarkdown for consistency with knitr as used in _Report > Rmd_
- Correctly handle decimal indicators when loading csv files in _Data > Manage_
- Don't overwrite a dataset to combine if combine generates an error when user sets the the name of the combined data to that of an already selected dataset
- When multiple variables were selected, data were not correctly summarized in Data > Transform
- Add (function) lable to bar plot when x-variable is an integer
- Maintain order of variables in Data > Visualize when using "color", "fill", "comby", or "combx"
- Avoid warning when switching datasets in Data > Transform and variables being summarized do not exists in the new dataset
- which.pmax produced a list but needed to be integer
- To customized predictions in radiant.model indexr must be able to customize the prediction dataframe
- describe now correctly resets the working directory on exit
- removed all calls to summarise_each and mutate_each from dplyr

## Deprecated
- varp_rm has been deprecated in favor of varpop 
- sdp_rm has been deprecated in favor of sdpop 
- mutate_each has been deprecated in favor of mutate_at, mutate_all, and radiant.data::mutate_ext
