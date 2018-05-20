> Create a (reproducible) report using R

The _Report > R_ tab allows you to run R-code with access to all functions and data in Radiant. By clicking the `Knit report (R)` button, the code will be evaluated and the output will be shown on the right of the _Report > R_ page. To evaluate only a part of the code use the cursor to select a section and press `CTRL-enter` (`CMD-enter` on mac).

You can load an R-code file into Radiant by clicking the `Load report` button and selecting an .r or .R file. If you started Radiant from Rstudio you can save a report in HTML, Word, or PDF format by selecting the desired format from the drop-down menu and clicking `Save report`. To save just the code choose `R` from the dropdown and press the `Save report` button.

If you started Radiant from Rstudio, you can also click the `Read files` button to browse for files and generate code to read it into Radiant. For example, read rda, rds, xls, yaml, and feather and add them to the `Datasets` dropdown. If the file type you want to load is not currently supported, the path to the file will be returned. The file path used will be relative to the Rstudio-project root. Paths to files synced to a local Dropbox or Google Drive folder will use the `find_dropbox` and `find_gdrive` functions to enhances reproducibility.

As an example you can copy-and-paste the code below into the editor and press `Knit report (R)` to generate results.

```r
## get the active dataset and show the first few observations
.get_data() %>%
  head()

## access a dataset
diamonds %>%
  select(price, clarity) %>%
  head()

## add a variable to the diamonds data
diamonds <- mutate(diamonds, log_price = log(price))

## show the first observations in the price and log_price columns
diamonds %>%
  select(price, log_price) %>%
  head()

## create a histogram of prices
diamonds %>%
  ggplot(aes(x = price)) +
    geom_histogram()

## and a histogram of log-prices using radiant.data::visualize
visualize(diamonds, xvar = \"log_price\", custom = TRUE)

## open help in the R-studio viewer from Radiant
help(package = \"radiant.data\")

## If you are familiar with Shiny you can call reactives when the code
## is evaluated inside a Shiny app. For example, if you transformed
## some variables in Data > Transform you can call the transform_main
## reacive to see the latest result. Very useful for debugging
# transform_main() %>% head()
```

## Options

The editor used in _Report > Rmd_ and _Report > R_ has several options that can be set in `.Rprofile`.

<pre>
options(radiant.ace_vim.keys = FALSE)
options(radiant.ace_theme = "cobalt")
options(radiant.ace_tabSize = 2)
options(radiant.ace_useSoftTabs = TRUE)
options(radiant.ace_showInvisibles = TRUE)
options(radiant.ace_autoComplete = "live")
</pre>

Notes:

* `vim.keys` enables a set of special keyboard short-cuts. If you have never used VIM you probably don't want this 
* For an overview of available editor themes see: `shinyAce::getAceThemes()`
* Tabs are converted to 2 spaces by default (i.e., 'soft' tabs). You can change the number of spaces used from 2 to, for example, 4
* `showInvisibles` shows tabs and spaces in the editor
* Autocomplete has options "live", "enabled", and "disabled" 
