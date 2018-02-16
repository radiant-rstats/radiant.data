> Create a (reproducible) report using R

The code feature allows you to run R-code with access to all functions and data in Radiant. By clicking the `Knit report (R)` button, the code will be evaluated and the output will be shown on the right of the _Report > R_ page. To evaluate only a part of the code use the cursor to select a section and press `CTRL-enter` (`CMD-enter` on mac).

You can load an R-code file into Radiant by clicking the `Load report` button and selecting an .r or .R file. If you started Radiant from Rstudio you can save a report in HTML, Word, or PDF format by selecting the desired format from the drop-down manu and clicking `Save report`. To save just the code choose `R` from the dropdown and press the `Save reporet` button.

If you started Radiant from Rstudio, you can also click the `Read files` button to browse for files and generate code to read it into Radiant. For example, read rda, rds, xls, yaml, and feather and add them to the `Datasets` dropdown. If the file type you want to load is not currently supported, the path to the file will be returned. If Radiant was started from an Rstudio project, the file paths used will be relative to the project root. Paths to files synced to local Dropbox or Google Drive folder will use the `find_dropbox` and `find_gdrive` functions to enhances reproducibility.

As an example you can copy-and-paste the code below into the editor and press `Knit report (R)` to generate results.

```r
## get the active dataset and show the first few observations
.getdata() %>% head

## access a specific dataset by name
r_data[['diamonds']] %>% select(price, clarity) %>% head

## add a variable to the diamonds data
dat <- r_data[['diamonds']]
dat$log_price <- log(dat$price)

## show the first observations
dat %>% select(price, log_price) %>% head

## create a histogram of prices
dat %>% ggplot(aes(x = price)) + geom_histogram()

## and a histogram of log-prices
dat %>% ggplot(aes(x = log_price)) + geom_histogram()

## open help in the R-studio viewer from Radiant
help(package = 'radiant.data')

## if you are familiar with Shiny you can call reactives here
## for example, if you just transformed some variables in Data > Transform
## you can call the transform_main reacive to see the latest result
## this can very useful for debugging
# transform_main() %>% head
```

## Options

The editor used in _Report > Rmd_ and _Report > R_ has several options that can be set in `.Rprofile`.

<pre>
options(radiant.vim.keys = FALSE)
options(radiant.ace_theme = "cobalt")
options(radiant.ace_tabSize = 2)
options(radiant.ace_showInvisibles = TRUE)
options(radiant.ace_autocomplete = "live")
</pre>

Notes:

* `vim.key` enables a variety of keyboard short-cuts. If you have never used VIM you probably don't want this 
* For an overview of available themes see: `shinyAce::getAceThemes()`
* Autocomplete has options "live", "enabled", and "disabled" 
* `showInvisibles` shows tabs and spaces in the editor
* Tabs are converted to 2 spaces by default. Change the number of spaces by changing this to, for example, 4
