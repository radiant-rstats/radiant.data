file_upload_button <- function(inputId, label = "",
                               multiple = FALSE,
                               accept = NULL,
                               buttonLabel = "Browse ...",
                               class = "") {

  ## next ... create an upload link https://stackoverflow.com/a/11406690/1974918
  if (length(accept) > 0) {
    accept <- paste(accept, collapse = ",")
  } else {
    accept <- ""
  }

  if (!is_empty(label)) {
    label <-  paste0("</br><label>", label, "</label></br>")
  }

  HTML(
    paste0(label, "
      <label class='input-group-btn'>
        <span class='btn btn-default btn-file-solitary ", class, "'>
          <i class='fa fa-upload'></i>
          ", buttonLabel, "
          <input id='", inputId, "' name='", inputId, "' type='file' style='display: none;' accept='", accept, "'/>
        </span>
      </label>
   ")
  )
}

## replace esc_slash by fixMS if not more formula issues popup
# esc_slash <- function(x) {
#   radiant.data::fixMS(x)
# }

## Thanks to @timelyportfolio for this comment/fix
## https://github.com/timelyportfolio/functionplotR/issues/1#issuecomment-224369431
## needed to include deps in saved reports rendered using rmarkdown
getdeps <- function() {
  htmltools::attachDependencies(
    htmltools::tagList(),
    c(
      htmlwidgets:::getDependency("DiagrammeR", "DiagrammeR"),
      # htmlwidgets:::getDependency("datatables", "DT"),
      htmlwidgets:::getDependency("plotly", "plotly")
    )
  )
}

## get information from rstudio editor
rstudio_context <- function(type = "rmd") {

  rse <- rstudioapi::getSourceEditorContext()
  path <- rse$path
  ext <- tools::file_ext(path)

  if (is_empty(path) || !file.exists(path) || tolower(ext) != type) {
    ## path will be empty of new file hasn't been save yet
    list(path = "", rpath = "", base = "", base_name = "", ext = "", content = "")
  } else {
    path <- normalizePath(path, winslash = "/")
    pdir <- getOption("radiant.project_dir", default = "")

    sel <- rse$selection[[1]][["text"]]
    if (is_empty(sel)) {
      content <- paste0(rse$content, collapse = "\n")
    } else {
      content <- paste0(sel, collapse = "\n")
    }

    base <- basename(path)
    base_name <- sub(paste0(".", ext), "", base)

    rpath <- if (is_empty(pdir)) {
      path
    } else {
      sub(paste0(pdir, "/"), "", path)
    }

    list(
      path = path,
      rpath = rpath,
      base = base,
      base_name = sub(paste0(".", ext, "$"), "", base),
      ext = tolower(ext),
      content = content
    )
  }
}

scrub <- . %>%
  gsub("&lt;!--/html_preserve--&gt;", "", .) %>%
  gsub("&lt;!--html_preserve--&gt;", "", .) %>%
  gsub("&lt;!&ndash;html_preserve&ndash;&gt;", "", .) %>%
  gsub("&lt;!&ndash;/html_preserve&ndash;&gt;", "", .) ## knitr adds this

## cleanout widgets not needed outside shiny apps
cleanout <- . %>%
  gsub("DiagrammeR::renderDiagrammeR", "", .) %>% ## leave for legacy reasons
  gsub("DT::renderDataTable", "", .) ## leave for legacy reasons

setup_report <- function(report, ech,
                         add_yml = TRUE,
                         type = "rmd",
                         save_type = "Notebook",
                         lib = "radiant") {

  report <- fixMS(report) %>%
    sub("^---\n(.*?)\n---", "", .) %>%
    sub("<!--(.*?)-->", "", .)

  ## screenshot option
  sopts <- ifelse(save_type == "PDF", ", screenshot.opts = list(vheight = 1200)", "")

  if (add_yml) {
    if (save_type %in% c("PDF", "Word")) {
      yml <- ""
    } else if (save_type == "HTML") {
      yml <- "---\ntitle: \"\"\noutput:\n  html_document:\n    highlight: textmate\n    theme: spacelab\n    df_print: paged\n    toc: yes\n---\n\n"
    } else if (save_type %in% c("Rmd", "Rmd + Data (zip)")) {
      yml <- "---\ntitle: \"\"\noutput:\n  html_document:\n    highlight: textmate\n    theme: spacelab\n    df_print: paged\n    toc: yes\n    code_folding: hide\n    code_download: true\n---\n\n"
    } else {
      yml <- "---\ntitle: \"\"\noutput:\n  html_notebook:\n    highlight: textmate\n    theme: spacelab\n    toc: yes\n    code_folding: hide\n---\n\n"
    }
  } else {
    yml = ""
  }

  if (missing(ech)) {
    ech <- if (save_type %in% c("PDF", "Word", "HTML")) "FALSE" else "TRUE"
  }

  if (grepl("```{r r_setup, include = FALSE}\n", report, fixed = TRUE)) {
    report
  } else {
    paste0(yml, "```{r r_setup, include = FALSE}
## initial settings
knitr::opts_chunk$set(
  comment = NA,
  echo = ", ech, ",
  error = TRUE,
  cache = FALSE,
  message = FALSE,
  dpi = 200,
  warning = FALSE", sopts, "
)

## width to use when printing tables etc.
options(
  width = 250,
  scipen = 100,
  max.print = 5000,
  stringsAsFactors = FALSE
)

## make all required libraries available by loading radiant package if needed
if (!exists(\"r_environment\")) library(", lib, ")

## load data (add path)
# r_data <- readr::read_rds(\"r_data.rds\")
```

<style>
.table {
  width: auto;
}
ul, ol {
  padding-left: 18px;
}
pre, code, pre code {
  overflow: auto;
  white-space: pre;
  word-wrap: normal;
  background-color: #ffffff;
}
</style>\n\n", report)
}}

## Based on http://stackoverflow.com/a/31797947/1974918
## as of 12/30/2017 doesn't seem to work anymore
knit_it_save <- function(report) {

  ## Read input and convert to Markdown
  md <- knitr::knit(text = report, envir = r_environment)

  ## Get dependencies from knitr
  deps <- knitr::knit_meta()

  ## Convert script dependencies into data URIs, and stylesheet
  ## dependencies into inline stylesheets
  dep_scripts <-
    lapply(deps, function(x) {
      lapply(x$script, function(script) file.path(x$src$file, script))
    }) %>%
    unlist() %>%
    unique()
  dep_stylesheets <-
    lapply(deps, function(x) {
      lapply(x$stylesheet, function(stylesheet) file.path(x$src$file, stylesheet))
    }) %>%
    unlist() %>%
    unique()
  dep_html <- c(
    sapply(dep_scripts, function(script) {
      sprintf(
        '<script type="text/javascript" src="%s"></script>',
        base64enc::dataURI(file = script)
      )
    }),
    sapply(dep_stylesheets, function(sheet) {
      sprintf(
        "<style>%s</style>",
        paste(sshhr(readLines(sheet)), collapse = "\n")
      )
    })
  )

  ## Extract the <!--html_preserve--> bits
  preserved <- htmltools::extractPreserveChunks(md)

  ## Render the HTML, and then restore the preserved chunks
  markdown::markdownToHTML(
    text = preserved$value,
    header = dep_html,
    options = c("mathjax", "base64_images"),
    stylesheet = file.path(getOption("radiant.path.data"), "app/www/bootstrap.min.css")
  ) %>%
    htmltools::restorePreserveChunks(preserved$chunks) %>%
    gsub("<table>", "<table class='table table-condensed table-hover'>", .)
}

## Knit for report in Radiant
knit_it <- function(report, type = "rmd") {

  if (type == "rmd") {
    report <- gsub("\\\\\\\\\\s*\n", "\\\\\\\\\\\\\\\\\n", report)
  }

  ## fragment also available with rmarkdown
  ## http://rmarkdown.rstudio.com/html_fragment_format.html
  pdir <- getOption("radiant.project_dir", default = "")
  if (!is_empty(pdir)) {
    owd <- setwd(pdir)
    on.exit(setwd(owd))
  }

  ## sizing issue with ggplotly and knitr
  ## see https://github.com/ropensci/plotly/issues/1171
  ## see also below unsuccesful fix setting height to 100%
  if (grepl("ggplotly\\(\\)", report)) {
    message("\n\nHeight of ggplotly objects may not be correct in Preview. The height will be correctly displayed in saved reports however.\n\n")
  }

  ## remove yaml headers and html comments and convert to md
  report <- sub("^---\n(.*?)\n---", "", report) %>%
    sub("<!--(.*?)-->", "", .)

  if (!grepl("```{r r_setup, include = FALSE}\n", report, fixed = TRUE)) {
    report <- paste0("```{r knit_it_setup, include = FALSE}\noptions(width = 250, scipen = 100, max.print = 5000, stringsAsFactors = FALSE)\n```\n\n", report)
  }

  ## convert to md
  md <- knitr::knit(
    text = report,
    envir = r_environment,
    quiet = TRUE
  )

  ## add basic styling to tables
  paste(
    markdown::markdownToHTML(text = md, fragment.only = TRUE, stylesheet = ""),
    paste0("<script type='text/javascript' src='", getOption("radiant.mathjax.path"), "/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>"),
    "<script>if (window.MathJax) MathJax.Hub.Typeset();</script>", sep = "\n"
  ) %>%
  gsub("<table>", "<table class='table table-condensed table-hover'>", .) %>%
  ## makes plots full height of screen (i.e., WAY too big)
  # gsub("style=\"width:100%; height:400px; \" class=\"plotly html-widget",
       # "style=\"width:100%; height:100%; \" class=\"plotly html-widget", ., fixed = TRUE) %>%
  scrub() %>%
  HTML()
}

parse_path <- function(path, chr = "\"") {

  ## could probably reduce the number checks for 'try-error' and is_empty
  if (is(path, "try-error") || is_empty(path)) {
    return(
      list(path = "", rpath = "", base = "", base_name = "", ext = "", content = "")
    )
  }

  path <- normalizePath(path[1], winslash = "/")
  filename <- basename(path)
  fext <- tools::file_ext(filename)

  ## objname is used as the name of the data.frame, make case insensitive
  objname <- sub(paste0("\\.", fext, "$"), "", filename, ignore.case = TRUE)
  fext <- tolower(fext)

  pdir <- getOption("radiant.project_dir", default = rstudioapi::getActiveProject())
  dbdir <- getOption("radiant.dropbox_dir", default = radiant.data::find_dropbox())
  gddir <- getOption("radiant.gdrive_dir", default = radiant.data::find_gdrive())

  ## for testing
  # pdir <- ""
  # dbdir <- radiant.data::find_dropbox()
  # gddir <- radiant.data::find_gdrive()

  if (!is_empty(pdir) && grepl(paste0("^", pdir), path)) {
    rpath <- paste0(chr, sub(paste0("^", pdir, "/"), "", path), chr)
  } else if (!is_empty(dbdir) && grepl(paste0("^", dbdir), path)) {
    rpath <- paste0("file.path(find_dropbox(), ", chr, sub(paste0("^", dbdir), "", path), chr, ")")
  } else if (!is_empty(gddir) && grepl(paste("^", gddir), path)) {
    rpath <- paste0("file.path(find_gdrive(), ", chr, sub(paste0("^", gddir), "", path), chr, ")")
  } else {
    rpath <- paste0(chr, path, chr)
  }

  list(path = path, rpath = rpath, filename = filename, fext = fext, objname = objname)
}

read_files <- function(path, type = "rmd", to = "", radiant = TRUE) {

  ## if no path is provided, an interactive file browser will be opened
  if (missing(path)) {
    if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
      path <- try(choose_files(), silent = TRUE)
    } else {
      ## Rstudio's file selector if running radiant in viewer
      path <- rstudioapi::selectFile(
        caption = "Generate to load files",
        filter = "All files (*)"
      )
    }
    if (is(path, "try-error") || is_empty(path)) {
      return("")
    }
  }

  if (is_empty(path)) {
    return("")
  } else {
    pp <- parse_path(path)

    if (to == "") {
      to <- gsub("\\s+", "_", pp$objname)
    }
    if (radiant) {
      ## generate code ## using r_data[["..."]] rather than r_data$... in case
      ## the dataset name has spaces, -, etc.
      to <- paste0("r_data[[\"", to, "\"]]")
    }

    ## see find_gdrive and find_dropbox
    ## gets parsed by report code can't have \\\\ att
    ## for windows in mac in parallel's VM
    path <- gsub("^\\\\", "\\\\\\\\", path)
    if (pp$fext %in% c("rda", "rdata")) {
      if (radiant) {
        cmd <- paste0("## `loadr` will put data in an r_data list by default (see ?radiant.data::loadr)\nloadr(", pp$rpath, ", objname = \"", pp$objname, "\")")
      } else {
        cmd <- paste0("## loaded object names assigned to `obj`\nobj <- load(", pp$rpath, ")\nprint(obj)")
      }
    } else if (pp$fext == "rds") {
      cmd <- paste0(to, " <- readr::read_rds(", pp$rpath, ")\nregister(\"", pp$objname, "\")")
    } else if (pp$fext == "csv") {
      cmd <- paste0(to, " <- readr::read_csv(", pp$rpath, ", n_max = Inf)\nregister(\"", pp$objname, "\")")
    } else if (pp$fext == "tsv") {
      cmd <- paste0(to, " <- readr::read_tsv(", pp$rpath, ")\nregister(\"", pp$objname, "\")")
    } else if (pp$fext %in% c("xls", "xlsx")) {
      cmd <- paste0(to, " <- readxl::read_excel(", pp$rpath, ", sheet = 1)\nregister(\"", pp$objname, "\")")
    } else if (pp$fext == "feather") {
      cmd <- paste0(to, " <- feather::read_feather(", pp$rpath, ", columns = c())\nregister(\"", pp$objname, "\", desc = feather::feather_metadata(\"", pp$path, "\")$description)")
    } else if (pp$fext == "yaml") {
      cmd <- paste0(to, " <- yaml::yaml.load_file(", pp$rpath, ")\nregister(\"", pp$objname, "\")") %>%
       paste0("\n", pp$objname, " <- ", "\"\n", paste0(readLines(pp$path), collapse = "\n"),"\n\"")
    } else if (grepl("sqlite", pp$fext)) {
      obj <- paste0(pp$objname, "_tab1")
      cmd <- "## see https://db.rstudio.com/dplyr/\n" %>%
        paste0("library(DBI)\ncon <- dbConnect(RSQLite::SQLite(), dbname = ", pp$rpath, ")\n(tables <- dbListTables(con))\n", obj, " <- tbl(con, from = tables[1])")
      if (radiant) {
        cmd <- paste0(cmd, "\n", sub(pp$objname, obj, to), " <- collect(", obj, ")\nregister(\"", obj, "\")")
      }
    } else if (pp$fext == "sql") {
      cmd <- "## see http://rmarkdown.rstudio.com/authoring_knitr_engines.html\n" %>%
       paste0(paste0(readLines(pp$path), collapse = "\n"))
      return(paste0("\n```{sql, connection = con, max.print = 20, output.var = \"", make.names(pp$objname), "\"}\n", cmd, "\n```\n"))
    } else if (pp$fext %in% c("py", "css", "js")) {
      cmd <- "## see http://rmarkdown.rstudio.com/authoring_knitr_engines.html\n" %>%
        paste0(paste0(readLines(pp$path), collapse = "\n"))
      return(paste0("\n```{", sub("py", "python", pp$fext), "}\n", cmd, "\n```\n"))
    } else if (pp$fext %in% c("md", "rmd") && type == "rmd") {
      return(paste0("\n```{r child = ", pp$rpath, "}\n```\n"))
    } else if (pp$fext %in% c("jpg", "jpeg", "png", "pdf") && type == "rmd") {
      cmd <- paste0("\n![](`r ", pp$rpath, "`)\n")
      if (!grepl("file.path", cmd)) cmd <- sub("`r \"", "", cmd) %>% sub("\"`", "", .)
      return(cmd)
    } else if (pp$fext %in% c("r", "R")) {
      cmd <- paste0("source(", pp$rpath, ", local = TRUE, echo = TRUE)")
    } else {
      cmd <-pp$rpath
    }

    if (!radiant) { ## nothing to register
      cmd <- gsub("\nregister\\(.*\\)","", cmd)
    }

    if (type == "rmd") {
      paste0("\n```{r}\n", cmd, "\n```\n")
    } else {
      paste0("\n", cmd, "\n")
    }
  }
}

sans_ext <- function(path) {
  sub("(\\.rda$|\\.rds$|\\.rmd$|\\.r$|\\.rdata$)", "", path, ignore.case = TRUE)
}

report_name <- function(type = "rmd", out = "report", full.name = FALSE) {

  ldir <- getOption("radiant.launch_dir", default = "~")
  pdir <- getOption("radiant.project_dir", default = ldir)

  ## generate report name based on state or project name
  if (input[[paste0(type, "_generate")]] %in% c("To Rmd", "To R")) {
    fn <- r_state[[paste0("radiant_", type, "_name")]]
  } else {
    fn <- ""
  }

  if (is_empty(fn)) {
    fn <- state_name()
    fn <- sans_ext(fn) %>%
      sub("-state", paste0("-", out), .)

    r_state[[paste0("radiant_", type, "_name")]] <<-
      paste(fn, sep = ".", switch(
        type,
        rmd = "Rmd",
        r = "R"
      ))
  } else {
    fn <- basename(fn) %>%
      sans_ext()
  }

  if (full.name) {
    file.path(pdir, fn)
  } else {
    fn
  }
}

report_save_filename <- function(type = "rmd", full.name = TRUE) {

  if (input[[paste0(type, "_generate")]] %in% c("To Rmd", "To R")) {
    cnt <- rstudio_context(type = type)
    if (!is_empty(cnt$path)) {
      if (cnt$path != cnt$rpath) {
        r_state[[paste0("radiant_", type, "_name")]] <<- cnt$rpath
      } else {
        r_state[[paste0("radiant_", type, "_name")]] <<- cnt$path
      }

      if (full.name) {
        fn <- cnt$path
      } else {
        fn <- cnt$base_name
      }

    } else {
      fn <- report_name(type = type, full.name = full.name)
    }
  } else {
    fn <- report_name(type = type, full.name = full.name)
  }

  fn <- sans_ext(fn)

  paste(fn, sep = ".", switch(
    input[[paste0(type, "_save_type")]],
    Notebook = "nb.html",
    HTML = "html",
    PDF = "pdf",
    Word = "docx",
    Rmd = "Rmd",
    `Rmd + Data (zip)` = "zip",
    R = "R",
    `R + Data (zip)` = "zip"
  ))
}

report_save_content <- function(file, type = "rmd") {

  if (isTRUE(getOption("radiant.report"))) {
    isolate({
      ldir <- getOption("radiant.launch_dir", default = "~/")
      pdir <- getOption("radiant.project_dir", default = ldir)

      tdir <- tempdir()
      owd <- ifelse(is_empty(pdir), setwd(tdir), setwd(pdir))
      on.exit(setwd(owd))

      save_type <- input[[paste0(type, "_save_type")]]
      generate <- input[[paste0(type, "_generate")]]

      zip_info <- getOption("radiant.zip")
      if (save_type %in% c("Rmd + Data (zip)", "R + Data (zip)")) {
        if (zip_info[1] == "") {
          ## No zip warning
          showModal(
            modalDialog(
              title = "ZIP attempt failed",
              span(
                "There is no zip utility in the path on this system. Please install a zip utility (e.g., 7-zip) and try again"
              ),
              footer = modalButton("OK"),
              size = "s",
              easyClose = TRUE
            )
          )
          return(invisible())
        }
      }

      lib <- if ("radiant" %in% installed.packages()) "radiant" else "radiant.data"

      if (generate %in% c("To Rmd", "To R")) {
        cnt <- rstudio_context(type)
        if (is_empty(cnt$path) || !cnt$ext == type) {
          if (generate == "To Rmd") {
            report <- "#### Radiant is set to use an rmarkdown document in Rstudio ('To Rmd').\n#### Please check that you have an .Rmd file open in Rstudio and that the file has been saved to disk.\n#### If you want to use the editor in Radiant instead, change 'To Rmd' to 'Auto paste' or 'Manual paste'."
          } else {
            report <- "#### Radiant is set to use an R-code document in Rstudio ('To R').\n#### Please check that you have an .R file open in Rstudio and that the file has been saved to disk.\n#### If you want to use the editor in Radiant instead, change 'To R' to 'Auto paste' or 'Manual paste'."
          }
        } else {
          report <- cnt$content
        }
      } else {
        report <- input[[paste0(type, "_edit")]]
      }

      if (save_type == "Rmd + Data (zip)") {
        withProgress(message = "Preparing Rmd + Data zip file", value = 1, {
          r_data <- toList(r_data)

          ## don't want to write to current dir
          currdir <- setwd(tempdir())
          readr::write_rds(r_data, path = "r_data.rds")

          setup_report(report, save_type = "Rmd", lib = lib) %>%
            fixMS() %>%
            cat(file = "report.Rmd", sep = "\n")

          zip(file, c("report.Rmd", "r_data.rds"),
            flags = zip_info[1], zip = zip_info[2]
          )
          setwd(currdir)
        })
      } else if (save_type == "R + Data (zip)") {
        withProgress(message = "Preparing R + Data zip file", value = 1, {
          r_data <- toList(r_data)

          ## don't want to write to current dir
          currdir <- setwd(tempdir())
          readr::write_rds(r_data, path = "r_data.rds")
          cat(report, file = "report.R", sep = "\n")

          zip(file, c("report.R", "r_data.rds"),
            flags = zip_info[1], zip = zip_info[2]
          )
          setwd(currdir)
        })
      } else if (save_type == "Rmd") {
        setup_report(report, save_type = "Rmd", lib = lib) %>%
          fixMS() %>%
          cat(file = file, sep = "\n")
      } else if (save_type == "R") {
        cat(report, file = file, sep = "\n")
      } else {

        ## hack for rmarkdown from Report > Rmd and Report > R
        # options(radiant.rmarkdown = TRUE)
        options(radiant.rmarkdown = TRUE)

        if (type == "r") {
          report <- paste0("\n```{r echo = TRUE}\n", report, "\n```\n")
        }

        init <- setup_report(fixMS(report), save_type = save_type, lib = lib)

        ## on linux ensure you have you have pandoc > 1.14 installed
        ## you may need to use http://pandoc.org/installing.html#installing-from-source
        ## also check the logs to make sure its not complaining about missing files
        withProgress(message = paste0("Saving report to ", save_type), value = 1, {
          if (isTRUE(rmarkdown::pandoc_available())) {
            ## have to use current dir so (relative) paths work properly
            tmp_fn <- tempfile(pattern = "report-", tmpdir = ".", fileext = ".Rmd")
            cat(fixMS(init), file = tmp_fn, sep = "\n")
            out <- rmarkdown::render(tmp_fn, switch(save_type,
              Notebook = rmarkdown::html_notebook(highlight = "textmate", theme = "spacelab", code_folding = "hide"),
              HTML = rmarkdown::html_document(highlight = "textmate", theme = "spacelab", code_download = TRUE, df_print = "paged"),
              PDF = rmarkdown::pdf_document(),
              Word = rmarkdown::word_document(reference_docx = file.path(system.file(package = "radiant.data"), "app/www/style.docx"))
            ), envir = r_environment, quiet = TRUE)
            file.rename(out, file)
            file.remove(tmp_fn)
          } else {
            ## still needed because rmarkdown requires pandoc
            setup_report(report, add_yml = FALSE, type = save_type, lib = lib) %>%
              knit_it_save() %>%
              cat(file = file, sep = "\n")
          }
        })

        ## hack for rmarkdown from Report > Rmd and Report > R
        # options(radiant.rmarkdown = FALSE)
        options(radiant.rmarkdown = FALSE)
      }
    })
  }
}

## updating the report when called
update_report <- function(inp_main = "",
                          fun_name = "",
                          inp_out = list("", ""),
                          cmd = "",
                          pre_cmd = "result <- ",
                          post_cmd = "",
                          xcmd = "",
                          outputs = c("summary", "plot"),
                          wrap,
                          figs = TRUE,
                          fig.width = 7,
                          fig.height = 7) {

  ## determine number of characters for main command for wrapping
  if (missing(wrap)) {
    lng <- nchar(pre_cmd) + nchar(fun_name) + nchar(post_cmd) + 2
    if (!is_empty(inp_main)) {
      lng <- lng + sum(nchar(inp_main)) +
        sum(nchar(names(inp_main))) +
        length(inp_main) * 5 - 1
    }
    wrap <- ifelse(lng > 75, TRUE, FALSE)
  }

  ## wrapping similar to styler
  depr <- function(x, wrap = FALSE) {
    if (wrap) {
      for (i in names(x)) {
        tmp <- x[[i]]
        wco <- ifelse(max(nchar(tmp)) > 20, 20L, 55L)
        tmp <- deparse(tmp, control = "keepNA", width.cutoff = wco)
        if ((nchar(i) + sum(nchar(tmp)) < 70) | (length(tmp) == 2 & tmp[2] == ")")) {
          tmp <- paste0(tmp, collapse = "")
        }
        if (length(tmp) > 1) {
          tmp <- c("c(", sub("^c\\(", "", tmp))
          if (tail(tmp, 1) != ")") {
            tmp <- c(sub("\\)$", "", tmp), ")")
          }
        }
        x[[i]] <- paste0(tmp, collapse = "\n    ") %>%
          sub("[ ]+\\)", "  \\)", .)
      }
      x <- paste0(paste0(paste0("\n  ", names(x)), " = ", x), collapse = ", ")
      x <- paste0("list(", x, "\n)")
    } else {
      x <- deparse(x, control = c("keepNA"), width.cutoff = 500L) %>%
        paste(collapse = "")
    }
    x
  }

  if (inp_main[1] != "") {
    cmd <- depr(inp_main, wrap = wrap) %>%
      sub("list", fun_name, .) %>%
      paste0(pre_cmd, .) %>%
      paste0(., post_cmd)
  }

  lout <- length(outputs)
  if (lout > 0) {
    for (i in 1:lout) {
      inp <- "result"
      if ("result" %in% names(inp_out[[i]])) {
        inp <- inp_out[[i]]["result"]
        inp_out[[i]]["result"] <- NULL
      }
      if (inp_out[i] != "" && length(inp_out[[i]]) > 0) {
        cmd <- deparse(inp_out[[i]], control = c("keepNA"), width.cutoff = 500L) %>%
          sub("list\\(", paste0(outputs[i], "\\(", inp, ", "), .) %>%
          paste0(cmd, "\n", .)
      } else {
        cmd <- paste0(cmd, "\n", outputs[i], "(", inp, ")")
      }
    }
  }

  if (xcmd != "") cmd <- paste0(cmd, "\n", xcmd)

  ## make into chunks if needed
  type <- ifelse(state_init("rmd_generate", "auto") == "Use R", "r", "rmd")

  if (type == "r") {
    update_report_fun(cmd, type = "r")
  } else {
    if (figs) {
      cmd <- paste0("\n```{r fig.width = ", round(7 * fig.width / 650, 2), ", fig.height = ", round(7 * fig.height / 650, 2), ", dpi = 200}\n", cmd, "\n```\n")
    } else {
      cmd <- paste0("\n```{r}\n", cmd, "\n```\n")
    }
    update_report_fun(cmd, type = "rmd")
  }
}

update_report_fun <- function(cmd, type = "rmd", read_files = FALSE) {
  isolate({
    generate <- paste0(type, "_generate")
    sinit <- state_init(generate, "auto")
    editor <- paste0(type, "_edit")
    sel <- ifelse(type == "rmd", "Rmd", "R")
    if (sinit == "manual") {
      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        cat(cmd, file = "clipboard")
      } else if (os_type == "Darwin") {
        out <- pipe("pbcopy")
        cat(cmd, file = out)
        close(out)
      } else if (os_type == "Linux") {
        cat("Clipboard not supported on linux")
      }
      withProgress(message = "Putting command in clipboard", value = 1, {
        cat("")
      })
    } else if (sinit == "To Rmd") {
      withProgress(message = "Putting code chunk in Rstudio", value = 1, {
        rstudioapi::insertText(Inf, fixMS(cmd))
      })
    } else if (sinit == "To R") {
      withProgress(message = "Putting R-code in Rstudio", value = 1, {
        gsub("(```\\{.*\\}\n)|(```\n)", "", fixMS(paste0("\n", cmd, "\n"))) %>%
          rstudioapi::insertText(Inf, .)
      })
    } else {
      if (is_empty(r_state[[editor]])) {
        r_state[[editor]] <<- paste0("## Your report title\n\n", cmd)
      } else {
        r_state[[editor]] <<- paste0(fixMS(r_state[[editor]]), "\n", cmd)
      }
      withProgress(message = paste0("Updating Report > ", sel), value = 1, {
        shinyAce::updateAceEditor(
          session, editor,
          value = fixMS(r_state[[editor]])
        )
      })
    }

    if (!read_files) {
      if (state_init(paste0(type, "_switch"), "switch") == "switch") {
        updateTabsetPanel(session, "nav_radiant", selected = sel)
      }
    }
  })
}
