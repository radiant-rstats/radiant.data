file_upload_button <- function(
  inputId, label = "", multiple = FALSE,
  accept = NULL, buttonLabel = "Load", title = "Load data",
  class = "", icn = "upload", progress = FALSE
) {

  if (getOption("radiant.shinyFiles", FALSE)) {
    shinyFiles::shinyFileChoose(
      input = input,
      id = inputId,
      session = session,
      roots = sf_volumes,
      filetype = gsub(".", "", accept, fixed = TRUE)
    )

    # actionButton(inputId, buttonLabel, icon = icon(icn), class = class)
    shinyFiles::shinyFilesButton(
      inputId, buttonLabel, label, title = title, 
      multiple = FALSE, class = class, icon = icon(icn)
    )
  } else {
    if (length(accept) > 0) {
      accept <- paste(accept, collapse = ",")
    } else {
      accept <- ""
    }

    if (!is_empty(label)) {
      label <-  paste0("</br><label>", label, "</label></br>")
    }

    btn <- paste0(label, "
        <label class='input-group-btn'>
          <span class='btn btn-default btn-file-solitary ", class, "'>
            <i class='fa fa-upload'></i>
            ", buttonLabel, "
            <input id='", inputId, "' name='", inputId, "' type='file' style='display: none;' accept='", accept, "'/>
          </span>
        </label>
     ")

    if (progress) {
      btn <- paste0(btn, "\n<div id='uploadfile_progress' class='progress progress-striped active shiny-file-input-progress'>
        <div class='progress-bar'></div>
      </div>")
    }

    HTML(btn)
  }
}

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

setup_report <- function(
  report, ech, add_yml = TRUE, type = "rmd",
  save_type = "Notebook", lib = "radiant"
) {

  report <- fix_smart(report) %>%
    sub("^---\n(.*?)\n---", "", .) %>%
    sub("<!--(.*?)-->", "", .)

  ## screenshot option
  sopts <- ifelse(save_type == "PDF", ",\n  screenshot.opts = list(vheight = 1200)", "")

  if (add_yml) {
    if (save_type %in% c("PDF", "Word")) {
      yml <- ""
    } else if (save_type == "HTML") {
      yml <- '---\noutput:\n  html_document:\n    highlight: textmate\n    theme: spacelab\n    df_print: paged\n    toc: yes\n---\n\n'
    } else if (save_type %in% c("Rmd", "Rmd + Data (zip)")) {
      yml <- '---\noutput:\n  html_document:\n    highlight: textmate\n    theme: spacelab\n    df_print: paged\n    toc: yes\n    code_folding: hide\n    code_download: true\n---\n\n'
    } else {
      yml <- '---\noutput:\n  html_notebook:\n    highlight: textmate\n    theme: spacelab\n    toc: yes\n    code_folding: hide\n---\n\n'
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
  message = FALSE,\n 
  dpi = 144,
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

## include code to load the data you require
## for interactive use attach the r_data environment
# attach(r_data)
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
  md <- knitr::knit(text = report, envir = r_data)

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

observeEvent(input$report_clean, {
  withProgress(message = "Cleaning report", value = 1, {
    report <- gsub("\nr_data\\[\\[\"([^\n]+?)\"\\]\\] \\%>\\%(.*?)\\%>\\%\\s*?store\\(\"(.*?)\", (\".*?\")\\)", "\n\\3 <- \\1 %>%\\2\nregister(\"\\3\", \\4)", input$rmd_edit) %>% 
      gsub("r_data\\[\\[\"([^\"]+?)\"\\]\\]", "\\1", .) %>%
      gsub("r_data\\$", "", .) %>%
      gsub("\"mean_rm\"", "\"mean\"", .) %>%
      gsub("\"median_rm\"", "\"median\"", .) %>%
      gsub("\"min_rm\"", "\"min\"", .) %>%
      gsub("\"max_rm\"", "\"max\"", .) %>%
      gsub("\"sd_rm\"", "\"sd\"", .) %>%
      gsub("\"var_rm\"", "\"var\"", .) %>%
      gsub("\"sum_rm\"", "\"sum\"", .) %>%
      gsub("\"length\"", "\"n_obs\"", .) %>%
      gsub("tabsort = \"desc\\(n\\)\"", "tabsort = \"desc\\(n_obs\\)\"", .) %>%
      gsub("Search\\(\"(.*?)\",\\s*?.\\)", "search_data(., \"\\1\")", .) %>%
      gsub("toFct\\(\\)", "to_fct()", .) %>%
      gsub("rounddf\\(", "round_df(", .) %>%
      gsub("formatnr\\(", "format_nr(", .) %>%
      gsub("formatdf\\(", "format_df(", .) %>%
      gsub("dataset\\s*=\\s*\"([^\"]+)\",", "\\1,", .) %>%
      gsub("store\\(pred, data\\s*=\\s*\"([^\"]+)\"", "\\1 <- store(\\1, pred", .) %>%
      gsub("pred_data\\s*=\\s*\"([^\"]+)\"", "pred_data = \\1", .) %>%
      gsub("(combinedata\\(\\s*?x\\s*?=\\s*?)\"([^\"]+?)\",(\\s*?y\\s*?=\\s*?)\"([^\"]+?)\",", "\\1\\2,\\3\\4,", .) %>%
      gsub("(combinedata\\((.|\n)*?),\\s*?name\\s*?=\\s*?\"([^\"`]+?)\"([^\\)]+?)\\)", "\\3 <- \\1\\4)\nregister(\"\\3\")", .) %>%
      gsub("combinedata\\(", "combine_data(", .) %>%
      gsub("result\\s*<-\\s*(simulater\\((.|\n)*?),\\s*name+\\s*=\\s*\"([^\"`]*?)\"([^\\)]*?)\\)", "\\3 <- \\1\\4)\nregister(\"\\3\")", .) %>% 
      gsub("data\\s*=\\s*\"([^\"]+)\",", "data = \\1,", .) %>%
      gsub("(simulater\\((\n|.)*?)(register\\(\"(.*?)\"\\))\nsummary\\(result", "\\1\\3\nsummary(\\4", .) %>% 
      gsub("(simulater\\((\n|.)*?)(register\\(\"(.*?)\"\\))\n(summary.*?)\nplot\\(result", "\\1\\3\n\\5\nplot(\\4", .) %>% 
      gsub("result\\s*<-\\s*(repeater\\((.|\n)*?),\\s*name+\\s*=\\s*\"([^\"`]*?)\"([^\\)]*?)\\)", "\\3 <- \\1\\4)\nregister(\"\\3\")", .) %>%
      gsub("(repeater\\((\n|.)*?)(register\\(\"(.*?)\"\\))\nsummary\\(result", "\\1\\3\nsummary(\\4", .) %>% 
      gsub("(repeater\\((\n|.)*?)(register\\(\"(.*?)\"\\))\n(summary.*?)\nplot\\(result", "\\1\\3\n\\5\nplot(\\4", .) %>% 
      gsub("repeater\\(((.|\n)*?),\\s*sim+\\s*=\\s*\"([^\"`]*?)\"([^\\)]*?)\\)", "repeater(\n  \\3,\\1\\4)", .) %>%
      gsub("(```\\{r.*?\\})(\nresult <- pivotr(\n|.)*?)(\\s*)store\\(result, name = \"(.*?)\"\\)", "\\1\\2\\4\\5 <- result$tab; register(\"\\5\")\\6", .) %>% 
      gsub("(```\\{r.*?\\})(\nresult <- explore(\n|.)*?)(\\s*)store\\(result, name = \"(.*?)\"\\)", "\\1\\2\\4\\5 <- result$tab; register(\"\\5\")\\6", .) %>%
      gsub("store\\(result,\\s*name\\s*=\\s*\"(.*?)\",\\s*type\\s*=\\s*\"((P|I)W)\"\\)", "\\1 <- result$\\2; register(\"\\1\")", .)
  })

  # if ("styler" %in% installed.packages()) {
  #   withProgress(message = "Styling report code", value = 1, {
  #     tmp_dir <- tempdir()
  #     tmp_fn <- tempfile(pattern = "report-to-style", tmpdir = tmp_dir, fileext = ".Rmd")
  #     cat(paste(report, "\n"), file = tmp_fn)
  #     ret <- styler::style_file(tmp_fn)
  #     report <- paste0(readLines(tmp_fn), collapse = "\n")
  #   })
  # }

  shinyAce::updateAceEditor(
    session, "rmd_edit",
    value = fix_smart(report)
  )
  removeModal()
})

observeEvent(input$report_ignore, {
  r_info[["report_ignore"]] <- TRUE
  removeModal()
})

## Knit for report in Radiant
knit_it <- function(report, type = "rmd") {

  if (type == "rmd") {
    report <- gsub("\\\\\\\\\\s*\n", "\\\\\\\\\\\\\\\\\n", report)
  }

  if (
    !isTRUE(r_info[["report_ignore"]]) &&
    (grepl("\\s*r_data\\[\\[\".*?\"\\]\\]", report) ||
     grepl("\\s*r_data\\$", report) ||
     grepl("\n(\\#|\\s)*store\\(result,\\s*name", report) ||
     grepl("store\\(pred,\\s*data\\s*=\\s*\"", report) ||
     grepl("\\s+data\\s*=\\s*\".*?\",", report)  ||
     grepl("\\s+dataset\\s*=\\s*\".*?\",", report)  ||
     grepl("\\s+pred_data\\s*=\\s*\".*?\",", report)  ||
     grepl("result\\s*<-\\s*simulater\\(", report)  ||
     grepl("result\\s*<-\\s*repeater\\(", report)  ||
     grepl("combinedata\\(\\s*x\\s*=\\s*\"[^\"]+?\"", report) ||
     grepl("formatnr\\(", report) ||
     grepl("formatdf\\(", report) ||
     grepl("rounddf\\(", report) ||
     grepl("tabsort = \"desc\\(n\\)\"", report) ||
     grepl("(mean_rm|median_rm|min_rm|max_rm|sd_rm|var_rm|sum_rm)", report))
  ) {
    showModal(
      modalDialog(
        title = "The report contains deprecated code",
        span("The use of, e.g., r_data[[...]]], dataset = \"...\", etc. in your report is 
           deprecated. Click the 'Clean report' button to remove references that are no 
           longer needed.", br(), br(), "Warning: It may not be possible to update all code 
           to the latest standard automatically. For example, the use of 'store(...)' 
           functions has changed and not all forms can be automatically updated. If this 
           applies to your report a message should be shown when you Knit the report 
           demonstrating how the code should be changed. You can, of course, also use the 
           browser interface to recreate the code you need or use the help function in R or 
           Rstudio for more information (e.g., ?radiant.model::store.model, 
           ?radiant.model::store.model.predict, or ?radiant.model::simulater)", br(), br(), 
           "To avoid the code-cleaning step click 'Cancel' or, if you believe the code is 
           correct as-is, click the 'Ignore' button and continue to Knit your report"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("report_ignore", "Ignore", title = "Ignore cleaning popup", class = "btn-primary"),
          actionButton("report_clean", "Clean report", title = "Clean report", class = "btn-success")
        ),
        size = "s",
        easyClose = TRUE
      )
    )
    return(invisible())
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
  # if (grepl("ggplotly\\(\\)", report)) {
  #   message("\n\nHeight of ggplotly objects may not be correct in Preview. The height will be correctly displayed in saved reports however.\n\n")
  # }

  ## remove yaml headers and html comments and convert to md
  report <- sub("^---\n(.*?)\n---", "", report) %>%
    sub("<!--(.*?)-->", "", .)

  if (!grepl("```{r r_setup, include = FALSE}\n", report, fixed = TRUE)) {
    report <- paste0("```{r knit_it_setup, include = FALSE}\noptions(width = 250, scipen = 100, max.print = 5000, stringsAsFactors = FALSE)\n```\n\n", report)
  }

  ## convert to md
  md <- knitr::knit(
    text = report,
    envir = r_data,
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

sans_ext <- function(path) {
  sub(
    "(\\.state\\.rda|\\.rda$|\\.rds$|\\.rmd$|\\.r$|\\.rdata$|\\.html|\\.nb\\.html|\\.pdf|\\.docx|\\.Rmd|\\.zip)", "", 
    path, ignore.case = TRUE
  )
}

report_name <- function(type = "rmd", out = "report", full.name = FALSE) {

  ldir <- getOption("radiant.launch_dir", default = radiant.data::find_home())
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
  req(input[[paste0(type, "_generate")]])

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
          ## don't want to write to current dir
          currdir <- setwd(tempdir())
          save(list = ls(envir = r_data), envir = r_data, file = "r_data.rda")

          setup_report(report, save_type = "Rmd", lib = lib) %>%
            fix_smart() %>%
            cat(file = "report.Rmd", sep = "\n")

          zip(file, c("report.Rmd", "r_data.rda"),
            flags = zip_info[1], zip = zip_info[2]
          )
          setwd(currdir)
        })
      } else if (save_type == "R + Data (zip)") {
        withProgress(message = "Preparing R + Data zip file", value = 1, {
          ## don't want to write to current dir
          currdir <- setwd(tempdir())
          save(list = ls(envir = r_data), envir = r_data, file = "r_data.rda")

          cat(report, file = "report.R", sep = "\n")

          zip(file, c("report.R", "r_data.rda"),
            flags = zip_info[1], zip = zip_info[2]
          )
          setwd(currdir)
        })
      } else if (save_type == "Rmd") {
        setup_report(report, save_type = "Rmd", lib = lib) %>%
          fix_smart() %>%
          cat(file = file, sep = "\n")
      } else if (save_type == "R") {
        cat(report, file = file, sep = "\n")
      } else {

        ## hack for rmarkdown from Report > Rmd and Report > R
        options(radiant.rmarkdown = TRUE)

        if (type == "r") {
          report <- paste0("\n```{r echo = TRUE}\n", report, "\n```\n")
        }

        init <- setup_report(fix_smart(report), save_type = save_type, lib = lib)

        ## on linux ensure you have you have pandoc > 1.14 installed
        ## you may need to use http://pandoc.org/installing.html#installing-from-source
        ## also check the logs to make sure its not complaining about missing files
        withProgress(message = paste0("Saving report to ", save_type), value = 1, {
          if (isTRUE(rmarkdown::pandoc_available())) {
            ## have to use current dir so (relative) paths work properly
            tmp_fn <- tempfile(pattern = "report-", tmpdir = ".", fileext = ".Rmd")
            cat(init, file = tmp_fn, sep = "\n")
            out <- rmarkdown::render(
              tmp_fn, 
              switch(
                save_type,
                Notebook = rmarkdown::html_notebook(highlight = "textmate", theme = "spacelab", code_folding = "hide"),
                HTML = rmarkdown::html_document(highlight = "textmate", theme = "spacelab", code_download = TRUE, df_print = "paged"),
                PDF = rmarkdown::pdf_document(),
                Word = rmarkdown::word_document(reference_docx = file.path(system.file(package = "radiant.data"), "app/www/style.docx"))
              ), 
              envir = r_data, quiet = TRUE, encoding = "UTF-8", 
              output_options = list(pandoc_args = "--quiet")
            )
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
        options(radiant.rmarkdown = FALSE)
      }
    })
  }
}

## updating the report when called
update_report <- function(
  inp_main = "", fun_name = "", inp_out = list("", ""),
  cmd = "", pre_cmd = "result <- ", post_cmd = "",
  xcmd = "", outputs = c("summary", "plot"), inp = "result", 
  wrap, figs = TRUE, fig.width = 7, fig.height = 7
) {

  ## determine number of characters for main command for wrapping
  if (missing(wrap)) {
    lng <- nchar(pre_cmd) + nchar(fun_name) + nchar(post_cmd) + 2
    if (!is_empty(inp_main)) {
      lng <- lng + sum(nchar(inp_main)) +
        sum(nchar(names(inp_main))) +
        length(inp_main) * 5 - 1
    }
    wrap <- ifelse(lng > 70, TRUE, FALSE)
  }

  dctrl <- getOption("dctrl")

  ## wrapping similar to styler
  depr <- function(x, wrap = FALSE) {
    if (wrap) {
      for (i in names(x)) {
        tmp <- x[[i]]
        wco <- ifelse(max(nchar(tmp)) > 20, 20L, 55L)
        tmp <- deparse(tmp, control = dctrl, width.cutoff = wco)
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
      x <- deparse(x, control = dctrl, width.cutoff = 500L) %>%
        paste(collapse = "")
    }
    x
  }

  if (inp_main[1] != "") {
    cmd <- depr(inp_main, wrap = wrap) %>%
      sub("list", fun_name, .) %>%
      paste0(pre_cmd, .) %>%
      paste0(., post_cmd) %>%
      sub("dataset = \"([^\"]+)\"", "\\1", .)
  }

  lout <- length(outputs)
  if (lout > 0) {
    for (i in seq_len(lout)) {
      if (inp %in% names(inp_out[[i]])) {
        inp_rep <- inp
        inp <- inp_out[[i]][[inp]]
        inp_out[[i]][inp_rep] <- NULL
      }
      if (inp_out[i] != "" && length(inp_out[[i]]) > 0) {
        if (sum(nchar(inp_out[[i]])) > 40L) {
          cmd <- depr(inp_out[[i]], wrap = wrap) %>%
            sub("list\\(", paste0(outputs[i], "\\(\n  ", inp, ", "), .) %>%
            paste0(cmd, "\n", .)
        } else {
          cmd <- deparse(inp_out[[i]], control = dctrl, width.cutoff = 500L) %>%
            sub("list\\(", paste0(outputs[i], "\\(", inp, ", "), .) %>%
            paste0(cmd, "\n", .)
        }
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
      cmd <- paste0("\n```{r fig.width = ", round(7 * fig.width / 650, 2), ", fig.height = ", round(7 * fig.height / 650, 2), ", dpi = 144}\n", cmd, "\n```\n")
    } else {
      cmd <- paste0("\n```{r}\n", cmd, "\n```\n")
    }
    update_report_fun(cmd, type = "rmd")
  }
}

update_report_fun <- function(cmd, type = "rmd", rfiles = FALSE) {
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
        rstudioapi::insertText(Inf, fix_smart(cmd))
      })
    } else if (sinit == "To R") {
      withProgress(message = "Putting R-code in Rstudio", value = 1, {
        gsub("(```\\{.*\\}\n)|(```\n)", "", fix_smart(paste0("\n", cmd, "\n"))) %>%
          rstudioapi::insertText(Inf, .)
      })
    } else {
      if (is_empty(r_state[[editor]])) {
        r_state[[editor]] <<- paste0("## Your report title\n\n", cmd)
      } else {
        r_state[[editor]] <<- paste0(fix_smart(r_state[[editor]]), "\n", cmd)
      }
      withProgress(message = paste0("Updating Report > ", sel), value = 1, {
        shinyAce::updateAceEditor(
          session, editor,
          value = fix_smart(r_state[[editor]])
        )
      })
    }

    if (!rfiles) {
      if (state_init(paste0(type, "_switch"), "switch") == "switch") {
        updateTabsetPanel(session, "nav_radiant", selected = sel)
      }
    }
  })
}
