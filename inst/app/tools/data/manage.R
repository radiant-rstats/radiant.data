descr_out <- function(descr, ret_type = "html") {
  ## if there is no data description
  if (is.empty(descr)) {
    return("")
  }

  ## if there is a data description and we want html output
  if (ret_type == "html") {
    markdown::mark_html(text = descr, template = FALSE, meta = list(css = ""), output = FALSE)
  } else {
    descr
  }
}

## create an empty data.frame and return error message as description
upload_error_handler <- function(objname, ret) {
  r_data[[objname]] <- data.frame(matrix(rep("", 12), nrow = 2), stringsAsFactors = FALSE) %>%
    set_attr("description", ret)
}

load_csv <- function(file, delim = ",", col_names = TRUE, dec = ".",
                     n_max = Inf, saf = TRUE, safx = 30) {
  n_max <- if (is_not(n_max) || n_max < 0) Inf else n_max
  dataset <- sshhr(try(
    readr::read_delim(
      file,
      delim = delim, locale = readr::locale(decimal_mark = dec, grouping_mark = delim),
      col_names = col_names, n_max = n_max, trim_ws = TRUE
    ),
    silent = TRUE
  ))
  if (inherits(dataset, "try-error")) {
    "#### There was an error loading the data. Please make sure the data are in csv format"
  } else {
    prb <- readr::problems(dataset)
    if (nrow(prb) > 0) {
      tab_big <- "class='table table-condensed table-hover' style='width:70%;'"
      rprob <- knitr::kable(
        prb[1:(min(nrow(prb):10)), , drop = FALSE],
        align = "l",
        format = "html",
        table.attr = tab_big,
        caption = "Read issues (max 10 rows shown):"
      )
    } else {
      rprob <- ""
    }

    if (saf) dataset <- to_fct(dataset, safx)
    as.data.frame(dataset, stringsAsFactors = FALSE) %>%
      {
        set_colnames(., fix_names(colnames(.)))
      } %>%
      set_attr("description", rprob)
  }
}

load_user_data <- function(fname, uFile, ext, header = TRUE,
                           man_str_as_factor = TRUE, sep = ",",
                           dec = ".", n_max = Inf) {
  filename <- basename(fname)
  fext <- tools::file_ext(filename) %>% tolower()

  ## switch extension if needed
  ext <- case_when(
    fext == ext ~ ext,
    fext == "rdata" ~ "rdata",
    fext == "rds" && ext == "rda" ~ "rds",
    fext == "rda" && ext == "rds" ~ "rda",
    fext == "txt" && ext == "csv" ~ "txt",
    fext == "tsv" && ext == "csv" ~ "tsv",
    TRUE ~ ext
  )

  ## objname is used as the name of the data.frame, make case insensitive
  objname <- sub(glue("\\.{ext}$"), "", filename, ignore.case = TRUE)

  ## if ext isn't in the filename nothing was replaced and so ...
  if (objname == filename) {
    if (fext %in% c("xls", "xlsx")) {
      ret <- "#### Radiant does not load xls files directly. Please save the data as a csv file and try again."
    } else {
      ret <- glue('#### The filename extension "{fext}" does not match the specified \\
        file-type "{ext}". Please specify the file type you are trying to upload')
    }

    upload_error_handler(objname, ret)
    ext <- "---"
  }

  ldir <- getOption("radiant.launch_dir", default = radiant.data::find_home())
  pdir <- getOption("radiant.project_dir", default = ldir)
  cmd <- NULL

  pp <- suppressMessages(
    radiant.data::parse_path(
      uFile,
      pdir = pdir,
      chr = "\"",
      mess = FALSE
    )
  )

  ## can't have spaces, dashes, etc. in objectname
  objname <- radiant.data::fix_names(objname)

  if (ext %in% c("rda", "rdata")) {
    ## objname will hold the name of the object(s) inside the R datafile
    robjname <- try(load(uFile), silent = TRUE)
    if (inherits(robjname, "try-error")) {
      upload_error_handler(objname, "#### There was an error loading the data. Please make sure the data are in rda format.")
    } else if (length(robjname) > 1) {
      if (sum(robjname %in% c("r_state", "r_data", "r_info")) > 1) {
        upload_error_handler(objname, "#### To restore state select 'radiant state file' from the 'Load data of type' drowdown before loading the file")
        ## need to remove the local copies of r_state, r_data, and r_info
        suppressWarnings(rm(r_state, r_data, r_info))
      } else {
        upload_error_handler(objname, "#### More than one R object contained in the data.")
      }
    } else {
      r_data[[objname]] <- as.data.frame(get(robjname), stringsAsFactors = FALSE)
      cmd <- glue("{objname} <- load({pp$rpath}) %>% get()")
    }
  } else if (ext == "rds") {
    ## objname will hold the name of the object(s) inside the R datafile
    robj <- try(readRDS(uFile), silent = TRUE)
    if (inherits(robj, "try-error")) {
      upload_error_handler(objname, "#### There was an error loading the data. Please make sure the data are in rds format.")
    } else {
      r_data[[objname]] <- as.data.frame(robj, stringsAsFactors = FALSE)
      cmd <- glue("{objname} <- readr::read_rds({pp$rpath})")
    }
  } else if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("The 'arrow' package is not installed. Please install it and try again.")
      upload_error_handler(objname, "#### The arrow package required to work with data in parquet format is not installed. Please use install.packages('arrow')")
    } else {
      robj <- arrow::read_parquet(uFile) # %>% set_attr("description", feather::feather_metadata(uFile)$description)
      if (inherits(robj, "try-error")) {
        upload_error_handler(objname, "#### There was an error loading the data. Please make sure the data are in parquet format.")
      } else {
        r_data[[objname]] <- as.data.frame(robj, stringsAsFactors = FALSE)
        cmd <- glue("{objname} <- arrow::read_parquet({pp$rpath})")
      }
    }
  } else if (ext %in% c("tsv", "csv", "txt")) {
    r_data[[objname]] <- load_csv(
      uFile,
      delim = sep, col_names = header, n_max = n_max,
      dec = dec, saf = man_str_as_factor
    ) %>%
      (function(x) if (is.character(x)) upload_error_handler(objname, "#### There was an error loading the data") else x)
    n_max <- if (is_not(n_max) || n_max < 0) Inf else n_max
    if (ext == "csv" && sep == "," && dec == "." && header) {
      cmd <- glue("{objname} <- readr::read_csv({pp$rpath}, n_max = {n_max})")
    } else {
      cmd <- glue('
        {objname} <- readr::read_delim(
          {pp$rpath},
          delim = "{sep}", col_names = {header}, n_max = {n_max},
          locale = readr::locale(decimal_mark = "{dec}", grouping_mark = "{sep}")
        )')
    }
    ## make sure all columns names are "fixed"
    cmd <- paste0(cmd, " %>%\n  fix_names()")
    if (man_str_as_factor) cmd <- paste0(cmd, " %>%\n  to_fct()")
  } else if (ext != "---") {
    ret <- glue("#### The selected filetype is not currently supported ({fext})")
    upload_error_handler(objname, ret)
  }

  if (exists(objname, envir = r_data) && !bindingIsActive(as.symbol(objname), env = r_data)) {
    shiny::makeReactiveBinding(objname, env = r_data)
  }

  r_info[[glue("{objname}_descr")]] <- attr(r_data[[objname]], "description")
  if (!is.empty(cmd)) {
    cn <- colnames(r_data[[objname]])
    fn <- radiant.data::fix_names(cn)
    if (!identical(cn, fn)) {
      colnames(r_data[[objname]]) <- fn
      cmd <- paste0(cmd, " %>%\n  fix_names()")
    }
    cmd <- glue('{cmd}\nregister("{objname}")')
  }
  r_info[[glue("{objname}_lcmd")]] <- cmd
  r_info[["datasetlist"]] <- c(objname, r_info[["datasetlist"]]) %>% unique()
}

load_description <- function(fname, uFile, objname) {
  ldir <- getOption("radiant.launch_dir", default = radiant.data::find_home())
  pdir <- getOption("radiant.project_dir", default = ldir)
  cmd <- NULL

  pp <- suppressMessages(
    radiant.data::parse_path(
      uFile,
      pdir = pdir,
      chr = "\"",
      mess = FALSE
    )
  )

  descr <- readLines(pp$path, warn = FALSE) %>% paste0(collapse = "\n")
  cmd <- glue("register(\"{objname}\", descr = paste0(readLines({pp$rpath}, warn = FALSE), collapse = \"\\n\"))")
  attr(r_data[[objname]], "description") <- descr
  r_info[[glue("{objname}_descr")]] <- descr
  r_info[[glue("{objname}_lcmd")]] <- sub(glue('register("{objname}")'), cmd, r_info[[glue("{objname}_lcmd")]], fixed = TRUE)
}
