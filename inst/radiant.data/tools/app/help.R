#######################################
## Other elements in help menu
#######################################
output$help_videos <- renderUI({
  file.path(r_path,"radiant.data/tools/app/tutorials.md") %>% inclMD %>% HTML
})

output$help_about <- renderUI({
  file.path(r_path,"radiant.data/tools/app/about.md") %>% inclMD %>% HTML
})

output$help_text <- renderUI({
  wellPanel(
    HTML("Help is available on each page by clicking the <i title='Help' class='fa fa-question'></i> icon on the bottom left of your screen.")
  )
})

#######################################
## Main function of help menu
#######################################
# help2html <- function(x) x %>% gsub("\\\\%","%",.) %>% HTML
# append_help <- function(help_str, help_path, Rmd = FALSE) {

append_help <- function(help_str, help_path, Rmd = TRUE) {
  if (length(input[[help_str]]) == 0) return()
  help_block <- get(help_str)
  local_hd <- help_block[which(help_block %in% input[[help_str]])]
  all_help <- c()
  for (i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),
                      inclRmd(file.path(help_path,local_hd[i])),
                      sep="\n")
  }
  mathjax_script <- ifelse (Rmd, "<script>if (window.MathJax) MathJax.Hub.Typeset();</script>", "")
  cc <- "&copy; Vincent Nijs (2016) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/80x15.png' /></a></br>"
  paste(all_help,"\n",mathjax_script,"\n",cc) %>% HTML
}

help_switch <- function(help_all, help_str, help_on = TRUE) {
  if (is.null(help_all) || help_all == 0) return()
  help_choices <- help_init <- get(help_str)
  init <- ""
  if (help_on) init <- help_init
  updateCheckboxGroupInput(session, help_str,
    label = NULL,
    choices = help_choices,
    selected = init, inline = TRUE)
}

help_data <- c("Manage" = "manage.md","View" = "view.md", "Visualize" = "visualize.md",
               "Pivot" = "pivotr.md", "Explore" = "explore.md", "Transform" = "transform.md",
               "Combine" = "combine.md")
output$help_data <- reactive(append_help("help_data", file.path(r_path,"radiant.data/tools/help/")))

observeEvent(input$help_data_all, {help_switch(input$help_data_all, "help_data")})
observeEvent(input$help_data_none, {help_switch(input$help_data_none, "help_data", help_on = FALSE)})

help_data_panel <-
      wellPanel(
        HTML("<label>Data menu: <i id='help_data_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
              <i id='help_data_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
        checkboxGroupInput("help_data", NULL, help_data, selected = state_init("help_data"), inline = TRUE)
      )

output$help_data_ui <- renderUI({
  sidebarLayout(
    sidebarPanel(
      help_data_panel,
      uiOutput("help_text")
    ),
    mainPanel(
      htmlOutput("help_data")
    )
  )
})

if ("radiant.data" %in% (installed.packages()[,'Package'])) {
  r_version <- packageVersion("radiant.data")
} else {
  r_version <- "unknown"
}
