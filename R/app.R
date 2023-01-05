# library(shiny)
# library(DT)
#
# dat <- data.frame(row_index = 1:10, price = 11:20)
#
# ui <- fluidPage(
#   titlePanel("Slice Rows Example"),
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("start_index", "Start index:", min = 1, max = 10, value = 1),
#       sliderInput("stop_index", "Stop index:", min = 1, max = 10, value = 10)
#     ),
#     mainPanel(
#       dataTableOutput("my_table")
#     )
#   )
# )
#
# server <- function(input, output) {
#
#   # output$my_table <- renderDataTable({
#   #   # datatable(dat, filter = "top", options = list(saveState = TRUE, pageLength = input$stop_index, order=list(list(2, "desc"))))
#   #   DT::datatable(
#   #     dat,
#   #     # filter = list(position = "top"),
#   #     options = list(saveState = TRUE, order=list(list(2, "desc")))
#   #   )
#   # }, server = TRUE, rownames = FALSE)
#
#   output$my_table <- DT::renderDataTable(datatable(
#     dat, filter = 'top',
#     # options = list(saveState = TRUE, order=list(list(2, "desc"))),
#     options = list(saveSate = TRUE, order=list(list(2, "desc")))
#     # rownames= FALSE
#   ))
#
#   # proxy <- dataTableProxy("my_table")
#   #
#   # observe({
#   #   print(input$my_table_rows_all)
#   #   # print(input$my_table_state)
#   # })
#   #
#   # observeEvent(c(input$start_index, input$stop_index), {
#   #   req(input$my_table_rows_all)
#   #   # start_ind <- input$start_index
#   #   # stop_ind <- input$stop_index
#   #   # ind <- input$my_table_rows_all[start_ind:stop_ind]
#   #   # dat["row_index"] <- 1:length(input$my_table_rows_all)
#   #   # replaceData(proxy, dat, rownames = FALSE)
#   #   # replaceData(proxy, dat[ind, "row_index" ,drop=FALSE])
#   # })
# }
#
# shinyApp(ui, server)



# library(shiny)
# library(DT)
#
# dat <- data.frame(row_index = 1:10, price = 11:20)
#
# ui <- fluidPage(
#   titlePanel("Slice Rows Example"),
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("start_index", "Start index:", min = 1, max = 10, value = 1),
#       sliderInput("stop_index", "Stop index:", min = 1, max = 10, value = 10)
#     ),
#     mainPanel(
#       dataTableOutput("my_table")
#     )
#   )
# )
#
# server <- function(input, output) {
#
#   output$my_table <- renderDataTable({
#     ## No rows are shown when rownames = FALSE - strange (DT 0.25)
#     # datatable(dat, rownames = FALSE, options = list(saveState = TRUE, order=list(list(2, "desc"))))
#     datatable(dat,
#       filter = list(position = "top"),
#       rownames = TRUE, options = list(saveState = TRUE, order=list(list(2, "desc")))
#     )
#   })
#
#   proxy <- dataTableProxy("my_table")
#
#   observe({
#     print(input$my_table_rows_all)
#   })
#
#   # observeEvent(c(input$start_index, input$stop_index), {
#   #   req(input$my_table_rows_all)
#   #   start_ind <- input$start_index
#   #   stop_ind <- input$stop_index
#   #   ind <- input$my_table_rows_all[start_ind:stop_ind]
#   #   replaceData(proxy, dat[ind, ,drop=FALSE])
#   # })
#   #
#   observe({
#     req(input$my_table_rows_all)
#     dat["row_index"] <- 1:nrow(dat)
#     replaceData(proxy, dat)
#   })
# }
#
# shinyApp(ui, server)
