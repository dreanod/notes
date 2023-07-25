library(shiny)
library(tidyverse)

ui <- fluidPage(
  actionButton("add", "Add"),
  uiOutput("container"),
  verbatimTextOutput("out")
)

server <- function(input, output, session) {

  dynamic_list <- reactiveVal(
    map(1:1, ~list(name = LETTERS[.x], value = .x))
  )

  observeEvent(input$add, {
    dynamic_list() |>
      c(list(list(name = "New", value = input$add))) |>
      dynamic_list()
  })

  output$container <- renderUI({
    imap(dynamic_list(), make_elem)
  })

  output$out <- renderPrint({
    str(dynamic_list())
  })

  delete_observers <- list()
  edit_observers <- list()

  observeEvent(dynamic_list(), {
    walk(delete_observers, ~ .x$destroy())
    walk(edit_observers, ~ .x$destroy())

    delete_observers <<- imap(dynamic_list(), function(x, id) {
      observeEvent(input[[paste0("delete-", id)]], {
        updated_list <- dynamic_list()
        updated_list[[id]] <- NULL
        dynamic_list(updated_list)
      }, ignoreInit = TRUE)
    })

    edit_observers <<- imap(dynamic_list(), function(x, id) {
      observeEvent(input[[paste0("edit-", id)]], {
        updated_list <- dynamic_list()
        updated_list[[id]]$value <- updated_list[[id]]$value + 1
        dynamic_list(updated_list)
      }, ignoreInit = TRUE)
    })
  })
}

make_elem <- function(elem, id) {
  wellPanel(
    strong(elem$name),
    tags$em(elem$value),
    actionButton(paste0("delete-", id), "Delete"),
    actionButton(paste0("edit-", id), "Edit")
  )
}


shinyApp(ui, server)
