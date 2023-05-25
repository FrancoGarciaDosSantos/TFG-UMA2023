library(shiny)

matrixUI <- function(id, i18n = NULL) {
  ns <- NS(id)
  tagList(
    usei18n(i18n),
    
    tags$label(style="width:100%; margin-top: 15px;",
               i18n$t("Fill the matrix:"),
               uiOutput(ns("matrix_help"), inline = TRUE)),
    
    matrixInput(
      ns("matrix_input"), label = NULL, value = diag(5),
      rows = list(names = FALSE), cols = list(names = FALSE),
      class = "character")
  )
}

matrixServer <- function(id, leng = NULL, i18n = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns

      matrix_iv <- InputValidator$new()
      matrix_iv$add_rule("matrix_input", 
                         function(x){ res <- check_na(x)
                          if(!res$valida){ i18n()$t(res$mensaje) } 
                          else { NULL }
                         })
      
      observe({
        updateMatrixInput(session, "matrix_input",
                          change_matrix_size(input$matrix_input, leng()))
      }) %>% bindEvent(leng())

      output$matrix_help <- renderUI({
        tags$div(class="pull-right", 
                 shiny_iconlink() %>%
                   bs_embed_popover(
                     title = i18n()$t("Valid inputs"),
                     content = tagList(
                       HTML(stri_c(
                         '<ul> <li>', i18n()$t("Values appearing in the chain"),'</li>
                    <li>', i18n()$t("Values with redundant left 0's are not allowed"),': <b>01, 02.5, 09/3</b></li> </ul>')),
                       tags$span(tags$b("'Tab'"), i18n()$t("to move between the same row")),
                       tags$br(),
                       tags$span(tags$b("'Enter'"), i18n()$t("to move between the same column"))
                     ),
                     placement = "bottom",
                     html = "true"),
                 tags$script("$(function () {$('[data-toggle=\"popover\"]').popover()})")
        )
      })

      
      return(list(
        matrix = reactive(input$matrix_input),
        matrix_validator = matrix_iv
      ))
      
    }
  )}
