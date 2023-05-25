library(shiny)

matrixInputUI <- function(id, i18n = NULL) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("select")),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 1", ns("how_matrix")),
      matrixUI(ns("matriz"), i18n)
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 2", ns("how_matrix")),
      fileUI(ns("file"), i18n)
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 3", ns("how_matrix")),
      formulaUI(ns("formula"), i18n)
    )
    
  )
}

matrixInputServer <- function(id, leng = NULL, i18n = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      output$select <- renderUI({
        v <- 1:3
        names(v) <- c(i18n()$t("Via a matrix input"), i18n()$t("Via a file input"), 
                      i18n()$t("Via a formula input"))
        pickerInput(
          inputId = ns("how_matrix"), label = i18n()$t("Select a way to enter matrix: "),
          choices = v, width = 'auto'
        )
      })
      
      matrix <- matrixServer("matriz", leng, i18n)
      matrix_iv <- matrix$matrix_validator
      matrix_iv$condition( function(){ input$how_matrix == 1 })
      matrix_iv$enable()
      
      file <- fileServer("file", i18n)
      file_iv <- file$file_validator
      file_iv$condition( function(){ input$how_matrix == 2 })
      file_iv$enable()
      
      formula <- formulaServer("formula", i18n)
      formula_iv <- formula$formula_validator
      formula_iv$condition( function(){ input$how_matrix == 3 })
      formula_iv$enable()


      matriz <- reactive({
        if(!matrix_iv$is_valid() || !file_iv$is_valid() || !formula_iv$is_valid()){
          return(NULL)
        }
        tipo <- input$how_matrix
        m <- switch(tipo,
                    "1" = matrix$matrix(),
                    "2" = read_file(file$file()$datapath, FALSE),
                    "3" = formula$formula()
        )
        if(tipo == 2){
          if(!m$valida){
            shinyalert(text = m$mensaje)
            return(NULL)
          }
          m <- m$matriz
        }
        m

      })
      
      return(list(matriz = matriz, tipo = reactive(input$how_matrix)))
      
    }
  )}
