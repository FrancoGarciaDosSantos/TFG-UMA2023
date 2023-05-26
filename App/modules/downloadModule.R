library(shiny)

downloadUI <- function(id, i18n) {
  ns <- NS(id)
  
  tagList(
    dropdown(
      tags$h3(i18n$t("Download results")),
      
      uiOutput(ns("format_list")),
      conditionalPanel(condition = "input.format != '.pdf'", ns = ns,
        textInputIcon(
          inputId = ns("file_name"), label = i18n$t("File name:"),
          value = "test", icon = list(NULL, textOutput(ns("format_selected")))
        ),
        uiOutput(ns("sep_list")),
        awesomeCheckbox(
          inputId = ns("add_header"), label = tags$b(i18n$t("Add header?")), 
          value = TRUE, status = "success"
        ),
        tags$div(class="right-btn",
                 downloadBttn(
                   ns("export1"), label = i18n$t("Download"), style = "unite",
                   color = "primary", size = "sm", block = FALSE,
                   no_outline = FALSE, icon = icon("download")
                 ))
      ),
      conditionalPanel(condition = "input.format == '.pdf'", ns = ns,
        textAreaInput(ns("descripcion_pdf"), label = i18n$t("Description:")),
        awesomeCheckbox(
         inputId = ns("add_result"), label = tags$b(i18n$t("Add properties?")), 
         value = FALSE, status = "success"
        ),
        tags$div(class="right-btn",
                 downloadBttn(
                   ns("export2"), label = i18n$t("Generate PDF"), style = "unite",
                   color = "primary", size = "sm", block = FALSE,
                   no_outline = FALSE, icon = icon("download")
                 ))
      ),
      
      label = i18n$t("Export"), size = "sm", style = "unite", 
      icon = icon("file-export"), status = "royal", width = "400px",
      animate = animateOptions(enter = "slideInRight", exit = "fadeOutRight", 
                               duration = 0.5),
      inputId = ns("dropdown_btn")
    ) # end dropdown
  )
  
}

downloadServer <- function(id, check, anal, i18n, r) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      disable("export1_bttn")
      disable("export2_bttn")
      disable("add_result")
      
      output$format_list <- renderUI({
        radioGroupButtons(
          inputId = ns("format"), label = i18n()$t("File extension:"),
          choiceNames = c("CSV", i18n()$t("Plain text"), "PDF"), 
          choiceValues = c(".csv",".txt", ".pdf"),
          direction = "horizontal", individual = TRUE
        )
      })
      
      output$sep_list <- renderUI({
        radioGroupButtons(
          inputId = ns("sep"), label = i18n()$t("Separator:"),
          choiceNames = c(i18n()$t("Whitespace"),i18n()$t("Comma"), 
                          i18n()$t("Semicolon"),i18n()$t("Tab")), 
          choiceValues = c(" ",",",";","\t"),
          direction = "horizontal", individual = TRUE
        )
      })
      
      output$format_selected <- renderText(input$format)
      
      observe({
        toggleState("export1_bttn", condition = check()$valida )
        toggleState("export2_bttn", condition = check()$valida )
      })
      
      observe({
        toggleState("add_result", condition = r() )
        if(!r()){
          updateAwesomeCheckbox(session ,"add_result", value = r())
        }
      })
      
      output$export1 <- downloadHandler(
        filename = function() {
          paste0(input$file_name, input$format)
        },
        content = function(file){
          if(input$add_header){
            cols <- check()$cadena$values
          } else { cols <- FALSE }
          
          res <- write.table(check()$matriz$values, file, sep = input$sep, col.names = cols, row.names = FALSE)
          res
        }
      )
      
      output$export2 <- downloadHandler(
        filename = function() {
          paste0(input$file_name, input$format)
        },
        content = function(file){
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("report.Rmd", tempReport, overwrite = TRUE)
          
          params <- data_pdf()

          out <- render(tempReport, pdf_document(), output_file = file,
                        params = params, envir = new.env(parent = globalenv()))
          file.rename(out, file)
        }
      )
      
      
      data_pdf <- reactive({
        cadena <- check()$cadena
        matriz <- check()$matriz
        descripcion <- input$descripcion_pdf
        print(descripcion)
        res_anal <- NULL
        if(input$add_result){
          res_anal <- anal()
          res_anal$text <- sapply(res_anal$text, i18n()$t )
        }
        
        list(title = i18n()$t("Binary Operator") , descripcion = descripcion, 
             formula_titulo = i18n()$t("Provided formula:"), formula = matriz$formula,
             cadena_titulo = i18n()$t("Provided chain:"), cadena_pdf = cadena$text, 
             matriz_titulo = i18n()$t("Provided matrix:"), 
             matriz_pdf = matrix_to_pdf(matriz$original, cadena$original),
             propiedades_titulo = i18n()$t("Properties:"), propiedades = res_anal)
      })
    }
  )}
