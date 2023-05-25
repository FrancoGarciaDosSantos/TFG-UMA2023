library(shiny)

fileUI <- function(id, i18n = NULL) {
  ns <- NS(id)
  tagList(
    usei18n(i18n),
    
    newFileInput(ns("file_input"), i18n$t("Select a file:"),
                 accept = c("text/csv", ".csv",
                            "text/comma-separated-values,text/plain"),
                 buttonLabel = tags$span(icon("folder-open"),"Browse"), 
                 placeholder = "test.txt") %>%
      shinyInput_label_embed( uiOutput(ns("file_help")) )
  )
}

fileServer <- function(id, i18n = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      check_file_ext <- function(fileI){
        file <- fileI$datapath
        ext <- file_ext(file)
        if(!(ext %in% c("txt", "csv"))){
          sprintf("%s (%s)", i18n()$t("File extension is not valid."), ext)
        }
      }
      
      file_iv <- InputValidator$new()
      file_iv$add_rule("file_input", ~ if(is.null(.)){ i18n()$t("File required.") } )
      file_iv$add_rule("file_input", ~ check_file_ext(.) )
      
      
      output$file_help <- renderUI({
        tagList(
          shiny_iconlink() %>%
            bs_embed_popover(
              title = i18n()$t("Valid files and separators"),
              content = 
                stri_c(
                  '<ul> <li>', i18n()$t("Files with extension .csv or .txt"),'</li>
                  <li>', i18n()$t("Columns separated by one of the following separators: "),
                  '<br/><b>',
                  stri_c(i18n()$t("Whitespace"), i18n()$t("Comma"),i18n()$t("Tab"), 
                         i18n()$t("Semicolon"), sep = ", "),'</b></li>
                  <li>', i18n()$t("Rows separated by line break"),'</li> </ul>'),
              placement = "bottom",
              html = "true"),
          tags$script("$(function () {$('[data-toggle=\"popover\"]').popover()})"))
      })
      
      
      return(list(
        file = reactive(input$file_input),
        file_validator = file_iv
      ))
      
    }
  )}