library(shiny)

chainUI <- function(id, i18n = NULL) {
  ns <- NS(id)
  tagList(
    usei18n(i18n),
    selectizeInput(
      ns("vector"), i18n$t("Enter a vector:"), 
      choices = 0:4, selected = 0:4, multiple = TRUE,
      options = list(create = TRUE,
                     createFilter = "^\\d+(\\.[0-9]+)?$|^[1-9]+/[1-9]+$",
                     maxOptions = 5, placeholder = "1, 2, 3, 4, ..",
                     persist = TRUE, hideSelected = TRUE)) %>%
      shinyInput_label_embed( uiOutput(ns("help")) ),
    uiOutput(ns("chain_btn"))
    )
}

chainServer <- function(id, leng = NULL, i18n = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      
      output$chain_btn <- renderUI({
        tags$div(class="btn-set",
                 actionBttn(
                   inputId = ns("clean"), label = i18n()$t("Clean"), style = "fill",
                   color = "danger", icon = icon("trash-arrow-up"), size="xs",
                   onClick = paste0("changeText('",ns('clean'),"','",
                                    i18n()$t("Done!"),"')")
                 ) %>% 
                   bs_embed_tooltip(title = i18n()$t("Click to clean chain"), 
                                    placement="bottom"),
                 actionBttn(
                   inputId = ns("copy"), label = i18n()$t("Copy"), style = "fill",
                   color = "primary", icon = icon("clipboard"), size="xs"
                 ) %>% 
                   bs_embed_tooltip(title = i18n()$t("Click to copy chain"), 
                                    placement="bottom"),
                 actionBttn(
                   inputId = ns("paste"), label = i18n()$t("Paste"), style = "fill",
                   color = "royal", icon = icon("paste"), size="xs"
                 ) %>%
                   bs_embed_tooltip(title = i18n()$t("Ctrl+V to paste chain"), 
                                    placement="bottom"),
                 uiOutput(ns("clip")))
      })
      
      output$clip <- renderUI({
        rclipButton(
          inputId = ns("clipbtt"), label = NULL,
          clipText = paste0(input$vector, collapse = ","),
          icon = icon("clipboard"), style = "display:none;")
      })
      
      new_leng <- reactive({
        if(is.reactive(leng)){ leng() } 
        else { NULL }
      })
      
      check_size_chain <- function(value){ 
        n <- new_leng()
        len_chain <- length(value)
        if(len_chain < n){
          sprintf("%s %i (%i %s)", i18n()$t("Chain length must be equal to"), 
                  n, n-len_chain, i18n()$t("left"))
        }
      }
      
      chain_iv <- InputValidator$new()
      chain_iv$add_rule("vector", ~ if(is.null(.)){ i18n()$t("Chain required.") } )
      if( is.reactive(leng) ){
        chain_iv$add_rule("vector", ~ check_size_chain(.))
      }
      
      
      observe({
        n <- new_leng()
        v <- input$vector
        if(is_null(v)) { v <- 0:(n-1) }

        updateSelectizeInput(session, "vector", choices = v, selected = v,
                             options = list(create = TRUE, maxItems = n,
                                            createFilter = "^\\d+(\\.[0-9]+)?$|^[1-9]+/[1-9]+$",
                                            maxOptions = 5, placeholder = "1, 2, 3, 4, ...",
                                            persist = TRUE, hideSelected = TRUE)
                             )
      }) %>% bindEvent(new_leng())
      
      observeEvent(input$clean,{
        v <- vector()
        updateSelectizeInput(session = session, "vector", choices = v, selected = v)
      })
      
      observeEvent(input$copy, {
        onClick = paste0("changeText('",ns('copy'),"','",
                         i18n()$t("Done!"),"')")
        
        shinyjs::runjs(onClick)
        shinyjs::click("clipbtt")
      })
      
      output$help <- renderUI({
        tagList(
          shiny_iconlink() %>%
            bs_embed_popover(
              title = i18n()$t("Valid inputs"),
              content = stri_c(
              '<ul> <li>', i18n()$t("Positive numbers"), '</li>
                    <li>', i18n()$t("Non-repeating values"), '</li>
                    <li>', i18n()$t("Integer numbers"), ': <b>5, 7, 9</b> </li>
                    <li>', i18n()$t("Decimal numbers"), ': <b>3.2, 4.5, 7.8</b> </li>
                    <li>', i18n()$t("Fractions"), ': <b>2/3, 5/2, 11/9</b> </li>
              </ul>'),
              placement = "bottom",
              html = "true"),
          tags$script("$(function () {$('[data-toggle=\"popover\"]').popover()})"))
      })
      
      return(list(
        chain = reactive(input$vector),
        chain_validator = chain_iv
      ))
      
    }
  )}
