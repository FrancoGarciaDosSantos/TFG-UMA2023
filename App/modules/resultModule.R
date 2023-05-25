library(shiny)

resultUI <- function(id, i18n = NULL) {
  ns <- NS(id)
  useShinyjs()
  
  tagList(
    tags$div(class = "header_resultado",
      downloadUI(ns("download"), i18n),
      awesomeCheckbox(
        inputId = ns("show_matrix_names"),
        label = tags$b(i18n$t("Display matrix row/column indexes")), 
        value = F,
        status = "success"
      )
    ),
    hr(),
    withSpinner(uiOutput(ns("res")), type=7, color = "#6F11A6", proxy.height = "200px"),
    withSpinner(uiOutput(ns("propiedades")), type=7, color = "#6F11A6", proxy.height = "200px"),
  )
}

resultServer <- function(id, inputs, tipo, actualizar_btn, header = FALSE, i18n) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      r <- reactiveVal(value = FALSE)

      check <- reactive({
        r(FALSE)
        if(is_null(inputs())){ return(list()) }
        inputs <- inputs()
        cadena_input <- inputs$cadena
        matriz_input <- inputs$matriz

        l <- get_chain_matrix(cadena_input, matriz_input, tipo, header(), 
                              show_mIndx = input$show_matrix_names)
        l
      }) %>% bindEvent(actualizar_btn())
      
      
      downloadServer("download", check, anal, i18n, r)
      
      
      output$res <- renderUI({
        check <- check()
        if( !is_empty(check) ){
          Sys.sleep(0.5)
          if(check$valida){
            tagList(
              tags$div(class = "chain_output",
                       tags$span("/  ", tag_i18n("Provided chain:", i18n()$t("Provided chain"))), 
                       withMathJax(check$cadena$text)),
              tags$div(class = "mat_output",
                       tags$span("/  ", tag_i18n("Provided matrix:", i18n()$t("Provided matrix"))),
                       withMathJax(check$matriz$text)),
              actionBttn(
                inputId = ns("analizar"), label = tag_i18n("Analize", i18n()$t("Analize")), 
                style = "simple", color = "success", size = "sm")
            )
          } else {
            shinyalert(title = i18n()$t(check$titulo), 
                       text = text_alert(i18n()$t(check$mensaje), check$mensaje2, check$matrix), 
                       type="error", html = TRUE)
            return(NULL)
          }
        } else {
          return(NULL)
        }
      }) %>% bindEvent(check())

      
      observeEvent(actualizar_btn(),{
        removeUI(paste0("#",ns("resultado")))
      })
            
      observeEvent(input$analizar,{
        r(TRUE)
        removeUI(paste0("#",ns("analizar")))
      })

      
      anal <- reactive({
        cadena <- check()$cadena
        matriz <- check()$matriz
        pos_neutro <- NULL
        
        resultados <- list(
          check_neutral_element(matriz$values, cadena$values, 
                                matriz$original, cadena$original),
          check_monotonic(matriz$values, matriz$original, cadena$original),
          check_commutativity(matriz$values, matriz$original, cadena$original),
          check_associativity(matriz$values, cadena$original, cadena$mapa),
          check_archimedean(matriz$values, cadena$values, cadena$mapa),
          check_divisibilidad(matriz$values, cadena$values, cadena$mapa))

        if(resultados[[1]]$res){
          pos_neutro <- resultados[[1]]$pos
          resultados[[1]]$pos <- NULL
        }
        resultados <- list_transpose(resultados, default = NA)
        
        clasif <- clasificar_operador(resultados$res, pos_neutro, length(cadena$values))
        clasif$text1 <- tag_i18n(clasif$text1, i18n()$t(clasif$text1), TRUE)
        if(!is.null(clasif$text2)){
          clasif$text2 <- tag_i18n(clasif$text2, i18n()$t(clasif$text2), TRUE)
        }
        if(!is.null(clasif$prop)){
          clasif$prop <- stri_c(
            sapply(clasif$prop, function(x){tag_i18n(x, i18n()$t(x), TRUE)}),
            collapse = ", ")
        }
        resultados$clasif <- clasif
        
        return(resultados)
      }) %>% bindEvent(input$analizar)
      
      
      output$propiedades <- renderUI({
        anal <- anal()
        clasif <- anal$clasif

        Sys.sleep(0.5)
        tags$div(id = ns("resultado"),
            tags$div(class = "resultado-propiedades",
                     tags$div(class = "mat_output",
                              tags$span("/  ", tag_i18n("Properties:", i18n()$t("Properties:")) )),
                     mapply(anal$res, anal$text, anal$link_text, anal$example, 1:6, FUN = 
                              function(res, text, link_t, cnt, n){
                                tagList(
                                  tags$div(class = "resultado-propiedad",
                                      tags$span(icon_result(res), tag_i18n(text, i18n()$t(text))),
                                      if(!is.na(cnt)){
                                        tags$span(
                                          actionLink(ns(paste0("counter_link",n)),
                                                     tag_i18n(link_t, i18n()$t(link_t))) %>%
                                            bs_attach_collapse(ns(paste0("counter_ex",n)))
                                        )
                                      }
                                  ),
                                  if(!is.na(cnt)){
                                    tags$div(class = "ejemplo-propiedad",
                                      bs_collapse(ns(paste0("counter_ex",n)), 
                                                  content = 
                                                    tags$div(class = "contraejemplo",
                                                             withMathJax(cnt),
                                                    style = "display: flex;flex-direction: column;")
                                    ))
                                  }
                                )
                              }, SIMPLIFY=FALSE)
                     ),#end div
            
            tags$div(class = "clasificacion",
                     tags$div(class = "mat_output",
                              tags$span("/  ", tag_i18n("Classification:", i18n()$t("Classification:")) )),
                     if(!is.null(clasif$text2)){
                       tagList(
                         span(HTML(clasif$text1)),
                         span(HTML(stri_c(clasif$text2,clasif$prop)))
                        )
                     } else {
                       tagList(
                        span(HTML(stri_c(clasif$text1, clasif$prop, 
                                         ignore_null = TRUE)))
                       )
                     }) #end div
        ) #end div
        
      }) %>% bindEvent(anal())
      
    }
)}
