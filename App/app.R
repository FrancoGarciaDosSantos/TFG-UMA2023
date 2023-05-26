# Load packages
library(shiny)
library(shinydashboard)
library(shinyMatrix)
library(shinyWidgets)
library(shinycssloaders)
library(shinyvalidate)
library(shinyalert)
library(shiny.i18n)
library(shinyjs)
library(rclipboard)
library(bsplus)
library(rmarkdown)
library(magrittr)
library(dplyr)
library(stringr)
library(stringi)
library(purrr)
library(tools)
library(data.table)
library(MASS)
library(RcppAlgos)

# Source script
source("propiedades.R")
source("utils.R")
source("contar.R")
invisible(sapply(list.files('modules', full.names = TRUE),source))

# File with translations
i18n <- Translator$new(translation_json_path = "data/translation.json")
i18n$set_translation_language("en") #default translation to display

# Shared objects
countries <- c("English (United Kingdom)", "Español (España)")
flags <- c("gb.svg", "es.svg")


header <- dashboardHeader(
    title = tagList(shiny.i18n::usei18n(i18n), i18n$t("Discrete operators")),
    tags$li(class = "dropdown",
            div(id = "lang", pickerInput(
                  inputId = "selected_language", label = NULL, 
                  choices = i18n$get_languages(),
                  selected = i18n$get_key_translation(),
                  choicesOpt = list(
                    content = 
                      mapply(countries, flags, FUN = function(country, flagUrl) {
                        HTML(paste(
                          tags$img(src=flagUrl, width=20, height=15),
                          country
                        ))
                      }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                  )
            ))
    )
  )
  
sidebar <- dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem(i18n$t("Analyze matrix"), icon = icon("sliders-h"),
        menuSubItem(i18n$t("Matrix by hand"), tabName = "tab1",
                    icon = icon("keyboard"), selected = TRUE),
        menuSubItem(i18n$t("Matrix by file"), tabName = "tab2", 
                    icon = icon("file-import")),
        menuSubItem(i18n$t("Matrix by formula"), tabName = "tab3",
                    icon = icon("code"))
        ),
      menuItem(i18n$t("Compare dominance"), tabName = "tab4",
                  icon = icon("greater-than-equal")),
      menuItem(i18n$t("Compute"), tabName = "tab5",
               icon = icon("calculator")),
      menuItem(i18n$t("Filter"), tabName = "tab6",
               icon = icon("filter"))
    )
  )
  
body <- dashboardBody(
    useShinyjs(),
    use_bs_tooltip(),
    use_bs_popover(),
    rclipboardSetup(),
    withMathJax(),
    chooseSliderSkin("Flat", color = "#7d39a6"),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "test.js")
    ),
    
    tabItems(
      tabItem(tabName = "tab1", 
              source( file.path("tabs","matrixTab.R"),  local = TRUE)$value),
      tabItem(tabName = "tab2", 
              source( file.path("tabs","fileTab.R"),  local = TRUE)$value),
      tabItem(tabName = "tab3", 
              source( file.path("tabs","formulaTab.R"),  local = TRUE)$value),
      tabItem(tabName = "tab4",
              source( file.path("tabs","compareTab.R"),  local = TRUE)$value),
      tabItem(tabName = "tab5", 
              source( file.path("tabs","computeTab.R"),  local = TRUE)$value),
      tabItem(tabName = "tab6", 
              source( file.path("tabs","filterTab.R"),  local = TRUE)$value)
    )
  )


ui = dashboardPage(
  skin = "purple",
  header = header,
  sidebar = sidebar,
  body = body,
)


server <- function(input, output, session){

  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  }) # Here is where we update language in session
  
  i18n_m <- reactive({
    req(input$selected_language)
    selected <- input$selected_language
    if (length(selected) > 0 & selected %in% i18n$get_languages()) {
      i18n$set_translation_language(selected)
    }
    i18n
  }) #reactive translator object for modules
  
  
  # ----------------------- chainTab server ----------------------- #
  chain1 <- chainServer("vector1", reactive(input$chain), i18n = i18n_m)
  chain1_iv <- chain1$chain_validator
  chain1_iv$enable()

  matrix1 <- matrixServer("matriz1", reactive(input$chain), i18n = i18n_m)
  matrix1_iv <- matrix1$matrix_validator
  matrix1_iv$enable()

  input_matrixTab <- reactive({
    if(!chain1_iv$is_valid() || !matrix1_iv$is_valid()){
      return(NULL)
    }
    n <- input$chain 
    matriz <- matrix(matrix1$matrix(), n, n)
    l <- list(cadena = chain1$chain(), matriz = matriz)
    l
    
  }) %>% bindEvent(input$actualizar)
  
  resultServer("resultados", input_matrixTab, tipo = "1", 
               actualizar_btn = reactive(input$actualizar), i18n = i18n_m)
  

  # ----------------------- fileTab server ----------------------- #
  file1 <- fileServer("file1", i18n = i18n_m)
  file1_iv <- file1$file_validator
  file1_iv$enable()
  
  chain2 <- chainServer("vector2", i18n = i18n_m)
  chain2_iv <- chain2$chain_validator
  chain2_iv$condition( function(){ !input$header })
  chain2_iv$enable()

  input_fileTab <- reactive({
    if(!file1_iv$is_valid() || !chain2_iv$is_valid()){ return(NULL) }

      l <- list(cadena = NULL, matriz = NULL)
      file_path <- file1$file()$datapath
      header <- input$header

      res <- read_file(file_path, header)
      if(!res$valida){
        shinyalert(text = res$mensaje)
        return(NULL)
      }

      l$matriz <- res$matriz
      l$cadena <- if(header){ res$cadena } else { chain2$chain() }
      l
    
  }) %>% bindEvent(input$actualizar2)

  resultServer("resultados2", input_fileTab, tipo = "2", 
               actualizar_btn = reactive(input$actualizar2), 
               header = reactive(input$header), i18n = i18n_m)

  
  # ----------------------- formulaTab server ----------------------- #
  chain3 <- chainServer("vector3", reactive(input$chain3), i18n = i18n_m)
  chain3_iv <- chain3$chain_validator
  chain3_iv$enable()
  
  formula1 <- formulaServer("formula1", i18n = i18n_m)
  formula1_iv <- formula1$formula_validator
  formula1_iv$enable()
  
  input_formulaTab <- reactive({
    if(!chain3_iv$is_valid() || !formula1_iv$is_valid()){
      return(NULL)
    }
    l <- list(cadena = chain3$chain(), matriz = formula1$formula())
    l
    
  }) %>% bindEvent(input$actualizar3)
  
  resultServer("resultados3", input_formulaTab, tipo = "3", 
               actualizar_btn = reactive(input$actualizar3), i18n = i18n_m)
  
  
  # ----------------------- computeTab server ----------------------- #
  
  output$select_properties <- renderUI({
    prettyCheckboxGroup(
      inputId = "properties", label = i18n$t("Choose properties:"), 
      status = "success", shape = "curve",
      outline = TRUE, bigger = TRUE,
      choiceNames = c(i18n$t("Identity element"),
                      i18n$t("Increasing monotony"),
                      i18n$t("Commutativity"),
                      i18n$t("Associativity")),
      choiceValues = c("1", "2", "3", "4"), 
    )
  })
  
  output$neutral_elem <- renderUI({
    pickerInput(
      inputId = "pos_neutral_elem",
      label = i18n$t("Position of the neutral element: "), 
      choices = seq(input$size), selected = 1, width = 'fit',
      options = list(size = 5)
    )
  })

  compute_res <- reactive({
    if(is.null(input$properties)){ return(NULL) }
    n <- input$size
    propiedades <- paste0(input$properties, collapse="")
    pos <- as.numeric(input$pos_neutral_elem)
    num_mat <- input$num_mat
    
    propiedades2 <- propiedades
    if(stri_detect_regex(propiedades, "12|123")){
      if(pos == n || pos == 1){
        propiedades2 <- paste0(propiedades2,"_n")
      }
    }
    
    formula <- get_formula_latex(propiedades2)
    contar <- get_formula_res(propiedades2, n, pos)
    seguir <- is.null(contar)
    
    res <- get_funcion_contar(propiedades, n, pos, num_mat, seguir)
    res$n <- n
    res$formula <- formula
    if(!is.null(contar)){ res$cont <- contar }
    res$propiedades <- input$properties
    res$pos <- pos
    return(res)
  }) %>% bindEvent(input$calcular)
  
  output$resprop <- renderUI({
    if(is.null(compute_res())){ return(NULL) }
    
    div(class="res-calcular",
      tags$span(tags$b("n: "), tag_i18n("Matrix size", i18n$t("Matrix size")) ),
      
      tags$span(tags$b( tag_i18n("Value of n:", i18n$t("Value of n:"))), compute_res()$n ),
      tags$span(withMathJax(HTML(paste(tags$b("Formula:"),
                                    tags$div(class="formula",
                                             compute_res()$formula)
                            )))
                ),
      tags$span(tags$b(tag_i18n("Result:", i18n$t("Result:"))), 
                           formatC(compute_res()$cont, big.mark = ".", 
                                   decimal.mark = ",", format = "f", 
                                   digits = 0)
                           ),
      if(length(compute_res()$lista) > 0){
        div(class = "btn-set-compare",
            downloadBttn("compute_dl1", tag_i18n("Generate PDF", i18n$t("Generate PDF")),
                         style = "simple", color = "primary", size = "sm",
                         icon = icon("file-pdf")),
            downloadBttn("compute_dl2", tag_i18n("Download as R Object", i18n$t("Download as R Object")), 
                         style = "simple", color = "primary", size = "sm"))
      }
    )
  }) %>% bindEvent(input$calcular)
  
  
  output$compute_dl1 <- downloadHandler(
    filename = function(){ 
      disable('compute_dl1_bttn')
      "matrices.pdf" 
      },
    content = function(file){
      tempReport <- file.path(tempdir(), "compute.Rmd")
      file.copy("compute.Rmd", tempReport, overwrite = TRUE)
      
      mapa <- list("1" = sprintf("%s (%i)", i18n$t("Identity element"), compute_res()$pos-1),
        "2" = i18n$t("Increasing monotony"),
        "3" = i18n$t("Commutativity"),
        "4" = i18n$t("Associativity"))
      props <- sapply(compute_res()$propiedades, function(x){
        mapa[[x]]
      })
      prop_name = i18n$t('Properties:')

      params <- list(matrices = compute_res()$lista, prop_name = prop_name, 
                     props = props, cadena = 0:(compute_res()$n-1))
      
      out <- render(tempReport, pdf_document(), output_file = file, 
                    params = params, envir = new.env(parent = globalenv()))
      file.rename(out, file)
      enable('compute_dl1_bttn')
    },
  )
  
  output$compute_dl2 <- downloadHandler(
    filename = function(){ "matrices.Rds" },
    content = function(file) {
      matrices <- isolate(compute_res()$lista)
      saveRDS(matrices, file)
    } 
  )
  
  
  # ----------------------- compareTab server ----------------------- #
  flag_compare <- reactiveVal('0')
  
  chain4 <- chainServer("vector4", reactive(input$chain4), i18n = i18n_m)
  chain4_iv <- chain4$chain_validator
  chain4_iv$enable()
  
  matrices_iv <- InputValidator$new()
  matrices_iv$add_rule("matrices_input", ~ if(is.null(.)){ i18n$t("File required.") })
  matrices_iv$add_rule("matrices_input", ~ if( !any(file_ext(.$datapath) == c("rds","Rds")) ){ 
    i18n$t("File is not formatted as a R object.")
  })
  matrices_iv$enable()
  
  matriz_comp1 <- matrixInputServer("compare1", reactive(input$chain4), i18n_m)
  matriz_comp2 <- matrixInputServer("compare2", reactive(input$chain4), i18n_m)
  
  
  output$compare_way <- renderUI({
    v <- c(1,2)
    names(v) <- c(i18n$t("Matrix vs Matrix"), i18n$t("Matrix vs List of Matrices"))
    pickerInput(
      inputId = "select_compare_way", label = i18n$t("Select a way to compare: "),
      width = 'auto', choices = v)
  })
  
  output$side_dom <- renderUI({
    v <- c(1,2)
    names(v) <- c(i18n$t("Matrices dominated by matrix 1º"), i18n$t("Matrices that dominate matrix 1º"))
    pickerInput(
      inputId = "side_dominar", label = i18n$t("Select side of dominance: "),
      choices = v
    )
  })
  

  input_compareTab <- reactive({
    if(!chain4_iv$is_valid()){ return(NULL) }
    
    matriz1 <- matriz_comp1$matriz()
    if(is_null(matriz1)){ return(NULL) }
    
    if(input$select_compare_way == "2"){
      if(!matrices_iv$is_valid()){ return(NULL) }
      matriz2 <- input$matrices_input$datapath
    } else {
      matriz2 <- matriz_comp2$matriz()
      if(is_null(matriz2)){ return(NULL) }
    }

    list(cadena = chain4$chain(), matriz1 = matriz1, matriz2 = matriz2,
         way = input$select_compare_way)
  })
  
  
  check_comparar <- reactive({
    input <- input_compareTab()
    way <- input$way
    if(is_null(input)){ return(list()) }

    cadena_input <- input$cadena
    matriz_input1 <- input$matriz1
    matriz_input2 <- if(way == "1"){ input$matriz2 } 
                      else { readRDS(input$matriz2) }

    res1 <- get_chain_matrix(cadena_input, matriz_input1, matriz_comp1$tipo(), 
                             header = FALSE, show_mIndx = TRUE)
    if(!res1$valida){ return(res1) }
    matriz_input1 <- res1$matriz
    
    if(way == "2"){
      res2 <- check_lista_matrices(matriz_input2, length(cadena_input))
    } else {
      res2 <- get_chain_matrix(cadena_input, matriz_input2, matriz_comp2$tipo(), 
                               header = FALSE, show_mIndx = TRUE)
      matriz_input2 <- res2$matriz
    }
    if(!res2$valida){ return(res2) }
    
    list(valida = TRUE, cadena = res1$cadena, matriz1 = matriz_input1, 
         matriz2 = matriz_input2, way = way)
    
  }) %>% bindEvent(input$actualizar_comp)


  observe({
    toggleState("comparar", condition = input$select_compare_way == flag_compare())
  })
  
  observeEvent(input$actualizar_comp,{
    removeUI("#resultado-compare")
  })
  
  
  output$res <- renderUI({
    check <- check_comparar()
    way <- check$way
    t <- tag_i18n("Matrix", isolate(i18n$t("Matrix")))
    
    if( !is_empty(check) ){
      if(check$valida){
        flag_compare(way)
        tagList(
          tags$div(class = "mat_output",
                   tags$span("/  ", t, " 1º" ),
                   withMathJax(check$matriz1$text)),
          if(way == "1"){
            tags$div(class = "mat_output",
                     tags$span("/  ", t, " 2º" ),
                     withMathJax(check$matriz2$text))  
          }
        )
      } else {
        flag_compare("0")
        shinyalert(title = i18n$t(check$titulo), 
                   text = text_alert(i18n$t(check$mensaje), check$mensaje2), 
                   type = "error", html = TRUE)
        return(NULL)
      }
    } else { 
      flag_compare("0")
      return(NULL) 
    }
  }) %>% bindEvent(check_comparar())
  
  
  res_comparar <- reactive({
    check <- check_comparar()
    if( flag_compare() == "1" ){
      dom <- check_dominancia(check$matriz1$values, check$matriz2$values,
                              check$cadena$original, check$cadena$mapa)
      
      mensaje <- tag_i18n(dom$mensaje, i18n$t(dom$mensaje))
      if(dom$res == 3){ return( list(mensaje = mensaje) ) } 
      else {
        mensaje <- tags$span(mensaje, tags$br(), withMathJax(dom$contraejemplo))
        return( list(mensaje = mensaje) )
      }
      
    } else {
      side_dom <- input$side_dominar
      n <- ncol(check$matriz1$values)
      mat <- matrix(check$matriz1$values, n,n)
      dom <- find_dominancia(mat, check$matriz2, side_dom)

      text_dom <- 
        if(side_dom == 1){stri_c("Matrices dominated by matrix ", c("1º", "A: "))} 
        else {stri_c("Matrices that dominate matrix ", c("1º", "A: "))}
      
      mensaje <- 
        div(class = "filter-res",
          span(tags$b(tag_i18n("Number of matrices in the list: ", i18n$t("Number of matrices in the list: "))), 
               formatC(length(check$matriz2), big.mark = ".", 
                       decimal.mark = ",", format = "f", 
                       digits = 0)),
          span(tags$b(tag_i18n(text_dom[1], i18n$t(text_dom[1])),":"), 
               formatC(length(dom), big.mark = ".", 
                       decimal.mark = ",", format = "f", 
                       digits = 0)),
          if(length(dom) > 0){
            div(class = "btn-set-compare", 
              downloadBttn("compare_dl1", tag_i18n("Generate PDF", i18n$t("Generate PDF")), 
                           style = "simple", color = "primary", size = "sm",
                           icon = icon("file-pdf")),
              downloadBttn("compare_dl2", tag_i18n("Download as R Object", i18n$t("Download as R Object")),
                           style = "simple", color = "primary", size = "sm"))
          }
        )
      
      return( list(mensaje = mensaje, lista = dom, text_dom = text_dom[2]) )
    }
  }) %>% bindEvent(input$comparar)
  
  
  output$res_compare <- renderUI({
      div(id = "resultado-compare", res_comparar()$mensaje)
  }) %>% bindEvent(res_comparar())
  
  
  output$compare_dl1 <- downloadHandler(
    filename = function(){ 
      disable('compare_dl1_bttn')
      "matrices.pdf" },
    content = function(file){
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed)
      tempReport <- file.path(tempdir(), "compare.Rmd")
      file.copy("compare.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      c <- 0:(input$chain4-1)
      A <- check_comparar()$matriz1$values
      lista <- res_comparar()$lista
      params <- list(c = c, A = A, lista = lista, 
                     text_dom = i18n$t(res_comparar()$text_dom) )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      out <- render(tempReport, pdf_document(), output_file = file,
                    params = params, envir = new.env(parent = globalenv()))
      file.rename(out, file)
      enable('compare_dl1_bttn')
    }
  )
  
  output$compare_dl2 <- downloadHandler(
    filename = function(){ "matrices.Rds" },
    content = function(file){
      matrices <- res_comparar()$lista
      saveRDS(matrices, file)
    } 
  )
  
  
  # ----------------------- filterTab server ----------------------- #
  matrices_iv2 <- InputValidator$new()
  matrices_iv2$add_rule("matrices_input2", ~ if(is.null(.)){ i18n$t("File required.") })
  matrices_iv2$add_rule("matrices_input2", ~ if( !any(file_ext(.$datapath) == c("rds","Rds")) ){ 
    i18n$t("File is not formatted as a R object.")
    })
  matrices_iv2$enable()
  
  
  output$filter_options <- renderUI({
  
    prettyRadioButtons(
      inputId = "filter_option", label = i18n$t("Choose property to filter by:"),
      status = "success", shape = "curve",
      choiceNames = c(i18n$t("Maximality"),
                      i18n$t("Archimedean"),
                      i18n$t("Divisibility")),
      choiceValues = c("1", "2", "3")
    )
  })
  
  
  input_filterTab <- reactive({
    if(!matrices_iv2$is_valid()){ return(NULL) }
    
    file_path <- input$matrices_input2$datapath

    matrices <- readRDS(file_path)

    res <- check_lista_matrices(matrices)
    if(!res$valida){
      shinyalert(title = i18n$t(res$titulo), 
                 text = "", type = "error", html = TRUE)
      return(NULL)
    }
    
    n <- length(matrices)
    opt <- input$filter_option
    
    if(input$filter_option == "2"){
      return( list(res = filter_archimedean(matrices), n = n) )
    } else if(input$filter_option == "3"){
      return( list(res = filter_divisibilidad(matrices), n = n) )
    } else {
      return( list(res = find_maximales_2(matrices), n = n) )
    }
    
  }) %>% bindEvent(input$filtrar)
  
  
  output$filter_res <- renderUI({
      filtered <- input_filterTab()
      if(is.null(filtered)){ return(NULL) }
    
      tagList(
        div(class = "filter-res",
            span(tags$b(tag_i18n("Number of matrices in the list: ", i18n$t("Number of matrices in the list: "))),
                 formatC(filtered$n, big.mark = ".", 
                         decimal.mark = ",", format = "f", 
                         digits = 0)),
            span(tags$b(tag_i18n("Number of matrices after being filtered: ", i18n$t("Number of matrices after being filtered: "))),
                 formatC(length(filtered$res), big.mark = ".", 
                         decimal.mark = ",", format = "f", 
                         digits = 0)),
            if(length(filtered$res) > 0){
              div(class = "btn-set-compare",
                  downloadBttn("filter_dl1", tag_i18n("Generate PDF", i18n$t("Generate PDF")),
                               style = "simple", color = "primary", size = "sm",
                               icon = icon("file-pdf")),
                  downloadBttn("filter_dl2", tag_i18n("Download as R Object", i18n$t("Download as R Object")),
                               style = "simple", color = "primary", size = "sm"))
            }
        )
      )
  }) %>% bindEvent(input$filtrar)
  
  
  output$filter_dl1 <- downloadHandler(
    filename = function(){ 
      disable('filter_dl1_bttn')
      "matrices.pdf" },
    content = function(file){
      tempReport <- file.path(tempdir(), "filter.Rmd")
      file.copy("filter.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(matrices = input_filterTab()$res)
      out <- render(tempReport, pdf_document(), output_file = file, 
                    params = params, envir = new.env(parent = globalenv()))
      file.rename(out, file)
      enable('filter_dl1_bttn')
    }
  )
  
  output$filter_dl2 <- downloadHandler(
    filename = function(){ "matrices.Rds" },
    content = function(file) {
      matrices <- isolate(input_filterTab()$res)
      saveRDS(matrices, file)
    } 
  )
  
  
} #end server


shinyApp(ui = ui, server = server)
