compareTab <- fluidPage(

  column(width = 12,
         uiOutput("compare_way", style = "margin-bottom: 10px;")
  ),
  
  sidebarLayout(
    
    sidebarPanel( width = 12,
      fluidRow(
        column(width = 4,
               sliderInput("chain4", label = i18n$t("Select chain length:"),
                           min = 2, max = 11, value = 5),
               chainUI("vector4", i18n)
        ),
        column(width = 4, matrixInputUI("compare1", i18n)),
        column(width = 4,
               conditionalPanel(
                 condition = 'input.select_compare_way == 1',
                 matrixInputUI("compare2", i18n)
               ),
               conditionalPanel(
                 condition = 'input.select_compare_way == 2',
                 newFileInput("matrices_input", i18n$t("Select file of matrices:"),
                              accept = c(".rds", ".Rds"),
                              buttonLabel = tags$span(icon("folder-open"),"Browse"), 
                              placeholder = "matrices.Rds"),
                 uiOutput("side_dom")
               )
        )
      )
    ),
    
    mainPanel( width = 12,
      fluidRow(
        column(width = 2,
               tags$div(
                 actionBttn(
                   inputId = "actualizar_comp", label = i18n$t("Check values"), 
                   style = "simple", color = "success", size = "sm"
                 ),
                 actionBttn(
                   inputId = "comparar", label = i18n$t("Compare"),
                   style = "simple", color = "success", size = "sm"
                 ),
                 class = "btn-set-compare")
        ),
        column(width = 6, 
               withSpinner(uiOutput("res"), type=7, color = "#6F11A6", proxy.height = "200px")),
        column(width = 4, 
               withSpinner(uiOutput("res_compare"), type=7, color = "#6F11A6", proxy.height = "200px") )
      ),
    )
  
  )
)