
filterTab <-
  sidebarLayout(
    sidebarPanel(
      
      newFileInput("matrices_input2", i18n$t("Select file of matrices:"),
                   accept = c(".rds", ".Rds"),
                   buttonLabel = tags$span(icon("folder-open"),"Browse"), 
                   placeholder = "matrices.Rds"),

      uiOutput("filter_options"),
      
      tags$div(class="right-btn",
               actionBttn(
                 inputId = "filtrar", label = i18n$t("Filter"), 
                 style = "simple", color = "success", size = "sm"
               ))
    ),
    
    mainPanel(
      withSpinner(uiOutput("filter_res"), type=7, color = "#6F11A6", proxy.height = "200px")
    )
  )
