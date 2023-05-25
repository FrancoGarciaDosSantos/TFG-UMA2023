formulaTab <- 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("chain3", label = i18n$t("Select chain length:"),
                  min = 2, max = 20, value = 3),
      
      chainUI("vector3", i18n),
      
      formulaUI("formula1", i18n),
      
      tags$div(class="right-btn",
               actionBttn(
                 inputId = "actualizar3", label = i18n$t("Update values"), 
                 style = "simple", color = "success", size = "sm"))
    ),
    
    mainPanel(
      resultUI("resultados3", i18n)
    )
  )