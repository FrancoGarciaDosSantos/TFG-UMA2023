sidebarLayout(
  sidebarPanel(
    
    sliderInput("chain", label = i18n$t("Select chain length:"),
                min = 2, max = 20, value = 5),
    
    chainUI("vector1", i18n),

    matrixUI("matriz1", i18n),
    
    tags$div(class="right-btn",
             actionBttn(
               inputId = "actualizar", label = i18n$t("Update values"), 
               style = "simple", color = "success", size = "sm")
             )
  ),
  mainPanel(
    resultUI("resultados", i18n)
  )
  
)
