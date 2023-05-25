
calcularTab <-
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("size", label = i18n$t("Select length:"),
                  min = 2, max = 11, value = 3),
      
      uiOutput("select_properties"),
      
      conditionalPanel(
        condition = 'input.properties && input.properties.indexOf("1") > -1',
        uiOutput("neutral_elem")
      ),
      
      autonumericInput(inputId = "num_mat", label = i18n$t("Number of matrices to keep: "), 
                       value = 10000, minimumValue = 0, maximumValue = 10000000, 
                       emptyInputBehavior = 0, allowDecimalPadding = FALSE,
                       decimalCharacter = ",", digitGroupSeparator = ".", 
                       align = "left", width = "90%"),
      helpText(i18n$t("Maximum possible number: 10.000.000")),
      
      tags$div(class="right-btn",
               actionBttn(
                 inputId = "calcular", label = i18n$t("Compute"), 
                 style = "simple", color = "success", size = "sm"
               ))
    ),
    
    mainPanel(
      withSpinner(uiOutput("resprop"), type=7, color = "#6F11A6", proxy.height = "200px")
    )
  )