sidebarLayout(
  sidebarPanel(

    fileUI("file1", i18n),

    materialSwitch(
      inputId = "header", value = TRUE, status = "success",
      label = tags$b("Header"), right = TRUE, inline = TRUE, width="40%"
    ) %>% bs_attach_collapse("chain2"),
    
    bs_collapse(
      id = "chain2", 
      content = chainUI("vector2", i18n)
    ),
    
    tags$div(class="right-btn",
             actionBttn(
               inputId = "actualizar2", label = i18n$t("Update values"), 
               style = "simple", color = "success", size = "sm"))
  ),
  
  mainPanel(
    resultUI("resultados2", i18n)
  )
)