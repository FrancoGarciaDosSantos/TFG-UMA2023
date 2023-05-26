library(shiny)

formulaUI <- function(id, i18n = NULL) {
  ns <- NS(id)
  tagList(
    usei18n(i18n),
    
    textInput(ns("formula"), i18n$t("Enter a formula:"), 
              placeholder = "min(x,y)") %>%
      shinyInput_label_embed( uiOutput(ns("help")) )
  )
}

formulaServer <- function(id, i18n = NULL) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      formula_iv <- InputValidator$new()
      formula_iv$add_rule("formula", ~ if(. == ""){ i18n()$t("Formula required.") } )
      formula_iv$add_rule("formula", ~ check_formula(.))
      
      check_formula <- function(formula){
        p_count <- stri_count(formula, regex = c("\\(","\\)"))
        if(p_count[1] != p_count[2]){ return("The number of left and right parenthesis must match.") }
        
        values <- stri_match_all_regex(formula, pattern = "\\b[a-zA-Z]\\b")
        values <- unique(as.vector(values[[1]]))
        if(!is.na(values[1])){
          ind <- which(!(values %in% c("x","y")))
          if(!is_empty(ind)){ return("The two operands must be referred to as x and y.") }
        }
        return(NULL)
      }
      
      output$help <- renderUI({
        tagList(
          shiny_iconlink() %>%
            bs_embed_popover(
              title = i18n()$t("Valid inputs"),
              content = stri_c(
                '<ul> <li>', i18n()$t("The formula must be valid according to the R language syntax."), '</li>
                    <li>', i18n()$t("Refer to 'x' as the first operand."), '</li>
                    <li>', i18n()$t("Refer to 'y' as the second operand."), '</li> </ul>'),
              placement = "bottom",
              html = "true"),
          tags$script("$(function () {$('[data-toggle=\"popover\"]').popover()})"))
      })
      
      return(list(
        formula = reactive(input$formula),
        formula_validator = formula_iv
      ))
      
    }
  )}