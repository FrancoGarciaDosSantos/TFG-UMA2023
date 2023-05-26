
#---------------------- Cadena/Matriz a valores/latex ----------------------#
frac_to_latex <- function(f){
  reduce_frac <- eval(parse(text = paste0("fractions(",f, ")") ))
  fr <- str_split_1(attr(reduce_frac,"fracs"),"/")
  if(length(fr) == 1){
    fr
  } else {
    stri_c("\\frac{",fr[1],"}{",fr[2],"}")
  }
}

chain_to_values_latex <- function(cadena_input){
  res <- list(original = NULL, text = NULL, values = NULL, mapa = NULL)
  
  cadena_values <- chain_to_values(cadena_input)
  orden <- order(cadena_values)
  cadena_values <- cadena_values[orden]
  cadena <- cadena_input[orden]
  res$values <- cadena_values
  
  ind_fraccion <- which(str_detect(cadena, "^[1-9]+/[1-9]+$"))
  cadena[ind_fraccion] <- cadena[ind_fraccion] %>% map_chr(frac_to_latex)
  res$original <- cadena
  
  mapa <- cadena
  names(mapa) <- cadena_values
  res$mapa <- mapa
  
  res$text <- chain_to_latex(cadena)

  return(res)
}

chain_to_values <- function(cadena){
  return( sapply(cadena, function(x) eval(parse(text=x)), 
                      USE.NAMES = FALSE) )
}

chain_to_latex <- function(cadena){
  start <- "$$ \\begin{pmatrix} "
  end <- " \\end{pmatrix} $$"
  return( stri_c(start, stri_c(cadena, collapse = ", & "), end) )
}


matrix_to_values <- function(matriz){
  return( apply(matriz, c(1,2), function(x) eval(parse(text=x)) ) )
}

matrix_to_latex_2 <- function(matriz){
  start <- "$$ \\begin{bmatrix}"
  end <- "\\end{bmatrix} $$"

  return( stri_c(start, 
                 apply(matriz, MARGIN=1, stri_c, collapse = " & ") %>%
                   stri_c(collapse = " \\\\ \n "),
                 end, sep = "\n") )
}

matrix_to_latex <- function(matriz, cadena, corner = "\\varphi") {
  n <- length(cadena)
  start <- stri_c("$$ \\begin{array}", 
                  stri_c(c("{","c", "|", rep("c",n-1), "}"), collapse = ""))
  end <- "\\end{array} $$"
  
  names <- mapply(cadena, 1:n, FUN = function(x,n){ sprintf("%s_{(%i)}",x,n) },
                  USE.NAMES = FALSE)
  first_row <- stri_c( c(corner, names), collapse = " & ")
  rest_row <- stri_c( stri_c(names, 
                             apply(matriz, MARGIN=1, stri_c, collapse = " & "),
                             sep = " & "), 
                      collapse = " \\\\ \n")
  
  return( stri_c(start, 
                 stri_c(first_row, stri_c("\\hline", rest_row, sep="\n"),
                        sep = " \\\\ \n"),
                 end, sep = "\n") )
}

matrix_to_pdf <- function(matriz, cadena, corner = " "){
  start <- "$$ \\bordermatrix{"
  end <- " } $$"
  n <- ncol(matriz)
  
  names <- mapply(cadena, 1:n, FUN = function(x, n){ 
    sprintf("%s_{(%i)}", x, n) }, USE.NAMES = FALSE)
  
  first_row <- stri_c( c(corner, names), collapse = " & ")
  rest_row <- stri_c(stri_c(names, 
                            apply(matriz, MARGIN=1, stri_c, collapse = " & "),
                            sep = " & "),
                     collapse = " \\cr \n")
  return( stri_c(start, 
                 stri_c(first_row, rest_row, sep = " \\cr \n "), 
                 end) )
}

#----------------------------- Funciones check -----------------------------#

title_names <- list("m" = "Error in matriz input", 
                    "c" = "Error in chain input",
                    "f" = "Error in formula input")

check_unique_values <- function(cadena){
  res <- list(valida = TRUE, titulo = NULL, mensaje = NULL)
  #Comprobar valores de la cadena sean unicos
  ind_duplicados <- which(duplicated(cadena))
  val_duplicados <- unique(cadena[ind_duplicados])
  if(length(ind_duplicados) > 0){
    res$valida <- FALSE
    res$titulo <- title_names$c
    res$mensaje <- "Chain contains duplicated values:"
    res$mensaje2 <- stri_c(val_duplicados, collapse = ", ")
    return(res)
  }
  return(res)
}

check_values_chain_matrix <- function(cadena, matriz){
  res <- list(valida = TRUE, titulo = NULL, mensaje = NULL)
  #Comprobar valores de la matriz estan en la cadena
  matriz_val_unicos <- unique(as.vector(matriz))
  dif_matriz_cadena <- setdiff(matriz_val_unicos, cadena)
  if (!is_empty(dif_matriz_cadena)){
    res$valida <- FALSE
    res$titulo <- title_names$m
    res$mensaje <- "Matrix contains different values from those of the chain:"
    res$mensaje2 <- stri_c(dif_matriz_cadena, collapse = ", ")
    return(res)
  }
  return(res)
}

check_dims <- function(cadena, matriz, header){
  res <- list(valida = TRUE, titulo = NULL, mensaje = NULL)
  dim_matriz <- dim(matriz)
  
  #comprobar que la matriz sea cuadrática
  if(dim_matriz[1] != dim_matriz[2]){
    res$valida <- FALSE
    res$titulo <- title_names$m
    res$mensaje <- "Matrix is not square."
    return(res)
  }
  
  #Si la matriz no tiene header, comprobar que la cadena tenga el tamaño correcto
  if(!header){
    chain_len = length(cadena)
    if(chain_len != dim_matriz[2]){
      res$valida <- FALSE
      res$titulo <- title_names$c
      res$mensaje <- "Chain length does not match the number of matrix columns."
      return(res)
    }
  }
  
  return(res)
}

check_header <- function(cadena, header){
  res <- list(valida = TRUE, titulo = NULL, mensaje = NULL)
  #Si hay header, comprobar que haya sido rellenada
  if(header){
    fill_chain <- str_detect(cadena, "^V[1-9]+")
    if(any(fill_chain)){
      res$valida <- FALSE
      res$titulo <- title_names$c
      res$mensaje <- "Chain is unfilled."
      return(res)
    }
  }
  return(res)
}

check_input <- function(x, header = FALSE){
  res <- list(valida = TRUE, titulo = NULL, mensaje = NULL, mensaje2 = NULL)
  if(is.matrix(x) || header){
    regex_valid <- str_detect(x, "^\\d+(\\.[0-9]+)?$|^[1-9]+/[1-9]+$")
    ind_not_valid <- which(!regex_valid)
    
    if(!is_empty(ind_not_valid)){
      res$valida <- FALSE
      if(is.matrix(x)){
        tipo <- "Matrix"
        res$titulo <- title_names$m
      } else {
        tipo <- "Chain"
        res$titulo <- title_names$c
      }
      res$mensaje <- stri_c(tipo, " contains unsupported values:")
      res$mensaje2 <- stri_c(unique(x[ind_not_valid]), collapse = ", ")
    }
  }
  
  return(res)
}

check_na <- function(x, header = FALSE){
  res <- list(valida = TRUE, titulo = NULL, mensaje = NULL)
  if(is.matrix(x) || header){
    if( any(str_detect(x, "^[\\s]*$")) ){
      res$valida <- FALSE
      if(is.matrix(x)){
        res$titulo <- title_names$m
        res$mensaje <- "Matrix has unfilled rows."  
      } else {
        res$titulo <- title_names$c
        res$mensaje <- "Chain is unfilled." 
      }
    }
  }
  return(res)
}


#---------------------- Comprobar cadena/matriz valida ----------------------#

list_of_checks <- list("1" = c("check_input(matriz)"),
                   "2" = c("check_header(cadena,header)",
                           "check_na(cadena, header)", "check_na(matriz)",
                           "check_dims(cadena,matriz,header)",
                           "check_input(cadena, header)","check_input(matriz)"),
                   "3" = c())

checks_chain_matrix <- function(cadena, matriz, tipo, header){
  res = list(valida = TRUE, titulo = NULL, mensaje = NULL, mensaje2 = NULL)
  funciones <- list_of_checks[[tipo]]
  
  # Aplica las funciones secuencialmente
  for (f in funciones) {
    res_check <- eval(parse(text = f))
    if(!res_check$valida){
      return(res_check)
    }
  }
  return(res)
}


get_chain_matrix <- function(cadena_input, matriz_input, tipo, header, show_mIndx){
  
  res <- checks_chain_matrix(cadena_input, matriz_input, tipo, header)
  if(!res$valida){ return(res) }
  
  cadena <- chain_to_values_latex(cadena_input)
  res <- check_unique_values(cadena$values)
  if(!res$valida){ return(res) }
  
  matriz_values <- if(tipo == 3){
    res <- form_matrix(matriz_input, cadena$values)
    if(res$valida){ res$matriz } 
    else { return(res) }
  } else { matrix_to_values(matriz_input) }
  
  res <- check_values_chain_matrix(cadena$values, matriz_values)
  if(!res$valida){ 
    if(tipo==3){ res$matrix <- matrix_to_latex_2(matriz_values) }
    return(res)
  }
  
  mapa <- cadena$mapa
  matriz_original <- matriz_values
  
  ind_algo <- which(str_detect(matriz_original, "[0-9]+.[1-9]+"))
  if(!is_empty(ind_algo)){
    matriz_original[ind_algo] <- matriz_original[ind_algo] %>% 
      sapply(function(x) mapa[[as.character(x)]], USE.NAMES = FALSE)
  }

  matriz_text <- if(show_mIndx){
    matrix_to_latex(matriz_original, cadena$original)
  } else {
    matrix_to_latex_2(matriz_original)
  }
  
  matriz <- list(original = matriz_original, text = matriz_text, 
                 values = matriz_values, formula = NULL)
  if(tipo==3){ matriz$formula <- matriz_input }
  
  list(valida = TRUE, cadena = cadena, matriz = matriz)
}


#--------------------------------------------------------------------------#

read_file <- function(file, header){
  res <- list(valida = FALSE, matriz = NULL, cadena = NULL, mensaje = NULL)
  
  tryCatch({
      tabla = fread(file = file, header = header, fill = TRUE, 
                    blank.lines.skip = TRUE, colClasses = 'character')
      
      matriz <- as.matrix(tabla)
      if(header){
        res$cadena <- colnames(matriz)
      }
      res$matriz <- unname(matriz)
      res$valida <- TRUE
    },
    error = function(e){ 
      res$mensaje <<- e
    },
    warning = function(w){
      res$mensaje <<- e
    }
  )
  return(res)
}

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

form_matrix <- function(formula, cadena_v){
  res <- list(valida = TRUE, matriz = NULL, titulo = NULL, mensaje = NULL)

  tryCatch({
    n <- length(cadena_v)
    matriz <- matrix(0,n,n)
    
    for(i in seq(1,n)){
      for(j in seq(1,n)){
        x <- cadena_v[i]
        y <- cadena_v[j]
        matriz[i,j] <- eval(parse(text=formula))
      }
    }
    res$matriz <- matriz
  },
  error = function(e){ 
    res$valida <<- FALSE
    res$titulo <<- title_names$f
    res$mensaje <<- "Check that the syntax of the formula is correct."
  },
  warning = function(w){
    res$valida <<- FALSE
    res$titulo <<- title_names$f
    res$mensaje <<- "Check that the syntax of the formula is correct."
  })
  
  return(res)
}

change_matrix_size = function(matriz, new_n){
  n = nrow(matriz)
  
  if (new_n < n) return(matriz[1:new_n,1:new_n])
  if (new_n > n){
    new_matriz <- diag(new_n)
    new_matriz[1:n,1:n] <- matriz
    return(new_matriz)
  }
  return(matriz)
}

check_lista_matrices <- function(lista, n = 0){
  l <- list(valida = FALSE, titulo = "")
  if(!is.list(lista) || is_empty(lista) ){
    l$titulo <- "R object must be a non-empty list."
    return(l)
  }
  res <- all(sapply(lista, is.matrix))
  if(!res){
    l$titulo <- "List must contain only matrices."
    return(l)
  }
  if(n==0){ n <- ncol(lista[[1]]) }
  res <- all(sapply(lista, function(x) all(dim(x) == n)))
  if(!res){
    l$titulo <- "All matrices must be the same size as the chain."
    return(l)
  }
  
  l$valida <- TRUE
  return(l)
}

#--------------------------------------------------------------------------#
propiedades_name <- list("1" = "Identity element",
                         "2" = "Increasing monotony",
                         "3" = "Commutativity",
                         "4" = "Associativity",
                         "5" = "Archimedean",
                         "6" = "Divisibility")

clasificar_operador <- function(propiedades, elem_neutro, n){
  res <- list(text1 = "", text2 = "", prop = NULL)
  
  indices <- which(propiedades)
  if(is_empty(indices)){
    res$text1 <- "It is a binary operator that does not satisfy any property."
    return(res)
  }
  
  indices_s <- paste0(indices, collapse = "")
  clasif <- if(stri_detect_fixed(indices_s, "1234")){
    x <- case_when(elem_neutro == 1 ~ "It is a t-conorm.", 
                   elem_neutro == n ~ "It is a t-norm.", 
                   TRUE ~ "It is a uninorm.")
    list(x, setdiff(indices, 1:4))
  } else if(stri_detect_fixed(indices_s, "12") && elem_neutro == n){
    list("It is a semicopula.", setdiff(indices, 1:2))
  } else if(stri_detect_fixed(indices_s, "2")){
    list("It is a conjuntor.", setdiff(indices, 2))
  } else {
    list("",indices)
  }

  if(clasif[[1]] == ""){
    res$text1 <- "It is a binary operator that satisfies the following properties: "
    res$prop <- sapply(clasif[[2]], function(x) propiedades_name[[x]])
  } else {
    res$text1 <- clasif[[1]]
    if(!is_empty(clasif[[2]])){
      res$text2 <- "In addition, it satisfies the following properties: "
      res$prop <- sapply(clasif[[2]], function(x) propiedades_name[[x]])
    }
  }
  return(res)
}


formula_latex <- list("1" = "$$ n^{(n-1)^{2}} $$",
                      "3" = "$$ n^{\\sum_{i=1}^{n}i} $$",
                      "12_n" = "$$ \\prod_{k=0}^{n-2}\\frac{(3k+1)(6k)!(2k)!}{(4k+1)!(4k)!} $$",
                      "13" = "$$ n^{(\\sum_{i=1}^{n-1}i)} $$",
                      "123_n" = "$$ \\prod_{k=0}^{n-2}\\frac{(3k+1)!}{((n-1)+k)!} $$"
                      )

formula <- list("1" = "n^((n-1)^2)",
                "3" = "n^(sum(1:n))",
                "12_n" = "formula_semicopula_n(n)",
                "13" = "n^(sum(1:(n-1)))",
                "123_n" = "formula_semicopula_conm_n(n)"
                )

formula_semicopula_n <- function(n){
  rango <- 0:(n-2)
  prod(
    sapply(rango, function(k){
      ((3*k+1)*factorial(6*k)*factorial(2*k)) / 
        (factorial(4*k+1)*factorial(4*k))
    })
  )
}

formula_semicopula_conm_n <- function(n){
  rango <- 0:(n-2)
  prod(
    sapply(rango, function(k){
      factorial(3*k+1) / 
        factorial((n-1)+k)
    })
  )
}

# formula_conjuntor_conm <- function(n){
#   rango <- 1:n
#   
#   res1 <- 
#   prod(
#     sapply(rango, function(i){
#       first <- ((2*i+n-1)/(2*i-1))
#       second <- 
#       prod(
#         sapply((i+1):n, function(j){
#           (i+j+n-1)/(i+j-1)
#         })
#       )
#       if(i != n){ first*second } else { first }
#     })
#   )
#   
#   res2 <- 
#     prod(
#       sapply(rango, function(i){
#         first <- ((2*i+n-2)/(2*i-1))
#         second <- 
#           prod(
#             sapply((i+1):n, function(j){
#               (i+j+n-2)/(i+j-1)
#             })
#           )
#         if(i != n){ first*second } else { first }
#       })
#     )
#   
#   res1-res2
# }

get_formula_latex <- function(propiedades){
  f <- formula_latex[[propiedades]]
  if(is.null(f)){
    f <- "-"
  }
  f
}

get_formula_res <- function(propiedades, n, pos){
  f <- formula[[propiedades]]
  if(!is.null(f)){
    f <- eval(parse(text = f))
  } else if(stri_detect_regex(propiedades, "12$")){
    a <- formula_semicopula_n(pos)
    b <- formula_semicopula_n(n-pos+1)
    c <- rellenar(n, pos, guardar = FALSE)
    f <- a*b*c*c
  } else if(stri_detect_regex(propiedades, "123$")){
    a <- formula_semicopula_conm_n(pos)
    b <- formula_semicopula_conm_n(n-pos+1)
    c <- rellenar(n, pos, guardar = FALSE)
    f <- a*b*c
  }
  f
}


funcion_contar <- list("1" = "neutro(n, pos, limite, seguir)",
                       "2" = "monotono(n, limite)",
                       "3" = "conmutativo(n, limite, seguir)",
                       "4" = "asociativo(n, limite)",
                       "14" = "neutro(n, pos, limite, seguir, TRUE)",
                       "24" = "monotono(n, limite, TRUE)",
                       "34" = "conmutativo(n, limite, seguir, TRUE)",
                       "13" = "neutro_conm(n, pos, limite, seguir)",
                       "23" = "monotono_conm(n, limite)",
                       "12" = "monotono_neutro_combogrid(n, pos, limite, seguir)",
                       "134" = "neutro_conm(n, pos, limite, seguir, TRUE)",
                       "234" = "monotono_conm(n, limite, TRUE)",
                       "124" = "monotono_neutro_combogrid(n, pos, limite, seguir, TRUE)",
                       "123" = "monotono_neutro_conm_combogrid(n, pos, limite, seguir)"
                       )


get_funcion_contar <- function(propiedades, n, pos, limite, seguir){
  
  if(propiedades=="1234"){
    if(pos %in% c(1,n) ){
      if(pos == n){
        file_name <- sprintf("data/lista%i.Rds", n-2)
      } else {
        file_name <- sprintf("data/tconorma%i.Rds", n)
      }

      l <- readRDS(file_name)
      cont <- length(l)
      
      if(limite == 0){
        l <- list()
      } else {
        if(limite < cont){ l <- l[1:limite] }
        if(pos == n && n>2){ l <- tnorma_entera(l, n) }
      }
      
      return( list(lista = l, cont = cont ) )
    } else {
      return( tuninormas(n, pos, limite) )
    }
  } else {
    f <- funcion_contar[[propiedades]]
    return( eval(parse(text = f )) )
  }

}


tnorma_entera <- function(lista, n){
  mat <- matrix(0,n,n)
  mat[n,] <- 0:(n-1)
  mat[,n] <- 0:(n-1)
  lapply(1:length(lista), function(x){
    mat[2:(n-1), 2:(n-1)] <- lista[[x]]
    mat
  })
}

# ----------------------------------------------------------------#

icon_result <- function(res){
  if(res){
    tags$i(class = "fa-solid fa-square-check")
  } else {
    tags$i(class = "fa-solid fa-square-xmark")
  }
}

tag_i18n <- function(s1, s2, asChar = FALSE){
  t <- tags$span(class = "i18n", "data-key" = s1, s2)
  if(asChar){
    t <- as.character(t)
  }
  t
}

text_alert <- function(mensaje, valores, matriz = NULL){
  if(is.null(mensaje)){ return("") }
  if(is.null(valores)){ return(mensaje) }
  as.character(
    div(mensaje, if(!is.null(matriz)){ matriz },
        span(tags$b(valores), style = "margin-top:10px;"),
        style = "display:flex; flex-direction:column;")
  )
}

newFileInput <- function(...){
  temp <- fileInput(...)
  temp$children[[3]] <- NULL
  temp$children[[2]]$children[[1]]$children[[1]]$attribs$class <- "btn btn-primary btn-file"
  temp$children[[2]]$children[[2]]$attribs$class <- "form-control file-name"
  temp
}
