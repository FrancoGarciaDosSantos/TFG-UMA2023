
check_neutral_element = function(matriz_values, cadena_values, matriz, cadena){
  l <- list(res = F, text = "Matrix has no neutral element.", 
            link_text = "See", example = NULL)
  n = ncol(matriz_values)
  cadena_values <- as.numeric(cadena_values)
  for(i in seq.int(n)){
    if(identical(matriz_values[i,], cadena_values) & 
       identical(matriz_values[,i], cadena_values)){
      l$res = T
      l$text = "Matrix has a neutral element."
      l$example = sprintf("$$ %s_{(%i)} $$", cadena[i], i)
      l$pos <- i
      return(l)
    }
  } #end for
  return(l)
}


check_monotonic = function(matriz_values, matriz, cadena){
  l <- list(res = T, text = "The matrix is ascending.", 
            link_text = "See counterexample", example = NULL)
  n <- ncol(matriz_values)
  name <- NULL
  values <- NULL
  
  for(i in seq(n)){
    if( is.unsorted(matriz_values[i, ]) ){
      name <-  sprintf("\\varphi(%s, -)", cadena[i])
      values <- matriz[i,]
    } else if( is.unsorted(matriz_values[, i]) ){
      name <- sprintf("\\varphi(-, %s)", cadena[i])
      values <- matriz[,i]
    }
    if(!is.null(name)){
      l$res = F
      l$text = "The matrix is not ascending."
      l$example = stri_c("$$ ",name, "\\to", "\\begin{pmatrix}",
               stri_c(values, collapse = ", & "),
               "\\end{pmatrix} $$")
      return(l)
    }
  } #end for
  return(l)
}


operator_latex <- function(x, y, res){
  sprintf("\\varphi(%s,%s) = %s", x, y, res)
}


check_commutativity = function(matriz_values, matriz, cadena){
  l <- list(res = T, text = "The commutative property holds.", 
            link_text = "See counterexample", example = NULL)
  n = ncol(matriz_values)
  
  for(i in seq(n-1) ){
    for(j in seq(i+1,n) ){
      if(matriz_values[i, j] != matriz_values[j, i]){
        x <- cadena[i]
        y <- cadena[j]
        res <- matriz[i,j]
        res2 <- matriz[j,i]
        l$res = F
        l$text = "The commutative property does not hold."
        l$example = stri_c(
                      sprintf("$$ x = %s, y = %s $$", x, y),
                      stri_c("$$", operator_latex(x, y, res), " \\neq ", 
                             operator_latex(y, x, res2), "$$")
                      )
        return(l)
      } #end if
    } #end for
  } #end for
  return(l)
}


example_asoc <- function(x, y, z, res1, res2, res3, res4){
  res <- stri_c(
      sprintf("$$ x = %s, y = %s, z = %s $$", x, y, z),
      sprintf("$$ \\varphi(%s,%s) \\to %s $$","y", "z", operator_latex(y, z, res1)),
      sprintf("$$ \\varphi(%s,%s) \\to %s $$","x", "y", operator_latex(x, y, res2)),
      stri_c(
        sprintf("$$ \\varphi(%s, \\varphi(%s,%s)) \\to %s ","x", "y", "z", operator_latex(x, res1, res3)),
        " \\neq ",
        sprintf("\\varphi(\\varphi(%s,%s), %s) \\to %s $$","x", "y", "z", operator_latex(res2, z, res4))
      ))
  return(res)
}


check_associativity = function(matriz_values, cadena, mapa){
  l <- list(res = T, text = "The associative property holds.", 
            link_text = "See counterexample", example = NULL)
  colnames(matriz_values) <- cadena
  rownames(matriz_values) <- cadena
  
  for(x in cadena){
    for(y in cadena){
      for(z in cadena){
        res1 <- mapa[[as.character(matriz_values[y, z])]]
        res2 <- mapa[[as.character(matriz_values[x, y])]]
        res3 <- matriz_values[x, res1]
        res4 <- matriz_values[res2, z]
        if(res3 != res4){
          l$res = F
          l$text = "The associative property does not hold."
          l$example = example_asoc(x, y, z, res1, res2,
                                   mapa[[as.character(res3)]],
                                   mapa[[as.character(res4)]])
          return(l)
        }
      }
    }
  }
  return(l)
}


# Metodo para comprobar la asociatividad cuando la matriz es conmutativa
assoc_conm <- function(m,c){
  n <- ncol(m)
  rownames(m) <- c
  colnames(m) <- c
  
  matrices <- lapply(1:n, function(x){
    rows <- as.character(unname(m[x,x:n]))
    n_rows <- length(rows)
    m2 <- matrix(NA, n_rows,n)
    for(i in 1:n_rows){ 
      m2[i,] <- m[rows[i],] 
    }
    m2
  })
  
  for(i in 1:(n-1)){
    mat <- matrices[[i]]
    nrow <- nrow(mat)
    
    fila <- mat[1,i:n]
    columna <- mat[,i]
    comp = which(fila != columna)
    if(!is_empty(comp)){ return(FALSE) }
    
    rest_row <- 2:nrow
    rest_column <- (i+1):n
    for(pos in seq(length(rest_row))){
      x <- rest_row[pos]
      y <- rest_column[pos]
      
      fila <- mat[x,y:n]
      columna <- mat[x:nrow,y]
      columna_2 <- matrices[[y]][,i]
      
      comp = which(fila != columna)
      if(!is_empty(comp)){ return(F) }
      
      comp = which(fila != columna_2)
      if(!is_empty(comp)){ return(F) }
    }
  }
  return(TRUE)
}


check_archimedean <- function(matriz_values, cadena_values, mapa){
  l <- list(res = FALSE, text = "The archimedean property does not hold.", 
            link_text = "See counterexample", example = NULL)
  n <- ncol(matriz_values)
  
  for(i in c(1,n)){
    if(matriz_values[i,i] != cadena_values[i]){
      v <- mapa[i]
      l$example <- sprintf("$$ \\varphi(%s,%s) \\neq %s $$", v, v, v)
      return(l)
    }
  }
  for(i in 2:(n-1)){
    if(matriz_values[i,i] == cadena_values[i]){
      v <- mapa[i]
      l$example <- sprintf("$$ %s $$", operator_latex(v, v, v))
      return(l)
    }
  }

  l$res <- TRUE
  l$text <- "The archimedean property holds."
  return(l)
}


check_divisibilidad <- function(matriz_values, cadena_values, mapa){
  l <- list(res = T, text = "The divisibility property holds.", 
            link_text = "See counterexample", example = NULL)
  n = ncol(matriz_values)
  
  for(i in seq(n)){
    values_to_check <- cadena_values[1:i]
    diff <- setdiff(values_to_check, matriz_values[i,])
    if( !is_empty(diff) ){
      l$res <- FALSE
      l$text <- "The divisibility property does not hold."
      l$example <- sprintf("$$ \\nexists z \\ | \\ \\varphi(%s,z)=%s $$", 
                           mapa[as.character(cadena_values[i])],
                           mapa[as.character(diff[1])]) 
      return(l)
    }
  }
  return(l)
}


#x,y,z,t en la cadena, se cumple que A(B(x,z),B(y,t)) ≥ B(A(x,y),A(z,t)).
# Comprueba si A >> B o B >> A o sean incomparables
check_dominancia <- function(m1,m2, c, mapa){
  l <- list(res = 0, mensaje = "", contraejemplo = "")
  
  rownames(m1) <- c
  colnames(m1) <- c
  rownames(m2) <- c
  colnames(m2) <- c
  flag1 <- FALSE
  flag2 <- FALSE
  
  for(x in c){
    for(y in c){
      for(z in c){
        for(t in c){
          
          b1 <- mapa[[as.character(m2[x, z])]]
          b2 <- mapa[[as.character(m2[y, t])]]
          a1 <- mapa[[as.character(m1[x, y])]]
          a2 <- mapa[[as.character(m1[z, t])]]
          res1 <- m1[b1,b2]
          res2 <- m2[a1,a2]
          
          if(!flag1 && res1 > res2){
            flag1 <- TRUE
            l$contraejemplo <- example_dom(c(x,y,z,t), b1, b2, a1, a2, 
                                           mapa[[as.character(res1)]], 
                                           mapa[[as.character(res2)]])
            
          } else if(!flag2 && res1 < res2){
            flag2 <- TRUE
            l$contraejemplo <- example_dom(c(x,y,z,t), b1, b2, a1, a2, 
                                           mapa[[as.character(res1)]], 
                                           mapa[[as.character(res2)]])
          }
          
          if(flag1 && flag2){
            l$res <- 3
            l$mensaje <- "Neither matrix dominates the other."
            return(l)
          }
          
        } 
      } 
    }
  }
  
  if(flag1){
    l$res <- 1
    l$mensaje <- "Matrix 1º dominates Matrix 2º."
  } else if (flag2){
    l$res <- 2
    l$mensaje <- "Matrix 2º dominates Matrix 1º."
  } else {
    l$res <- 3
    l$mensaje <- "Neither matrix dominates the other."
  }
  return(l)
}


example_dom <- function(comb, b1, b2, a1, a2, res1, res2){
  x = comb[1]; y = comb[2]; z = comb[3]; t = comb[4];
  res <- stri_c(
    sprintf("$$ x = %s, y = %s, z = %s, t = %s $$", x, y, z, t),
    sprintf("$$ %s, \\ %s $$", operator_latex(x,z,b1), operator_latex(y,t,b2)),
    sprintf("$$ %s, \\ %s $$", operator_latex(x,y,a1), operator_latex(z,t,a2)),
    sprintf("$$ %s \\ < \\ %s $$", operator_latex(b1, b2, res1),
            operator_latex(a1, a2, res2))
    )
  return(res)
}


# Comprueba si A >> B
check_dominancia_2 <- function(m1, m2){
  n <- ncol(m1)
  c <- 1:n
  for(x in c){
    for(y in c){
      for(z in c){
        for(t in c){
          flag <- m1[m2[x,y]+1, m2[z,t]+1] < m2[m1[x,z]+1, m1[y,t]+1]
          if(flag){ return(FALSE) }
        } 
      } 
    }
  }
  return(TRUE)
}


# Devuelve todas la matrices dominadas por A (si opcion == 1)
# o todas la matrices que dominan a A (si opcion == 2)
find_dominancia <- function(A, lista_mat, opcion){
  if(opcion == "1"){
    ind <- sapply(lista_mat, function(x) check_dominancia_2(A,x) )
  } else {
    ind <- sapply(lista_mat, function(x) check_dominancia_2(x,A) )
  }
  
  return( lista_mat[ind] )
}


# Funcion que uso para la maximalidad
mayor_o_menor <- function(A,B){
  res1 <- sum(A > B)
  res2 <- sum(B > A)
  
  if(res1>0 && res2>0){
    return("3") #Incomparables
  } else if(res1 > 0){
    return("1") #A >= B
  } else {
    return("2") #B >= A
  }
}


find_maximales <- function(matrices){
  n <- length(matrices)
  m <- 1
  lista <- list()
  
  for(i in 1:n){
    A <- matrices[[i]]
    flag <- TRUE #se pone false si A no es maximal
    
    for(j in 1:n){
      
      if(flag){
        B <- matrices[[j]]
        if (all(B >= A)){
          if( !all(B == A) ){
            flag <- FALSE
          }
        }
      }
      
    }
    
    if(flag){
      lista[[m]] <- A
      m <- m+1
    }
  }
  
  lista
}


find_maximales_2 <- function(matrices){
  n <- length(matrices)
  lista <- list()
  m2 <- 1 #indice de la lista de matrices incomparables provisional
  incomp <- list()
  
  i <- 1
  A <- matrices[[i]]
  
  # Comparo la matriz A con las posteriores (B).
  # 1er caso: Si B es incomparable, guardo B en 'incomp'
  # 2do caso: Si B es mayor que A, me quedo con las matrices de 'incomp' 
  # incomparables a B y sigo a partir de B
  while(i < n){
    
    for(j in (i+1):n){
      B <- matrices[[j]]
      i <- j
      res <- mayor_o_menor(A,B)
      
      if(res == "3"){
        incomp[[m2]] <- B
        m2 <- m2+1
      } else if(res == "2"){
        if(!is_empty(incomp)){
          ind <- sapply(incomp, function(x){
            mayor_o_menor(B,x) == "3"
          })
          incomp <- incomp[ind]
          m2 <- sum(ind)+1
        }
        A <- B
        break
      }
      
    }
  }
  
  if(length(incomp)>0){
    lista <- find_maximales(incomp)
  }
  lista[[length(lista)+1]] <- A
  
  return(lista)
}


filter_divisibilidad <- function(lista_matrices){
  n <- ncol(lista_matrices[[1]])
  cadena <- 0:(n-1)
  mapa <- cadena
  names(mapa) <- cadena
  
  ind <- sapply(lista_matrices, function(x){
    check_divisibilidad(x, cadena, mapa)$res
  })
  
  return(lista_matrices[ind])
}


filter_archimedean <- function(lista_matrices){
  n <- ncol(lista_matrices[[1]])
  cadena <- 0:(n-1)
  mapa <- cadena
  names(mapa) <- cadena
  
  ind <- sapply(lista_matrices, function(x){
    check_archimedean(x, cadena, mapa)$res
  })
  
  return(lista_matrices[ind])
}


rellenar_tnormas <- function(lista){
  n <- ncol(lista[[1]])
  mat <- matrix(0,n+2,n+2)
  mat[n+2,] <- 0:(n+1)
  mat[,n+2] <- 0:(n+1)
  
  lista <- lapply(lista, function(x){
    mat[2:(n+1), 2:(n+1)] <- x
    mat
  })
  
  return(lista)
}

