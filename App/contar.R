# Obtengo las t-normas con n a partir de las t-normas con n-1 
# rellenando la ultima columna
tnormas <- function(n){
  list_name <- sprintf("data/lista%i.Rds",n-1)
  lista_m <- readRDS(list_name)
  
  m <- 0
  lista <- list()
  fin <- 0
  
  for (r in 1:length(lista_m)){
    M <- lista_m[[r]]
    P <- cbind(rbind(M,M[n-1,]), c(M[,n-1],M[n-1,n-1]-1))
    fin <- 0
    while( fin == 0 ){
      bandera <- 0
      for (i in n:1){
        if(bandera == 0){
          if (P[i,n] != i){
            P[i,n] <- P[i,n]+1
            for(j in i:n){
              P[j,n] = max(P[i,n], P[j,n-1])
              P[n,j] = P[j,n]
            }
            
            bandera <- 2
            for (i in 1:(n-1)){
              if(bandera==2){
                for (k in 1:n){
                  if (P[k,n] != 0 && P[i,k] != 0 && P[P[i,k],n] != P[i,P[k,n]] ) {
                    bandera <- 3
                    break
                  }
                }
              } else { break }
            }
            
          }
        }
        
      }
      
      if (bandera == 2){
        m <- m+1
        lista[[m]] <- P
      }
      if (bandera == 0){
        fin <- 1
      }
      
    }
    
  }
  #list_name <- sprintf("data/lista%i.Rds",n)
  #saveRDS(lista, list_name)
  lista
}


#Función que transforma la matriz de un operador en su dual correspondiente
dual_func <- function(m){
  n <- ncol(m)
  m2 <- m
  m[1:n,1:n] <- n-1-m2[n+1-(1:n),n+1-(1:n)]
  m
}


#Obtengo las t-conormas a partir de las t-normas
tconormas <- function(n){
  list_name <- sprintf("data/lista%i.Rds",n-2)
  lista_m <- readRDS(list_name)
  lista <- list()
  ind <- 1
  for (r in length(lista_m):1){
    M <- lista_m[[r]]
    M <- unname(cbind(0, rbind(0, M, 1:(n-2)), 0:(n-1)))
    M <- dual_func(M)  
    lista[[ind]] <- M
    ind <- ind+1
  }
  #list_name <- sprintf("data/tconorma%i.Rds",n)
  #saveRDS(lista, list_name)
  lista
}


# Función que me devuelve todas las posibles combinaciones de la esquina superior
# derecha de matrices monotonas y con 'pos_neutro < n'.
rellenar <- function(n, pos, guardar = TRUE){
  n_col <- n-pos
  n_fila <- pos-1
  m <- matrix(0,n_fila, n_col)
  
  max_valores <- pos:(n-1)
  cont <- 0
  lista <- list()
  
  recursivo <- function(fila, combs, mat){
    n_combs <- length(combs[,1])
    
    if(fila == n_fila){
      for(i in 1:n_combs){
        comb <- as.numeric(combs[i,])
        mat[fila,] <- comb
        cont <<- cont+1
        if(guardar){ lista[[cont]] <<- mat }
      }
    } else {
      for(i in 1:n_combs){
        comb <- as.numeric(combs[i,])
        mat[fila,] <- comb
        l <- lapply(1:n_col , function(x) max(fila, comb[x]):max_valores[x] )
        combs_new <- comboGrid(l)
        recursivo(fila+1, combs_new, mat)
      }
    }
  }
  
  l <- lapply(1:n_col , function(x) 0:max_valores[x] )
  recursivo(1, comboGrid(l), m)
  
  if(guardar){ return(lista) } else { return(cont) }
}


# Obtengo las demás t-uninormas necesarias a partir de las combinaciones de 
# las t-normas, t-conormas y la esquina derecha superior.
tuninormas <- function(n, pos, limite){
  list_name <- sprintf("data/lista%i.Rds", pos-2)
  lista_m <- readRDS(list_name)
  
  list_name <- sprintf("data/tconorma%i.Rds",n-pos+1)
  lista_m2 <- readRDS(list_name)
  
  lista_m3 <- rellenar(n, pos)
  
  P <- matrix(0,n,n)
  P[pos,] <- 0:(n-1)
  P[,pos] <- 0:(n-1)
  
  m <- 0
  lista <- list()
  
  columnas <- (pos+1):n
  filas <- 1:(pos-1)
  
  for (r in 1:length(lista_m)){
    M <- lista_m[[r]]
    if(pos==2){
      P[1:2, 1:2] <- M 
    } else {
      P[2:(pos-1), 2:(pos-1)] <- M 
    }
    
    for (r2 in 1:length(lista_m2)){
      M2 <- lista_m2[[r2]]
      n2 <- ncol(M2)
      P[(pos+1):n, (pos+1):n] <- M2[2:n2, 2:n2] + pos-1 ####
      
      for(r3 in 1:length(lista_m3)){
        M3 <- lista_m3[[r3]]
        P[filas, columnas] <- M3
        P[columnas,filas] <- t(M3)
        
        if(assoc_conm(P,0:(n-1))){
          m <- m+1
          if(m<=limite){ lista[[m]] <- P }
        }
        
      }
    }
  }
  #list_name <- sprintf("data/tuninorma%i_%i.Rds", n, pos)
  #saveRDS(lista, list_name)
  return( list(lista = lista, cont = m) )
}


# Ej: (7,6) <-> (7,2)
tuninorma_contraria <- function(n, pos){
  list_name <- sprintf("data/tuninorma%i_%i.Rds", n, pos)
  lista_m <- readRDS(list_name)
  m <- 1
  lista <- list()
  for(i in length(lista_m):1){
    M <- lista_m[[i]]
    M <- dual_func(M)  
    lista[[m]] <- M
    m <- m+1
  }
  #list_name <- sprintf("data/tuninorma%i_%i.Rds", n, n-pos+1)
  #saveRDS(lista, list_name)
  lista
}


# Funcion para obtener semicopulas conmutativas
monotono_neutro_conm_combogrid <- function(n, pos, limite, seguir){
  cadena <- 0:(n-1)
  indices <- setdiff(1:n, pos)
  
  mat <- matrix(0,n,n)
  mat[pos,] <- cadena
  mat[,pos] <- cadena
  
  
  recursive_function <- function(m, mat, combinations, contador) {
    row <- indices[m]
    n_combs <- length(combinations[,1])
    
    if (m == (n-1)) {
      for (i in 1:n_combs) {
        mat[row,] <- as.numeric(combinations[i,])
        mat[,row] <- as.numeric(combinations[i,])
        contador <- contador + 1
        if(contador <= limite){
          matrices[[contador]] <<- mat
        } else if(!seguir){ return(contador) }
      }
    } else {
      next_row <- indices[m+1]
      
      for (i in 1:n_combs) {
        comb <- as.numeric(combinations[i,])
        mat[row, ] <- comb
        mat[, row] <- comb
        
        cola <- mat[next_row, 1:(next_row-1)]
        
        if(next_row < pos){
          
          l <- lapply(1:n, function(x){
            if(x < next_row){
              cola[x]
            } else if(x < pos){
              comb[x]:cadena[next_row]
            } else if (x > pos){
              max(cadena[next_row], comb[x]):cadena[x]
            } else { cadena[next_row] }
          })
        } else {
          l <- lapply(1:n, function(x){
            if(x < next_row){
              cola[x]
            } else if(x < pos){
              mat[(next_row-1),x]:cadena[next_row]
            } else if(x > pos){
              max(cadena[next_row] , mat[next_row-1, x]):(n-1)
            } else { cadena[next_row] }
          })
        }
        contador <- recursive_function(m+1, mat, comboGrid(l) , contador)
        if(contador >= limite && !seguir){ return(contador) }
      }
      
    }
    return(contador)
  }
  
  matrices <- list()
  contador <- 0
  
  if(pos>1){
    l <- lapply(1:n, function(x){ if(x<=pos){ 0 } else { 0:cadena[x] }} )
  } else {
    l <- lapply(1:n, function(x){ if(x==pos){ 1 } else { cadena[x]:(n-1) }} )
  }
  
  contador <- recursive_function(1, mat, comboGrid(l) , contador)
  list(lista = matrices, cont = contador)
}


# Funcion para obtener semicopulas (asociativas si 'asoc = TRUE')
monotono_neutro_combogrid <- function(n, pos, limite, seguir, asoc = FALSE){
  cadena <- 0:(n-1)
  mapa <- as.character(cadena)
  names(mapa) <-as.character(cadena)
  indices <- setdiff(1:n, pos)
  
  mat <- matrix(0,n,n)
  mat[pos,] <- cadena
  mat[,pos] <- cadena
  
  
  recursive_function <- function(m, mat, combinations, contador) {
    row <- indices[m]
    n_combs <- length(combinations[,1])
    
    if (m == (n-1)) {
      for (i in 1:n_combs) {
        valido <- TRUE
        mat[row, ] <- as.numeric(combinations[i,])

        if(asoc){ valido <- check_associativity(mat, as.character(cadena), mapa)$res }
        if(valido){
          contador <- contador + 1
          if(contador <= limite){
            matrices[[contador]] <<- mat
          } else if(!seguir){ return(contador) }
        }
      }
    } else {
      next_row <- indices[m+1]
      
      for (i in 1:n_combs) {
        comb <- as.numeric(combinations[i,])
        mat[row, ] <- comb
        
        if(next_row < pos){
          l <- lapply(1:n, function(x){
            if(x < pos){
              comb[x]:cadena[min(next_row,x)]
            } else if (x > pos){
              max(cadena[next_row], comb[x]):cadena[x]
            } else { cadena[next_row] }
          })
        } else {
          l <- lapply(1:n, function(x){
            if(x < pos){
              mat[(next_row-1),x]:cadena[next_row]
            } else if(x > pos){
              max(cadena[next_row] , mat[next_row-1, x]):(n-1)
            } else { cadena[next_row] }
          })
        }
        contador <- recursive_function(m+1, mat, comboGrid(l) , contador)
        if(contador >= limite && !seguir){ return(contador) }
      }
      
    }
    return(contador)
  }
  
  matrices <- list()
  contador <- 0
  
  if(pos>1){
    l <- lapply(1:n, function(x){ if(x<=pos){ 0 } else { 0:cadena[x] }} )
  } else {
    l <- lapply(1:n, function(x){ if(x==pos){ 1 } else { cadena[x]:(n-1) }} )
  }
  
  contador <- recursive_function(1, mat, comboGrid(l) , contador)
  
  return(list(lista = matrices, cont = contador))
}


# Funcion para obtener conjuntores conmutativos (asociativos si 'asoc = TRUE')
monotono_conm <- function(n, limite, asoc = FALSE){
  cadena <- 0:(n-1)
  P <- matrix(0,n,n)
  P[n,n] <- P[n,n] - 1
  
  m <- 0
  lista <- list()
  cadena <- as.character(0:(n-1))

  fin <- 0
  while (fin == 0){
    bandera <- 0
    for (i in n:1){
      for (j in n:i){
        
        if (bandera == 0){
          if (P[i,j] != (n-1) ){
            v <- P[i,j] + 1
            
            #Cuando la diferencia entre i y j sea >= 2, asegurar de mantener
            #la monotonia para la submatriz m[i:(j-1), i:(j-1)]
            if( j-i >= 2 ){
              for(col in i:(j-1)){
                v3 <- P[i, col]
                P[i:col,col] <- v3
                P[col, i:col] <- v3
              }
            }
            
            if(i==1){
              #Si es la primera fila, rellenar todo con el mismo valor
              P[1:n, j:n] <- v
              P[j:n, 1:j] <- v
            } else {
              P[i:j,j] <- v
              P[j,i:j] <- v
              
              if(j<n){
                for(col in (j+1):n){
                  v_m <- max(v, P[i-1,col])
                  P[i:col, col] <- v_m
                  P[col, i:col] <- v_m
                }
              }
            }
            
            bandera <- 1
            if(!asoc){
              m <- m + 1
              bandera <- 2
            } else if( assoc_conm(P, cadena) ){
                m <- m + 1
                bandera <- 2
            }
            
          }
        }
        
      }
    }
    
    if(bandera == 2){
      if(m<=limite){
        lista[[m]] <- P
      }
    } else if(bandera == 0){ fin <- 1 }
  }
  
  list(lista = lista, cont = m)
}


# Genera matrices con elemento neutro y conmutativas (asociativas si 'asoc = TRUE')
neutro_conm <- function(n, pos, limite, seguir, asoc = FALSE){
  indices <- setdiff(n:1, pos)

  mat <- matrix(0,n,n)
  mat[pos,] <- 0:(n-1)
  mat[,pos] <- 0:(n-1)
  mat[ indices[1], indices[1] ] <- mat[ indices[1], indices[1] ] - 1
  
  m <- 0
  lista <- list()

  cadena <- as.character(0:(n-1))
  
  fin <- 0
  while (fin == 0){
    bandera <- 0
    for (i in indices){
      for (j in n:i){
        if(bandera==0){
          if (j != pos){
            if (mat[i,j] != (n-1) ){
              bandera <- 1
              mat[i,j] <- mat[i,j] + 1
              mat[j,i] <- mat[i,j]
              
              if(!asoc){
                m <- m + 1
                bandera <- 2
              } else {
                if( assoc_conm(mat, cadena) ){
                  m <- m + 1
                  bandera <- 2
                }
              }
  
            } else { 
              mat[i,j] <- 0
              mat[j,i] <- 0
            }
          }
        }
        
      }
    }
    
    if(bandera == 2){
      if(m<=limite){
        lista[[m]] <- mat
      } else if(!seguir){
        return( list(lista = lista, cont = m) )
      }
    } else if( bandera == 0 ){ fin <- 1 }
  }
  
  list(lista = lista, cont = m)
}


# Genera matrices con elemento neutro (asociativas si 'asoc = TRUE')
neutro <- function(n, pos, limite, seguir, asoc = FALSE){
  indices <- setdiff(n:1, pos)
  
  mat <- matrix(0,n,n)
  mat[pos,] <- 0:(n-1)
  mat[,pos] <- 0:(n-1)
  mat[ indices[1], indices[1] ] <- mat[ indices[1], indices[1] ] - 1
  
  m <- 0
  lista <- list()
  fin <- 0
  
  cadena <- as.character(0:(n-1))
  mapa <- cadena
  names(mapa) <- cadena
  
  while (fin == 0){
    bandera <- 0
    for (i in indices){
      for (j in indices){
        
        if(bandera == 0){
          if (mat[i,j] != (n-1) ){
            bandera <- 1
            mat[i,j] <- mat[i,j] + 1

            if(!asoc){
              m <- m + 1
              bandera <- 2
            } else {
              if( check_associativity(mat, cadena, mapa)$res ){
                m <- m + 1
                bandera <- 2
              }
            }
          } else { 
            mat[i,j] <- 0
          }
        }
        
      }
    }
    
    if(bandera == 2){
      if(m<=limite){
        lista[[m]] <- mat
      } else if(!seguir){
          return( list(lista = lista, cont = m) )
      }
    } else if( bandera == 0 ){ fin <- 1 }
  }
  
  list(lista = lista, cont = m)
}


# Genera matrices monotonas (asociativas si 'asoc = TRUE')
monotono <- function(n, limite, asoc = FALSE){
  indices <- n:1
  mat <- matrix(0,n,n)
  mat[n,n] <- mat[n,n] - 1
  
  m <- 0
  lista <- list()
  fin <- 0
  
  cadena <- as.character(0:(n-1))
  mapa <- cadena
  names(mapa) <- cadena
  
  while (fin == 0){
    bandera <- 0
    for (i in indices){
      for (j in indices){
        
        if(bandera == 0){
          if (mat[i,j] != (n-1) ){
            bandera <- 1
            mat[i,j] <- mat[i,j] +1 #sumo +1
            
            # Para cada columna desde la actual hasta la primera, relleno desde 
            # la fila i hasta la ultima el valor actual de M[i,j] 
            # (mantener monotonia izquierda)
            for(col in j:1){
              mat[i:n,col] <- mat[i, col]
            }
            
            #Para las columnas posteriores:
            if(j < n){
              # Cojo el maximo entre la 'columna anterior - misma fila' y 
              # 'anterior fila - misma columna' (mantener monotonia derecha)
              if(i>1){
                for(col in (j+1):n){
                  max_v <- max(mat[i,col-1], mat[i-1,col])
                  mat[i:n,col] <- max_v
                }
              } else {
                  mat[i:n,j:n] <- mat[i,j]
              }
            }
            
            if(!asoc){
              m <- m + 1
              bandera <- 2
            } else {
              if( check_associativity(mat, cadena, mapa)$res ){
                m <- m + 1
                bandera <- 2
              }
            }

          }
        }
        
      }
    }
    
    if(bandera == 2){
      if(m<=limite){
        lista[[m]] <- mat
      }
    } else if( bandera == 0 ){ fin <- 1 }
  }
  
  list(lista = lista, cont = m)
}


# Genera matrices conmutativas (asociativas si 'asoc = TRUE')
conmutativo <- function(n, limite, seguir, asoc = FALSE){
  indices <- n:1
  mat <- matrix(0,n,n)
  mat[n,n] <- mat[n,n] - 1

  m <- 0
  lista <- list()
  fin <- 0
  
  cadena <- as.character(0:(n-1))
  
  while (fin == 0){
    bandera <- 0
    for (i in indices){
      for (j in n:i){
        
        if(bandera == 0){
          if (mat[i,j] != (n-1) ){
            bandera <- 1
            mat[i,j] <- mat[i,j] + 1
            mat[j,i] <- mat[i,j]
            
            if(!asoc){
              m <- m + 1
              bandera <- 2
            } else {
              if( assoc_conm(mat, cadena) ){
                m <- m + 1
                bandera <- 2
              }
            }

          } else { 
            mat[i,j] <- 0
            mat[j,i] <- 0
          }
        }
        
      }
    }
    
    if(bandera == 2){
      if(m<=limite){
        lista[[m]] <- mat
      } else if(!seguir){
        return( list(lista = lista, cont = m) )
      }
    } else if( bandera == 0 ){ fin <- 1 }
  }
  
  list(lista = lista, cont = m)
}


# Genera matrices asociativas
asociativo <- function(n, limite){
  indices <- n:1
  mat <- matrix(0,n,n)
  mat[n,n] <- mat[n,n] - 1
  
  cadena <- as.character(0:(n-1))
  mapa <- cadena
  names(mapa) <- cadena
  
  m <- 0
  lista <- list()
  fin <- 0
  while (fin == 0){
    bandera <- 0
    for (i in indices){
      for (j in indices){
        if(bandera == 0){
          if (mat[i,j] != (n-1) ){
            bandera <- 1
            mat[i,j] <- mat[i,j] + 1
            if(check_associativity(mat, cadena, mapa)$res){ 
              m <- m + 1
              bandera <- 2
            }
          } else { mat[i,j] <- 0 }
        }
      }
    }
    
    if(bandera == 2){
      if(m<=limite){
        lista[[m]] <- mat
      }
    } else if( bandera == 0 ){ fin <- 1 }
  }
  
  list(lista = lista, cont = m)
}


# Función que me devuelve todas las posibles combinaciones de la esquina superior
# derecha de matrices monotonas y con 'pos_neutro < n'.
# rellenar_2 <- function(n, pos){
#   columna_max <- n-pos
#   fila_max <- pos-1
#   m <- matrix(0, fila_max, columna_max)
#   
#   max_valores_col <- pos:(n-1)
#   min_valores_fila <- 0:(fila_max)
# 
#   cont <- 0
#   lista <- list()
#   
#   indices_col <- columna_max:1
#   indices_fila <- fila_max:1
# 
#   for(i in 1:fila_max){
#     m[i,] <- i-1 
#   }
#   m[fila_max, columna_max] <- m[fila_max, columna_max]-1
#   
#   fin <- 0
#   while(fin == 0){
#     bandera <- 0
#     for(i in indices_fila){
#       for(j in indices_col){
#         
#         if(bandera == 0){
#           if(m[i,j] < max_valores_col[j] ){
#             bandera <- 1
#             m[i,j] <- m[i,j] + 1
#             
#             for(col in j:1){
#               v <- sapply(min_valores_fila[i:fila_max] , function(x) max(x, m[i,col]) )
#               m[i:fila_max, col] <- v
#             }
#             # v2 <- min_valores_fila[i:fila_max]
#             # v1 <- m[i,1:j]
#             # m[i:fila_max, 1:j] <- sapply(v1, function(x){ sapply(v2, function(y) max(x,y) ) } )
#             # 
#             
#             if(j < columna_max){
#               if(i>1){
#                 for(col in (j+1):columna_max){
#                   act <- max( m[i,j], m[i-1,col] )
#                   v <- sapply(min_valores_fila[i:fila_max] , function(x) max(x, act) )
#                   m[i:fila_max, col] <- v
#                 }
#               } else {
#                 act <- m[1, j]
#                 v <- sapply(min_valores_fila[1:fila_max] , function(x) max(x, act) )
#                 m[1:fila_max, (j+1):columna_max] <- v
#               }
#             }
#             
#             cont <- cont+1
#             #lista[[cont]] <- m
#           }
#         }
#         
#       }
#     }
#     
#     if(bandera == 0){ fin <- 1 }
#   }
#   cont
# }
