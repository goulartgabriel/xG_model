new.resultados <- function(rodada.maxima){
  library(gsheet)
  source("tabela.R")
  bra16 = gsheet2tbl("https://docs.google.com/spreadsheets/d/1LobEkmNGqp_WLF_vRWqqDWTp5Fz730HwehVwTzYrbPw/edit#gid=414497342")
  bra16$Casa <- as.factor(bra16$Casa)
  bra16$Fora <- as.factor(bra16$Fora)
  times16 = levels(bra16$Casa)
  resultados.casa = matrix(9,length(times16),length(times16))
  resultados.fora = matrix(9,length(times16),length(times16))
  
  for (i in 1:rodada.maxima){
    index = which(bra16$Rodada == i)
    
    for (j in index){
      time.casa = bra16$Casa[j]
      time.fora = bra16$Fora[j]
      ind.casa = which(times16 == time.casa)
      ind.fora = which(times16 == time.fora)
      if (bra16$Gol.C[j] > bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 3
        resultados.fora[ind.fora,ind.casa] = 0
      }
      if (bra16$Gol.C[j] == bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 1
        resultados.fora[ind.fora,ind.casa] = 1
        
      }
      if (bra16$Gol.C[j] < bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 0
        resultados.fora[ind.fora,ind.casa] = 3
        
      }
      
      }
  }
  
  for (i in (rodada.maxima+1):max(bra16$Rodada)){
    index = which(bra16$Rodada == i)
   
    for (j in index){
      time.casa = bra16$Casa[j]
      time.fora = bra16$Fora[j]
      ind.casa = which(times16 == time.casa)
      ind.fora = which(times16 == time.fora)
      if (bra16$Gol.C[j] > bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = NA
        resultados.fora[ind.fora,ind.casa] = NA
      }
      if (bra16$Gol.C[j] == bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = NA
        resultados.fora[ind.fora,ind.casa] = NA
      }
      if (bra16$Gol.C[j] < bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = NA
        resultados.fora[ind.fora,ind.casa] = NA
      }
      
    }
  }
  newList <- list('resultados.casa' = resultados.casa,
                  'resultados.fora' = resultados.fora)
  return(newList)
  
}
