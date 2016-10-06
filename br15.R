br15 <- function(rodadas){
  source("tabela.R")
  bra15 = read.table("resultados2015.csv",T,sep=",")
  chutes = read.table("chutes15.csv",T,sep=",")
  classificacao = tabela.campeonato(campeonato = bra15,chutes.passes = chutes,rodadas = rodadas)
  times15 = levels(bra15$Casa)
  jogos = bra15
  resultados.casa = matrix(NA,20,20)
  rownames(resultados.casa) = times15
  colnames(resultados.casa) = times15
  
  
  for (i in 1:rodadas){
    index = which(bra15$Rodada == i)
    for (j in index){
      time.casa = bra15$Casa[j]
      time.fora = bra15$Fora[j]
      ind.casa = which(times15 == time.casa)
      ind.fora = which(times15 == time.fora)
      
      if (bra15$Gol.C[j] > bra15$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 2
      }
      if (bra15$Gol.C[j] == bra15$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 1
      }
      if (bra15$Gol.C[j] < bra15$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 0
      }
      
    }
  }
  
  newList <- list('jogos' = jogos, 'classificacao' = classificacao,
                  'resultados.casa' = resultados.casa)
  
  return(newList)
}
  