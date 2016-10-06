br16 <- function(rodadas = NULL, xG.data = NULL, rodada.inicial = 1){
  library(gsheet)
  source("tabela.R")
  bra16 = gsheet2tbl("https://docs.google.com/spreadsheets/d/1LobEkmNGqp_WLF_vRWqqDWTp5Fz730HwehVwTzYrbPw/edit#gid=414497342")
  if (!is.null(rodadas)){
    #bra16 <- bra16[1:(10*rodadas), ]
    bra16 <- bra16[bra16$Rodada>=rodada.inicial & bra16$Rodada<=rodadas,]
  }
  njogos = rodadas - rodada.inicial +1
  bra16[is.na(bra16)] <- 0
  bra16$Casa <- as.factor(bra16$Casa)
  bra16$Fora <- as.factor(bra16$Fora)
  if( rodadas == rodada.inicial){
    times16 = c(levels(bra16$Casa), levels(bra16$Fora) )
    times16 = sort(times16)
  } else {
    times16 = levels(bra16$Casa)
  }
  chutes.certos = rep(0,length(times16))
  chutes.errados = rep(0,length(times16))
  chutes.contra.certos = rep(0,length(times16))
  chutes.contra.errados = rep(0,length(times16))
  passes.certos = rep(0,length(times16))
  passes.errados = rep(0,length(times16))
  
  desarmes.certos = rep(0,length(times16))
  desarmes.errados = rep(0,length(times16))
  
  resultados.casa = matrix(NA,length(times16),length(times16))
  rownames(resultados.casa) = times16
  colnames(resultados.casa) = times16
  
  for (i in 1:rodadas){
    index = which(bra16$Rodada == i)
    for (j in index){
      time.casa = bra16$Casa[j]
      time.fora = bra16$Fora[j]
      ind.casa = which(times16 == time.casa)
      ind.fora = which(times16 == time.fora)
      chutes.certos[ind.casa] = chutes.certos[ind.casa] + bra16$Casa.Chutes.Certos[j]
      chutes.errados[ind.casa] = chutes.errados[ind.casa] + bra16$Casa.Chutes.Errados[j]
      chutes.certos[ind.fora] = chutes.certos[ind.fora] + bra16$Fora.Chutes.Certos[j]
      chutes.errados[ind.fora] = chutes.errados[ind.fora] + bra16$Fora.Chutes.Errados[j]
      
      
      chutes.contra.certos[ind.casa] = chutes.contra.certos[ind.casa] + bra16$Fora.Chutes.Certos[j]
      chutes.contra.errados[ind.casa] = chutes.contra.errados[ind.casa] + bra16$Fora.Chutes.Errados[j]
      chutes.contra.certos[ind.fora] = chutes.contra.certos[ind.fora] + bra16$Casa.Chutes.Certos[j]
      chutes.contra.errados[ind.fora] = chutes.contra.errados[ind.fora] + bra16$Casa.Chutes.Errados[j]
      
      passes.certos[ind.casa] = passes.certos[ind.casa] + bra16$Casa.Passes[j]
      passes.errados[ind.casa] = passes.errados[ind.casa] + bra16$Casa.Passes.E[j]
      passes.certos[ind.fora] = passes.certos[ind.fora] + bra16$Fora.Passes[j]
      passes.errados[ind.fora] = passes.errados[ind.fora] + bra16$Fora.Passes.E[j]
      
      desarmes.certos[ind.casa] = desarmes.certos[ind.casa] + bra16$Casa.Desarmes.Certos[j]
      desarmes.certos[ind.fora] = desarmes.certos[ind.fora] + bra16$Fora.Desarmes.Certos[j]
      desarmes.errados[ind.casa] = desarmes.errados[ind.casa] + bra16$Casa.Desarmes.Errados[j]
      desarmes.errados[ind.fora] = desarmes.errados[ind.fora] + bra16$Fora.Desarmes.Errados[j]
    
      if (bra16$Gol.C[j] > bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 2
      }
      if (bra16$Gol.C[j] == bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 1
      }
      if (bra16$Gol.C[j] < bra16$Gol.F[j]){
        resultados.casa[ind.casa,ind.fora] = 0
      }
      
    }
  }
  
  chutes.passes16 = data.frame(Equipe = times16,
                        Chutes.Certos = chutes.certos,
                        Chutes.Errados = chutes.errados,
                        Passes.Certos = passes.certos,
                        Passes.Errados = passes.errados
                       
  )
  
  classificacao = tabela.campeonato(campeonato = bra16, chutes.passes = chutes.passes16,
                                    rodadas = rodadas, rodada.inicial = rodada.inicial)

  if(!is.null(xG.data)){
    ind.xG = match(classificacao$Times,xG.data$equipes)
    new.xG = xG.data[ind.xG,]
    classificacao$xG = new.xG$xG
    classificacao$xGC = new.xG$xGC
    classificacao$xG.casa = new.xG$xG.casa
    classificacao$xGC.casa = new.xG$xGC.casa
    classificacao$xG.fora = new.xG$xG.fora
    classificacao$xGC.fora = new.xG$xGC.fora
    
  }
  
  Desarmes.Ratio = desarmes.certos/(desarmes.certos+desarmes.errados)
  
  classificacao$Chutes.Contra.Certos = chutes.contra.certos[as.numeric(rownames(classificacao))]
  classificacao$Chutes.Contra.Errados = chutes.contra.errados[as.numeric(rownames(classificacao))]
  classificacao$Chutes.Contra.Ratio = classificacao$Chutes.Contra.Certos/(classificacao$Chutes.Contra.Certos+classificacao$Chutes.Contra.Errados)
  classificacao$Desarmes.ratio = Desarmes.Ratio[as.numeric(rownames(classificacao))]
  
 total.chutes = sum(bra16$Casa.Chutes.Certos)+sum(bra16$Casa.Chutes.Errados)+
   sum(bra16$Fora.Chutes.Certos)+sum(bra16$Fora.Chutes.Errados)
  
  newList <- list('jogos' = bra16[,1:5], 'classificacao' = classificacao,
                  'resultados.casa' = resultados.casa,
                  'total.chutes' = total.chutes)
                  
}