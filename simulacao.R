simulacao <- function(rodadas, nsimulacoes, m.btm, m.xG, peso.momentum, type, models){
  source("br16.R")
  source("simulacaoxG.R")
  source("teamratingbtm.R")
  source("simulacaobtm.R")
  source("cores.R")
  bra16 = br16(rodadas = rodadas)
  times = levels(bra16$classificacao$Times)
  rank.btm16 = team.rating.btm(jogos = bra16$jogos, rodadas = rodadas, 
                               resultados.casa = bra16$resultados.casa,
                               momentum = m.btm, peso = peso.momentum)

  sim.btm16 = simulacao.btm(rating = rank.btm16, 
                            nsimulacoes = nsimulacoes,regressao = FALSE,
                            momentum = TRUE)
  
  sim.xG = simulacao.xG(rodadas = rodadas, nsimulacoes = nsimulacoes, 
                        type = type, momentum = m.xG, peso = peso.momentum, models = models)
  
  newList = list("sim.btm16" = sim.btm16,
                 
                 'sim.xG' = sim.xG
                 
              
  )                   

 
  return(newList) 
  
}