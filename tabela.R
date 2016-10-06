tabela.campeonato <- function(campeonato,chutes.passes,rodadas, rodada.inicial = 1){

times15 = c(levels(campeonato$Casa), levels(campeonato$Fora) )
times15 = unique(times15)
times15 = sort(times15)
pontuacao = rep(0,length(times15))
gols.pro = rep(0,length(times15))
gols.contra = rep(0,length(times15))
vitorias = rep(0,length(times15))

for (i in rodada.inicial:rodadas){
  index = which(campeonato$Rodada == i)
  for (j in index){
    time.casa = campeonato$Casa[j]
    time.fora = campeonato$Fora[j]
    ind.casa = which(times15 == time.casa)
    ind.fora = which(times15 == time.fora)
    gols.pro[ind.casa] = gols.pro[ind.casa] + campeonato$Gol.C[j]
    gols.pro[ind.fora] = gols.pro[ind.fora] + campeonato$Gol.F[j] 
    gols.contra[ind.casa] = gols.contra[ind.casa] + campeonato$Gol.F[j] 
    gols.contra[ind.fora] = gols.contra[ind.fora] + campeonato$Gol.C[j] 
    if (campeonato$Gol.C[j] > campeonato$Gol.F[j] ){
      pontuacao[ind.casa] = pontuacao[ind.casa]+3
      vitorias[ind.casa] = vitorias[ind.casa]+1
    }
    if (campeonato$Gol.C[j] == campeonato$Gol.F[j] ){
      pontuacao[ind.casa] = pontuacao[ind.casa]+1
      pontuacao[ind.fora] = pontuacao[ind.fora]+1
      
    }
    if (campeonato$Gol.C[j] < campeonato$Gol.F[j] ){
      pontuacao[ind.fora] = pontuacao[ind.fora]+3
      vitorias[ind.fora] = vitorias[ind.fora]+1
      
    }
    
  }
  
}

brasileirao = data.frame(Times = times15,
                           Pontuacao = pontuacao,
                           PPJ = pontuacao/(rodadas-rodada.inicial+1),
                           Vitorias = vitorias,
                           Gols.Pro = gols.pro,
                           Gols.Contra = gols.contra,
                           Saldo = gols.pro - gols.contra,
                           Gols.Ratio = gols.pro/(gols.pro+gols.contra),
                           Chutes.Certos = chutes.passes$Chutes.Certos,
                           Chutes.Errados = chutes.passes$Chutes.Errados,
                           Chutes.Ratio = chutes.passes$Chutes.Certos/(chutes.passes$Chutes.Certos+chutes.passes$Chutes.Errados),
                           Passes.Certos = chutes.passes$Passes.Certos,
                           Passes.Errados = chutes.passes$Passes.Errados,
                           Passes.Ratio = chutes.passes$Passes.Certos/(chutes.passes$Passes.Certos+chutes.passes$Passes.Errados)
                             )
brasileirao = brasileirao[order(-brasileirao$Pontuacao, -brasileirao$Vitorias, -brasileirao$Saldo, -brasileirao$Gols.Pro),]
return(brasileirao)
}
