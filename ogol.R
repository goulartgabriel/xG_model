ogol <- function(best.players){
  ogol = read.csv("/Users/gabrielgoulart/Documents/Rodeo/ogol.csv",
                  header = F,fileEncoding = "ISO-8859-1", stringsAsFactors=FALSE)
  colnames(ogol) = c("Rank","Jogador","Time",'Jogos','Gols','Penalti',
                     'Gols.Contra','Min.por.gol','Gols.Titular','Gols.Reserva',
                     'Gols.garantiram.pontos','Gols.garantiram.vit',
                     'Gols.de.virada','Porcentagem.equipe')
  
  
  ogol$Time[ogol$Time=="América Mineiro"] <- "América"
  ogol$Time[ogol$Time=="Atlético Mineiro"] <- "Atlético MG"
  ogol$Time[ogol$Time=="Atlético Paranaense"] <- "Atlético PR"
  ogol$Jogador[ogol$Jogador=='Vitinho' & ogol$Time=='América'] <- 'Victor Rangel'
#   rogerio = data.frame(Rank = 10,
#                        Jogador = 'Rogério',
#                        Time = 'Sport',
#                        Jogos = 11,
#                        Gols = 3,
#                        Penalti = 0,
#                        Gols.Contra = 0,
#                        Min.por.gol = 267,
#                        Gols.Titular = 3,
#                        Gols.Reserva = 0,
#                        Gols.garantiram.pontos = 0,
#                        Gols.garantiram.vit = 1,
#                        Gols.de.virada = 0,
#                        Porcentagem.equipe = 10
#   )
#   ogol = rbind(ogol,rogerio)
  
  
  #ogol$Jogador = as.character(ogol$Jogador)
  best.players[,4:14] = 0
  colnames(best.players) <- c('jogador','Time','chutes', 'Jogos','Gols','Penalti',
                              'Gols.Contra','Min.por.gol','Gols.Titular','Gols.Reserva',
                              'Gols.garantiram.pontos','Gols.garantiram.vit',
                              'Gols.de.virada','Porcentagem.equipe')
  for(i in 1:nrow(best.players)){
    time.xG = best.players$Time[i]
    jogador.xG = best.players$jogador[i]
    ogol.times = ogol[which(ogol$Time == time.xG),]
    ogol.jogador = grep(jogador.xG,ogol.times$Jogador,value=T)
#     if (jogador.xG == 'Carlos'){
#       ogol.jogador = ogol.jogador[1]
#     }
    if (identical(ogol.jogador, character(0))){
      next
    } 
    ogol.jogador = ogol.times[which(ogol.times$Jogador == ogol.jogador),]
    best.players[i,] = cbind(best.players[i,1:3],ogol.jogador[4:14])
    
  }
  
return(best.players)

}