jogadores <- function(dados, model, rodadas, threshold, 
                      type, njogadores = 20){
  source("ogol.R")
  library(ggrepel)
  library(stringr)
  ##MELHORES FINALIZADORES ========
  dados$Jogador = as.character(dados$Jogador)
  for (i in 1:nrow(dados)){
    jogador = dados$Jogador[i]
    #jogador.ass = dados$Jogador.ASS[i]
    if (word(jogador,-1) == ""){
      jogador = word(jogador,1,-2)
      if (word(jogador,-1) == ""){
        jogador = word(jogador,1,-2)
      }
      dados$Jogador[i] = jogador
    }
    
  }
  
  dados = dados[dados$Tipo.de.Jogada != 'Penalti',]
  
  todos.chutes = count(dados,vars=c("Jogador","Time"))
  todos.chutes = arrange(todos.chutes,desc(freq))
  index = which(todos.chutes$freq > 5)
  best.players = data.frame(jogador = todos.chutes$Jogador[index],
                            Time = todos.chutes$Time[index],
                            chutes = todos.chutes$freq[index])
  
  best.players = ogol(best.players)
  expF = rep(0,length(best.players$jogador))
  expJ = rep(0,length(best.players$jogador))
  
  for (i in 1:length(best.players$jogador)){
    nome = best.players$jogador[i]
    tim = best.players$Time[i]
    o = expected(jogador = nome, equipe = tim, data = dados, modelo = model, njogos = rodadas,thresh = threshold, 
                 type = type)
    expF[i] = round(o$xGpF, digits = 3)
    expJ[i] = round(o$xG, digits = 3)
  }
  best.players = data.frame(best.players,xG.Chute = expF,xG = expJ)
  best.players = best.players[best.players$Jogos>0,]
  best.players$xG.Jogo = best.players$xG/best.players$Jogos
  best.players = arrange(best.players,desc(xG.Jogo))
  
 # best.players$chutes = 1.1*best.players$chutes
 
  #best.players[1:5, ]
  ##GGPLOT
  #b = paste('Players',"'", sep="")
  #b = paste(b,'xG per match', sep =' ')
  finalizadores.jogo = ggplot(best.players[1:27,], aes(x=reorder(jogador,xG.Jogo), y=xG.Jogo))+
    geom_bar(stat = "identity",alpha=0.85,width=.3,fill = 'dodgerblue4')+
    coord_flip()+
    theme_classic()+
    labs(title = 'xG de jogadores por partida', x = "",y = 'xG por partida jogada',
         subtitle = 'Pênaltis não foram considerados')+
    theme(legend.position="none",
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(size = 12.8),
          axis.text.x = element_text(size = 13.5, color = 'gray26'),
          axis.title.x=element_text(size=13.5, color = 'gray26'),
          text=element_text(family="Avenir"),
          plot.title = element_text(family="Avenir", color = "gray26",
                                    size=17, hjust=0.5, vjust=0),
          plot.subtitle = element_text(family="Avenir", color = "gray26",
                                       size = 13,hjust=0.5, vjust=-1),
          panel.grid.major = element_line(colour = "gray26",size = .03),
          panel.grid.minor = element_line(colour = "gray26",
                                          linetype = 'dashed',size = .045))+
    annotate("text", label = "@ProjecaoDeGol", x = 1.9, y = 0.58, size = 3.7, 
             colour = "gray34",family="Avenir",fontface = 'italic')    
   
  
  
  finalizadores.chute = ggplot(arrange(best.players,desc(xG.Chute))[1:20,], aes(x=reorder(jogador,xG.Chute), y=xG.Chute))+
    geom_bar(stat = "identity", fill = "dodgerblue3", width = .65, alpha = .85)+
    coord_flip()+
    theme_minimal()+
    annotate("text", label = "@ProjecaoDeGol", x = 1.9, y = 0.36, size = 3.7, colour = "gray34")+    
    labs(title = 'Valor de xG por chute de cada jogador', x = "Jogadores",y = 'xG por chute')
  
  corr.xG.gol = ggplot(best.players[1:njogadores,], aes(xG.Jogo, Gols/Jogos, label = jogador))+
    geom_text_repel(family="Avenir Next Condensed")+
    geom_point(color = "orchid3",size = 2.15)+
    ylab("Gols por Jogo")+
    xlab("xG")+
    ggtitle(expression(atop("Gols por jogo e valor esperado de gols xG", 
                            atop(italic("Penâltis não foram considerados."), "")))) +
    theme_minimal()+
    theme(legend.position="none",
          axis.text.y = element_text(size = 12.8),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          plot.title = element_text(family="Avenir Next Condensed", color = "gray26" ))+
    annotate("text", label = "@ProjecaoDeGol", x = .6, y = .2, size = 3.7, colour = "gray34", family = 'Avenir Next Condensed')+    
    geom_smooth(linetype="dashed", method="lm", se = T,color = "dodgerblue3", 
                alpha=.1,size=0.65)
  
  
  # geom_smooth(linetype="dashed", method="lm", se=T, alpha=.17,size=0.7, color = "dodgerblue3")
  
  b = paste('Players',"'", sep="")
  b = paste(b,'shots and their xG', sep =' ')
  
  corr.xG.chute = ggplot(best.players[1:njogadores,], aes((chutes/Jogos)*1.1,xG.Jogo, label = jogador))+
    geom_text_repel(point.padding = unit(0.22, "lines"), family = 'Avenir')+
    geom_point(color = "dodgerblue3",aes(size = Gols), alpha = .9)+
    labs(title = 'Finalizações e xG dos jogadores', x = "Finalizações por jogo",y = 'xG por jogo',
         subtitle = 'Tamanho do ponto ~ gols marcados. Pênaltis não foram considerados')+
    theme_minimal()+
    ylim(0.18,0.65)+
    theme(legend.position="none",
          axis.text.y = element_text(size = 12.8),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir"),
          plot.title = element_text(family="Avenir", color = "gray26",
                                    size=17, hjust=0.5, vjust=0),
          plot.subtitle = element_text(family="Avenir", color = "gray26",
                                       size = 13,hjust=0.5, vjust=-1))+
    annotate("text", label = "@ProjecaoDeGol", x = 2.75, y = 0.25, size = 3.7, 
             colour = "gray34", family = 'Avenir', fontface = 'italic')+    
    geom_smooth(linetype="dashed", method="lm", se = T,color = "orchid3", 
                alpha=0,size=0.5)
  
  
  
   ##MELHORES PASSADORES -  ----------- 
  todos.pass = count(dados,vars=c("Jogador.ASS","Time"))
  index = which(todos.pass$Jogador.ASS == '-')
  todos.pass = todos.pass[-index,]
  todos.pass = arrange(todos.pass,desc(freq))
  index = which(todos.pass$freq > 3)
  best.pass = data.frame(jogador = todos.pass$Jogador[index],
                         Time = todos.pass$Time[index],
                         passes = todos.pass$freq[index])
  expAP = rep(0,length(best.pass$jogador))
  expAJ = rep(0,length(best.pass$jogador))
  
  for (i in 1:length(best.pass$jogador)){
    nome = best.pass$jogador[i]
    tim = best.pass$Time[i]
    o = expected(passador = nome, equipe = tim, data = xG, modelo = model, njogos = rodadas,thresh = threshold, type = type)
    expAP[i] = round(o$xGpF, digits = 3)
    expAJ[i] = round(o$xGpJ, digits = 3)
  }
  
  best.pass = data.frame(best.pass,xA.Passe = expAP,xA.Jogo = expAJ)
  best.pass = arrange(best.pass,desc(xA.Jogo))
  best.pass$jogador = as.character(best.pass$jogador)
  best.pass$jogador[23] <- 'Robinho-CRU'
  best.pass$jogador[20] = 'Robinho-CAM'
  best.pass$jogador = as.factor(best.pass$jogador)
  
  b = paste('Players',"'", sep="")
  b = paste(b,'xA per match', sep =' ')
  
  passadores = ggplot(best.pass[1:30,], aes(x=reorder(jogador,xA.Jogo), y=xA.Jogo))+
    geom_bar(stat = "identity", fill = "dodgerblue4", width = .3, alpha = .85)+
    coord_flip()+
    theme_classic()+
    labs(title = b, x = "",y = 'xA per match'
         )+
    theme(legend.position="none",
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(size = 12.8),
          axis.text.x = element_text(size = 13.5, color = 'gray26'),
          axis.title.x=element_text(size=13.5, color = 'gray26'),
          text=element_text(family="Avenir"),
          plot.title = element_text(family="Avenir", color = "gray26",
                                    size=17, hjust=0.5, vjust=-1),
          plot.subtitle = element_text(family="Avenir", color = "gray26",
                                       size = 13,hjust=0.5, vjust=-1),
          panel.grid.major = element_line(colour = "gray26",size = .03),
          panel.grid.minor = element_line(colour = "gray26",
                                          linetype = 'dashed',size = .045))+
    annotate("text", label = "@ProjecaoDeGol", x = 1.9, y = 0.3, size = 3.7, 
             colour = "gray34",family="Avenir", fontface = 'italic')    
  
#   ##PASSADORES POR CHANCE BOA -------
#   chance.pass = count(dados,vars=c("Jogador.ASS","Time","Chance.boa"))
#   index = which(chance.pass$Chance.boa == 1)
#   chance.pass = chance.pass[index,]
#   
#   index = which(chance.pass$Jogador.ASS == '-')
#   chance.pass = chance.pass[-index,]
#   chance.pass = arrange(chance.pass,desc(freq))
#   index = which(chance.pass$freq > 2)
#   chance.pass = chance.pass[index,]
#   
#   for (i in 1:length(chance.pass$Jogador.ASS)){
#     nome = chance.pass$Jogador.ASS[i]
#     tim = chance.pass$Time[i]
#     for (l in 1:length(best.pass$jogador)){
#       if (best.pass$jogador[l] == nome && best.pass$Time[l] == tim){
#         chance.pass$passes[i] = best.pass$passes[l]}
#     }
#   }
#   
#   
#   chance.pass = data.frame(jogador = chance.pass$Jogador.ASS,
#                            Time = chance.pass$Time,
#                            Total.passes = chance.pass$passes,
#                            Passes.bons = chance.pass$freq,
#                            Percentagem = chance.pass$freq/chance.pass$passes
#   )
#   expCP = rep(0,length(chance.pass$jogador))
#   expCJ = rep(0,length(chance.pass$jogador))
#   
#   for (i in 1:length(chance.pass$jogador)){
#     nome = chance.pass$jogador[i]
#     tim = chance.pass$Time[i]
#     o = expected(passador = nome, equipe = tim, chance = 1, data = xG, modelo = model, njogos = rodadas,thresh = threshold, type = type)
#     expCP[i] = round(o$xGpF, digits = 3)
#     expCJ[i] = round(o$xGpJ, digits = 3)
#   }
#   
#   chance.pass = data.frame(chance.pass,xCB.Passe = expCP,xCB.Jogo = expCJ)
#   chance.pass = arrange(chance.pass,desc(xCB.Jogo))
#   
#   passadores.chanceboa = ggplot(chance.pass[1:20,], aes(x=reorder(jogador,xCB.Jogo), y=xCB.Jogo))+
#     geom_bar(stat = "identity", fill = "dodgerblue3",width = .65, alpha = .85)+
#     coord_flip()+
#     theme_minimal()+
#     labs(title = 'Melhores Passadores de Chance Boa', x = "Jogadores",y = 'xA por jogo dado que o passe foi de chance boa')
#   
#   passadores.chanceboa.100 = ggplot(chance.pass[1:20,], aes(x=reorder(jogador,Percentagem), y=Percentagem))+
#     geom_bar(stat = "identity", fill = "dodgerblue3",width = .65, alpha = .85)+
#     coord_flip()+
#     theme_minimal()+
#     labs(title = '% de passes para finalização que são chance boa', x = "Jogadores",y = '%')
#   
  ##PARTICIPACAO EM CHUTES---------
  chutes = data.frame(Jogador = dados$Jogador, time = dados$Time)
  ass = data.frame(Jogador = dados$Jogador.ASS, time = dados$Time)
  todos = rbind(chutes,ass)
  index = which(todos$Jogador == '-')
  todos = todos[-index,]
  todos = count(todos)
  todos = arrange(todos,desc(freq))
  todos[1:15,]
  
  participacao = ggplot(todos[1:15,], aes(x=reorder(Jogador,freq), y=freq))+
    geom_bar(stat = "identity", fill = "dodgerblue3",width = .65, alpha = .85)+
    coord_flip()+
    theme_minimal()+
    labs(title = 'Total participação em chutes', x = "Jogadores",y = 'Participações')
  
  newList <- list('Finalizadores.jogo' = finalizadores.jogo,
                  'Finalizadores.chute' = finalizadores.chute,
                  'xG.chute' = corr.xG.chute,
                  'xG.gol' = corr.xG.gol,
                  'Passadores' = passadores, 
#                   'Passadores.chance.boa' = passadores.chanceboa,
#                   'Passadores.chance.aprov' = passadores.chanceboa.100,
                  'Participacao' = participacao,
                  'best.players' = best.players,
                  'best.pass' = best.pass)
  
  return(newList)
  
  
}