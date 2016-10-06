plot.sim <- function(rodadas, nsimulacoes, peso.btm, sim.btm16, sim.xG){
  source("br16.R")
  source("boxsim.R")
  bra16 = br16(rodadas = rodadas)
  times = levels(bra16$classificacao$Times)
  
  #box
  box.mix = box.sim(rodadas= rodadas, peso.btm = peso.btm, btm = sim.btm16$df.pontos, xG = sim.xG$df.pontos)
  
  #posicoes
  posicoes.btm = sim.btm16$Posicoes
  posicoes.xG = sim.xG$Posicoes
  posicoes = posicoes.btm*peso.btm + posicoes.xG*(1-peso.btm)
  
  #pontos
  pontos.btm = sim.btm16$matriz.pontos
  pontos.xG = sim.xG$matriz.pontos
  pontos = pontos.btm*peso.btm + pontos.xG*(1-peso.btm)
  
  
  ##Sort ------
  ind = rep(0,20)
  porcent = rep(0,20)
  sim = posicoes/nsimulacoes
  matriz.pontos = pontos
  
  df.pontos = data.frame(Equipes = times,
                         Pts = matriz.pontos)
  df.pontos = df.pontos[order(df.pontos$Pts,decreasing=T),]  
  ind = match(df.pontos$Equipes,rownames(sim))
  sim = sim[rev(ind),]
  
  df = vector("list", 20)
  for (i in 1:20){
    df[[i]] = data.frame(Equipes = rownames(sim),
                         Posicao = i,
                         Porcentagem = sim[,i])
  }
  
  new.df = rbind(df[[1]],df[[2]])
  for (i in 3:20){
    new.df = rbind(new.df,df[[i]])
  }
  new.df$Equipe2 <- factor(new.df$Equipes, levels = unique(as.character(new.df$Equipes)))
  subt = paste("Número de campeonatos simulados:",nsimulacoes,sep=" ")
  titt = paste("Mapa de Calor da tabela final de classificação após", 
               rodadas,"rodadas",sep=" ")
  ##Heat  
  heat = ggplot(new.df, aes(Equipe2, Posicao))+
    geom_tile(aes(fill = Porcentagem), colour = "white")+
    coord_flip()+
    scale_fill_gradient(low = "white",high = "dodgerblue3")+
    ylab(" ")+
    xlab("")+
    theme_classic()+
    theme(axis.line.y = element_line(colour = "white"),
          axis.line.x = element_line(colour = "white"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(colour='dodgerblue4'),
          axis.text.x = element_text(color = 'dodgerblue4'),
          legend.title = element_text(colour="dodgerblue4"),
          legend.text = element_text(colour="dodgerblue4"))+
    scale_y_continuous(trans = "identity", breaks = unique(new.df$Posicao))+
    labs(title=titt,
         subtitle=subt)+
    theme(plot.title=element_text(size=20, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))+
    theme(plot.subtitle=element_text(size=15, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))+
    theme(plot.title = element_text(family="Avenir Next Condensed", color = "dodgerblue4" ))+
    annotate("text", label = "@ProjecaoDeGol", family="Avenir Next Condensed", 
             x = 1.45, y = 3, size = 3.7, colour = "dodgerblue4")    
    
  
  #campeoes  
  pos1 = posicoes
  times = rownames(pos1)
  rownames(pos1) = NULL
  pos1 = pos1[,1]
  times = times[which(pos1 >15)]
  pos1 = pos1[which(pos1 >15)]
  times = times[order(pos1,decreasing = T)]
  pos1 = pos1[order(pos1,decreasing = T)]
  pos1 = pos1/nsimulacoes
  #pos1 = pos1/sum(pos1)
  pos1 = round(pos1,digits = 3)
  df.campeoes = data.frame(Equipe = times,
                           chance = pos1*100,
                           rank = seq(1:length(pos1)))
  
  df.campeoes = cores(dataframe = df.campeoes)
  titt = paste("Campeões em simulações após", 
               rodadas,"rodadas",sep=" ")
  subt = paste("Número de campeonatos simulados:",(nsimulacoes),sep=" ")
  campeoes = ggplot(df.campeoes, aes(x=reorder(Equipe,chance),y=chance, 
                                     fill = as.factor(rank) ))+
    geom_bar(stat="identity",alpha=0.85, width = 0.7)+
    geom_text(aes(label = chance, x = reorder(Equipe,chance), y = chance), 
              position = position_dodge(width = 0.8), hjust = -0.50,
              size = 5, family="Avenir Next Condensed",
              color =  rev(df.campeoes$paletta))+
    theme_classic()+
    coord_flip()+
    scale_fill_manual(values = df.campeoes$paletta)+
    ylim(0,max(df.campeoes$chance)*1.1)+
    ylab("% de vezes campeão nas simulações")+xlab("")+
    theme(legend.position="none",
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(size = 15,
                                     colour=rev(df.campeoes$paletta)),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          plot.title = element_text(family="Avenir Next Condensed", color = "gray26" ))+
    annotate("text", label = "@ProjecaoDeGol", x = 0.9, y = max(df.campeoes$chance),
             size = 4.7, family="Avenir Next Condensed", colour = "gray34")+    
    labs(title=titt,
         subtitle=subt)+
    theme(plot.title=element_text(size=20, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))+
    theme(plot.subtitle=element_text(size=12, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))

  
  ##G6
  posG4 = posicoes
  times = rownames(posG4)
  rownames(posG4) = NULL
  posG4 = posG4[,1:6]
  posG4 = rowSums(posG4)
  times = times[which(posG4 >20)]
  posG4 = posG4[which(posG4 >20)]
  times = times[order(posG4,decreasing = T)]
  posG4 = posG4[order(posG4,decreasing = T)]
  posG4 = posG4/nsimulacoes
  #posG4 = posG4/sum(posG4/4)
  posG4 = round(posG4,digits = 3)
  df.G4 = data.frame(Equipe = times,
                     chance = posG4*100,
                     rank = seq(1:length(posG4)))
  df.G4 = cores(df.G4)
  df.G4 = df.G4[df.G4$chance > 2, ]
  titt = paste("Projeção de G6 após", 
               rodadas,"rodadas",sep=" ")
  subt = paste("Número de campeonatos simulados:",(nsimulacoes),sep=" ")
  
  G4 = ggplot(df.G4, aes(x=reorder(Equipe,chance),y=chance, 
                         fill = as.factor(rank) ))+
    geom_bar(stat="identity",alpha=0.85, width = 0.6)+
    geom_text(aes(label = chance, x = reorder(Equipe,chance), y = chance), 
              position = position_dodge(width = 0.8), hjust = -0.30,
              size = 5.5, family="Avenir Next Condensed", color= 'dodgerblue3')+
    theme_classic()+
    coord_flip()+
    scale_fill_manual(values = rep("dodgerblue3",nrow(df.G4)))+
    scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,115) )+
    ylab("% de vezes no G6 nas simulações")+xlab("")+
    theme(legend.position="none",
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(size = 15, colour = 'gray26'),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          plot.title = element_text(family="Avenir Next Condensed", color = "gray26" ))+
          annotate("text", label = "@ProjecaoDeGol", x = 0.9, y = max(df.G4$chance),
             size = 4.7, family="Avenir Next Condensed", colour = "gray34")+
    labs(title=titt,
         subtitle=subt)+
    theme(plot.title=element_text(size=20, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))+
    theme(plot.subtitle=element_text(size=15, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))
  
  #Z4
  posZ4 = posicoes
  times = rownames(posZ4)
  rownames(posZ4) = NULL
  posZ4 = posZ4[,17:20]
  posZ4 = rowSums(posZ4)
  times = times[which(posZ4 >50)]
  posZ4 = posZ4[which(posZ4 >50)]
  times = times[order(posZ4,decreasing = T)]
  posZ4 = posZ4[order(posZ4,decreasing = T)]
  posZ4 = posZ4/nsimulacoes
  #posZ4 = posZ4/sum(posZ4/4)
  posZ4 = round(posZ4,digits = 3)
  df.Z4 = data.frame(Equipe = times,
                     chance = posZ4*100,
                     rank = seq(1:length(posZ4)))
  
  df.Z4 = cores(df.Z4)
  df.Z4 = df.Z4[df.Z4$chance > 2, ]
  titt = paste("Projeção de rebaixamento após", 
               rodadas,"rodadas",sep=" ")
  subt = paste("Número de campeonatos simulados:",(nsimulacoes),sep=" ")
  
  Z4 = ggplot(df.Z4, aes(x=reorder(Equipe,chance),y=chance, 
                         fill = as.factor(rank) ))+
    geom_bar(stat="identity",alpha=0.85, width = 0.6)+
    geom_text(aes(label = chance, x = reorder(Equipe,chance), y = chance), 
              position = position_dodge(width = 0.8), hjust = -0.30,
              size = 5.5, family="Avenir Next Condensed", color= '#D11B1B')+
    theme_classic()+
    coord_flip()+
    scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,115) )+
    scale_fill_manual(values = rep("#D11B1B",nrow(df.Z4)))+
    #ylim(0,max(df.G4$chance)*1.4)+
    ylab("% de vezes rebaixado nas simulações")+xlab("")+
    theme(legend.position="none",
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(size = 15, colour = 'gray26'),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          plot.title = element_text(family="Avenir Next Condensed", color = "gray26" ))+
    annotate("text", label = "@ProjecaoDeGol", x = 0.9, y = max(df.Z4$chance),
             size = 4.7, family="Avenir Next Condensed", colour = "gray34")+
    labs(title=titt,
         subtitle=subt)+
    theme(plot.title=element_text(size=20, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))+
    theme(plot.subtitle=element_text(size=15, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))
  
  
  
  
  newList = list("Mapa" = heat,
                 
                 'Campeoes' = campeoes,
                 
                 'Box' = box.mix,
                 'G4' = G4,
                 'Z4' = Z4,
                 'posicoes' = posicoes,
                 'pontos' = df.pontos
  )                   
  return(newList) 
  
}
  