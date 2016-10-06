jogo.btm <- function(equipe,adversario,jogos,rank){
  ###Setup ------
  source("teamratingbtm.R")
  library("ggplot2")
  
  #times =  levels(jogos$Casa)
  times = levels(rank$Ranking$individual)
  probabilidades = rank$probs
  ##Busca-----
  ind.casa = which(times == equipe)
  ind.fora = which(times == adversario)
  
  prob.jogo = probabilidades[ind.casa,ind.fora,]
  vitoria.visitante = prob.jogo[3]
  empate = prob.jogo[2]
  vitoria.casa = prob.jogo[1]
  
  df <- data.frame(Casa = vitoria.casa,
                   Empate = empate,
                   Fora = vitoria.visitante
  )
  resultados = c(equipe, "Empate", adversario)
  chances = c(vitoria.casa,empate,vitoria.visitante)
  df.plot = data.frame(Resultados = resultados,
                       Probabilidades = chances)
  df.plot$Resultados <- factor(df.plot$Resultados, levels = df.plot$Resultados)
  
  ##Plot ------
  cores = c("deepskyblue4","ivory3","lightcoral")
  num = factor(seq(0,100,10))
  num = levels(num)
  total = ggplot(df.plot, aes(x=Resultados,y=100*Probabilidades, fill = Resultados))+
    geom_bar(stat = "identity")+
    geom_text(aes(x=Resultados, y=100*Probabilidades,# + 0.7*sign(Probabilidades),
                  label=round(100*df.plot$Probabilidades, digits=2)),
              hjust=0.45, 
              size=5,
              color="dimgray",
              vjust = -0.5)+
    scale_y_continuous(breaks=seq(0, 100, 10),
                       labels=num, limits = c(0,100) )+
    scale_fill_manual(values=cores)+
    ggtitle("Ranking de Forca dos Times")+
    theme(legend.position="none")+
    labs(title = paste(equipe, 'vs', adversario), x = "",y = 'Probabilidade em %')+
    theme(axis.ticks.x=element_blank())
  
  
  
  newList <- list('Jogo' = df,
                  'Plot' = total)
  return(newList)
  
}