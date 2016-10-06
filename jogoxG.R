jogo.xG <- function(data, model, rodada, casa, type, tempo = FALSE){
  source("cores.R")
  source("minutos.R")
  library(gridExtra)
  
  data1 = data[data$Rodada == rodada,]
  #data1$Minuto = as.numeric(as.character(data1$Minuto))
  fora = rep(0,length(casa))
  gol.casa = rep(0,length(casa))
  gol.fora = rep(0,length(casa))
  
  for (i in 1:length(casa)){
    fora[i] = as.character(data1[data1$Time == casa[i],]$Adversario[1])
  }
  xG.casa = 0
  xG.fora = 0
  
  if (tempo == TRUE){
    ggplot.minutos = list()
  }
  
  for(i in 1:length(casa)){
    data.casa = data1[data1$Time == casa[i],]
    data.fora = data1[data1$Time == fora[i],]
    gol.casa[i] = sum(as.numeric(as.character(data1$Gol[data1$Time == casa[i]])))
    gol.fora[i] = sum(as.numeric(as.character(data1$Gol[data1$Time == fora[i]])))
    
    if( type == 'rpart'){
      prediction.casa = predict(model,data.casa)
      prediction.fora = predict(model,data.fora)
      prediction.casa = prediction.casa[,2]
      prediction.fora = prediction.fora[,2]
      
    } 
    if (type == 'bayes.glm') {
      
      prediction.casa = predict(model,data.casa,type="response")
      prediction.fora = predict(model,data.fora,type="response")
    }
    if (type == 'neuralnetwork'){
      m1 <- model.matrix( 
        ~ Gol + Cruzamento.Cruz..rasteiro + Passe.Profundo + Regiao + Regiao.ASS +
        Contra.ataque + Erro.da.zaga + Em.Casa + Regiao + Tipo.de.Jogada, 
        data = data.casa
      )
      m2 <- model.matrix( 
        ~ Gol + Cruzamento.Cruz..rasteiro + Passe.Profundo + + Regiao + Regiao.ASS +
        Contra.ataque + Erro.da.zaga + Em.Casa + Regiao + Tipo.de.Jogada, 
        data = data.fora)
        
        colnames(m1)[66] <- "Tipo.de.JogadaFaltaDireta"
        colnames(m1)[67] <- "Tipo.de.JogadaFaltaIndireta"
        
        colnames(m2)[66] <- "Tipo.de.JogadaFaltaDireta"
        colnames(m2)[67] <- "Tipo.de.JogadaFaltaIndireta"
        
        pr.nn1 <- compute(model,m1[,3:ncol(m1)])
        pr.nn2 <- compute(model,m2[,3:ncol(m2)])
        
        prediction.casa = pr.nn1$net.result
        prediction.fora = pr.nn2$net.result
        
        
    }
    if (tempo == TRUE){
      plot.min = minutos(data = data1, prediction.casa = prediction.casa,
                         prediction.fora = prediction.fora, casa = casa[i], fora = fora[i])
      ggplot.minutos[[length(ggplot.minutos)+1]] = plot.min
    }
    
    xG.casa[i] = round(sum(prediction.casa),2)
    xG.fora[i] = round(sum(prediction.fora),2)
  }
  
  equipe2 = 0
  for(i in 1:length(casa)){
    equipe2[i] = paste(c(casa[i],gol.casa[i]), collapse = ' ')
  }
  for(i in 1:length(casa)){
    equipe2[i+length(casa)] = paste(c(fora[i],gol.fora[i]), collapse = ' ')
  }
  
  xG = data.frame(Equipe = c(casa,fora),
                  xG = c(xG.casa,xG.fora),
                  gols = c(gol.casa,gol.fora),
                  rank = c(1:(2*length(casa))),
                  Equipe2 = equipe2   )
  xG = cores(xG)
  b = xG
  ggplot.objects = list()
  
  for (i in 1:length(casa)){
    ind.fora = which(xG$Equipe == fora[i])
    ind.casa = which(xG$Equipe == casa[i])
    jogo.df = xG[c(ind.casa,ind.fora),]
    jogo.df = jogo.df[nrow(jogo.df):1, ]
    jogo.df$Equipe2 <- factor(jogo.df$Equipe2, levels = jogo.df$Equipe2)
#     titt = paste('\n',casa[i], 
#                  xG$gols[ind.casa],"x",xG$gols[ind.fora],fora[i],
#                  sep=" ")
    jogo = ggplot(jogo.df, aes(x = Equipe2, y = xG, fill = as.factor(rank)))+
      geom_bar(stat = 'identity', alpha = 0.85, width = 0.52)+
      coord_flip()+
      theme_classic()+
      scale_fill_manual(values = rev(jogo.df$paletta))+
      geom_text(aes(label = xG, x = Equipe2, y = xG), 
                position = position_dodge(width = 0.8), hjust = -0.30,
                size = 5,family="Avenir Next Condensed",color =  jogo.df$paletta)+
      ylim(0,max(xG$xG*1.25))+
      xlab("")+
      theme(legend.position="none",
            axis.line.y = element_line(
              colour = "white"),
            axis.line.x = element_line(
              colour = "gray26"),
            axis.ticks.y = element_line(color="white"),
            axis.text.y = element_text(size = 15,
                                       colour=(jogo.df$paletta)),
            axis.text.x = element_text(size = 12, color = 'gray26'),
            axis.title.x=element_text(size=12, color = 'gray26'))+
      #ggtitle(bquote(atop(.(titt))))+
      theme(#plot.title = element_text(size=13, vjust = 0.1),
            text=element_text(family="Avenir Next Condensed"))
    
      plot(jogo)
      ggplot.objects[[length(ggplot.objects)+1]] = jogo
  }
  if( tempo == F){
    newList = list('plots' = ggplot.objects,
                 'njogos' = length(casa),
                 'xG' = xG)
  }
  if (tempo == T){
    newList = list('plots' = ggplot.objects,
                   'plots.tempo' = ggplot.minutos,
                   'njogos' = length(casa)
                   )
    
  }

   return(newList)
  
  #grid.arrange(c(ggplot.objects[[1:length(ggplot.objects)]]),nol = 5,nrow=2)
  
}