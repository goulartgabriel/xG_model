minutos <- function(data, prediction.casa, prediction.fora, casa, fora){
    source("cores.R")
    library(gridExtra)
    data.casa = data[data$Time == casa,]
    data.fora = data[data$Time == fora,]
    linha.data = as.numeric(rownames(data)[1])-1
    linha.data = 0
    nchutes.casa = length(prediction.casa)
    nchutes.fora = length(prediction.fora)
    ind.casa = as.numeric(names(prediction.casa)) - linha.data
    ind.fora = as.numeric(names(prediction.fora)) - linha.data
    sum.casa = prediction.casa[1]
    sum.fora = prediction.fora[1]
    for (k in 2:(nchutes.casa)){
      sum.casa[k] = sum.casa[k-1] + prediction.casa[k]
    } 
    for (k in 2:(nchutes.fora)){
      sum.fora[k] = sum.fora[k-1] + prediction.fora[k]
    } 
    sum.casa = round(sum.casa,2)
    sum.fora = round(sum.fora,2)
    
   
    minutos.casa = c(data.casa$Minuto[ind.casa],96)
    minutos.fora = c(data.fora$Minuto[ind.fora],96)
    
    
    total.casa = rep(0,95)
    total.fora = rep(0,95)
    total.casa[1:(minutos.casa[1]-1)] = 0
    total.fora[1:(minutos.fora[1]-1)] = 0
   
    
    for (k in 1:(nchutes.casa)){
      #print(k)
     
      total.casa[(minutos.casa[k]):(-1+minutos.casa[k+1])] = sum.casa[k]
    }

    for (j in 1:(nchutes.fora)){
    
      total.fora[(minutos.fora[j]):(minutos.fora[j+1]-1)] = sum.fora[j]
    }
    
  
    
    xG.minuto = data.frame(Equipe = c(rep(casa,95),rep(fora,95)),
                           minuto = c(seq(1,95),seq(1,95)),
                           xG = c(total.casa,total.fora),
                          
                           rank = c(rep(1,95),rep(2,95)))
    
    xG.minuto = cores(xG.minuto)
    df.0 = data.frame(Equipe = casa,
                      minuto = 0,
                      xG = 0,
                      rank = 1,
                      paletta = xG.minuto$paletta[1])
    xG.minuto = rbind(df.0,xG.minuto)
    df.0 = data.frame(Equipe = fora,
                      minuto = 0,
                      xG = 0,
                      rank = 2,
                      paletta = xG.minuto$paletta[180])
    xG.minuto2 = rbind(xG.minuto[1:96,],df.0)
    xG.minuto = rbind(xG.minuto2,xG.minuto[97:191,])
    
    
    minutos.casa.gol = data[which(data$Time == casa),]
    minutos.casa.gol = as.numeric(as.character(minutos.casa.gol$Minuto[which(minutos.casa.gol$Gol ==1)]))
    
    minutos.fora.gol = data[which(data$Time == fora),]
    minutos.fora.gol = as.numeric(as.character(minutos.fora.gol$Minuto[which(minutos.fora.gol$Gol ==1)]))
    
    xG.casa.gol = sum.casa[which(minutos.casa %in% minutos.casa.gol)]
    xG.fora.gol = sum.fora[which(minutos.fora %in% minutos.fora.gol)]
    
    golc = length(minutos.casa.gol)
    golf = length(minutos.fora.gol)
    
    df.gols = data.frame(Equipe = c(rep(casa,golc),
                                    rep(fora,golf)),
                         minuto = c(minutos.casa.gol,minutos.fora.gol),
                         xG = c(xG.casa.gol,xG.fora.gol))
    
    paletta = c(as.character(as.factor(xG.minuto$paletta[1])),
                as.character(as.factor(xG.minuto$paletta[192])))
    
        val.casa = paste('(',max(sum.casa),')',sep="")
        val.fora = paste('(',max(sum.fora),')',sep="")
        str.casa = paste(casa,golc,val.casa,sep=" ")
        str.fora = paste(fora,golf,val.fora,sep=" ")
        str = paste(str.casa,'\n',str.fora,sep=" ")
        
        
    
    plot.min = ggplot()+
      geom_line(data = xG.minuto, aes(x = minuto, y = xG, colour = factor(Equipe,levels=unique(Equipe))),
                size = 1.8, alpha = 0.85)+
      scale_color_manual(values=paletta)+
      scale_x_continuous(name = "tempo",breaks = seq(0, 90, 15))+
      theme_classic()+
      geom_point(data = df.gols, aes(x = minuto, y = xG,colour = factor(Equipe,levels=unique(Equipe))),
                 size = 5)+
      ylim(c(0,max(xG.minuto$xG)))+
      theme(legend.position="none",
            text=element_text(family="Avenir"),
            axis.line.y = element_line(
              colour = "white"),
            axis.line.x = element_line(
              colour = "gray26"),
            axis.ticks.y = element_line(color="white"),
            axis.text.x = element_text(size = 12, color = 'gray26'),
            axis.text.y = element_text(size = 12, color = 'gray26'),
            axis.title.y=element_text(size=12, color = 'gray26'),
            axis.title.x=element_text(size=12, color = 'gray26'),
            panel.grid.major = element_line(colour = "gray26",size = .03),
            panel.grid.minor = element_line(colour = "gray26",
                                            linetype = 'dashed',size = .045))+
      annotate("text", label = 'Pontos representam gols', x =11, y =max(xG.minuto$xG), 
               size = 4.2, colour = "gray34",family="Avenir",fontface = "italic")+
      annotate("text", label = '@ProjecaoDeGol', x = 85, y = 0, 
               size = 4.2, colour = "gray34",family="Avenir",fontface = "italic")+
     labs(title=toupper(str.casa),
           subtitle=toupper(str.fora))+
      theme(plot.title=element_text(size=20, hjust=0,colour=paletta[1], 
                                    vjust=-1),
            text=element_text(family='Avenir Next Condensed'))+
      theme(plot.subtitle=element_text(size=20, hjust=0,colour=paletta[2], 
                                       vjust=-1),
            text=element_text(family='Avenir Next Condensed'))
  
                           
  return(plot.min)
  
}