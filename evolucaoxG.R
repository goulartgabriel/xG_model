evolucao.xG <- function(historico.xG, historico.xGC, historico.xSG, casa, fora, xG.avg, xG.data){
  source('cores.R')
 
  evolucao.df = historico.xG[which(historico.xG$Equipe %in% c(casa,fora)),]
  evolucao.df = melt(evolucao.df, id=c("Equipe"))
  evolucao.df = cores(evolucao.df)
  evolucao.df$variable = as.numeric(as.character(evolucao.df$variable))
  evolucao.df$value = as.numeric(as.character(evolucao.df$value))
  pos.xG.casa = which(xG.data$equipes[order(xG.data$xG, decreasing = T)] == casa)
  pos.xG.fora = which(xG.data$equipes[order(xG.data$xG, decreasing = T)] == fora)
  leg.casa = paste(round(evolucao.df$value[39], 2),' (',pos.xG.casa,'º',')',sep = '')
  leg.fora = paste(round(evolucao.df$value[40], 2),' (',pos.xG.fora,'º',')',sep = '')
  
  evolucao.xG = ggplot()+
    geom_line(data = evolucao.df, aes(x = variable, y = value, colour = factor(Equipe,levels=unique(Equipe))),
              size = 1.8, alpha = 0.85)+
    scale_color_manual(values=evolucao.df$paletta[1:2],labels = c(leg.casa,leg.fora))+
    theme_classic()+
    geom_point(data = evolucao.df[39:40,], aes(x = variable, y = value,colour = factor(Equipe,levels=unique(Equipe))),
               size = 4)+
    ggtitle('Evolução de xG')+
    labs(color = 'xG (Ranking)')+
    ylim(c(0.9*min(evolucao.df$value),1.25*max(evolucao.df$value)))+
    xlab('')+
    theme(
          plot.title = element_text(hjust = 0.5, size = 18, colour = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "white"),
          axis.ticks.y = element_line(color="white"),
          axis.ticks.x = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'white'),
          axis.text.y = element_text(size = 12, color = 'white'),
          axis.title.y=element_text(size=12, color = 'white'),
          axis.title.x=element_text(size=18, color = 'gray26'),
          legend.title = element_text(size=16, color = 'gray26'),
          legend.text = element_text(size=16, color = 'gray26'))+
    geom_hline(yintercept = xG.avg, color = 'gray26', size = .25,linetype="dashed")
  
  evolucao.df.xGC = historico.xGC[which(historico.xGC$Equipe %in% c(casa,fora)),]
  evolucao.df.xGC = melt(evolucao.df.xGC, id=c("Equipe"))
  evolucao.df.xGC = cores(evolucao.df.xGC)
  evolucao.df.xGC$variable = as.numeric(as.character(evolucao.df.xGC$variable))
  evolucao.df.xGC$value = as.numeric(as.character(evolucao.df.xGC$value))
  pos.xGC.casa = which(xG.data$equipes[order(xG.data$xGC, decreasing = F)] == casa)
  pos.xGC.fora = which(xG.data$equipes[order(xG.data$xGC, decreasing = F)] == fora)
  leg.casa = paste(round(evolucao.df.xGC$value[39], 2),' (',pos.xGC.casa,'º',')',sep = '')
  leg.fora = paste(round(evolucao.df.xGC$value[40], 2),' (',pos.xGC.fora,'º',')',sep = '')
  
  evolucao.xGC = ggplot()+
    geom_line(data = evolucao.df.xGC, aes(x = variable, y = value, colour = factor(Equipe,levels=unique(Equipe))),
              size = 1.8, alpha = 0.85)+
    scale_color_manual(values=evolucao.df.xGC$paletta[1:2],labels = c(leg.casa,leg.fora))+
    theme_classic()+
    geom_point(data = evolucao.df.xGC[39:40,], aes(x = variable, y = value,colour = factor(Equipe,levels=unique(Equipe))),
               size = 4)+
    ggtitle('Evolução de xGC')+
    ylim(c(0.9*min(evolucao.df.xGC$value),1.25*max(evolucao.df.xGC$value)))+
    xlab('')+
    labs(color = 'xGC (Ranking)')+
    theme(
          plot.title = element_text(hjust = 0.5, size = 18, colour = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "white"),
          axis.ticks.y = element_line(color="white"),
          axis.ticks.x = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'white'),
          axis.text.y = element_text(size = 12, color = 'white'),
          axis.title.y=element_text(size=12, color = 'white'),
          axis.title.x=element_text(size=18, color = 'gray20'),
          legend.title = element_text(size=16, color = 'gray26'),
          legend.text = element_text(size=16, color = 'gray26'))+
    geom_hline(yintercept = xG.avg, color = 'gray26', size = .25,linetype="dashed")
  
  evolucao.df.xSG = historico.xSG[which(historico.xSG$Equipe %in% c(casa,fora)),]
  evolucao.df.xSG = melt(evolucao.df.xSG, id=c("Equipe"))
  evolucao.df.xSG = cores(evolucao.df.xSG)
  evolucao.df.xSG$variable = as.numeric(as.character(evolucao.df.xSG$variable))
  evolucao.df.xSG$value = as.numeric(as.character(evolucao.df.xSG$value))
  pos.xSG.casa = which(xG.data$equipes[order(xG.data$xSG, decreasing = T)] == casa)
  pos.xSG.fora = which(xG.data$equipes[order(xG.data$xSG, decreasing = T)] == fora)
  leg.casa = paste(round(evolucao.df.xSG$value[39], 2),' (',pos.xSG.casa,'º',')',sep = '')
  leg.fora = paste(round(evolucao.df.xSG$value[40], 2),' (',pos.xSG.fora,'º',')',sep = '')
  if(min(evolucao.df.xSG$value) < 0){
    coef = 1.1
  } else{
    coef = 0.9
  }
  evolucao.xSG = ggplot()+
    geom_line(data = evolucao.df.xSG, aes(x = variable, y = value, colour = factor(Equipe,levels=unique(Equipe))),
              size = 1.8, alpha = 0.85)+
    scale_color_manual(values=evolucao.df.xSG$paletta[1:2], labels = c(leg.casa,leg.fora))+
    theme_classic()+
    geom_point(data = evolucao.df.xSG[39:40,], aes(x = variable, y = value,colour = factor(Equipe,levels=unique(Equipe))),
               size = 4)+
    ggtitle('Evolução de xSG')+
    ylim(c(coef*min(evolucao.df.xSG$value),1.25*max(evolucao.df.xSG$value)))+
    xlab('')+
    labs(color = 'xSG (Ranking)')+
    theme(
          plot.title = element_text(hjust = 0.5, size = 18, colour = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "white"),
          axis.ticks.y = element_line(color="white"),
          axis.ticks.x = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'white'),
          axis.text.y = element_text(size = 12, color = 'white'),
          axis.title.y=element_text(size=12, color = 'white'),
          axis.title.x=element_text(size=18, color = 'gray26'),
          legend.title = element_text(size=16, color = 'gray26'),
          legend.text = element_text(size=16, color = 'gray26'))+
    geom_hline(yintercept = 0, color = 'gray26', size = .25,linetype="dashed")
  
  
  newlist = list('xG' = evolucao.xG,
                 'xGC' = evolucao.xGC,
                 'xSG' = evolucao.xSG)
  
  return(newlist)
  
  
  
}