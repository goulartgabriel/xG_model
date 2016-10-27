correlacao <- function(xG.campeonato, tabela){
  library(ggrepel)
  library(ggplot2)
  source('cores.R')
# setup ----
  xG.data = xG.campeonato$xG.data
  xG.data = xG.data[order(xG.data$equipes),]
  real.data = tabela$classificacao
  real.data = real.data[order(real.data$Times),]
  rodadas = max(tabela$jogos$Rodada) - min(tabela$jogos$Rodada) +1
  real.data$GPJ = real.data$Gols.Pro/rodadas
  real.data$GCPJ = real.data$Gols.Contra/rodadas
  
  times = xG.data$equipes
  
  correlation.data = data.frame(Equipe = times,
                                xG = xG.data$xG,
                                xGC = xG.data$xGC,
                                GP = real.data$GPJ,
                                Gols = real.data$Gols.Pro,
                                GC = real.data$GCPJ,
                                xSG = xG.data$xSG,
                                SG = real.data$Saldo/rodadas,
                                #xG.ratio = xG.data$xGratio,
                                #G.ratio = real.data$Gols.Ratio,
                                PPJ = real.data$PPJ,
                                Pontos = real.data$PPJ*rodadas,
                                Chutes.Certos = real.data$Chutes.Certos,
                                Chutes.Ratio = real.data$Chutes.Ratio,
                                Chutes = (real.data$Chutes.Certos+real.data$Chutes.Errados)/rodadas,
                                Chutes.Contra = (real.data$Chutes.Contra.Certos+real.data$Chutes.Contra.Errados)/rodadas,
                                Gols.chuteC = (real.data$Chutes.Contra.Certos+real.data$Chutes.Contra.Errados)/real.data$Gols.Contra,
                                Gols.chute = (real.data$Chutes.Certos+real.data$Chutes.Errados)/real.data$Gols.Pro,
                                Passes.Certos = real.data$Passes.Certos,
                                Passes.Ratio = real.data$Passes.Ratio
                                )
  
  correlation.data = cores(correlation.data)
# xG ----  
  cor.gols = round(cor(correlation.data$xG, correlation.data$GP), 2)
  xG.plot = ggplot(correlation.data, aes(y = GP, x = xG, label = Equipe,
                                         colour = paletta))+
    geom_text_repel(family = 'Avenir', colour = 'gray26')+
    geom_point(size = 2.5)+
    scale_colour_identity()+
    ylab("Gols por jogo")+
    xlab("xG por jogo")+
    theme_minimal()+
    theme(legend.position="none",
          text=element_text(family="Avenir Next Condensed"),
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "white"),
          axis.ticks.y = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.text.y = element_text(size = 12, color = 'gray26'),
          axis.title.y=element_text(size=13.5, color = 'gray26'),
          axis.title.x=element_text(size=13.5, color = 'gray26'),
          panel.grid.major = element_line(colour = "gray26",size = .03),
          panel.grid.minor = element_line(colour = "gray26",
                                          linetype = 'dashed',size = .045))+
    labs(title='Relação de xG e Gols Pró do Brasileirão no 2º turno')+
    theme(plot.title=element_text(size=20),
          text=element_text(family='Avenir Next Condensed'))+
    annotate("text", label = "@ProjecaoDeGol", x = (0.9*max(correlation.data$xG)), 
             y = 0.8, size = 3.7, colour = "gray34", family = 'Avenir')+    
          geom_smooth(linetype="dashed", method="lm", se = T,color = "azure3", 
                      alpha=.00,size=0.65)
# xGC ----  
  cor.golsC = round(cor(correlation.data$xGC, correlation.data$GC), 2)
  xGC.plot = ggplot(correlation.data, aes(y = GC, x = xGC, label = Equipe,
                                                     colour = paletta))+
    geom_text_repel(family = 'Avenir', colour = 'gray26')+
    geom_point(size = 2.5)+
    scale_colour_identity()+
    ylab("Gols Contra por jogo")+
    xlab("xGC por jogo")+
    theme_minimal()+
    theme(legend.position="none",
          text=element_text(family="Avenir Next Condensed"),
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "white"),
          axis.ticks.y = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.text.y = element_text(size = 12, color = 'gray26'),
          axis.title.y=element_text(size=13.5, color = 'gray26'),
          axis.title.x=element_text(size=13.5, color = 'gray26'),
          panel.grid.major = element_line(colour = "gray26",size = .03),
          panel.grid.minor = element_line(colour = "gray26",
                                          linetype = 'dashed',size = .045))+
    labs(title='Relação de xGC e Gols Contra do Brasileirão até a 24ª rodada')+
    theme(plot.title=element_text(size=20),
          text=element_text(family='Avenir Next Condensed'))+
    annotate("text", label = "@ProjecaoDeGol", x = 1.5, y = 0.8, size = 3.7, colour = "gray34", family = 'Avenir')+    
    geom_smooth(linetype="dashed", method="lm", se = T,color = "azure3", 
                alpha=.08,size=0.65)
  #annotate("text", x = 2.25, y = 19, label = cor.golsC, fontface="bold",size=4)
# xSG ----
  cor.SG = round(cor(correlation.data$xSG, correlation.data$SG), 2)
  xSG.plot = ggplot(correlation.data, aes(y = SG, x = xSG, label = Equipe,
                                         colour = paletta))+
    geom_text_repel(family = 'Avenir', colour = 'gray26')+
    geom_point(size = 2.5)+
    scale_colour_identity()+
    ylab("Saldo de Gols por jogo")+
    xlab("xSG por jogo")+
    theme_minimal()+
    theme(legend.position="none",
          text=element_text(family="Avenir Next Condensed"),
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "white"),
          axis.ticks.y = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.text.y = element_text(size = 12, color = 'gray26'),
          axis.title.y=element_text(size=13.5, color = 'gray26'),
          axis.title.x=element_text(size=13.5, color = 'gray26'),
          panel.grid.major = element_line(colour = "gray26",size = .03),
          panel.grid.minor = element_line(colour = "gray26",
                                          linetype = 'dashed',size = .045))+
    labs(title='Relação de xSG e saldo de gols do Brasileirão até a 24ª rodada')+
    theme(plot.title=element_text(size=20),
          text=element_text(family='Avenir Next Condensed'))+
    annotate("text", label = "@ProjecaoDeGol", x = .6, y = -0.8, size = 3.7, colour = "gray34", family = 'Avenir')+    
    geom_smooth(linetype="dashed", method="lm", se = T,color = "azure3", 
                alpha=.08,size=0.65)
# xG xGC -----  
  xG.xGC = ggplot(correlation.data, aes(y = xGC, x = xG, label = Equipe,
                               colour = paletta))+
    geom_text_repel(family = 'Avenir', colour = 'gray26')+
    geom_point(size = 2.5)+
    scale_colour_identity()+
    ylab("Valor esperado de gols tomados xGC por jogo")+
    xlab("Valor esperado de gols xG por jogo")+
    theme_minimal()+
    theme(legend.position="none",
          text=element_text(family="Avenir"),
          axis.line.y = element_line(
            colour = "gray26"),
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
    labs(title='Valor esperado de gols xG do Brasileirão até a 24ª rodada')+
    theme(plot.title=element_text(size=20),
          text=element_text(family='Avenir Next Condensed'))+
    geom_vline(xintercept = 1.2, color = 'gray26', size = .2)+
    geom_hline(yintercept = 1.2, color = 'gray26', size = .2)+
    annotate("text", label = "Boas defesas, ataques fracos", x = 1, y = 1, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+
    annotate("text", label = "Defesas fracas, ataques fracos", x = 1, y = 1.6, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Bons ataques, defesas fracas", x = 1.5, y = 1.6, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Bons ataques, boas defesas", x = 1.5, y = 1, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')
  
  
  
# xG e pontos -----
  xG.pontos.plot = ggplot(correlation.data, aes(xG, PPJ, label = times))+
    geom_text_repel()+
    geom_point(colour = "salmon")+
    theme(legend.position="none")+ylab("Pontos por jogo")+
    xlab("xG")+
    theme_minimal()+
    theme(axis.title.y = element_text(angle = 90), plot.title = element_text(face="bold"))+
    ggtitle("Relação entre xG e Pontos por jogo")+
    geom_smooth(linetype="dashed", method="lm")
  
# Chutes e Gols Pro ------
  mediana.y = median(correlation.data$Gols.chute)
  mediana.x = median(correlation.data$Chutes)
 # title.att = paste('Eficiência de ataque do Brasileirão até a ',rodadas, 'ª rodada',sep = '')
  title.att = paste('Eficiência de ataque do 2º turno do Brasileirão')
      Chutes.gols = ggplot(correlation.data, aes(y = Gols.chute, x = Chutes, label = Equipe,
                                                  colour = paletta))+
    geom_text_repel(family = 'Avenir', colour = 'gray26')+
    geom_point(size = 3.5)+
    scale_colour_identity()+
   ylab("Finalizações por gol marcado")+
    xlab("Finalizações por jogo")+
    theme_minimal()+
    theme(legend.position="none",
          text=element_text(family="Avenir"),
          axis.line.y = element_line(
            colour = "gray26"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.text.y = element_text(size = 12, color = 'gray26'),
          axis.title.y=element_text(size=14, color = 'gray26'),
          axis.title.x=element_text(size=14, color = 'gray26'),
          panel.grid.major = element_line(colour = "gray26",size = .03),
          panel.grid.minor = element_line(colour = "gray26",
                                          linetype = 'dashed',size = .045))+
    labs(title=title.att)+
    theme(plot.title=element_text(size=20, hjust = .5, colour = 'gray26'),
          text=element_text(family='Avenir Next Condensed'))+
    geom_vline(xintercept = mediana.x, color = 'gray26', size = .25,linetype="dashed")+
    geom_hline(yintercept = mediana.y, color = 'gray26', size = .25,linetype="dashed")+
    annotate("text", label = "Ameaças constantes", x = 13, y = 7, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+
    annotate("text", label = "Quietamente precisos", x = 8.3, y = 7, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Ineficientes", x = 8.3, y = 16.5, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Ativamente ineficientes", x = 13, y = 16.5, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')

  # Chutes e Gols Contra ------
      mediana.y = median(correlation.data$Gols.chuteC)
      mediana.x = median(correlation.data$Chutes.Contra)
      title.def = paste('Eficiência de defesa do Brasileirão até a ',rodadas, 'ª rodada',sep = '')
      
  Chutes.golsC = ggplot(correlation.data, aes(y = Gols.chuteC, x = Chutes.Contra, label = Equipe,
                                             colour = paletta))+
    geom_text_repel(family = 'Avenir', colour = 'gray26')+
    geom_point(size = 3.5)+
    scale_colour_identity()+
      ylab("Finalizações por gol concedido")+
      xlab("Finalizações concedidas por jogo")+
    theme_minimal()+
    theme(legend.position="none",
          text=element_text(family="Avenir"),
          axis.line.y = element_line(
            colour = "gray26"),
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
    labs(title=title.def)+
    theme(plot.title=element_text(size=20, hjust = .5, colour = 'gray26'),
          text=element_text(family='Avenir Next Condensed'))+
    geom_vline(xintercept = mediana.x, color = 'gray26', size = .25,linetype="dashed")+
    geom_hline(yintercept = mediana.y, color = 'gray26', size = .25,linetype="dashed")+
    annotate("text", label = "Peneiras", x = 13.5, y = 6.5, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+
    annotate("text", label = "Quietamente ineficientes", x = 11, y = 6.5, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Sólidos", x = 11, y = 16.5, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Ativamente ineficientes", x = 13.5, y = 16.5, 
             size = 6, colour = "gray16", family = 'Avenir Next Condensed')
  
# return -------

  newList <- list('xG' = xG.plot, 'xGC' = xGC.plot,'xGxGC' = xG.xGC,
                  'xSG' = xSG.plot,
                  'xG.pontos' = xG.pontos.plot, 'chutes.gols' = Chutes.gols,
                  'chutes.golsC' = Chutes.golsC
                  )
  return(newList)

}  