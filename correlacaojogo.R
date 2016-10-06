correlacao.jogo <- function(xG.campeonato, rodadas, casa, fora){
  source('cores.R')
  source('br16.R')
  library(ggplot2)
  bra16 = br16(rodadas = rodadas, rodada.inicial = 1)
  xG.data = xG.campeonato$xG.data
  xG.data = xG.data[order(xG.data$equipes),]
  real.data = bra16$classificacao
  real.data = real.data[order(real.data$Times),]
  rodadas = max(bra16$jogos$Rodada) - min(bra16$jogos$Rodada) +1
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
  correlation.data$paletta[which(!(correlation.data$Equipe %in% c(casa,fora)))] = 'gray92'
  correlation.data$ordem <- ifelse(correlation.data$paletta=="gray92", 1, 2)
  #ataque
  mediana.y = median(correlation.data$Gols.chute)
  mediana.x = median(correlation.data$Chutes)
  title.att = paste('Eficiência de ataque')
  Chutes.gols = ggplot()+
    geom_point(data = correlation.data, aes(y = Gols.chute, x = Chutes, label = Equipe,
                                            colour = paletta), size = 5.5) +
    geom_point(data = subset(correlation.data, ordem == 2),
               aes(y = Gols.chute, x = Chutes, label = Equipe,
                   colour = paletta), size = 5.5)+
    scale_colour_identity()+
    ylab("Finalizações por gol marcado")+
    xlab("Finalizações por jogo")+
    theme_classic()+
    theme(legend.position="none",
          text=element_text(family="Avenir"),
          axis.line.y = element_line(
            colour = "gray26"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.ticks.x = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'white'),
          axis.text.y = element_text(size = 12, color = 'white'),
          axis.title.y=element_text(size=16.5, color = 'gray26'),
          axis.title.x=element_text(size=16.5, color = 'gray26'))+
    labs(title=title.att)+
    theme(plot.title=element_text(size=20, hjust = .5, colour = 'gray26'),
          text=element_text(family='Avenir Next Condensed'))+
    geom_vline(xintercept = mediana.x, color = 'gray26', size = .25,linetype="dashed")+
    geom_hline(yintercept = mediana.y, color = 'gray26', size = .25,linetype="dashed")+
    annotate("text", label = "Ameaças constantes", x = 13, y = 7, 
             size = 4.5, colour = "gray16", family = 'Avenir Next Condensed')+
    annotate("text", label = "Quietamente precisos", x = 10.5, y = 7, 
             size = 4.5, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Ineficientes", x = 10.3, y = 16.5, 
             size = 4.5, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Ativamente ineficientes", x = 13, y = 16.5, 
             size = 4.5, colour = "gray16", family = 'Avenir Next Condensed')
  
  #defesa
  mediana.y = median(correlation.data$Gols.chuteC)
  mediana.x = median(correlation.data$Chutes.Contra)
  title.def = paste('Eficiência de defesa')
  
  Chutes.golsC = ggplot()+
    geom_point(data = correlation.data, aes(y = Gols.chuteC, x = Chutes.Contra, label = Equipe,
                                     colour = paletta),size = 5.5)+
    geom_point(data = subset(correlation.data, ordem == 2),
               aes(y = Gols.chuteC, x = Chutes.Contra, label = Equipe,
                   colour = paletta), size = 5.5)+
    scale_colour_identity()+
    ylab("Finalizações por gol concedido")+
    xlab("Finalizações concedidas por jogo")+
    theme_classic()+
    theme(legend.position="none",
          text=element_text(family="Avenir"),
          axis.line.y = element_line(
            colour = "gray26"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.ticks.x = element_line(color="white"),
          axis.text.x = element_text(size = 12, color = 'white'),
          axis.text.y = element_text(size = 12, color = 'white'),
          axis.title.y=element_text(size=16.5, color = 'gray26'),
          axis.title.x=element_text(size=16.5, color = 'gray26'))+
    labs(title=title.def)+
    theme(plot.title=element_text(size=20, hjust = .5, colour = 'gray26'),
          text=element_text(family='Avenir Next Condensed'))+
    geom_vline(xintercept = mediana.x, color = 'gray26', size = .25,linetype="dashed")+
    geom_hline(yintercept = mediana.y, color = 'gray26', size = .25,linetype="dashed")+
    annotate("text", label = "Peneiras", x = 13.5, y = 6.5, 
             size = 4.5, colour = "gray16", family = 'Avenir Next Condensed')+
    annotate("text", label = "Quietamente frágeis", x = 10.5, y = 6.5, 
             size = 4.5, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Sólidos", x = 11, y = 16.5, 
             size = 4.5, colour = "gray16", family = 'Avenir Next Condensed')+ 
    annotate("text", label = "Ativamente ineficientes", x = 13.75, y = 16.5, 
             size = 4.5, colour = "gray16", family = 'Avenir Next Condensed')
 
  
  nlist = list('ataque' = Chutes.gols,
               'defesa' = Chutes.golsC)
  return(nlist)
  
}