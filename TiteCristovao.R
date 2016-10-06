cristovao.xG = xGtimes(dados = xG.chutes, model = model, 
                     rodada.inicial = 10, rodadas = rodadas, threshold = threshold,type = 'bayes.glm')

tite.xG = xGtimes(dados = xG.chutes, model = model, 
                       rodada.inicial = 1, rodadas = 8, threshold = threshold, type = 'bayes.glm')

cristovao.xG$xG.data[cristovao.xG$xG.data$equipes == 'Corinthians',]

tite.xG$xG.data[tite.xG$xG.data$equipes == 'Corinthians',]

tite.xG = 0
tite.xGC = 0
tite.gols = 0
tite.golsC = 0
tite.chutes = 0
tite.passesr = 0
tite.passes = 0
tite.pontos = 0
for (i in 1:8){
  tite = xGtimes(dados = xG.chutes, model = model, 
                    rodada.inicial = i, rodadas = i, threshold = threshold, type = 'bayes.glm')
  tite = tite$xG.data[tite$xG.data$equipes == 'Corinthians',]
  tite.xG[i] = tite$xG
  tite.xGC[i] = tite$xGC
  
  titebr = br16(rodadas = i, rodada.inicial = i)
  titebr = titebr$classificacao
  titebr = titebr[titebr$Times == 'Corinthians',]
  tite.gols[i] = titebr$Gols.Pro
  tite.golsC[i] = titebr$Gols.Contra
  tite.chutes[i] = titebr$Chutes.Certos + titebr$Chutes.Errados
  tite.passes[i] = titebr$Passes.Certos
  tite.passesr[i] = titebr$Passes.Ratio
  tite.pontos[i] = titebr$Pontuacao
}

c.xG = 0
c.xGC = 0
c.gols = 0
c.golsC = 0
c.chutes = 0
c.passes = 0
c.passesr = 0
c.pontos = 0
for (i in 10:26){
  print(i)
  c = xGtimes(dados = xG.chutes, model = model, 
                 rodada.inicial = i, rodadas = i, threshold = threshold, type = 'bayes.glm')
  c = c$xG.data[c$xG.data$equipes == 'Corinthians',]
  c.xG[i-9] = c$xG
  c.xGC[i-9] = c$xGC
  
  titebr = br16(rodadas = i, rodada.inicial = i)
  titebr = titebr$classificacao
  titebr = titebr[titebr$Times == 'Corinthians',]
  c.gols[i-9] = titebr$Gols.Pro
  c.golsC[i-9] = titebr$Gols.Contra
  c.chutes[i-9] = titebr$Chutes.Certos + titebr$Chutes.Errados
  c.passesr[i-9] = titebr$Passes.Ratio
  c.passes[i-9] = titebr$Passes.Certos
  c.pontos[i-9] = titebr$Pontuacao
}


df = data.frame(Tecnico = c(rep('Tite',8),rep('Cristóvão',17)),
                Pontos = c(tite.pontos,c.pontos),
                xG = c(tite.xG,c.xG),
                xGC = c(tite.xGC,c.xGC),
                Gols = c(tite.gols,c.gols),
                GolsC = c(tite.golsC,c.golsC),
                Chutes = c(tite.chutes,c.chutes),
                Passes = c(tite.passes,c.passes),
                Passesr = c(tite.passesr,c.passesr)
                
                
                )
cols<-c("purple4", "gray88")

t.test(df$Pontos~df$Tecnico)
df$legenda[1:8] = 'Média pontos = 1.63'
df$legenda[9:25] = 'Média pontos = 1.47'
pontos = ggplot(df, aes(x=Tecnico, y = Pontos, fill = legenda))+
  geom_violin(alpha=0.7)+
  geom_jitter(width=0.25, height=0.05)+
  coord_flip()+
  theme_minimal()+
  guides(fill = guide_legend(reverse=T, title = NULL))+
  scale_fill_manual(values=cols)+
  xlab("")+
  ylab("Pontos")+
  theme(#plot.title = element_text(vjust = 0.1),
    text=element_text(family="Avenir Next Condensed"))+
  labs(title = "Corinthians com Tite possuia maior média de pontos",
       subtitle='Pontos representam jogos')+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                            size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                 size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 12, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'))+
  annotate("text", label = "@ProjecaoDeGol", 
           x = 0.5, y = 0.36, size = 3.7, colour = "gray34",
           family="Avenir Next Condensed")
  
pontos

png("Cpontos.png", width = 8, height = 6, units = 'in', res = 400)
plot(pontos) # Make plot
dev.off()

t.test(df$xG~df$Tecnico)
df$legenda[1:8] = 'Média xG = 1.57'
df$legenda[9:25] = 'Média xG = 1.32'

xG = ggplot(df, aes(x=Tecnico, y = xG, fill = legenda))+
  geom_violin(alpha=0.7)+
  geom_jitter(width=0.25, height=0.05)+
  coord_flip()+
  theme_minimal()+
  guides(fill = guide_legend(reverse=T, title = NULL))+
  scale_fill_manual(values=cols)+
  xlab("")+
  ylab("xG")+
  theme(#plot.title = element_text(vjust = 0.1),
    text=element_text(family="Avenir Next Condensed"))+
  labs(title = "Corinthians com Tite possuia maior média de xG",
       subtitle='Pontos representam jogos')+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                  size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                     size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 12, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'))+
  annotate("text", label = "@ProjecaoDeGol", 
           x = 0.5, y = .75, size = 4.5, colour = "gray34",
           family="Avenir Next Condensed")
xG  

png("CXG.png", width = 8, height = 6, units = 'in', res = 400)
plot(xG) # Make plot
dev.off()

t.test(df$xGC~df$Tecnico)
df$legenda[1:8] = 'Média xGC = 0.83'
df$legenda[9:25] = 'Média xGC = 1.17'
xGC = ggplot(df, aes(x=Tecnico, y = xGC, fill = legenda))+
  geom_violin(alpha=0.7)+
  geom_jitter(width=0.25, height=0.05)+
  coord_flip()+
  theme_minimal()+
  guides(fill = guide_legend(reverse=F, title = NULL))+
  scale_fill_manual(values=rev(cols))+
  xlab("")+
  ylab("xGC")+
  theme(#plot.title = element_text(vjust = 0.1),
    text=element_text(family="Avenir Next Condensed"))+
  labs(title = "Corinthians com Tite possuia menor média de xGC",
       subtitle='Pontos representam jogos')+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                  size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                     size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 12, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'))+
  annotate("text", label = "@ProjecaoDeGol", 
           x = 0.5, y = 0.5, size = 4.5, colour = "gray34",
           family="Avenir Next Condensed")
          
xGC 

png("CXGC.png", width = 8, height = 6, units = 'in', res = 400)
plot(xGC) # Make plot
dev.off()




t.test(df$Gols~df$Tecnico)
df$legenda[1:8] = 'Média de gols = 1.25'
df$legenda[9:25] = 'Média de gols = 1.29'
Gols = ggplot(df, aes(x=Tecnico, y = Gols, fill = legenda))+
  geom_violin(alpha=0.7)+
  geom_jitter(width=0.25, height=0.05)+
  coord_flip()+
  theme_minimal()+
  guides(fill = guide_legend(reverse=F, title = NULL))+
  scale_fill_manual(values=rev(cols))+
  xlab("")+
  ylab("Gols")+
  theme(#plot.title = element_text(vjust = 0.1),
    text=element_text(family="Avenir Next Condensed"))+
  labs(title = "Corinthians com Cristóvão possuia maior média de gols",
       subtitle='Pontos representam jogos')+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                  size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                     size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 12, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'))+
  annotate("text", label = "@ProjecaoDeGol", 
           x = 0.5, y = 0.5, size = 4.5, colour = "gray34",
           family="Avenir Next Condensed")

Gols

png("Cgols.png", width = 8, height = 6, units = 'in', res = 400)
plot(Gols) # Make plot
dev.off()

t.test(df$GolsC~df$Tecnico)
df$legenda[1:8] = 'Média de gols sofridos = 0.75'
df$legenda[9:26] = 'Média de gols sofridos = 1.11'
GolsC = ggplot(df, aes(x=Tecnico, y = GolsC, fill = legenda))+
  geom_violin(alpha=0.7)+
  geom_jitter(width=0.25, height=0.05)+
  coord_flip()+
  theme_minimal()+
  guides(fill = guide_legend(reverse=F, title = NULL))+
  scale_fill_manual(values=rev(cols))+
  xlab("")+
  ylab("Gols")+
  theme(#plot.title = element_text(vjust = 0.1),
    text=element_text(family="Avenir Next Condensed"))+
  labs(title = "Corinthians com Tite possuia menor média de gols sofridos",
       subtitle='Pontos representam jogos')+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                  size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                     size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 12, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'))+
  annotate("text", label = "@ProjecaoDeGol", 
           x = 0.5, y = 0.5, size = 4.5, colour = "gray34",
           family="Avenir Next Condensed")
GolsC

png("CgolsC.png", width = 8, height = 6, units = 'in', res = 400)
plot(GolsC) # Make plot
dev.off()

t.test(df$Chutes~df$Tecnico)
df$legenda[1:8] = 'Média de Chutes = 13.50'
df$legenda[9:26] = 'Média de Chutes = 11.78'
Chutes = ggplot(df, aes(x=Tecnico, y = Chutes, fill = legenda))+
  geom_violin(alpha=0.7)+
  geom_jitter(width=0.25, height=0.05)+
  coord_flip()+
  theme_minimal()+
  guides(fill = guide_legend(reverse=T, title = NULL))+
  scale_fill_manual(values=cols)+
  xlab("")+
  ylab("Pontos")+
  theme(#plot.title = element_text(vjust = 0.1),
    text=element_text(family="Avenir Next Condensed"))+
  labs(title = "Corinthians com Tite possuia maior média de chutes",
       subtitle='Pontos representam jogos')+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                  size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                     size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 12, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'))+
  annotate("text", label = "@ProjecaoDeGol", 
           x = 0.5, y = 5, size = 4.5, colour = "gray34",
           family="Avenir Next Condensed")
Chutes

png("Cchutes.png", width = 8, height = 6, units = 'in', res = 400)
plot(Chutes) # Make plot
dev.off()

t.test(df$Passes~df$Tecnico)
df$legenda[1:8] = 'Média de passes certos = 457.9'
df$legenda[9:26] = 'Média de passes certos = 394.2'
Passes = ggplot(df, aes(x=Tecnico, y = Passes, fill = legenda))+
  geom_violin(alpha=0.7)+
  geom_jitter(width=0.25, height=0.05)+
  coord_flip()+
  theme_minimal()+
  guides(fill = guide_legend(reverse=T, title = NULL))+
  scale_fill_manual(values=cols)+
  xlab("")+
  ylab("Pontos")+
  theme(#plot.title = element_text(vjust = 0.1),
    text=element_text(family="Avenir Next Condensed"))+
  labs(title = "Corinthians com Tite possuia maior média de passes certos",
       subtitle='Pontos representam jogos')+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                  size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                     size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 12, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'))+
  annotate("text", label = "@ProjecaoDeGol", 
           x = 0.5, y = 300, size = 4.5, colour = "gray34",
           family="Avenir Next Condensed")

Passes

png("Cpasses.png", width = 8, height = 6, units = 'in', res = 400)
plot(Passes) # Make plot
dev.off()
