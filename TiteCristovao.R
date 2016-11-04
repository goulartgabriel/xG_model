jair.xG = xGtimes(dados = xG.chutes, model = model, 
                     rodada.inicial = 1, rodadas = 13, threshold = threshold,type = 'bayes.glm')


tite.xG = xGtimes(dados = xG.chutes, model = model, 
                       rodada.inicial = 14, rodadas = 14, threshold = threshold, type = 'bayes.glm')



jair.xG$xG.data[jair.xG$xG.data$equipes == 'Palmeiras',]*13


tite.xG$xG.data[tite.xG$xG.data$equipes == 'Palmeiras',]


jair.xG = (j1$xG + j2$xG +j3$xG+j4$xG+j5$xG)/22
jair.xGC = (j1$xGC + j2$xGC +j3$xGC+j4$xGC+j5$xGC)/22
t.xG = (t1$xG+t2$xG+t3$xG+t4$xG)/9
t.xGC = (t1$xGC+t2$xGC+t3$xGC+t4$xGC)/9

#jogos.jair = c(seq(1:13),15,seq(from = 22, to = 25),27,28,30,31)
#jogos.t = c(14,seq(from = 16, to = 21),26,29)

jogos.jair = c(seq(from = 20, to = 33))
jogos.t = c(seq(from = 1, to = 19))
jogos.t2 = c(seq(from = 20, to = 33))


tite.xG = 0
tite.xGC = 0
tite.gols = 0
tite.golsC = 0
tite.chutes = 0
tite.passesr = 0
tite.passes = 0
tite.pontos = 0
i = 0
for (k in jogos.t){
  i = i+1
  tite = xGtimes(dados = xG.chutes, model = model, 
                    rodada.inicial = k, rodadas = k, threshold = threshold, type = 'bayes.glm')
  tite = tite$xG.data[tite$xG.data$equipes == 'Flamengo',]
  tite.xG[i] = tite$xG
  tite.xGC[i] = tite$xGC
  
  titebr = br16(rodadas = k, rodada.inicial = k)
  titebr = titebr$classificacao
  titebr = titebr[titebr$Times == 'Flamengo',]
  tite.gols[i] = titebr$Gols.Pro
  tite.golsC[i] = titebr$Gols.Contra
  tite.chutes[i] = titebr$Chutes.Certos + titebr$Chutes.Errados
  tite.passes[i] = titebr$Passes.Certos
  tite.passesr[i] = titebr$Passes.Ratio
  tite.pontos[i] = titebr$Pontuacao
}

tite.xG = 0
tite.xGC = 0
tite.gols = 0
tite.golsC = 0
tite.chutes = 0
tite.passesr = 0
tite.passes = 0
tite.pontos = 0
i = 0
for (k in jogos.t2){
  i = i+1
  tite = xGtimes(dados = xG.chutes, model = model, 
                 rodada.inicial = k, rodadas = k, threshold = threshold, type = 'bayes.glm')
  tite = tite$xG.data[tite$xG.data$equipes == 'Flamengo',]
  tite.xG[i] = tite$xG
  tite.xGC[i] = tite$xGC
  
  titebr = br16(rodadas = k, rodada.inicial = k)
  titebr = titebr$classificacao
  titebr = titebr[titebr$Times == 'Flamengo',]
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
i = 0
for (k in jogos.jair){
  i = i+1
  c = xGtimes(dados = xG.chutes, model = model, 
                 rodada.inicial = k, rodadas = k, threshold = threshold, type = 'bayes.glm')
  c = c$xG.data[c$xG.data$equipes == 'Botafogo',]
  c.xG[i] = c$xG
  c.xGC[i] = c$xGC
  
  titebr = br16(rodadas = k, rodada.inicial = k)
  titebr = titebr$classificacao
  titebr = titebr[titebr$Times == 'Botafogo',]
  c.gols[i] = titebr$Gols.Pro
  c.golsC[i] = titebr$Gols.Contra
  c.chutes[i] = titebr$Chutes.Certos + titebr$Chutes.Errados
  c.passesr[i] = titebr$Passes.Ratio
  c.passes[i] = titebr$Passes.Certos
  c.pontos[i] = titebr$Pontuacao
}


df = data.frame(Tecnico = c(rep('Zé Ricardo',length(jogos.t)),rep('Jair Ventura',length(jogos.jair))),
                Pontos = c(tite.pontos,c.pontos),
                xG = c(tite.xG,c.xG),
                xGC = c(tite.xGC,c.xGC),
                Gols = c(tite.gols,c.gols),
                GolsC = c(tite.golsC,c.golsC),
                Chutes = c(tite.chutes,c.chutes),
                Passes = c(tite.passes,c.passes),
                Passesr = c(tite.passesr,c.passesr)
                
                
                )

dfz = rbind(df[1:19,],df2[1:14,])
dfz$Tecnico = as.character(dfz$Tecnico)
dfz$Tecnico[1:19] = '1º turno'
dfz$Tecnico[20:33] = '2º turno'

cols<-c("firebrick", "gray40")

t.test(dfz$Pontos~dfz$Tecnico)
df$legenda[1:33] = 'Média pontos = 1.88'
df$legenda[34:47] = 'Média pontos = 2.21'
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

t.test(dfz$xG~dfz$Tecnico)
dfz$legenda[1:19] = 'Média xG = 1.25'
dfz$legenda[20:33] = 'Média xG = 1.82'

xG = ggplot(dfz, aes(x=Tecnico, y = xG, fill = legenda))+
  geom_violin(alpha=0.7)+
  geom_jitter(width=0.25, height=0.05)+
  coord_flip()+
  theme_minimal()+
  guides(fill = guide_legend(reverse=F, title = NULL))+
  scale_fill_manual(values=cols)+
  xlab("")+
  ylab("xG")+
  theme(#plot.title = element_text(vjust = 0.1),
    text=element_text(family="Avenir Next Condensed"))+
  labs(title = "xG do Flamengo por returno",
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
           family="Avenir Next Condensed", fontface = 'italic')
xG  

png("AXG.png", width = 8, height = 6, units = 'in', res = 400)
plot(xG) # Make plot
dev.off()

t.test(dfz$xGC~dfz$Tecnico)
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




t.test(dfz$Gols~dfz$Tecnico)
df$legenda[1:9] = 'Média de gols = 1.25'
df$legenda[10:31] = 'Média de gols = 1.29'
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

t.test(dfz$Chutes~dfz$Tecnico)
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

t.test(dfz$Passes~dfz$Tecnico)
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


bla.xg = data.frame(Equipe = c('Flamengo','Flamengo','Botafogo','Botafogo','Palmeiras','Palmeiras'),
                 Turno = c(1,2,1,2,1,2),
                 Valor = c(1.25,1.82,1.11, 1.34, 1.78,1.47))

bla.xgc = data.frame(Equipe = c('Flamengo','Flamengo','Botafogo','Botafogo','Palmeiras','Palmeiras'),
                    Turno = c(1,2,1,2,1,2),
                    Valor = c(1.05,1.22,1.26, 0.88, 0.91,0.95))

xg = ggplot(bla.xg, aes(x=Equipe, y = Valor, fill = factor(Turno)))+
  geom_bar(stat="identity",position="dodge",alpha=0.85,width=.6)+
  xlab("")+
  ylab("xG")+
  ylim(c(0,1.9))+
  theme_minimal()+
  labs(title = "xG por turno do Brasileirão")+
  scale_fill_manual(guide = guide_legend(title = "Turno"),
                    values=c('gray64','dodgerblue3'))+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                  size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                     size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 13.5, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'),
        text=element_text(family="Avenir Next Condensed"),
        legend.position="none")

xgc = ggplot(bla.xgc, aes(x=Equipe, y = Valor, fill = factor(Turno)))+
  geom_bar(stat="identity",position="dodge",alpha=0.85,width=.6)+
  xlab("")+
  ylab("xGC")+
  ylim(c(0,1.9))+
  theme_minimal()+
  labs(title = "xGC por turno do Brasileirão")+
  scale_fill_manual(guide = guide_legend(title = "Turno"),
                    values=c('gray64','dodgerblue3'))+
  theme(plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                  size=17, hjust=0.5, vjust=-1),
        plot.subtitle = element_text(family="Avenir Next Condensed", color = "gray26",
                                     size = 13,hjust=0.5, vjust=-1),
        axis.text.x = element_text(size = 13.5, color = 'gray26'),
        axis.text.y = element_text(size = 12, color = 'gray26'),
        axis.title.y=element_text(size=13.5, color = 'gray26'),
        axis.title.x=element_text(size=13.5, color = 'gray26'),
        text=element_text(family="Avenir Next Condensed"))

b = ggdraw()+
  draw_plot(xg, 0, 0, .5, 1) +
  draw_plot(xgc, 0.5, 0, .5, 1)

png("Axg.png", width = 8, height = 7, units = 'in', res = 500)
plot(b) # Make plot
dev.off()
