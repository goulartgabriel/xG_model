xGtimes <- function(dados, model, threshold, rodadas, rodada.inicial = 1, type, momentum = 0, peso = NULL){
  source("expected.R")
##Setup ------  
  dados = dados[which(dados$Rodada >= rodada.inicial & dados$Rodada <= rodadas), ]
  if(momentum > 0){
    dados2 = dados[which(dados$Rodada >= (rodadas - momentum) & dados$Rodada <= rodadas), ]
    exp.gol2 = rep(0,20)
    exp.golC2 = rep(0,20)
  }
  rodadas = rodadas - rodada.inicial + 1
  times = levels(dados$Time)
  exp.gol = rep(0,20)
  acc = rep(0,20)
  exp.golC = rep(0,20)
  for (i in 1:length(times)){
    nome = times[i]
    o = expected(equipe = nome, data = dados, modelo = model, njogos = rodadas,thresh = threshold, type = type)
    a = expected(adv = nome, data = dados, modelo = model, njogos = rodadas,thresh = threshold, type = type)
    exp.gol[i] = o$xG
    exp.golC[i] = a$xG
    acc[i] = o$acuracia
  }
  if(momentum > 0){
   for (i in 1:length(times)){
      nome = times[i]
      o = expected(equipe = nome, data = dados2, modelo = model, njogos = momentum,thresh = threshold, type = type)
      a = expected(adv = nome, data = dados2, modelo = model, njogos = momentum,thresh = threshold, type = type)
      exp.gol2[i] = o$xG
      exp.golC2[i] = a$xG
   }
   
  } else{
    exp.gol2 = 0
    exp.golC2 = 0
    momentum = 1
    peso = 0
  }

 
  exp.gol =  peso*(exp.gol2/momentum) + (1-peso)*exp.gol/rodadas
  exp.golC = peso*(exp.golC2/momentum) + (1-peso)*exp.golC/rodadas
  
##only xG ------
  
  df.xg <- data.frame(
    equipes = factor(c(times)),
    xG.jogo = exp.gol
  )
  df.xg = df.xg[order(-df.xg$xG.jogo),]
  df.xg$rank = as.numeric(seq(20:1))
  df.xg$paleta = 0
  df.xg$paleta[which(df.xg$xG.jogo > 1.46)] = '0'
  df.xg$paleta[which(df.xg$xG.jogo > 1.35 & df.xg$xG.jogo <= 1.46)] = '2'
  df.xg$paleta[which(df.xg$xG.jogo > 1.05 & df.xg$xG.jogo <= 1.35)] = '3'
  df.xg$paleta[which(df.xg$xG.jogo < 1.05)] = '4'
  
  paleta = c("steelblue4","skyblue3","gray67","firebrick3")


  xG.plot = ggplot(df.xg, aes(x=reorder(equipes,xG.jogo),y=xG.jogo,fill=paleta))+
    geom_bar(stat = "identity",alpha=0.85,width=.11)+
     geom_dotplot(binaxis = "y", method = "histodot",
                  binwidth = 0.05, stackdir = "center",colour=NA)+
    labs(title = 'xG por equipe', x = "",y = 'xG por jogo')+
    coord_flip()+
    theme_classic()+
    scale_x_discrete(name = "")+
    scale_y_continuous(name = "xG por jogo",
                       breaks = seq(0, max(df.xg$xG.jogo), 0.2)
                      )+
    scale_fill_manual(values=paleta)+
    theme(legend.position="none",
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(size = 12.8),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
         plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                   size=17, hjust=0.5, vjust=-1),
         panel.grid.major = element_line(colour = "gray26",size = .03),
         panel.grid.minor = element_line(colour = "gray26",
                                         linetype = 'dashed',size = .045))+
    annotate("text", label = "@ProjecaoDeGol", x = 1.2, y = 1.6, 
             size = 3.7, colour = "gray34",family="Avenir Next Condensed", fontface = 'italic')   
    
  xG.plot
  
  ## ONLY xGC --------
  
  df.xgc <- data.frame(
    equipes = factor(c(times)),
    xGC.jogo = exp.golC
  )
  df.xgc = df.xgc[order(df.xgc$xGC.jogo),]
  df.xgc$rank = as.numeric(seq(20:1))
  df.xgc$paleta = 0
  df.xgc$paleta[which(df.xgc$xGC.jogo > 1.4)] = '0'
  df.xgc$paleta[which(df.xgc$xGC.jogo > 1.2 & df.xgc$xGC.jogo < 1.4)] = '1'
  df.xgc$paleta[which(df.xgc$xGC.jogo > 1.1 & df.xgc$xGC.jogo <= 1.2)] = '2'
  df.xgc$paleta[which(df.xgc$xGC.jogo < 1.1)] = '4'
  
  paleta2 = rev(c("steelblue4","skyblue3","gray67","firebrick3"))
  
  xGC.plot = ggplot(df.xgc, aes(x=reorder(equipes,-xGC.jogo),y=xGC.jogo,fill=paleta))+
    geom_bar(stat = "identity",alpha=0.85,width=.11)+
    geom_dotplot(binaxis = "y", method = "histodot",
                 binwidth = 0.05, stackdir = "center",colour=NA)+
    coord_flip()+
    labs(title = 'xGC por equipe', x = "",y = 'xGC por jogo')+
    theme_minimal()+
    scale_x_discrete(name = "")+
    scale_y_continuous(name = "xGC por jogo",
                       breaks = seq(0, 1.9, 0.2)
    )+
    scale_fill_manual(values=paleta2)+
    theme(legend.position="none",
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(size = 12.8),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          plot.title = element_text(family="Avenir Next Condensed", color = "gray26",
                                    size=17, hjust=0.5, vjust=-1))
  
  xGC.plot

  
  
##xSG -------------------
  expSG = exp.gol - exp.golC
  dat2 <- data.frame(
    equipes = factor(c(times)),
    xSG.jogo = expSG,  
    xSG.total = expSG
  )
  dat2 = dat2[order(-dat2$xSG.jogo),]
  dat2$rank = as.factor(seq(20:1))
  
  dat2$paleta = 0
  dat2$paleta[which(dat2$xSG.jogo > 0.4)] = '0'
  dat2$paleta[which(dat2$xSG.jogo > 0 & dat2$xSG.jogo <= 0.4)] = '2'
  dat2$paleta[which(dat2$xSG.jogo > -0.4 & dat2$xSG.jogo <= 0)] = '3'
  dat2$paleta[which(dat2$xSG.jogo < -0.4)] = '4'
  
  paleta.SG = c("darkslategrey","skyblue4","snow3","firebrick3")
  
  xSG = ggplot(dat2, aes(x=reorder(equipes,xSG.jogo),y=xSG.jogo,fill=paleta))+
    geom_bar(stat = "identity",alpha=0.85,width=.09)+
    geom_dotplot(binaxis = "y", method = "histodot",
                 binwidth = 0.03, stackdir = "centerwhole", colour = NA)+
    coord_flip()+
    labs(title = 'xSG por equipe', x = "",y = 'xSG por jogo')+
    theme_classic()+
    scale_x_discrete(name = "")+
    scale_y_continuous(name = "xSG por jogo",breaks = seq(-0.8, 0.9, 0.2))+
    scale_fill_manual(values=paleta.SG)+
    theme(legend.position="none",
          axis.line.y = element_line(
            colour = "white"),
          axis.line.x = element_line(
            colour = "gray26"),
          axis.ticks.y = element_line(color="white"),
          axis.text.y = element_text(size = 12.8),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          plot.title = element_text(family="Avenir Next Condensed", color = "gray26", hjust=0.5, vjust=-1, size = 17),
          panel.grid.major = element_line(colour = "gray26",size = .03),
          panel.grid.minor = element_line(colour = "gray26",
                                         linetype = 'dashed',size = .045))+
    annotate("text", label = "@ProjecaoDeGol", x = 1.2, y = 0.89*max(dat2$xSG.jogo), 
             size = 3.7, colour = "gray34",family="Avenir Next Condensed",fontface = 'italic') 
  
  
  ##MANDO DE CAMPO--------
    exp.golCasa = rep(0,20)
    exp.golFora = rep(0,20)
    exp.golCCasa = rep(0,20)
    exp.golCFora = rep(0,20)
    jogos.casa = rep(0,20)
    jogos.fora = rep(0,20)
    
    
    for (i in 1:length(times)){
      nome = times[i]
      ind = which(dados$Time == nome)
      jogos.time = dados[ind,]
      
      em.casa = jogos.time[which(jogos.time$Em.Casa == 1),]
      em.casa = count(em.casa$Adversario)
      jogos.casa[i] = length(em.casa$x)
      
      
      em.fora = jogos.time[which(jogos.time$Em.Casa == 0),]
      em.fora = count(em.fora$Adversario)
      jogos.fora[i] = length(em.fora$x)
      
      home.gp = expected(equipe = nome, data = dados, modelo = model, casa = 1, njogos = jogos.casa[i],thresh = threshold,type = type)
      home.gc = expected(adv = nome, data = dados, modelo = model, casa = 0, njogos = jogos.fora[i],thresh = threshold,type = type)
      exp.golCasa[i] = home.gp$xG
      exp.golCCasa[i] = home.gc$xG
      
      away.gp = expected(equipe = nome, data = dados, modelo = model, casa = 0, njogos = rodadas,thresh = threshold,type = type)
      away.gc = expected(adv = nome, data = dados, modelo = model, casa = 1, njogos = rodadas,thresh = threshold,type = type)
      exp.golFora[i] = away.gp$xG
      exp.golCFora[i] = away.gc$xG   
    }
    

##RETURN---------  
xG.times = data.frame(equipes = times,
                      xG = exp.gol,
                      xGC = exp.golC,
                   #   xGratio = xG.ratio,
                      xSG = (exp.gol-exp.golC),
                       xG.casa = exp.golCasa/jogos.casa,
                       xGC.casa = exp.golCCasa/jogos.casa,
                       xG.fora = exp.golFora/jogos.fora,
                   xGC.fora = exp.golCFora/jogos.fora )
#                    xG.m = peso*(exp.gol2/momentum) + (1-peso)*exp.gol/rodadas,
#                    xGC.m = peso*(exp.golC2/momentum) + (1-peso)*exp.golC/rodadas)
  

# xG.times = cbind(xG.times,xG.casa.df[,2:3])
# xG.times = cbind(xG.times,xG.fora.df[,2:3])

xG.times = xG.times[order(xG.times$equipes),]



  newList <- list('xG.data' = xG.times, 'xG.plot' = xG.plot, 
                  'xGC.plot' = xGC.plot,'df' = df.xg,'xSG' = xSG
                 
                  )
  
  return(newList)
  
  
}
  