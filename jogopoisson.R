poisson.pred <- function(casa, fora = NULL, rodadas, models, type, momentum, peso){
  source("br16.R")
  source("xGtimes.R")
  source("setupxG.R")
  source('cores.R')
  source('probabilidades.R')
  source("modeloPdG.R")
  source('evolucaoxG.R')
  source('correlacaojogo.R')
  library(cowplot)
  xG = setup.xG(rodadas)
  xG.chutes = xG$xG.chutes
  threshold = 0.35
  
 
  model = models$model
  lm.Gol.Casa = models$Casa
  lm.Gol.Fora = models$Fora
  jogos = models$jogos
  campeonato = xGtimes(dados = xG.chutes, model = model,
                       rodadas = rodadas, threshold = threshold, 
                       type = type, momentum = momentum, peso = peso)
  xG.data = campeonato$xG.data
  xG.avg = mean(jogos$xG.time.casa.m)
  times = levels(xG.data$equipes)
  
  ##prediction
  data = xG.chutes
  data = data[data$Rodada == (rodadas),]
  
  gol.casa = rep(0,length(casa))
  gol.fora = rep(0,length(casa))
  
  if (is.null(fora)){
    fora = rep(0,length(casa))
    for (i in 1:length(casa)){
      fora[i] = as.character(data[data$Time == casa[i],]$Adversario[1])
    }
  }
  poisson.casa = 0
  poisson.fora = 0
  prob.casa = 0
  empate = 0
  prob.fora = 0
  for(i in 1:length(casa)){
    
    
    index.casa = which(times == casa[i])
    index.fora = which(times == fora[i])
    
    df.casa =  data.frame(xG.pre = xG.data$xG[index.casa],
                          xGC.adv.pre = xG.data$xGC[index.fora],
                          xG.adv.pre = xG.data$xG[index.fora],
                          xGC.pre = xG.data$xGC[index.casa],
                          Casa = times[index.casa],
                          Fora = times[index.fora],
                          Mando = 'Casa')
    p.casa = exp(predict(lm.Gol.Casa,df.casa))
    df.fora =  data.frame(xG.adv.pre = xG.data$xG[index.casa],
                          xGC.pre = xG.data$xGC[index.fora],
                          xG.pre = xG.data$xG[index.fora],
                          xGC.adv.pre = xG.data$xGC[index.casa],
                          Fora = times[index.fora],
                          Casa = times[index.casa],
                          Mando = 'Fora')
    p.fora = exp(predict(lm.Gol.Fora,df.fora))
    p.casa = round(p.casa,2)
    p.fora = round(p.fora,2)
    poisson.casa[i] = p.casa
    poisson.fora[i] = p.fora
    
    probs = probabilidades(p.casa,p.fora)
    prob.casa[i] = probs$Casa
    prob.fora[i] = probs$Fora
    empate[i] = probs$Empate
    
  }
  
  probs = data.frame(casa = casa,
                  vitoria = prob.casa,
                  empate = empate,
                  fora = prob.fora,
                  adv = fora)
  print(probs)
  xG = data.frame(Equipe = c(casa,fora),
                  xG = c(poisson.casa,poisson.fora),
                  rank = c(1:(2*length(casa)))
                  
  )
  xG = cores(xG)
  
  historico.xG = matrix(0,2*length(casa),21)
  historico.xGC = matrix(0,2*length(casa),21)
  historico.xSG = matrix(0,2*length(casa),21)
  
  times.rodada = c(casa,fora)
  for (i in 1:length(times.rodada)){
    historico.xG[i,1] = times.rodada[i]
    last20.games = jogos[jogos$Rodada >= (rodadas-18),]
    indexes = which(last20.games == times.rodada[i], arr.ind = T) #2_>8 3_> 9
    indexes = indexes[order(indexes[,1]),]
    indexes.xG = cbind(indexes[,1],(indexes[,2]+6))
    last20.games = as.matrix(last20.games)
    last20.xG = last20.games[indexes.xG]
    historico.xG[i,2:20] = last20.xG  
    historico.xG[i,21] = xG.data$xG[which(xG.data$equipes == times.rodada[i])]
    
    historico.xGC[i,1] = times.rodada[i]
    indexes.xGC = cbind(indexes[,1],(indexes[,2]+8))
    last20.xGC = last20.games[indexes.xGC]
    historico.xGC[i,2:20] = last20.xGC  
    historico.xGC[i,21] = xG.data$xGC[which(xG.data$equipes == times.rodada[i])]
    
    
    sg1 = as.numeric(historico.xG[i,2:21])
    sg2 = as.numeric(historico.xGC[i,2:21])
    xsg = c(sg1-sg2)
    xsg = matrix(xsg,nrow = 1,ncol = 20)
    historico.xSG[i,] = cbind(times.rodada[i],xsg) 
    
  }
  historico.xG = as.data.frame(historico.xG)
  historico.xGC = as.data.frame(historico.xGC)
  historico.xSG = as.data.frame(historico.xSG)
  
  colnames(historico.xG) = c('Equipe',seq(1:(20)))
  colnames(historico.xSG) = c('Equipe',seq(1:(20)))
  colnames(historico.xGC) = c('Equipe',seq(1:(20)))
  
  xG$Equipe2 = toupper(xG$Equipe)
  
  ggplot.objects = list()
  
  for (i in 1:length(casa)){
    ##PxG
    ind.fora = which(xG$Equipe == fora[i])
    ind.casa = which(xG$Equipe == casa[i])
    jogo.df = xG[c(ind.casa,ind.fora),]
    jogo.df = jogo.df[nrow(jogo.df):1, ]
    jogo.df$Equipe <- factor(jogo.df$Equipe, levels = jogo.df$Equipe)
   # jogo.df$Equipe = toupper(jogo.df$Equipe)

    jogo = ggplot(jogo.df, aes(x = Equipe, y = xG, fill = as.factor(rank)))+
      geom_bar(stat = 'identity', alpha = 0.85, width = 0.65)+
      coord_flip()+
      theme_classic()+
      scale_fill_manual(values = rev(jogo.df$paletta))+
      geom_text(aes(label = xG, x = Equipe, y = xG), 
                position = position_dodge(width = 0.8), hjust = -0.30,
                size = 7,family="Avenir Next Condensed",
                color =  jogo.df$paletta)+
      ylim(0,max(xG$xG*1.15))+
      ylab('')+
      xlab("")+
      theme(legend.position="none",
            plot.title = element_text(hjust = 0.428, size = 22.5,vjust = -.1,
                                       color = 'gray26'),
            axis.line.y = element_line(
                                       colour = "white"),
            axis.line.x = element_line(
              colour = "gray26"),
            axis.ticks.y = element_line(color="white"),
            axis.text.y = element_text(size = 20,
                                       colour=(jogo.df$paletta)),
            axis.text.x = element_text(size = 15, color = 'gray26'),
            axis.title.x=element_text(size=17, color = 'gray26'))+
      ggtitle('Projeções de Gol')+
      theme(#plot.title = element_text(vjust = 0.1),
            text=element_text(family="Avenir Next Condensed"))
    
    ##Probabilidades
    prob.jogo = probs[i,]
    prob.jogo = data.frame(Equipe = c(as.character(prob.jogo$casa),'Empate',
                                      as.character(prob.jogo$adv),'vazio'),
                           Prob = c(prob.jogo$vitoria,prob.jogo$empate,prob.jogo$fora,0))
    prob.jogo$rank = 1
    prob.jogo$rank[4] = 2
    prob.jogo=prob.jogo[order(-1:-4),] 
    prob.jogo$Equipe <- factor(prob.jogo$Equipe,levels=unique(prob.jogo$Equipe))
    prob.jogo$pos[4] = prob.jogo$Prob[4]/2
    prob.jogo$pos[3] = prob.jogo$Prob[3]/2 +prob.jogo$Prob[4]
    prob.jogo$pos[2] = prob.jogo$Prob[2]/2 +(100-prob.jogo$Prob[2])
    prob.jogo$pos[1] = 1
    
    prob.jogo = cores(prob.jogo)
    if(prob.jogo$pos[2] > 95){
      empate.pos = prob.jogo$pos[3]-5
      fora.pos = 93
    } else{
      empate.pos = prob.jogo$pos[3]
      fora.pos = prob.jogo$pos[2]
    }
    
    chances = ggplot(prob.jogo,aes(x = factor(rank), fill = factor(Equipe), 
                                   y = Prob)) +
      geom_bar(stat = 'identity',alpha = 0.85) +
      coord_flip()+
      xlab("")+
      ylab("")+
      ylim(c(0,110))+
      geom_text(data=prob.jogo, aes(x = rank, y = pos, 
                                      label = paste0(Prob,"%")), size=11, 
                colour = 'white', family = "Avenir Next Condensed")+
      scale_fill_manual(values=prob.jogo$paletta)+
      theme(legend.position="none",
            axis.line.y = element_line(
              colour = "white"),
            axis.line.x = element_line(
              colour = "white"),
            axis.ticks.y = element_line(color="white"),
            axis.ticks.x = element_line(color="white"),
            axis.text.y = element_text(colour = 'white'),
            axis.text.x = element_text(color = 'white'),
            axis.title.x=element_text(color = 'white'),
            text=element_text(family="Avenir Next Condensed"))+
      annotate("text", label = (prob.jogo$Equipe[4]), x = 1.75, y = prob.jogo$pos[4],
               size = 8, family="Avenir Next Condensed", 
               colour = prob.jogo$paletta[4])+
      annotate("text", label = (prob.jogo$Equipe[3]), x = 1.75, y = empate.pos,
               size = 8, family="Avenir Next Condensed", 
               colour = prob.jogo$paletta[3])+
      annotate("text", label = (prob.jogo$Equipe[2]), x = 1.75, y = prob.jogo$pos[2],
               size = 8, family="Avenir Next Condensed", 
               colour = prob.jogo$paletta[2])+
      annotate("text", label = 'Probabilidades', x = 2.4, y = 50,
               size = 8, family="Avenir Next Condensed", 
               colour = 'gray26')

    evolucao = evolucao.xG(historico.xG = historico.xG, historico.xGC = historico.xGC,
                                historico.xSG = historico.xSG, xG.avg = xG.avg,
                                xG.data = xG.data, casa = casa[i], fora = fora[i])
   
    eficiencia = correlacao.jogo(xG.campeonato = campeonato, rodadas = rodadas,
                                 casa = casa[i], fora = fora[i])
    
    title = paste(casa[i],' x ',fora[i], sep = '')
    subtitle = paste((rodadas+1),'ª rodada',sep='')
   draw.all =  ggdraw() +
      draw_plot(jogo, 0, .66, .95, .25) +
      draw_plot(chances, -0.05, 0.45, 1.15, .25) +
      draw_plot(evolucao$xG, 0, 0.31, 1, 0.2) +
      draw_plot(evolucao$xGC, 0, 0.16, 1, 0.2)+
      draw_plot(evolucao$xSG, 0, 0, 1, 0.2)+
      draw_text(c(title), x = 0.5, y = .96, size = 30,
                      family="Avenir Next Condensed", 
                      colour = 'gray26', fontface = 'plain')+
     draw_text(c(subtitle), x = .5, y = c(.925), size = 16,
                      family="Avenir Next Condensed", 
                      colour = 'gray26', fontface = 'plain')+
     draw_plot_label(c('@ProjecaoDeGol'), x = .78, y = c(1), size = 13,
                     family="Avenir Next Condensed", 
                     colour = 'gray26', fontface = 'italic')
   
  bottom = ggdraw()+
     draw_plot(eficiencia$ataque, 0, 0, .5, 1) +
     draw_plot(eficiencia$defesa, 0.5, 0, .5, 1)
   
   
   b = grid.arrange(draw.all,bottom, ncol = 1, heights = c(.9,0.3))
  
    
  
    ggplot.objects[[length(ggplot.objects)+1]] = b
  }
  
  newList = list('plots' = ggplot.objects,
                 'njogos' = length(casa),
                 'probs' = probs)
#               )

  return(newList)
  
}