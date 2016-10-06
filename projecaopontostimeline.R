projecao.pontos.timeline <- function(rodadas,type, momentum, models, peso.btm, nsim){
  source("cores.R")
  source("projecaopontos.R")
  source("br16.R")
  
  bra16 = br16(rodadas = rodadas)
  times = levels(bra16$classificacao$Times)
  campeonato = 0
  list.campeoes <- vector(mode = "list", length = (rodadas-9+1))
  list.z4 <- vector(mode = "list", length = (rodadas-9+1))
  k = 0
  for(i in 9:rodadas){
    cat(sprintf("Analisando rodada: %s\n", i))
    projecao = projecao.pontos(rodadas = i, type = 'bayes.glm', momentum = 6,
                               peso.m = (.1+0.01*i), peso.btm = peso.btm, lm.Gol.Casa = models$Casa,
                               lm.Gol.Fora = models$Fora, model = models$model, nsim = nsim, 
                               sd = (5.3-k))
    k = k+0.05
     #  projecao$matriz.pontos$rodada = i
   # campeonato = rbind(campeonato,projecao$matriz.pontos)
    list.campeoes[[i-8]] = projecao$campeoes
    list.z4[[i-8]] = projecao$z4
  }
  df.campeoes = rbind.fill(list.campeoes)
  df.z4 = rbind.fill(list.z4)
  
#   campeonato = campeonato[2:nrow(campeonato),]
#   campeonato2 = cores(campeonato)
  #Campeao
  provaveis = c('Palmeiras','Santos','Corinthians','Flamengo','Atlético MG')
  df.campeoes2 = df.campeoes
  df.campeoes2 = df.campeoes2[which(df.campeoes2$Equipe %in% provaveis),]
  colours = df.campeoes2[1:length(provaveis),]
  colours = colours[order(colours$Equipe),]
  colours = colours$paletta
   campeoes = ggplot(df.campeoes2, aes(x=rodada, y=chance, group=Equipe, 
                                        colour = Equipe))+ 
     geom_line(size = .7)+
     geom_point()+
     scale_colour_manual(values = colours)+
     xlab('Rodadas')+
     ylab('Probabilidades')+
     labs(title = "Probabilidade de campeão")+
     theme( 
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
                                           linetype = 'dashed',size = .045))
    
    #Z4
   provaveis = c('América','Santa Cruz','Figueirense','Internacional','Cruzeiro',
                 'Sport','Coritiba','Vitória','São Paulo','Botafogo')
   df.z42 = df.z4
   df.z42 = df.z42[which(df.z42$Equipe %in% provaveis),]
   colours = df.z42[1:length(provaveis),]
   colours = colours[order(colours$Equipe),]
   colours = colours$paletta
   campeoes = ggplot(df.z42, aes(x=rodada, y=chance, group=Equipe, 
                                       colour = Equipe))+ 
     geom_line(size = .7)+
     geom_point()+
     scale_colour_manual(values = colours)+
     xlab('Rodadas')+
     ylab('Probabilidades')+
     labs(title = "Probabilidade de campeão")+
     theme( 
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
                                       linetype = 'dashed',size = .045))
   
  
  }