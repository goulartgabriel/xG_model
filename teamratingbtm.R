team.rating.btm <- function(jogos, rodadas, resultados.casa, momentum = NULL, peso){
##Setup ------
  library(sirt)
  library("ggplot2")
  
  new.jogos = jogos[jogos$Rodada<=rodadas,]
  

  resultados.btm = data.frame(Casa = jogos$Casa[1:nrow(new.jogos)],
                              Fora = jogos$Fora[1:nrow(new.jogos)] )
  
  
  if (is.null(momentum)){
    momentum = rodadas
  }
  
  index.momentum = which(jogos$Rodada >= (rodadas-momentum))
  
## Resultados ------  
  #njogos = length(1:(10*rodadas))
  resultados.btm$result = rep(0,nrow(new.jogos))
  
  for (i in 1:nrow(new.jogos)){
    #print(i)
    if (new.jogos$Gol.C[i] > new.jogos$Gol.F[i]){
      resultados.btm$result[i] = 1
    }
    if (new.jogos$Gol.C[i] == new.jogos$Gol.F[i]){
      resultados.btm$result[i] = 0.5
    }
    if (new.jogos$Gol.C[i] < new.jogos$Gol.F[i]){
      resultados.btm$result[i] = 0
    }
  }
  
  model.btm = btm(resultados.btm)
  ranking = model.btm$effects
  pars = model.btm$pars
  ranking$rank = as.numeric(rownames(ranking))

  resultados.btm.momentum = resultados.btm[index.momentum,]
  model.btm.momentum = btm(resultados.btm.momentum)
  ranking.momentum = model.btm.momentum$effects
  pars.momentum = model.btm.momentum$pars
  ranking.momentum$rank = as.numeric(rownames(ranking.momentum))
  

  ranking.momentum = ranking.momentum[order(ranking.momentum$individual),]
  ranking3 = ranking[order(ranking$individual),]
  
  new.rank = ranking.momentum
  
  new.rank$theta = (1-peso)*(ranking3$theta) + peso*(ranking.momentum$theta)
  
  new.rank = new.rank[order(new.rank$theta, decreasing = TRUE),]
  new.rank
  new.rank$rank = seq(1:20)
  
  
##Plot ------  
  rank.plot = ggplot(ranking, aes(x=reorder(individual,theta),y=theta, 
                                  fill = rank ))+
    geom_bar(stat = "identity")+
    coord_flip()+
    scale_fill_gradient(low="gray16",high="steelblue1")+
    ylab("Coeficiente de Forca")+xlab("")+
    ggtitle("Ranking de Forca dos Times")+
    theme(legend.position="none")
  
  rank.plot.mom = ggplot(new.rank, aes(x=reorder(individual,theta),y=theta, 
                                  fill = rank ))+
    geom_bar(stat = "identity")+
    coord_flip()+
    scale_fill_gradient(low="gray16",high="steelblue1")+
    ylab("Coeficiente de Forca")+xlab("")+
    ggtitle("Ranking de Forca dos Times")+
    theme(legend.position="none")
  
  
  
  
#   a = ranking[which(model.btm$effects$individual == "Gremio"),]
#   
#   pgremio = exp(eta + gre$theta)
#   pcru = exp(cru$theta)
#   pemp = exp(delta + (eta + gre$theta + cru$theta)/2)
#   pall = b$probs[90,]
#   
#   pgremio/(pgremio+pcru+pemp)
  
  
  eta = pars$est[2]
  delta = pars$est[1]
  
  probs = array(0,dim=c(20,20,3)) ##1 TIME CASA i GANHAR
                                  ##2 TIME FORA j GANHAR
                                  ##3 EMPATE
  probs2 = array(0,dim=c(20,20,3))
  
  times = levels(ranking$individual)
  ranking2 = ranking[match(times,ranking$individual),]
  
  new.rank2 = new.rank[order(new.rank$individual),]
  
    for(i in 1:20){
      for(j in 1:20){
        prob = rep(0,3)
        prob2 = rep(0,3)
        
        theta.casa = ranking2[i,]$theta
        theta.fora = ranking2[j,]$theta
        theta.casa2 = new.rank2[i,]$theta
        theta.fora2 = new.rank2[j,]$theta
        
        prob[1] = exp(eta + theta.casa)  ##i ganhar
        prob[2] = exp(delta + (eta+theta.casa+theta.fora)/2)  ##empate
        prob[3] = exp(theta.fora)        ##j ganhar
        prob = prob/sum(prob)
        probs[i,j,] = prob
        
        prob2[1] = exp(eta + theta.casa2)  ##i ganhar
        prob2[2] = exp(delta + (eta+theta.casa2+theta.fora2)/2)  ##empate
        prob2[3] = exp(theta.fora2)        ##j ganhar
        prob2 = prob2/sum(prob2)
        probs2[i,j,] = prob2
        
        
      }
    }

  
  newList <- list("Ranking.Plot" = rank.plot,
                  "Ranking" = ranking,
                  "Ranking.Plot.Momento" = rank.plot.mom,
                  "Ranking.Momento" = new.rank,
                  "Resultados.Casa" = resultados.casa,
                  "Pars" = pars,
                  "probs" = probs,
                  "probs.momento" = probs2,
                  "Rodadas" = rodadas
                 )
  
  
  
  
  
}