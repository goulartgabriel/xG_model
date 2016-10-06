projecao.pontos <- function(rodadas,type, model, momentum, peso.m, peso.btm,
                            lm.Gol.Casa, lm.Gol.Fora,nsim,sd){
  source("br16.R")
  source("teamratingbtm.R")
  source("cores.R")
  source("setupxG.R")
  source("xGtimes.R")
  source("probabilidades.R")
  library(plyr)
  bra16 = br16(rodadas = rodadas)
  times = levels(bra16$classificacao$Times)
  rating = team.rating.btm(jogos = bra16$jogos, rodadas = rodadas, 
                               resultados.casa = bra16$resultados.casa,
                               momentum = momentum, peso = peso.m)
  
  probs = rating$probs.momento
  
  xG = setup.xG(rodadas)
  xG.chutes = xG$xG.chutes
  threshold = .35
    resultados.casa = bra16$resultados.casa
    resultados.casa[which(resultados.casa == 2)] = 3
    resultados.fora = t(resultados.casa)
    resultados.fora[which(resultados.fora == 3)] = 4
    resultados.fora[which(resultados.fora == 0)] = 3
    resultados.fora[which(resultados.fora == 4)] = 0
    
    resultados.casa.btm = resultados.casa
    resultados.fora.btm = resultados.fora
    
    campeonato = xGtimes(dados = xG.chutes, model = model, 
                         rodadas = rodadas, threshold = threshold,
                         type = type, momentum = momentum, peso = peso.m)
    xG.data = campeonato$xG.data
    br = br16(rodadas = rodadas, xG.data = xG.data)
    br = br$classificacao[order(br$classificacao$Times),]
    
    for(i in 1:20){
      for(j in 1:20){
        if(i!=j){
          if(is.na(resultados.casa[i,j])){
            
            #Poisson
            df.casa =  data.frame(xG.time.casa.m = br$xG[i],
                                  xGC.time.fora.m = br$xGC[j],
                                  xG.time.fora.m = br$xG[j],
                                  xGC.time.casa.m = br$xGC[i],
                                  Chutes.ratio.casa = br$Chutes.Ratio[i],
                                  Passes.ratio.casa = br$Passes.Ratio[i],
                                  Casa = times[i],
                                  Fora = times[j])
            poisson.casa = exp(predict(lm.Gol.Casa,df.casa))
            df.fora =  data.frame(xG.time.fora.m = br$xG[j],
                                  xGC.time.casa.m = br$xGC[i],
                                  xG.time.casa.m = br$xG[i],
                                  xGC.time.fora.m = br$xGC[j],
                                  Chutes.ratio.fora = br$Chutes.Ratio[j],
                                  Passes.ratio.fora = br$Passes.Ratio[j],
                                  Casa = times[i],
                                  Fora = times[j])
            poisson.fora = exp(predict(lm.Gol.Fora,df.fora))
            
            probs.poisson = probabilidades(poisson.casa,poisson.fora)
            probs.poisson = probs.poisson/100
            resultados.casa[i,j] = 3*probs.poisson$Casa + probs.poisson$Empate
            resultados.fora[j,i] = 3*probs.poisson$Fora + probs.poisson$Empate
            
            #BTM
            chances = probs[i,j,]
            resultados.casa.btm[i,j] = 3*chances[1] + chances[2]
            resultados.fora.btm[j,i] = 3*chances[3] + chances[2]
            
          }
        }
      }
    }
    
    resultados.casa[is.na(resultados.casa)] <- 0
    resultados.fora[is.na(resultados.fora)] <- 0
    resultados.casa.btm[is.na(resultados.casa.btm)] <- 0
    resultados.fora.btm[is.na(resultados.fora.btm)] <- 0
    pontuacao = rep(0,20)
    pontuacao.btm = rep(0,20)
    for (h in 1:20){
        pontuacao[h] = sum(resultados.casa[h,]) + sum(resultados.fora[h,])
        pontuacao.btm[h] = sum(resultados.casa.btm[h,]) + sum(resultados.fora.btm[h,])
    }
    campeonato = data.frame(Equipe = times,
                            Pontuacao = (1-peso.btm)*pontuacao+peso.btm*pontuacao.btm)
    campeonato.order = campeonato[order(-campeonato$Pontuacao),]
    campeonato$Pontuacao = round(campeonato$Pontuacao,2)
    campeonato.order$group[1] = 'C'
    campeonato.order$group[2:4] = 'G'
    campeonato.order$group[5:16] = 'N'
    campeonato.order$group[17:20] = 'Z'
    
    df.pontos = data.frame(Equipe = rep(NA,20*nsim),
                           Pontos = rep(NA,20*nsim),
                           Nsimulacao = rep(NA,20*nsim),
                           media = rep(NA,20*nsim),
                           group = rep(NA,20*nsim)
                             )
    pontos.total = matrix(0,20,nsim)
    matriz.posicoes = matrix(0,20,20)
    rownames(pontos.total) = times
    rownames(matriz.posicoes) = times
    for (i in 1:20){
      pontos.total[i,] = rnorm(nsim,mean = campeonato$Pontuacao[i],sd = sd)
    }
    df.list <- vector(mode = "list", length = 20*nsim)
    for(i in 1:nsim){
      #print('campeonato =')
      #print(i)
       classificacao = pontos.total[,i]
       df = data.frame(Equipe = times,
                              Pontos = classificacao,
                              Nsimulacao = i,
                              media = campeonato$Pontuacao,
                              group = campeonato.order$group[order(campeonato.order$Equipe)]
       )
       
      df.list[[i]] = df
      
      classificacao = order(classificacao,decreasing = T)
      classificacao =  match(seq(1:20),classificacao)
      for (h in 1:20){
        posicao = classificacao[h]
        matriz.posicoes[h,posicao] = matriz.posicoes[h,posicao]+1
      }
      
    }
    
    df.pontos = rbind.fill(df.list)
    
   #campeoes 
    campeoes = matriz.posicoes
    times.campeoes = rownames(campeoes)
    rownames(campeoes) = NULL
    campeoes = campeoes[,1]
    times.campeoes = times.campeoes[order(campeoes,decreasing = T)]
    campeoes = campeoes[order(campeoes,decreasing = T)]
    campeoes = campeoes/nsim
    
    campeoes = round(campeoes,digits = 3)
    df.campeoes = data.frame(Equipe = times.campeoes,
                             chance = campeoes*100,
                             rank = seq(1:length(campeoes)),
                             rodada = rodadas)
    
    df.campeoes = cores(dataframe = df.campeoes)
    
    
    #Z4
    z4 = matriz.posicoes
    times.z4 = rownames(z4)
    rownames(z4) = NULL
    z4 = z4[,17:20]
    z4 = rowSums(z4)
    times.z4 = times.z4[order(z4,decreasing = T)]
    z4 = z4[order(z4,decreasing = T)]
    z4 = z4/nsim
    z4 = round(z4,digits = 3)
    df.Z4 = data.frame(Equipe = times.z4,
                       chance = z4*100,
                       rank = seq(1:length(z4)),
                       rodada = rodadas)
    df.Z4 = cores(dataframe = df.Z4)
    newlist = list('df.pontos' = df.pontos,
                   'Posicoes' = matriz.posicoes,
                   'Pontos' = campeonato.order,
                   'matriz.pontos' = as.numeric(campeonato$Pontuacao),
                   'campeoes' = df.campeoes,
                   'z4' = df.Z4)
    
  return(newlist)
  
  
}

