projecao.gol <- function(rodadas, type = 'bayes.glm', peso, casa = TRUE){
  source("br16.R")
  source("setupxG.R")
  source("xGtimes.R")
  bra16 = br16(rodadas = rodadas)
  times = levels(bra16$classificacao$Times)
  
  jogos = bra16$jogos
  
  #new.jogos = jogos[jogos$Rodada<=rodadas,]
  jogos = jogos[jogos$Rodada >=10,]
  jogos$xG.jogo.casa = 0
  jogos$xG.jogo.fora = 0

  
  xG = setup.xG(rodadas)
  xG.chutes = xG$xG.chutes
  xG = xG$xG
  times = levels(xG.chutes$Time)
  threshold = 0.35
  if (type == 'rpart'){
    rpart.xG=rpart(Gol~.,data=xG[ , (names(xG) %in% c('Gol', 'Regiao',
                                                      'Cruzamento.Cruz..rasteiro',
                                                      'Passe.Profundo','Contra.ataque',
                                                      'Erro.da.zaga','Em.Casa',
                                                      'Perigo.de.gol','Regiao','Saldo','Tipo.de.Jogada'))])
    model = rpart.xG
  }
  
  
  if (type == 'bayes.glm'){
    bayes.xG=bayesglm(Gol~ .,
                      data=xG.chutes[ , (names(xG.chutes) %in% c('Gol',
                                                                 'Cruzamento.Cruz..rasteiro',
                                                                 'Passe.Profundo','Contra.ataque',
                                                                 'Erro.da.zaga','Em.Casa','Tipo.de.chute',
                                                                 'Regiao.ASS','Regiao','Tipo.de.Jogada'
                      ))],
                      family=binomial, drop.unused.levels = F )
    model = bayes.xG
    
  }
  
  if( type == 'neuralnetwork'){
    m <- model.matrix( 
      ~ Gol + Cruzamento.Cruz..rasteiro + Passe.Profundo + Regiao + Regiao.ASS +
        Contra.ataque + Erro.da.zaga + Em.Casa + Regiao + Tipo.de.Jogada, 
      data = xG.chutes
    )
    colnames(m)[66] <- "Tipo.de.JogadaFaltaDireta"
    colnames(m)[67] <- "Tipo.de.JogadaFaltaIndireta"
    
    n <- colnames(m)
    n = n[2:length(n)]
    
    f <- as.formula(paste("Gol1 ~", paste(n[!n %in% "Gol1"], collapse = " + ")))
    nn <- neuralnet(f,data=m,hidden=20,linear.output=F,
                    stepmax=1e6)
    
    model = nn
  }
  
  #matrix.xG = matrix(nrow = 20, ncol = 15)
  k = 0
  for (i in 9:(rodadas-1)){
    cat(sprintf("Analisando rodada: %s\n", i))
    campeonato.m = xGtimes(dados = xG.chutes, model = model, rodadas = i, 
                        momentum = 5, peso = (k*0.03),threshold = threshold, type = type)
                     #   momentum = 5, peso = .65,threshold = threshold, type = type)
                   #   momentum = 5, peso = (1-5/i),threshold = threshold, type = type)
    xG.data.m = campeonato.m$xG.data
    k = k+1
    
    index.rodada = which(jogos$Rodada == (i+1))
    
    data.rodada = xG.chutes[xG.chutes$Rodada == (i+1),]
   
    for(j in index.rodada){
      index.casa = which(times == jogos$Casa[j])
      index.fora = which(times == jogos$Fora[j])
      
      data.casa = data.rodada[data.rodada$Time == times[index.casa],]
      data.fora = data.rodada[data.rodada$Time == times[index.fora],]
      prediction.casa = predict(model,data.casa,type="response")
      
      if (nrow(data.fora)==0){
        prediction.fora = 0
      } else{
        prediction.fora = predict(model,data.fora,type="response")
      }
      jogos$xG.jogo.casa[j] = round(sum(prediction.casa),2)
      jogos$xG.jogo.fora[j] = round(sum(prediction.fora),2)
      
      jogos$xG.time.casa.m[j] = xG.data.m$xG[index.casa]
      jogos$xG.time.fora.m[j] = xG.data.m$xG[index.fora]
      jogos$xGC.time.casa.m[j] = xG.data.m$xGC[index.casa]
      jogos$xGC.time.fora.m[j] = xG.data.m$xGC[index.fora]
     
      
#       jogos$xG.time.casa[j] = xG.data$xG[index.casa]
#       jogos$xG.time.fora[j] = xG.data$xG[index.fora]
#       jogos$xGC.time.casa[j] = xG.data$xGC[index.casa]
#       jogos$xGC.time.fora[j] = xG.data$xGC[index.fora]
      
      
      }
  }
  jogos.casa = data.frame(Rodada = jogos$Rodada,
                          Equipe = times[jogos$Casa],
                          Gol = jogos$Gol.C,
                          xG.jogo = jogos$xG.jogo.casa,
                          xG.pre = jogos$xG.time.casa.m,
                          xG.adv.pre = jogos$xG.time.fora.m,
                          xGC.pre = jogos$xGC.time.casa.m, 
                          xGC.adv.pre = jogos$xGC.time.fora.m
  )
  
  jogos.fora = data.frame(Rodada = jogos$Rodada,
                          Equipe = times[jogos$Fora],
                          Gol = jogos$Gol.F,
                          xG.jogo = jogos$xG.jogo.fora,
                          xG.pre = jogos$xG.time.fora.m,
                          xG.adv.pre = jogos$xG.time.casa.m,
                          xGC.pre = jogos$xGC.time.fora.m, 
                          xGC.adv.pre = jogos$xGC.time.casa.m
  )
  
  
  df.sem.mando = data.frame(Rodada = c(jogos$Rodada,jogos$Rodada),
                            Equipe = times[c(jogos$Casa,jogos$Fora)],
                            Gol = c(jogos$Gol.C,jogos$Gol.F),
                            xG.jogo = c(jogos$xG.jogo.casa,jogos$xG.jogo.fora),
                            xG.pre = c(jogos$xG.time.casa.m,jogos$xG.time.fora.m),
                            xG.adv.pre = c(jogos$xG.time.fora.m,jogos$xG.time.casa.m),
                            xGC.pre = c(jogos$xGC.time.casa.m, jogos$xGC.time.fora.m),
                            xGC.adv.pre = c(jogos$xGC.time.fora.m,jogos$xGC.time.casa.m),
                            Mando = c(rep('Casa',length(jogos$Rodada)),rep('Fora',length(jogos$Rodada)))
                            
  )

  
  if(casa == TRUE){
    lm.Gol.Casa = glm(Gol~.,data=jogos.casa[ , (names(jogos.casa) %in%c('Gol','xG.pre',
                                                                          'xGC.pre','xG.adv.pre',
                                                                          'xGC.adv.pre'
    ))],
    family = quasipoisson )
    
    lm.Gol.Fora = glm(Gol~.,data=jogos.fora[ , (names(jogos.fora)%in%c('Gol','xG.pre',
                                                                       'xGC.pre','xG.adv.pre',
                                                                       'xGC.adv.pre'
    ))],
    family = quasipoisson)
  } else{
    lm.Gol.Casa = glm(Gol~.,data=df.sem.mando[ , (names(df.sem.mando) %in% c('Gol','xG.pre',
                                                                 'xGC.pre','xG.adv.pre',
                                                                 'xGC.adv.pre'
    ))],
    family = quasipoisson )
    
    lm.Gol.Fora = lm.Gol.Casa
  }
  jogos$PxG.casa = 0
  jogos$PxG.fora = 0
  
  for(i in 9:rodadas){
    index.rodada = which(jogos$Rodada == i)
    for(j in index.rodada){
      df.casa =  data.frame(xG.pre = jogos$xG.time.casa.m[j],
                            xGC.adv.pre = jogos$xGC.time.fora.m[j],
                            xG.adv.pre = jogos$xG.time.fora.m[j],
                            xGC.pre = jogos$xGC.time.casa.m[j],
                            Casa = jogos$Casa[j],
                            Fora = jogos$Fora[j],
                            Mando = 'Casa')
                            
                            
                          
      p.casa = exp(predict(lm.Gol.Casa,df.casa))
      df.fora =  data.frame(xG.adv.pre = jogos$xG.time.casa.m[j],
                            xGC.pre = jogos$xGC.time.fora.m[j],
                            xG.pre = jogos$xG.time.fora.m[j],
                            xGC.adv.pre = jogos$xGC.time.casa.m[j],
                            Casa = jogos$Casa[j],
                            Fora = jogos$Fora[j],
                            Mando = 'Fora')
      p.fora = exp(predict(lm.Gol.Fora,df.fora))
      p.casa = round(p.casa,2)
      p.fora = round(p.fora,2) 
      jogos$PxG.casa[j] = p.casa
      jogos$PxG.fora[j] = p.fora
    }
    
    
  }
  
  
  newlist = list('Casa' = lm.Gol.Casa,
              'Fora' = lm.Gol.Fora,
              'model' = model,
              'jogos' = jogos)
  
  return(newlist)
}