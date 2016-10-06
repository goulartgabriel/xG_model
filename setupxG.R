setup.xG <- function(rodadas = NULL, imputar = FALSE){
  source("br16.R")
  library(gsheet)
  library(stringr)
  

  bra16 = br16(rodadas = rodadas)
  total.chutes = bra16$total.chutes
  print("Coletando dados do campeonato")
  xG <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1LobEkmNGqp_WLF_vRWqqDWTp5Fz730HwehVwTzYrbPw/edit#gid=0')
  xG[,1] = NULL
  xG$Rodada = as.numeric(as.character(xG$Rodada))
  xG = xG[which(xG$Rodada <= rodadas), ]
  
  for (i in 1:nrow(xG)){
    time = xG$Time[i]
    adv = xG$Adversario[i]
    #jogador.ass = dados$Jogador.ASS[i]
    if (word(time,-1) == ""){
      time = word(time,1,-2)
      if (word(time,-1) == ""){
        time = word(time,1,-2)
      }
      xG$Time[i] = time
    }
    if (word(adv,-1) == ""){
      adv = word(adv,1,-2)
      if (word(adv,-1) == ""){
        adv = word(adv,1,-2)
      }
      xG$Adversario[i] = adv
    }
    
  }
  
  xG$Perigo.de.gol = as.numeric((xG$Chance.boa)|(xG$Chute.Perigoso))
  xG.names = colnames(xG)
  if (isTRUE(imputar)){
    missing.chutes = total.chutes - nrow(xG)
    print("Coletando dados das finalizacoes")
    
    tipo.jogada = count(xG,c('Tipo.de.Jogada','Gol'))
    tipo.jogada = tipo.jogada[tipo.jogada$Gol == 0,]
    tipo.jogada = tipo.jogada[complete.cases(tipo.jogada),]
    tipo.jogada = tipo.jogada[1:6,]
    tipo.jogada$percent = round(tipo.jogada$freq/sum(tipo.jogada$freq)*missing.chutes)
    tipo.chute = count(xG,c('Tipo.de.chute','Gol'))
    tipo.chute = tipo.chute[tipo.chute$Gol == 0,]
    tipo.chute = tipo.chute[complete.cases(tipo.chute),]
    tipo.chute$percent = round(tipo.chute$freq/nrow(xG)*missing.chutes)
    
    cruzamento = count(xG,c('Cruzamento.Cruz..rasteiro','Gol'))
    cruzamento = cruzamento[cruzamento$Gol == 0,]
    cruzamento = cruzamento[complete.cases(cruzamento),]
    cruzamento$percent = round(cruzamento$freq/nrow(xG)*missing.chutes)
    
    regiao = count(xG,c('Regiao','Gol'))
    regiao = regiao[regiao$Gol == 0,]
    regiao = regiao[complete.cases(regiao),]
    regiao$percent = round(regiao$freq/nrow(xG)*missing.chutes)
    
    m <- matrix(0, ncol = ncol(xG), nrow = missing.chutes)
    m <- data.frame(m)
    colnames(m) = xG.names
    print("Imputando dados")
    #Inputation
    m$Tipo.de.Jogada = NA
    for (i in 1:6){
      nas.index = which(is.na(m$Tipo.de.Jogada))
      new.index = sample(nas.index, tipo.jogada$percent[i], replace=FALSE)
      m$Tipo.de.Jogada[new.index] = tipo.jogada$Tipo.de.Jogada[i]
    }
    m$Regiao = NA
    for (i in 1:29){
      nas.index = which(is.na(m$Regiao))
      new.index = sample(nas.index, regiao$percent[i], replace=FALSE)
      m$Regiao[new.index] = regiao$Regiao[i]
    }
    
    index.chute.C = sample(missing.chutes,tipo.chute$percent[1],replace=F) 
    m$Tipo.de.chute[index.chute.C] = 'C'
    m$Tipo.de.chute[-index.chute.C] = 'P'
    
    index.cruzamento = sample(missing.chutes,cruzamento$percent[2],replace=F) 
    m$Cruzamento.Cruz..rasteiro[index.cruzamento] = 1
    
    xG.new = rbind(xG,m)
  } else{
    xG.new = xG
  }
    
    xG = xG[which(xG$Rodada <= rodadas), ]
    xG.new = xG.new[which(xG.new$Rodada <= rodadas), ]
    xG.names = colnames(xG)
    
    for (i in 2:length(xG.names)){
      if (i==10){
        next
        cat(i)
      }
      if (i==3){
        next
        cat(i)
      }
      name <- xG.names[i]
      xG[[name]] <- as.factor(xG[[name]])
      xG.new[[name]] <- as.factor(xG.new[[name]])
    }
  
  
  new.list = list('xG' = xG.new,
                  'xG.chutes' = xG)
  
  return(new.list)
}