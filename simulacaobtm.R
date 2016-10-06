simulacao.btm <- function(rating,nsimulacoes,regressao = NULL, momentum){
  library(plyr)
  library("ggplot2")
  library(formattable)
##Setup -------  
  resultados.casa = rating$Resultados.Casa
  times = levels(rating$Ranking$individual)
  ranking = rating$Ranking
  ranking = ranking[match(times,ranking$individual),]
  if (isTRUE(momentum)){
    probs = rating$probs.momento
  } else{
    probs = rating$probs
  }
  
  matriz.jogos = array(0,dim=c(20,20,nsimulacoes))
  matriz.pontos = rep(0,20)
  pontos.total = data.frame(Equipe = character(),
                         Pontos = numeric(),
                         Nsimulacao = numeric())
  matriz.posicoes = matrix(0,20,20)
  rownames(matriz.posicoes) = times
  
  for (i in 1:nsimulacoes){
    matriz.jogos[,,i] = resultados.casa
  }
  
  if(is.null(regressao)){
    regressao = FALSE
  }
  df.pontos = data.frame(Equipe = rep(NA,20*nsimulacoes),
                         Pontos = rep(NA,20*nsimulacoes),
                         Nsimulacao = rep(NA,20*nsimulacoes)
                         
  )
  df.list <- vector(mode = "list", length = 20*nsimulacoes)
##Simulacao -----  
  for(k in 1:nsimulacoes){
    print(paste0("Simulando campeonato: ", k))
    for(i in 1:20){
      for(j in 1:20){
        if(i!=j){
          if(is.na(matriz.jogos[i,j,k])){
            if(regressao == FALSE){
              chances = probs[i,j,]
            }
            if(regressao == TRUE){
#              media = rep(0,3)
#               media[1] = sum(probs[i,,1])/20
#               media[2] = ((sum(probs[i,,2])/20) + (sum(probs[,j,2])/20))/2
#               media[3] = sum(probs[,j,3])/20
#               media = (media+probs[i,j,])/2
              media = probs[i,j,]
              
              if(media[1] > 0.7){
                media[2] = media[2] + media[1]*0.1
                media[3] = media[3] + media[1]*0.1
                media[1] = 0.8*media[1]
              }
              if(media[3] > 0.6){
                media[1] = media[1] + media[3]*0.1
                media[2] = media[2] + media[3]*0.1
                media[3] = 0.8*media[3]
              }
              chances = media
            }
            chances[2] = chances[1]+chances[2]
            chances[3] = chances[2]+chances[3]
            monte.carlo = runif(1)
            
            if (monte.carlo < chances[1]){
              matriz.jogos[i,j,k] = 2 }
            if ((monte.carlo > chances[1]) & (monte.carlo <= chances[2]) ){
              matriz.jogos[i,j,k] = 1
            }
            if (monte.carlo > chances[2]){
              matriz.jogos[i,j,k] = 0
            }
          }
        }
      }
    }
    pontuacao = rep(0,20)
    for (h in 1:20){
      em.casa = count(matriz.jogos[h,,k])
      if (1 %in% em.casa$x){
        pontuacao[h] = pontuacao[h] + em.casa[which(em.casa$x == 1),2] }
      if (2 %in% em.casa$x){
        pontuacao[h] = pontuacao[h] + 3*em.casa[which(em.casa$x == 2),2]
      }
      if (9 %in% em.casa$x){
        pontuacao[h] = pontuacao[h] 
      }
      fora = count(matriz.jogos[,h,k])
      if (1 %in% fora$x){
        pontuacao[h] = pontuacao[h] + fora[which(fora$x == 1),2] }
      if (0 %in% fora$x){
        pontuacao[h] = pontuacao[h] + 3*fora[which(fora$x == 0),2]
      }
      if (9 %in% fora$x){
        pontuacao[h] = pontuacao[h] 
      }
      matriz.pontos[h] = matriz.pontos[h] + pontuacao[h] 
      df = data.frame(Equipe = times,
                      Pontos =  pontuacao,
                      Nsimulacao = k      )
      
      df.list[[k]] = df
    }
    
    campeonato = data.frame(Equipe = times,
                            Pontuacao = pontuacao)
    campeonato = campeonato[order(-campeonato$Pontuacao),]
    #print(paste0("Campeao: ", campeonato$Equipe[1]))

    for (h in 1:20){
      equipe = times[h]
      posicao = which(campeonato$Equipe == equipe)
      matriz.posicoes[h,posicao] = matriz.posicoes[h,posicao]+1
    }
    freq.campeao = matriz.posicoes[order(matriz.posicoes[,1], decreasing = TRUE),]
    freq.campeao = freq.campeao[,1]
    freq.campeao = freq.campeao[!freq.campeao %in% c(0)]
    freq.campeao = cbind(c(freq.campeao))
    colnames(freq.campeao) = "Campeao"
    print(freq.campeao)
  }
  pontos.total = rbind.fill(df.list)
  
##Sort ------
  ind = rep(0,20)
  porcent = rep(0,20)
  sim = matriz.posicoes/nsimulacoes
  matriz.pontos = matriz.pontos/nsimulacoes
  
  df.pontos = data.frame(Equipes = times,
                         Pts = matriz.pontos)
  df.pontos = df.pontos[order(df.pontos$Pts,decreasing=T),]  
  ind = match(df.pontos$Equipes,rownames(sim))
  sim = sim[rev(ind),]
  
  df = vector("list", 20)
  for (i in 1:20){
    df[[i]] = data.frame(Equipes = rownames(sim),
                         Posicao = i,
                         Porcentagem = sim[,i])
  }
  
  new.df = rbind(df[[1]],df[[2]])
  for (i in 3:20){
    new.df = rbind(new.df,df[[i]])
  }
  new.df$Equipe2 <- factor(new.df$Equipes, levels = unique(as.character(new.df$Equipes)))
  subt = paste("Numero de campeonatos simulados:",nsimulacoes,sep=" ")
  titt = paste("Mapa de Calor da Tabela de Classificacao apos", 
               rating$Rodadas,"rodadas",sep=" ")
  ##Plot ----  
  heat = ggplot(new.df, aes(Equipe2, Posicao))+
    geom_tile(aes(fill = Porcentagem), colour = "white")+
    coord_flip()+
    scale_fill_gradient(low = "white",high = "steelblue")+
    ylab(" ")+
    xlab("")+
    scale_y_continuous(trans = "identity", breaks = unique(new.df$Posicao))+
    ggtitle(bquote(atop(.(titt),atop(italic(.(subt))))))
  
  
#pontos
  pts = df.pontos
  rownames(pts) = NULL
  colnames(pts) = c("Equipes","Pontuação Média")
  pts$`Pontuação Média` = round(pts$`Pontuação Média`, digits = 2)
  pts = formattable(pts, list('Pontuação Média' = color_tile("white", "lightskyblue")))
  
  
#campeoes  
  pos = matriz.posicoes
  times = rownames(pos)
  rownames(pos) = NULL
  pos = pos[,1]
  times = times[which(pos >0)]
  pos = pos[which(pos >0)]
  times = times[order(pos,decreasing = T)]
  pos = pos[order(pos,decreasing = T)]
  pos = pos/nsimulacoes
  
  df.campeoes = data.frame(Equipe = times,
                           chance = pos,
                           rank = seq(1:length(pos)))
  
  campeoes = ggplot(df.campeoes, aes(x=reorder(Equipe,chance),y=chance, 
                          fill = rank ))+
    geom_bar(stat="identity")+
    coord_flip()+
    ylab("Vezes campeao nas simulacoes em %")+xlab("")+
    ggtitle("Campeoes na Simulacao")+
    theme(legend.position="none")
  
  
  #BOX PLOT
  df.pontos$group[1] = 'C'
  df.pontos$group[2:4] = 'G'
  df.pontos$group[5:16] = 'N'
  df.pontos$group[17:20] = 'Z'
  ordered = df.pontos[order(df.pontos$Equipes),]
  
  pontos.total$media = rep(ordered$Pts,nsimulacoes)
  pontos.total$group = rep(ordered$group,nsimulacoes)
  
  paleta = c("slategrey","dodgerblue2","snow2","lightcoral")
  box.pts <- ggplot(pontos.total, aes(x = reorder(Equipe,media), y = Pontos, fill = group)) + 
    geom_boxplot(alpha=0.7)+
    coord_flip()+
    theme_minimal()+
    scale_fill_manual(values = paleta)+
    theme(legend.position="none")+
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "Pontos",
                       breaks = seq(15, 90, 5),
                       limits=c(15, 90))
                        
  
  
  
  newList = list("Mapa" = heat,
                 "Pontos" = df.pontos,
                 "df" = new.df,
                 'Posicoes' = matriz.posicoes,
                 'df.pontos' = pontos.total,
                 'Pontos.Plot' = pts,
                 'Campeoes' = campeoes,
                 'df.campeoes' = df.campeoes,
                 'Box' = box.pts,
                 'matriz.pontos' = matriz.pontos)                   
  return(newList) 
  
}