expected <- function(equipe = NULL,adv = NULL, jogador = NULL, passador = NULL, chance = NULL, casa = NULL, data, modelo, njogos, thresh, type = NULL){
  library(pROC)
  
  if(!is.null(equipe)){
    new = data[data$Time %in% equipe,]
    if(!is.null(jogador)){
      new = new[new$Jogador %in% jogador,]
    }
    if(!is.null(adv)){
      new = new[new$Adversario %in% adv,]
    }
    if(!is.null(passador)){
      new = new[new$Jogador.ASS %in% passador,]
    }
    if(!is.null(chance)){
      new = new[new$Chance.boa %in% chance,]
    }
    if(!is.null(casa)){
      new = new[new$Em.Casa %in% casa,]
    }
  }
  
  if(is.null(equipe)){
    if(!is.null(jogador)){
      new = data[data$Jogador %in% jogador,]
      if(!is.null(adv)){
        new = new[new$Adversario %in% adv,]
      }
      if(!is.null(passador)){
        new = new[new$Jogador.ASS %in% passador,]
      }
      if(!is.null(chance)){
        new = new[new$Chance.boa %in% chance,]
      }
      if(!is.null(casa)){
        new = new[new$Em.Casa %in% casa,]
      }
    }
  }
  
  if(is.null(equipe)){
    if(is.null(jogador)){
      if(is.null(passador)){
        if(is.null(chance)){
            if(!is.null(adv)){
            new = data[data$Adversario %in% adv,]
            }
          if(!is.null(casa)){
            new = new[new$Em.Casa %in% casa,]
          }
          }
        }
      }
    }
  
  
  if(is.null(equipe)){
    if(is.null(jogador)){
      if(is.null(chance)){
        if(!is.null(passador)){
         new = data[data$Jogador.ASS %in% passador,]
         if(!is.null(adv)){
            new = new[new$Adversario %in% adv,]
         }
         if(!is.null(casa)){
           new = new[new$Em.Casa %in% casa,]
         }
        }
      }
    }
  }
  
  
  if(is.null(equipe)){
    if(is.null(jogador)){
      if(is.null(adv)){
        if(is.null(chance)){
          if(is.null(passador)){
            if(is.null(casa)){
              new = data
              njogos = njogos*10
            }
          }
        }
      }
    }
  }
  if (nrow(new)==0){
    prediction = 0
  }
  else {
    if (type == 'rpart'){
      prediction = predict(modelo,new)
      prediction = prediction[,2]
    }  
    if (type == 'bayes.glm') {
      prediction = predict(modelo,new,type="response")
      
    }
    if (type == 'naive') {
      prediction = predict(modelo,new,type="raw")
      prediction = prediction[,2]
    }
    if (type == 'gbm') {
      prediction = predict(modelo, new, n.trees = 5000)
      prediction<-plogis(2*prediction)
    }
    if( type == 'neuralnetwork'){
      m <- model.matrix( 
        ~ Gol + Cruzamento.Cruz..rasteiro + Passe.Profundo + + Regiao + Regiao.ASS + 
        Contra.ataque + Erro.da.zaga + Em.Casa + Regiao + Tipo.de.Jogada, 
        data = new
      )
      colnames(m)[38] <- "Tipo.de.JogadaFaltaDireta"
      colnames(m)[39] <- "Tipo.de.JogadaFaltaIndireta"
      
      pr.nn <- compute(modelo,m[,3:ncol(m)])
      prediction = pr.nn$net.result
    }
  }
if (prediction == 0){
  df = 0
  soma = 0
  ratioF = 0
  ratioJ = 0
  tabela = 0
  acc = 0
} else{ 
  prob = as.numeric(prediction >= thresh)  
  tabela = table(prob, new$Gol)
  soma = sum(prediction)
  ratioF = soma/length(prediction)
  ratioJ = soma/njogos
  options(scipen = 999)
  df <- data.frame(
    Predicoes = prediction,
    Gol = new$Gol,
    Modelo = prob
  )
  
  #acc = auc(new$Gol,prediction)
  acc = 0
}
  
  newList <- list('Finalizacoes' = new, 'Comparacao'= df , "xG" = soma, 
                  'xGpF' = ratioF, 'xGpJ' = ratioJ, "tabela" = tabela,
                  'acuracia' = acc)

  return(newList)
}