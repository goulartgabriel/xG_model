probabilidades <- function(l1, l2){

  quantiles = seq(0,8)
  d1 = dpois(quantiles,l1)
  d2 = dpois(quantiles,l2)
  
  
  if(l1 > l2 & l2 > 1 & l1-l2 > 0.07){
    vitoria1 = d1[2]*d2[1]
    vitoria2 = 0
    diferenca1 = d2[2]*d1[1]
    empate = d1[1]*d2[1]+d1[2]*d2[2]
    for (i in 3:8){
      vitoria1 = vitoria1 + sum(d1[i]*d2[1:(i-1)])
      vitoria2 = vitoria2 + sum(d2[i]*d1[1:(i-2)])
      diferenca1 = diferenca1 + d2[i]*d1[i-1]
      empate = empate + d1[i]*d2[i]
    }
    vitoria2 = vitoria2+diferenca1*(empate/(vitoria2+empate))
    empate = empate+diferenca1*(vitoria2/(vitoria2+empate))
  } else if( l1 < l2 & l1 > 1 & l2-l1 > 0.07){
    vitoria2 = d1[1]*d2[2]
    vitoria1 = 0
    diferenca1 = d2[1]*d1[2]
    empate = d1[1]*d2[1]+d1[2]*d2[2]
    for (i in 3:8){
      vitoria1 = vitoria1 + sum(d1[i]*d2[1:(i-2)])
      vitoria2 = vitoria2 + sum(d2[i]*d1[1:(i-1)])
      diferenca1 = diferenca1 + d1[i]*d2[i-1]
      empate = empate + d1[i]*d2[i]
    }
    vitoria1 = vitoria1+diferenca1*(empate/(vitoria1+empate))
    empate = empate+diferenca1*(vitoria1/(vitoria1+empate))
    
  } else if(abs(l1-l2)<0.07){
    vitoria1 = 0
    vitoria2 = 0
    empate = d1[1]*d2[1]
    for (i in 2:8){
      vitoria1 = vitoria1 + sum(d1[i]*d2[1:(i-1)])
      vitoria2 = vitoria2 + sum(d2[i]*d1[1:(i-1)])
      empate = empate + d1[i]*d2[i]
      
    }
    empate = empate+0.1*vitoria1+0.1*vitoria2
    vitoria1 = 0.9*vitoria1
    vitoria2 = 0.9*vitoria2
    
  } else{ #original
    vitoria1 = 0
    vitoria2 = 0
    empate = d1[1]*d2[1]
    for (i in 2:8){
      vitoria1 = vitoria1 + sum(d1[i]*d2[1:(i-1)])
      vitoria2 = vitoria2 + sum(d2[i]*d1[1:(i-1)])
     empate = empate + d1[i]*d2[i]
    }
  }
  
  

  
  
  Casa = round(vitoria1*100)
  Empate = round(empate*100)
  Fora = round(vitoria2*100)
  

  
  df = data.frame(Casa = Casa,
                  Empate = Empate,
                  Fora = Fora)
  df
  
  if(Casa+Empate+Fora > 100){
    if(Empate > Fora){
      Fora = Fora-1
      
    }
    if(Fora > Empate){
      
      Empate = Empate-1
    }
  }
  
  if(Casa+Empate+Fora < 100){
    if(Empate > Fora){
      Fora = Fora+1
      
    }
    if(Fora > Empate){
      
      Empate = Empate+1
    }
  }
  df = data.frame(Casa = Casa,
                  Empate = Empate,
                  Fora = Fora)
  
  return(df)
  
}