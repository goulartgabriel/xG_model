cores <- function(dataframe){
  for (i in 1:nrow(dataframe)){
    if(dataframe$Equipe[i] == 'vazio'){
      dataframe$paletta[i] = "white"
    }
    if(dataframe$Equipe[i] == 'Empate'){
      dataframe$paletta[i] = "ivory3"
    }
    if(dataframe$Equipe[i] == 'Palmeiras'){
      dataframe$paletta[i] = "#0D7A52"
    }
    if(dataframe$Equipe[i] == 'Santos'){
      dataframe$paletta[i] = "#858585"
    }
    if(dataframe$Equipe[i] == 'Corinthians'){
      dataframe$paletta[i] = "slateblue4"
    }
    if(dataframe$Equipe[i] == 'Grêmio'){
      dataframe$paletta[i] = "deepskyblue3"
    }
    if(dataframe$Equipe[i] == 'Atlético MG'){
      dataframe$paletta[i] = 'gray36'
      #dataframe$paletta[i] = "#424242"
    }
    if(dataframe$Equipe[i] == 'Flamengo'){
      #dataframe$paletta[i] = "firebrick"
      dataframe$paletta[i] = "#D14141"
      
    }
    if(dataframe$Equipe[i] == 'Atlético PR'){
      dataframe$paletta[i] = "#87313C"
    }
    if(dataframe$Equipe[i] == 'Fluminense'){
      #dataframe$paletta[i] = "#A82A4A"
      dataframe$paletta[i] = "#7A3254"
      
    }
    if(dataframe$Equipe[i] == 'Ponte Preta'){
      dataframe$paletta[i] = "#858585"
    }
    if(dataframe$Equipe[i] == 'Chapecoense'){
      dataframe$paletta[i] = "#5A9C71"
    }
    if(dataframe$Equipe[i] == 'São Paulo'){
      dataframe$paletta[i] = "indianred2"
    }
    if(dataframe$Equipe[i] == 'Sport'){
      dataframe$paletta[i] = "lightsalmon4"
    }
    if(dataframe$Equipe[i] == 'Botafogo'){
      dataframe$paletta[i] = "gray36"
    }
    if(dataframe$Equipe[i] == 'Internacional'){
      dataframe$paletta[i] = "indianred2"
    }
    if(dataframe$Equipe[i] == 'Vitória'){
      dataframe$paletta[i] = "#87313C"
    }
    if(dataframe$Equipe[i] == 'Coritiba'){
      dataframe$paletta[i] = "#5A8A58"
      #dataframe$paletta[i] = "#28B559"
    }
    if(dataframe$Equipe[i] == 'Figueirense'){
      dataframe$paletta[i] = "gray36"
    }
    if(dataframe$Equipe[i] == 'Cruzeiro'){
      dataframe$paletta[i] = "dodgerblue3"
    }
    if(dataframe$Equipe[i] == 'Santa Cruz'){
      dataframe$paletta[i] = "indianred2"
    }
    if(dataframe$Equipe[i] == 'América'){
      dataframe$paletta[i] = "#33B06F"
    }
    if(dataframe$Equipe[i] == 'Brasil'){
      dataframe$paletta[i] = "gold2"
    }
    if(dataframe$Equipe[i] == 'Bolívia'){
      dataframe$paletta[i] = "#0D7A52"
    }
  }
  return(dataframe)  
  
}