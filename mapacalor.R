mapa.de.calor <- function(xG.chutes){

  #probs = count(xG.chutes, vars = c('Gol','Regiao'))
  
  b = table(xG.chutes$Regiao,xG.chutes$Gol)
  b = as.data.frame(b)
  colnames(b) = c('Regiao','Gol','Freq')
  perc = data.frame(Regiao = seq(0,28),
                    perc = b$Freq[30:58]/(b$Freq[30:58]+b$Freq[1:29]))
  #  perc = b$Freq[30:58] )
  perc = perc[2:29,]
  x = seq(1:9)
  map = data.frame(x = x,
                   y = 1)
  for(i in 2:8){
    k = data.frame(x = x,
                   y = i)
    map = rbind(map,k)
  }
  area = matrix(nrow = 4, ncol = 5)
  j = 1
  for (k in 1:4){
    area[k,] = perc$perc[j:(j+4)]
    j = j+5
  }
  area[is.nan(area)] <- 0
  map$perc = 0
  area = area[nrow(area):1,ncol(area):1]
  map$perc[3:7] = area[4,]
  map$perc[12:16] = area[3,]
  map$perc[21:25] = area[2,]
  map$perc[30:34] = area[1,]
  
  map$perc[1:2] = perc$perc[22]  #22
  map$perc[10:11] = perc$perc[22]
  
  map$perc[19:20] = perc$perc[24]  #24
  map$perc[28:29] = perc$perc[24]
  
  map$perc[8:9] = perc$perc[21]  #21
  map$perc[17:18] = perc$perc[21]
  
  map$perc[26:27] = perc$perc[23]  #23
  map$perc[35:36] = perc$perc[23]
  
  map$perc[37:39] = perc$perc[27] #27
  map$perc[46:48]=perc$perc[27]
  
  map$perc[40:42] = perc$perc[26] #26
  map$perc[49:51]=perc$perc[26]
  
  map$perc[43:45] = perc$perc[25] #25
  map$perc[52:54] = perc$perc[25]
  
  map$perc[55:72] = perc$perc[28]
  map = replace(map, is.na(map), 0) 
  heat = ggplot(map, aes(x, y))+
    geom_tile(aes(fill = perc), colour = "white")+
    
    scale_fill_gradient(low = "white",high = "dodgerblue3")+
    ylab(" ")+
    xlab("")+
    geom_curve(aes(x = 2.5, y = 0.5, xend = 2.5, yend = 3.5), data = map, curvature = 0,
               color = 'gray34', lineend = 'butt')+
    geom_curve(aes(x = 7.5, y = 0.5, xend = 7.5, yend = 3.5), data = map, curvature = 0,
               color = 'gray34', lineend = 'butt')+
    geom_curve(aes(x = 4, y = 0.5, xend = 4, yend = 1.5), data = map, curvature = 0,
               color = 'gray34', lineend = 'butt')+
    geom_curve(aes(x = 6, y = 0.5, xend = 6, yend = 1.5), data = map, curvature = 0,
               color = 'gray34', lineend = 'butt')+
    geom_curve(aes(x = 4, y = 1.5, xend = 6, yend = 1.5), data = map, curvature = 0,
               color = 'gray34', lineend = 'butt')+
    geom_curve(aes(x = 2.5, y = 3.5, xend = 7.5, yend = 3.5), data = map, curvature = 0,
               color = 'gray34', lineend = 'butt')+
    geom_curve(aes(x = 4.2, y = 3.5, xend = 5.8, yend = 3.5), data = map, curvature = -0.5,
               color = 'gray34', lineend = 'butt')+
    geom_curve(aes(x = 4.5, y = 0.5, xend = 5.5, yend = 0.5), data = map, curvature = 0,
               color = 'gray34', lineend = 'butt')+
    geom_point(aes(x = 5, y = 2.5))+
    theme_classic()+
    theme(legend.position = 'none',
          axis.line.y = element_line(colour = "white"),
          axis.line.x = element_line(colour = "white"),
          axis.ticks.y = element_line(color="white"),
          axis.ticks.x = element_line(color="white"),
          axis.text.y = element_text(colour='white'),
          axis.text.x = element_text(color = 'white'),
          legend.title = element_text(colour="dodgerblue4"),
          legend.text = element_text(colour="dodgerblue4"))+
    labs(title='Mapa de calor de gols por região')+
    #    subtitle=subt)+
    theme(plot.title=element_text(size=20, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))+
    annotate("text", label = "@ProjecaoDeGol", family="Avenir Next Condensed", 
             x = 1, y = 1, size = 3.7, colour = "gray34",fontface = 'italic')    
  
  
  return(heat)

}
