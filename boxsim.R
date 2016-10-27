box.sim <- function(peso.btm, btm, xG, rodadas){
  new = btm
  new$Pontos = (btm$Pontos)*peso.btm + (xG$Pontos)*(1-peso.btm)
  new$media = (btm$media)*peso.btm + (xG$media)*(1-peso.btm)
  
  medias = new[1:20,]
  medias = medias[order(medias$media, decreasing = T),]
  medias$group[1] = 'C'
  medias$group[2:3] = 'G'
  medias$group[4:6] = 'H'
  medias$group[7:16] = 'N'
  medias$group[17:20] = 'Z'
  medias = medias[order(medias$Equipe),]
  
  new$group = rep(medias$group,nrow(btm)/20)
  
  paleta = c("khaki","dodgerblue2",'slategrey',"snow2","lightcoral")
  
  subt = paste("Número de campeonatos simulados:",(nrow(btm)/20),sep=" ")
  titt = paste("Projeção da Tabela de Classificação após", 
               rodadas,"rodadas",sep=" ")
  
  box.pts <- ggplot(new, aes(x = reorder(Equipe,media), y = Pontos, fill = group)) + 
    geom_boxplot(alpha=0.7,outlier.colour="NA", colour = 'gray34')+
    coord_flip()+
    theme_minimal()+
    scale_fill_manual(values = paleta)+
    theme(legend.position="none",
          axis.text.y = element_text(size = 13,
                                     colour='gray26'),
          axis.text.x = element_text(size = 12, color = 'gray26'),
          axis.title.x=element_text(size=12, color = 'gray26'),
          text=element_text(family="Avenir Next Condensed"),
          plot.title = element_text(family="Avenir Next Condensed", color = "gray26" ))+
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "Pontos",
                       breaks = seq(20, 85, 5),
                       limits=c(20, 85))+
    labs(title=titt,
         subtitle=subt)+
    theme(plot.title=element_text(size=20, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))+
    theme(plot.subtitle=element_text(size=15, hjust=0.5,colour='gray34'),
          text=element_text(family='Avenir Next Condensed'))+
    annotate("text", label = "@ProjecaoDeGol", y = max(new$Pontos)*0.95, 
             x = 1.45, size = 5, colour = "gray34",family="Avenir Next Condensed") 
  
  return(box.pts)
  
  }