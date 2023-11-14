#Librerias
library(tidyverse)

#Funciones utiles para la materia------------------------------

#Parametros graficos (base)
graph_par <- function(){
  par(family = "Verdana", cex.axis=0.7, cex.lab=0.7, mar=c(4,4,2,3) - 1.5,
      mgp=c(1.1,0.25,0), tcl=0)
}
graph_par()


#Scatter de pares
scatter_pares <- function(data){
  panel.hist <- function(x, ...)
  {
    usr <- par("usr")
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "lightblue", ...)
  }
  
  pairs(data, lower.panel = panel.smooth,
        upper.panel= NULL, pch=20, lwd=2, diag.panel = panel.hist,
        cex.labels = 1.3)
  
}

#Heatmap de correlaciones
cor_heatmap <- function(data){
  # Load and install heatmaply package
  library(heatmaply)
  
  heatmaply_cor(x = data, Rowv = NA, Colv = NA)
  
}

#Plot tridimensional (plotly)
plot3d <- function(X,col=NULL,id=NULL, size=6, cube=TRUE){
  
  library(plotly)
  library(RColorBrewer)
  
  n <- nrow(X)
  p <- ncol(X)
  
  data <- data.frame(scale(X, scale=FALSE))
  names(data) <- c('x1','x2','x3')
  
  if(is.null(col)==TRUE){
    data$col <- rep('black',n)
  } else {
    data$col <-col}
  
  if(is.null(id)==TRUE){
    data$id<-1:n
  } else {data$id <- id}
  
  fig <- plot_ly(data,
                 x = ~data[,1], y = ~data[,2], z = ~data[,3],
                 colors = brewer.pal(p,'Set1'), text=~id,
                 marker = list(size=size))
  fig <- fig %>% add_markers(color = ~col)
  fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(X)[1],
                                                  range = c(min(data$x1),max(data$x1))),
                                     yaxis = list(title = colnames(X)[2],
                                                  range = c(min(data$x2),max(data$x2))),
                                     zaxis = list(title = colnames(X)[3],
                                                  range = c(min(data$x3),max(data$x3))),
                                     aspectmode = ifelse(cube==TRUE,'cube','auto')))
  fig
  
}