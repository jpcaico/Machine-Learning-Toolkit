#Sys.setlocale(locale = "portuguese")
####### gerais #################

graficarImportanciaClassificacao <- function(modelo){
  
  data = data.frame(x = rownames(varImp(modelo)$importance),
                    y = varImp(modelo)$importance)
  data = data[c(1,2)]
  colnames(data) = c("x", "y")
  data = arrange(data, y)
  data = mutate(data, x = factor(x, x))
  
  p <- ggplot(data = data, aes(x = x, y = y)) +
    geom_segment(aes(
      x = x,
      xend = x,
      y = 0,
      yend = y
    ),
    color = "cyan",
    size = 1) +
    geom_point(color = "magenta",
               size = 4,
               alpha = 0.7) +
    theme_light() +
    #coord_filp() +
    theme_bw() +
    xlab("Variável") +
    ylab("Importância (%)") +
    ggtitle("Importância das variáveis") + theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=18),
      plot.title = element_text(size=20, face = "bold"))
  
  
  ggplotly(p)
  style(p, text = paste0("X: ", data$x ,"\n" ,"Y: ", data$y ))
}




graficarImportancia <- function(modelo){
  
  data = data.frame(x = rownames(varImp(modelo)$importance),
                    y = varImp(modelo)$importance)
  colnames(data) = c("x", "y")
  data = arrange(data, y)
  data = mutate(data, x = factor(x, x))
  
  p <- ggplot(data = data, aes(x = x, y = y)) +
    geom_segment(aes(
      x = x,
      xend = x,
      y = 0,
      yend = y
    ),
    color = "cyan",
    size = 1) +
    geom_point(color = "magenta",
               size = 4,
               alpha = 0.7) +
    theme_light() +
   # coord_flip() +
    theme_bw() +
    xlab("Variável") +
    ylab("Importância (%)") +
    ggtitle("Importância das Variáveis") + theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=18),
      plot.title = element_text(size=20, face = "bold")) + theme(axis.text.x = element_text(angle = 45))
  
  ggplotly(p)
  style(p, text = paste0("X: ", data$x ,"\n" ,"Y: ", data$y ))
}

##### graficos classificacao ###########


ggplotConfusionMatrix <- function(m){
  # mytitle <- paste0('Matriz de Confusão Treino ',"Acurácia Geral: ", percent_format()(m$overall[1]),
  #                  "Kappa Geral: ", percent_format()(m$overall[2]))
  # 
  mytitle <- 'Matriz de Confusão Teste '
  
  data_c <-  mutate(group_by(as.data.frame(m$table), Reference ), percentage = 
                      percent(Freq/sum(Freq)))
   ggplot(data = data_c,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "green") +
     xlab("Referência") +
     ylab("Classificação") +
    geom_text(aes(x = Reference, y = Prediction, label = paste0(Freq,' (', percentage, ')')  )) +
    theme(legend.position = "none") +
    ggtitle(mytitle) +  theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=20),
      plot.title = element_text(size=20, face = "bold"))
     
     
  
}



graficarLimite <- function(grid, X,Y, y, teste_plot, predgrid){
  

  ggplot(data=grid) + stat_contour(aes(x=grid[,X], y=grid[,Y], z=predgrid),
                                        bins=2) +
    
    geom_point(data=teste_plot, aes(x=teste_plot[,X], y=teste_plot[,Y], colour=as.factor(teste_plot[,y])),
               size=4, alpha = 0.7)+
    theme_bw() + xlab(X) + ylab(Y) + ggtitle("Limite de Decisão - Treino") + labs(title = "Limite de Decisão - Treino") +
    theme(legend.title = element_blank()) + theme(legend.position  ="bottom") + theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=20),
      plot.title = element_text(size=20, face = "bold"))
  
    
  
}


graficarSuperficie <- function(grid, X, Y, predgrid){
  
  ggplot(data=grid) + 
    geom_point(aes(x=grid[,X], y=grid[,Y], colour=as.factor(predgrid))) +

    theme_bw() + xlab(X) + ylab(Y) + ggtitle("Região de Decisão") + labs(title = "Região de Decisão")+
    theme(legend.title = element_blank()) + theme(legend.position  ="bottom") +
    theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=20),
      plot.title = element_text(size=20, face = "bold"))
  

  
}


###### graficos regressao #########

gerarTabelaCoeficientes <- function(modelo){
  
   
   data = as.data.frame(t(summary(modelo)$coefficients))
   data = round(data[1,],3)
 
  return(data)
}


graficarTreinoResidual <- function(modelo, data, X, Y){
  residuals <- residuals(modelo)
  predicted <- predict(modelo, newdata = data)
  
  p <-
    ggplot(data, aes(x =  data[, X], y = data[, Y])) +
    geom_line(aes(x = data[, X], y = predicted), color =
                'cyan') +
    geom_segment(aes(xend =  data[, X], yend = predicted), alpha = .2) +
    geom_point(aes(
      color = abs(residuals),
      size = abs(residuals)
    )) +
    scale_color_continuous(low = "green", high = "red") +
    guides(color = FALSE, size = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw() +
    xlab(X) +
    ylab(Y) +
    ggtitle('Conjunto de Treino - Resíduos') + theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=18), plot.title = element_text(size=20, face = "bold" )) 
  
  ggplotly(p)
  
}

graficarTesteMultiplo <- function(teste, X, Y){
  
  p<- ggplot() + geom_point(aes(x = teste[, X], y = teste[, Y]),
                        colour = 'magenta') +
    xlab(X) +
    ylab(Y) +
    theme_bw() + ggtitle('Gráfico de Dispersão') + theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=18),
      plot.title = element_text(size=20, face = "bold"))
  ggplotly(p)
    style(p, text = paste0("X: ", teste[, X] ,"\n" ,"Y: ", teste[, Y] ))

  
}

graficarTeste <- function(modelo, teste, treino, X, Y){
  
  p<- ggplot() + geom_point(aes(x = teste[, X], y = teste[, Y]),
                        colour = 'magenta') +
    geom_line(aes(
      x = treino[, X],
      y = predict(modelo, newdata = treino)
    ),
    colour = 'cyan',
    size = 1) +
    geom_segment(
      aes(x = teste[, X], y = teste[, Y]),
      xend = teste[, X],
      yend = predict(modelo, newdata = teste),
      linetype = "dashed",
      alpha=0.5
    ) +
    xlab(X) +
    ylab(Y) +
    theme_bw() + ggtitle('Conjunto de Teste') + theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=18),
      plot.title = element_text(size=20, face = "bold"))
  ggplotly(p)
    style(p, text = paste0("Pontos (Real):","\n", "X: ", teste[, X] ,"\n" ,"Y: ", teste[, Y],
                           "\n", "Linha (Predito):", "\n",
                           'X: ', teste[,X] ,"\n", 'Y: ',predict(modelo,newdata=treino)))

  
}


graficarTreino <- function(modelo, treino, X, Y){
ggplot() +
  geom_point(aes(x = treino[, X], y = treino[, Y]),
             colour = 'magenta') +
  geom_line(aes(
    x = treino[, X],
    y = predict(modelo, newdata = treino)
  ),
  colour = 'cyan',
  size = 1) +
  xlab(X) +
  ylab(Y) +
  theme_bw() + ggtitle('Conjunto de Treino') + theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=18),
    plot.title = element_text(size=20, face = "bold"))
  
}


graficarMultiplos <- function(treino_resid, x, Y, predicted, iv){
  ggplot(treino_resid, aes(x = x, y = treino_resid[, Y])) +
    geom_point(aes(y = predicted), color = 'cyan', size = 0.5) +
    geom_segment(aes(xend = x, yend = predicted), alpha = .4) +
    geom_point(aes(y = treino_resid[, Y]), color = 'magenta') +
    facet_grid( ~ iv, scales = 'free') + theme_bw() +
    ylab(Y) + xlab('Variável') + ggtitle('Relação das Variáveis') + theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=18),
      plot.title = element_text(size=20, face = "bold"))
  
  
}

