library(shiny)


setwd('/home/jalvi/Área de Trabalho/MLToolkit')

runApp(launch.browser = T)

library(rsconnect)


 rsconnect::setAccountInfo(name='machinelearningtoolkit',
                           token='285F1AE1005D934909DC18435375E58B',
                           secret='FWxcyHPx9eKnxOd394AeLqbmbbLxXD2SQb2b8JXG')
 deployApp()

??svmPoly

 