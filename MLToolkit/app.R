
rm(list=ls()) 
#options(warn = -1)
options(shiny.sanitize.errors = TRUE)
options(shiny.maxRequestSize=300*1024^2)
#Sys.setlocale(locale = "portuguese")
 tags$style(type="text/css",
            ".shiny-output-error { visibility:hidden;}",
           ".shiny-output-error:before {visibility: hidden;}")


source('Bibliotecas.R')
eval(parse('Funcoes.R', encoding = "UTF-8"))

ui <- navbarPage(
  title = "Machine Learning Toolkit",
  theme = shinytheme("cerulean"),
  position = 'static-top',
  

  
  ######### INICIO - ABA INICIO #########
  
  tabPanel("Página Inicial",
           icon = icon('home'),
           withMathJax(includeMarkdown('Inicio.Rmd'))),
  
  ######### FIM - ABA INICIO #######
  
  navbarMenu(
    title = 'Informações sobre os Modelos',
    icon = icon('code'),
    tabPanel(
      'Visão Geral dos Modelos',
      includeMarkdown('Modelos.Rmd')
      
    ),
    tabPanel(
      'Pré-Processamento dos Dados',
      includeMarkdown('preprocess.Rmd')
      
    ),
    tabPanel(
      'Hiperparâmetros dos Modelos',
      includeMarkdown("Hiperparametros.Rmd")
    ),
    tabPanel(
      'Validação e Reamostragem',
      includeMarkdown("Metodos_Treinamento.Rmd")
    ),
    tabPanel(
      'Avaliação os Modelos',
      includeMarkdown("Interpretacao.Rmd")
    )
    
  ),
  
  navbarMenu(
    title = "Modelos - Regressão",
    icon = icon("bar-chart"),
    
    ########### REGRESSAO LINEAR ##########
    
    
    tabPanel(
      "Regressão Linear",
      
      tabsetPanel(
        id = "tabs_LR",
        tabPanel(
          "Criar Modelo",
          icon = icon('tasks'),
          
          tabPanel(
            "Criar Modelo",
            
            sidebarPanel(
              width = 3,
              
              fileInput(
                "arquivo_LR",
                "Insira seu Dataset:",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),
              
              materialSwitch("mostrar_LR", "Mostrar Dataset", FALSE),
              uiOutput('XLR'),
              uiOutput('YLR'),
              hr(),
              radioButtons(
                inputId = "opcao_LR",
                label = "Método de Validação Cruzada:",
                choices = c("Padrão" = "Padrao",
                            "Escolher" = "Avancado"),
                selected = "Padrao",
                inline = T
              ),
              
            
              uiOutput('opcoes_treinar_LR'),
              uiOutput('opcoes_treinar2_LR'),
              uiOutput('opcoes_treinar3_LR'),
            
              numericInput("treinar_LR",
                           "Porcentagem de Divisão (Treino)",
                           value = 0.8),
              hr(),
              selectInput('metrica_LR',
                          'Métrica:',
                           choices = c('Raiz do Erro Quadrático Médio (RMSE)'='RMSE',
                                       'R² (Rsquared)'= 'Rsquared'),
                          selected = c('RMSE')),
              
              actionButton("computar_LR", "Criar Modelo", class =
                             'btn-primary'),
              downloadButton("baixar_LR", "Exportar Modelo", class = 'btn-primary')
            ),
            mainPanel(
              fluidRow(column(
                12,
                h4('Conjunto de Dados Importado'),
                h5(
                  'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                ),
                DT::dataTableOutput("importacao_LR")
                %>% withSpinner(type = 8, size = 0.5),
                uiOutput('mensagem_erro_lr')
              )),
              hr(),
              fluidRow(column(
                12,
               uiOutput("coeficientes_LR"),
                DT::dataTableOutput("tabela_coeficientes_LR")
                %>% withSpinner(type = 8, size = 0.5)
                
                
              )),
              br(),
             fluidRow( column(12,
                        uiOutput('resultados_obtidos_LR'),
                     DT::dataTableOutput(
                       "tabela_parametro_LR"
                     ) %>% withSpinner(type = 8, size = 0.5))),
             
             br(),
                     hr(),
                     fluidRow(column(
                       12,
                      
                       plotlyOutput("graficos_LR")
                       %>% withSpinner(type = 8, size = 0.5)
                     ) 
                     #),
                     # column(5,
                     #        plotlyOutput("residuos_LR")
                     #        %>% withSpinner(type = 8, size = 0.5))
                     
                     ),
                            hr(),
                            fluidRow(column(
                              12,
                              uiOutput("nomes_treino_LR"),
                              plotlyOutput('graficos_multiplos_LR')
                              %>% withSpinner(type = 8, size = 0.5)
                              
                              
                            ))
  
                     )
                     )
              ),
              tabPanel(
                "Realizar Previsões",
                icon = icon('cogs'),
                
                
                sidebarPanel(
                  width = 3,
                  fileInput(
                    "arquivo_teste_LR",
                    "Insira seu Dataset:",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                  materialSwitch("mostrar_teste_LR", "Mostrar Dataset", TRUE),  
                  checkboxInput('opcao_prever_LR',
                                'Importar Modelo?',
                                value = F),
                  
                  uiOutput('importar_LR1'),
                
                  actionButton("prever_LR", "Calcular Previsão", class = 'btn-primary')
                ),
                
                mainPanel(
                  DT::dataTableOutput('importacao_teste_LR'),
                  
                  uiOutput('resultados_obtidos_previsoes_LR'),
                  DT::dataTableOutput('resultado_previsoes_LR'),
                  downloadButton('download_LR', "Download Resultados Completos", class = 'btn-primary')
                  
                )
              )
              
              )
             
      

              
            ),
            
            ########## FIM REGRESSAO LINEAR ##########
            
            ########## INICIO Ãrvore de Decisão #########
            
            tabPanel(
              "Árvore de Decisão",
              tabsetPanel(
                id = "tabs_AD",
                  
                  tabPanel(
                    "Criar Modelo",
                    icon = icon("tasks"),
                    
                    sidebarPanel( width = 3,
                      fileInput(
                        "arquivo_AD",
                        "Insira seu Dataset:",
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                      ),
                      
                      materialSwitch("mostrar_AD", "Mostrar Dataset", FALSE),
                      uiOutput('XAD'),
                      uiOutput('YAD'),
                      hr(),   
                      radioButtons(
                        inputId = "parametros_AD",
                        label = "Escolher Hiperparâmetro:",
                        choices = c("Padrão" = "Padrao",
                                    "Escolher"),
                        selected = "Padrao",
                        inline = T
                      ),
                      uiOutput('opcoes_parametros_AD'),
                      hr(),
                      radioButtons(
                        inputId = "opcao_AD",
                        label = "Método de Validação Cruzada:",
                        choices = c("Padrão" = "Padrao",
                                    "Escolher" = "Avancado"),
                        selected = "Padrao",
                        inline = T
                      ),
                      
                      uiOutput('opcoes_treinar_AD'),
                      uiOutput('opcoes_treinar2_AD'),
                      uiOutput('opcoes_treinar3_AD'),
                      numericInput("treinar_AD",
                                   "Porcentagem de Divisão (Treino)",
                                   value = 0.8),
                      selectInput('metrica_AD',
                                  'Métrica:',
                                  choices = c('Raiz do Erro Quadrático Médio (RMSE)'='RMSE',
                                              'R² (Rsquared)'= 'Rsquared'),
                                  selected = c('RMSE')),
                      
                      hr(),
                      actionButton("computar_AD", "Criar Modelo", class =
                                     'btn-primary'),
                      downloadButton("baixar_AD", "Exportar Modelo", class = 'btn-primary')
                    ),
                    
                    mainPanel(
                      fluidRow(column(
                        12,
                        h4('Conjunto de Dados Importado'),
                        h5(
                          'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                        ),
                        DT::dataTableOutput("importacao_AD")
                        %>% withSpinner(type = 8, size = 0.5)
                        
                      )),
                      hr(),
                      
                      fluidRow(column(
                        12,
                        uiOutput('resultados_obtidos_AD'),
                        DT::dataTableOutput("tabela_parametro_AD")
                        %>% withSpinner(type = 8, size = 0.5),
                        verbatimTextOutput("melhor_parametro_AD"),
                        verbatimTextOutput("melhor_metrica_AD"),
                        
                        tags$head(tags$style(HTML("
                            #melhor_metrica_AD {
                              font-size: 24px;
                            }
                            "))),
                        tags$head(tags$style(HTML("
                            #melhor_parametro_AD {
                              font-size: 24px;
                            }
                            "))),
                        uiOutput('mensagem_erro_ad')
                      )),
                      
                      hr(),
                      
                      fluidRow(column(
                        6,
                        
                        plotlyOutput("graficos_AD")
                        %>% withSpinner(type = 8, size = 0.5)
                        
                      ),
                      column(6,
                             plotOutput("residuos_AD") %>% withSpinner(type = 8, size = 0.5))),
                      hr(),
                      fluidRow(column(
                        12,
                        uiOutput("nomes_treino_AD"),
                        plotlyOutput('graficos_multiplos_AD')
                        %>% withSpinner(type = 8, size = 0.5)
                        
                        
                      ))
                      
                    )

                ),
                tabPanel(
                  "Realizar Previsões",
                  icon = icon('cogs'),
                  
                  
                  sidebarPanel(
                    width = 3,
                    fileInput(
                      "arquivo_teste_AD",
                      "Insira seu Dataset:",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    ),
                    materialSwitch("mostrar_teste_AD", "Mostrar Dataset", TRUE),
                    checkboxInput('opcao_prever_AD',
                                  'Importar Modelo?',
                                  value = F),
                    
                    uiOutput('importar_AD1'),
                    
                    actionButton("prever_AD", "Calcular Previsão", class = 'btn-primary')
                    
                  ),
                  
                  
                  mainPanel(
                    DT::dataTableOutput('importacao_teste_AD'),
                    uiOutput('resultados_obtidos_previsoes_AD'),
                    DT::dataTableOutput('resultado_previsoes_AD'),
                    downloadButton('download_AD', "Download Resultados Completos", class = 'btn-primary')
                  )
                )
                
              )
              
              
              
            ),
            
            ############# FIM Ãrvore de Decisão #########
            
            ############ INICIO Floresta Aleatória (Random Forest) #########
            
            tabPanel(
              "Floresta Aleatória (Random Forest)",
              
              tabsetPanel(
                id = "tabs_RF",
                tabPanel(
                  "Criar Modelo",
                  icon = icon('tasks'),
                  
                  tabPanel(
                    "Criar Modelo",
                    
                    sidebarPanel(
                      width = 3,
                      
                      fileInput(
                        "arquivo_RF",
                        "Insira seu Dataset:",
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                      ),
                      
                      materialSwitch("mostrar_RF", "Mostrar Dataset", FALSE),
                      uiOutput('XRF'),
                      uiOutput('YRF'),
                      hr(),
                      radioButtons(
                        inputId = "parametros_RF",
                        label = "Escolher Hiperparâmetro:",
                        choices = c("Padrão" ="Padrao",
                                    "Escolher"),
                        selected = "Padrao",
                        inline = T
                      ),
                      uiOutput('opcoes_parametros_RF'),
                      hr(),
                      radioButtons(
                        inputId = "opcao_RF",
                        label = "Método de Validação Cruzada:",
                        choices = c("Padrão" = "Padrao",
                                    "Escolher" = "Avancado"),
                        selected = "Padrao",
                        inline = T
                      ),
                      
                      uiOutput('opcoes_treinar_RF'),
                      uiOutput('opcoes_treinar2_RF'),
                      uiOutput('opcoes_treinar3_RF'),
                      numericInput("treinar_RF",
                                   "Porcentagem de Divisão (Treino)",
                                   value = 0.8),
                      selectInput('metrica_RF',
                                  'Métrica:',
                                  choices = c('Raiz do Erro Quadrático Médio (RMSE)'='RMSE',
                                              'R² (Rsquared)'= 'Rsquared'),
                                  selected = c('RMSE')),
                      
                      hr(),
                      actionButton("computar_RF", "Criar Modelo", class =
                                     'btn-primary'),
                      downloadButton("baixar_RF", "Exportar Modelo", class = 'btn-primary')
                      
                    
                    ),
                    
                    mainPanel(
                      fluidRow(column(
                        12,
                        h4('Conjunto de Dados Importado'),
                        h5(
                          'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                        ),
                        DT::dataTableOutput("importacao_RF")
                        %>% withSpinner(type = 8, size = 0.5)
                        
                      )),
                      hr(),
                      
                      fluidRow(column(
                        12,
                        uiOutput('resultados_obtidos_RF'),
                        DT::dataTableOutput("tabela_parametro_RF")
                        %>% withSpinner(type = 8, size = 0.5),
                        verbatimTextOutput("melhor_parametro_RF"),
                        verbatimTextOutput("melhor_metrica_RF"),
                        
                        tags$head(tags$style(HTML("
                            #melhor_metrica_RF {
                              font-size: 24px;
                            }
                            "))),
                        tags$head(tags$style(HTML("
                            #melhor_parametro_RF {
                              font-size: 24px;
                            }
                            "))),
                        uiOutput('mensagem_erro_rf')
                      )),
                      
                      hr(),
                      
                      fluidRow(column(
                        6,
                        
                        plotlyOutput("graficos_RF")
                        %>% withSpinner(type = 8, size = 0.5)
                        
                      ),
                      column(6,
                             plotOutput("residuos_RF") %>% withSpinner(type = 8, size = 0.5))),
                      hr(),
                      fluidRow(column(
                        12,
                        uiOutput("nomes_treino_RF"),
                        plotlyOutput('graficos_multiplos_RF')
                        %>% withSpinner(type = 8, size = 0.5)
                        
                        
                      ))
                      
                    )
                  )
                ),
                tabPanel(
                  "Realizar Previsões",
                  icon = icon('cogs'),
                  
                  sidebarPanel(
                    width = 3,
                    fileInput(
                      "arquivo_teste_RF",
                      "Insira seu Dataset:",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    ),
                    materialSwitch("mostrar_teste_RF", "Mostrar Dataset", TRUE),
                    
                    checkboxInput('opcao_prever_RF',
                                  'Importar Modelo?',
                                  value = F),
                    
                    uiOutput('importar_RF1'),
                    
                    
                    
                    actionButton("prever_RF", "Calcular Previsão", class = 'btn-primary')
                    
                  ),
                  
                  
                  mainPanel(
                    DT::dataTableOutput('importacao_teste_RF'),
                    uiOutput('resultados_obtidos_previsoes_RF'),
                    DT::dataTableOutput('resultado_previsoes_RF'),
                    downloadButton('download_RF', "Download Resultados Completos", class = 'btn-primary')
                  )
                )
                
              )
              
            ),
            
            ############### FIM Floresta Aleatória (Random Forest) #############
            
            ############### INICIO SUPPORT VECTOR REGRESSION ########
            
            tabPanel(
              "Máquina Vetor de Suporte (SVM)",
              
              tabsetPanel(
                id = "tabs_SV",
                tabPanel(
                  "Criar Modelo",
                  icon = icon('tasks'),
                  
                  tabPanel(
                    "Criar Modelo",
                    
                    sidebarPanel( width = 3,
                      fileInput(
                        "arquivo_SV",
                        "Insira seu Dataset:",
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                      ),
                      materialSwitch("mostrar_SV", "Mostrar Dataset", FALSE),
                      uiOutput('XSV'),
                      uiOutput('YSV'),
                      
                      hr(),
                      pickerInput(
                        inputId = 'kernel_SV',
                        label = 'Selecione o Kernel',
                        choices = c(
                          "Polinomial" = "svmPoly",
                          "Linear" = "svmLinear",
                          "Radial" = "svmRadial"
                          
                        ),
                        selected = "svmLinear",
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"
                        ),
                        multiple = FALSE
                      ),
                      hr(),
                      radioButtons(
                        inputId = "parametros_SV",
                        label = "Escolher Parametro:",
                        choices = c("Padrão" ="Padrao",
                                    "Escolher"),
                        selected = "Padrao",
                        inline = T
                      ),
                      uiOutput('opcoes_parametros_SV'),
                      
                      hr(),
                      radioButtons(
                        inputId = "opcao_SV",
                        label = "Método de Validação Cruzada:",
                        choices = c("Padrão"= "Padrao",
                                    "Escolher" = "Avancado"),
                        selected = "Padrao",
                        inline = T
                      ),
                      
                      uiOutput('opcoes_treinar_SV'),
                      uiOutput('opcoes_treinar2_SV'),
                      uiOutput('opcoes_treinar3_SV'),
                      numericInput('treinar_SV', 'Porcentagem de Divisão (Treino):', value = 0.8),
                      selectInput('metrica_SV',
                                  'Métrica:',
                                  choices = c('Raiz do Erro Quadrático Médio (RMSE)'='RMSE',
                                              'R² (Rsquared)'= 'Rsquared'),
                                  selected = c('RMSE')),
                      hr(),
                      actionButton("computar_SV", "Criar Modelo", class =
                                     'btn-primary'),
                      downloadButton("baixar_SV", "Exportar Modelo", class = 'btn-primary')
                      
                    ),
                    
                    mainPanel(
                      fluidRow(column(
                        12,
                        h4('Conjunto de Dados Importado'),
                        h5(
                          'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                        ),
                        DT::dataTableOutput("importacao_SV")
                        %>% withSpinner(type = 8, size = 0.5)
                        
                      )),
                      hr(),
                      
                      fluidRow(column(
                        12,
                        uiOutput('resultados_obtidos_SV'),
                        DT::dataTableOutput("tabela_parametro_SV")
                        %>% withSpinner(type = 8, size = 0.5),
                        verbatimTextOutput("melhor_parametro_SV"),
                        verbatimTextOutput("melhor_metrica_SV"),
                        tags$head(tags$style(HTML("
                            #melhor_parametro_SV {
                              font-size: 24px;
                            }
                            "))),
                        tags$head(tags$style(HTML("
                            #melhor_metrica_SV {
                              font-size: 24px;
                            }
                            "))),
                        uiOutput('mensagem_erro_sv')
                      )),
                      
                      hr(),
                      
                      fluidRow(column(
                        6,
                        
                        plotlyOutput("graficos_SV")
                        %>% withSpinner(type = 8, size = 0.5)
                        
                      ),
                      column(6,
                             plotOutput("residuos_SV") %>% withSpinner(type = 8, size = 0.5))),
                             hr(),
                             fluidRow(column(
                               12,
                               uiOutput("nomes_treino_SV"),
                               plotlyOutput('graficos_multiplos_SV')
                               %>% withSpinner(type = 8, size = 0.5)
                               
                               
                             ))
                             
                      )
                      )
                    ),
                    tabPanel(
                      "Realizar Previsões",
                      icon = icon('cogs'),
                      
                      
                      sidebarPanel( width = 3,
                        fileInput(
                          "arquivo_teste_SV",
                          "Insira seu Dataset:",
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")
                        ),
                        materialSwitch("mostrar_teste_SV", "Mostrar Dataset", TRUE),
                        checkboxInput('opcao_prever_SV',
                                      'Importar Modelo?',
                                      value = F),
                        
                        uiOutput('importar_SV1'),
                        actionButton("prever_SV", "Calcular Previsão", class = 'btn-primary')
                      ),
                      
                      
                      mainPanel(
                        DT::dataTableOutput('importacao_teste_SV'),
                        uiOutput('resultados_obtidos_previsoes_SV'),
                        DT::dataTableOutput('resultado_previsoes_SV'),
                        downloadButton('download_SV', "Download Resultados Completos", class = 'btn-primary')
                      )
                    )
                    
                  )
                ),
    
    tabPanel(
      "Perceptron Multi-Camadas",
      
      tabsetPanel(
        id = "tabs_MLP",
        tabPanel(
          "Criar Modelo",
          icon = icon('tasks'),
          
          tabPanel(
            "Criar Modelo",
            
            sidebarPanel(
              width = 3,
              
              fileInput(
                "arquivo_MLP",
                "Insira seu Dataset:",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
              ),
              
              materialSwitch("mostrar_MLP", "Mostrar Dataset", FALSE),
              uiOutput('XMLP'),
              uiOutput('YMLP'),
              hr(),
              radioButtons(
                inputId = "parametros_MLP",
                label = "Escolher Hiperparâmetro:",
                choices = c("Padrão" ="Padrao",
                            "Escolher"),
                selected = "Padrao",
                inline = T
              ),
              uiOutput('opcoes_parametros_MLP'),
              hr(),
              radioButtons(
                inputId = "opcao_MLP",
                label = "Método de Validação Cruzada:",
                choices = c( "Padrão" = "Padrao",
                            "Escolher" = "Avancado"),
                selected = "Padrao",
                inline = T
              ),
              
              uiOutput('opcoes_treinar_MLP'),
              uiOutput('opcoes_treinar2_MLP'),
              uiOutput('opcoes_treinar3_MLP'),
              numericInput("treinar_MLP",
                           "Porcentagem de Divisão (Treino)",
                           value = 0.8),
              selectInput('metrica_MLP',
                          'Métrica:',
                          choices = c('Raiz do Erro Quadrático Médio (RMSE)'='RMSE',
                                      'R² (Rsquared)'= 'Rsquared'),
                          selected = c('RMSE')),
              
              hr(),
              actionButton("computar_MLP", "Criar Modelo", class =
                             'btn-primary'),
              downloadButton("baixar_MLP", "Exportar Modelo", class = 'btn-primary')
              
              
            ),
            
            mainPanel(
              fluidRow(column(
                12,
                h4('Conjunto de Dados Importado'),
                h5(
                  'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                ),
                DT::dataTableOutput("importacao_MLP")
                %>% withSpinner(type = 8, size = 0.5)
                
              )),
              hr(),
              
              fluidRow(column(
                12,
                uiOutput('resultados_obtidos_MLP'),
                DT::dataTableOutput("tabela_parametro_MLP")
                %>% withSpinner(type = 8, size = 0.5),
                verbatimTextOutput("melhor_parametro_MLP"),
                verbatimTextOutput("melhor_metrica_MLP"),
                
                tags$head(tags$style(HTML("
                            #melhor_metrica_MLP {
                              font-size: 24px;
                            }
                            "))),
                tags$head(tags$style(HTML("
                            #melhor_parametro_MLP {
                              font-size: 24px;
                            }
                            "))),
                uiOutput('mensagem_erro_mlp')
              )),
              
              hr(),
              
              fluidRow(column(
                6,
                
                plotlyOutput("graficos_MLP")
                %>% withSpinner(type = 8, size = 0.5)
                
              ),
              column(6,
                     plotOutput("residuos_MLP") %>% withSpinner(type = 8, size = 0.5))),
              hr(),
              fluidRow(column(
                12,
                uiOutput("nomes_treino_MLP"),
                plotlyOutput('graficos_multiplos_MLP')
                %>% withSpinner(type = 8, size = 0.5)
                
                
              ))
              
            )
          )
        ),
        tabPanel(
          "Realizar Previsões",
          icon = icon('cogs'),
          
          
          sidebarPanel(
            width = 3,
            fileInput(
              "arquivo_teste_MLP",
              "Insira seu Dataset:",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),
            materialSwitch("mostrar_teste_MLP", "Mostrar Dataset", TRUE),
            
            checkboxInput('opcao_prever_MLP',
                          'Importar Modelo?',
                          value = F),
            
            uiOutput('importar_MLP1'),
            
            
            
            actionButton("prever_MLP", "Calcular Previsão", class = 'btn-primary')
            
          ),
          
          
          mainPanel(
            DT::dataTableOutput('importacao_teste_MLP'),
            uiOutput('resultados_obtidos_previsoes_MLP'),
            DT::dataTableOutput('resultado_previsoes_MLP'),
            downloadButton('download_MLP', "Download Resultados Completos", class = 'btn-primary')
          )
        )
        
      )
      
    )
                
              ),
  
  
  navbarMenu(
    "Modelos - Classificação",
    icon = icon('th-large'),
    tabPanel(
      "K-Vizinhos Mais Próximos",
      tabsetPanel(
        id = "tabs_KNN",
       
                 tabPanel("Criar Modelo",
                          icon = icon('tasks'),
                         
                          sidebarPanel( width = 3,
                                        fileInput(
                                          "arquivo_KNN",
                                          "Insira seu Dataset:",
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                        ),
                                        materialSwitch("mostrar_KNN", "Mostrar Dataset", FALSE),
                                        uiOutput('XKNN'),
                                        uiOutput('YKNN'),
                                        hr(),
                                        radioButtons(
                                          inputId = "parametros_KNN",
                                          label = "Escolher Hiperparâmetro:",
                                          choices = c("Padrão" ="Padrao",
                                                      "Escolher"),
                                          selected = "Padrao",
                                          inline = T
                                        ),
                                        uiOutput('opcoes_parametros_KNN'),
                                        
                                        hr(),
                                        radioButtons(
                                          inputId = "opcao_KNN",
                                          label = "Método de Validação Cruzada:",
                                          choices = c("Padrão" = "Padrao",
                                                      "Escolher" = "Avancado"),
                                          selected = "Padrao",
                                          inline = T
                                        ),
                                        
                                        uiOutput('opcoes_treinar_KNN'),
                                        uiOutput('opcoes_treinar2_KNN'),
                                        uiOutput('opcoes_treinar3_KNN'),
                                        numericInput('treinar_KNN', 'Porcentagem de Divisão (Treino):', value = 0.8),
                                        selectInput('metrica_KNN',
                                                    'Métrica:',
                                                    choices = c('Acurácia'='Accuracy'),
                                                                # 'Kappa'),
                                                    selected = c('Accuracy')),
                                        
                                        hr(),
                                        actionButton("computar_KNN", "Criar Modelo", class =
                                                       'btn-primary'),
                                        downloadButton("baixar_KNN", "Exportar Modelo", class = 'btn-primary')
                                      
                          ),
                          
                          mainPanel(
                            
                            fluidRow(column(
                              12,
                              h4('Conjunto de Dados Importado'),
                              h5(
                                'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                              ),
                              DT::dataTableOutput("importacao_KNN")
                              %>% withSpinner(type = 8, size = 0.5)
                              
                            )),
                            hr(),
                            
                            fluidRow(column(
                              12,
                              uiOutput('resultados_obtidos_KNN'),
                              DT::dataTableOutput("tabela_parametro_KNN")
                              %>% withSpinner(type = 8, size = 0.5),
                              verbatimTextOutput("melhor_parametro_KNN"),
                              verbatimTextOutput("melhor_metrica_KNN"),
                              
                              tags$head(tags$style(HTML("
                            #melhor_metrica_KNN {
                              font-size: 24px;
                            }
                            "))),
                              tags$head(tags$style(HTML("
                            #melhor_parametro_KNN {
                              font-size: 24px;
                            }
                            "))),
                              uiOutput('mensagem_erro_knn')
                            )),
                            
                            hr(),
                            
                            fluidRow(column(
                              6,
                          
                              plotlyOutput("graficos_KNN")
                              %>% withSpinner(type = 8, size = 0.5)
                              
                            ),
                            column(6,
                                   plotOutput("erro_KNN")
                                   %>% withSpinner(type = 8, size = 0.5))),
                                  
                            hr(),
                            fluidRow(column(
                              6,
                              DT::dataTableOutput('specifics_KNN')
                              
                              %>% withSpinner(type = 8, size = 0.5)
       
                            ),
                            column(6,
                                   plotOutput('confusao_KNN') 
                                   %>% withSpinner(type = 8, size = 0.5)
                                  
                                   
                                   )
                        
                            ),
                            hr(),
                            fluidRow(column(6,
                                           
                                            plotOutput("superficie_KNN") 
                                            %>% withSpinner(type = 8, size = 0.5)
                                            
                                            
                                        
                                            ),
                                     column(6,
                                            plotOutput("limite_KNN") 
                                            %>% withSpinner(type = 8, size = 0.5)
                                            
                                           
                                            ))
                            
                            
                          )
                          

                          ),
        tabPanel("Realizar Previsões",
                 icon = icon('cogs'),
                 
                 
                 sidebarPanel(
                   width = 3,
                   fileInput(
                     "arquivo_teste_KNN",
                     "Insira seu Dataset:",
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                   ),
                   
                   materialSwitch("mostrar_teste_KNN", "Mostrar Dataset", TRUE),
                   checkboxInput('opcao_prever_KNN',
                                 'Importar Modelo?',
                                 value = F),
                   
                   uiOutput('importar_KNN1'),
                   
                   actionButton("prever_KNN", "Calcular Previsão", class = 'btn-primary')
                   
                 ),
                 
                 
                 mainPanel(
                   DT::dataTableOutput('importacao_teste_KNN'),
                   uiOutput('resultados_obtidos_previsoes_KNN'),
                   DT::dataTableOutput('resultado_previsoes_KNN'),
                   downloadButton('download_KNN', "Download Resultados Completos", class = 'btn-primary')
                   
                 ))
        
      )
    ),
    
    tabPanel("Máquina Vetor de Suporte (SVM)",
             tabsetPanel(
               id = "tabs_SVM",
               tabPanel("Criar Modelo",
                        icon = icon('tasks'),
                        sidebarPanel( width = 3,
                                      fileInput(
                                        "arquivo_SVM",
                                        "Insira seu Dataset:",
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                      ),
                                      materialSwitch("mostrar_SVM", "Mostrar Dataset", FALSE),
                                      uiOutput('XSVM'),
                                      uiOutput('YSVM'),
                                      hr(),
                                      pickerInput(
                                        inputId = 'kernel_SVM',
                                        label = 'Selecione o Kernel',
                                        choices = c(
                                          "Polinomial" = "svmPoly",
                                          "Linear" = "svmLinear",
                                          "Radial" = "svmRadial"
                                          
                                        ),
                                        selected = "svmLinear",
                                        options = list(
                                          `actions-box` = TRUE,
                                          size = 10,
                                          `selected-text-format` = "count > 3"
                                        ),
                                        multiple = FALSE
                                      ),
                                      hr(),
                                      radioButtons(
                                        inputId = "parametros_SVM",
                                        label = "Escolher Hiperparâmetro:",
                                        choices = c("Padrão" = "Padrao",
                                                    "Escolher"),
                                        selected = "Padrao",
                                        inline = T
                                      ),
                                      uiOutput('opcoes_parametros_SVM'),
                                      
                                      hr(),
                                      radioButtons(
                                        inputId = "opcao_SVM",
                                        label = "Método de Validação Cruzada:",
                                        choices = c("Padrão" = "Padrao",
                                                    "Escolher" = "Avancado"),
                                        selected = "Padrao",
                                        inline = T
                                      ),
                                      
                                      uiOutput('opcoes_treinar_SVM'),
                                      uiOutput('opcoes_treinar2_SVM'),
                                      uiOutput('opcoes_treinar3_SVM'),
                                      numericInput('treinar_SVM', 'Porcentagem de Divisão (Treino):', value = 0.8),
                                      selectInput('metrica_SVM',
                                                  'Métrica:',
                                                  choices = c('Acurácia'='Accuracy'),
                                                              # 'Kappa' ),
                                                  selected = c('Accuracy')),
                                      hr(),
                                      actionButton("computar_SVM", "Criar Modelo", class =
                                                     'btn-primary'),
                                      downloadButton("baixar_SVM", "Exportar Modelo", class = 'btn-primary')
                                      
                        ),
                        
                        mainPanel(
                          
                          fluidRow(column(
                            12,
                            h4('Conjunto de Dados Importado'),
                            h5(
                              'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                            ),
                            DT::dataTableOutput("importacao_SVM")
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          )),
                          hr(),
                          
                          fluidRow(column(
                            12,
                            uiOutput('resultados_obtidos_SVM'),
                            DT::dataTableOutput("tabela_parametro_SVM")
                            %>% withSpinner(type = 8, size = 0.5),
                            verbatimTextOutput("melhor_parametro_SVM"),
                            verbatimTextOutput("melhor_metrica_SVM"),
                            
                            tags$head(tags$style(HTML("
                            #melhor_metrica_SVM {
                              font-size: 24px;
                            }
                            "))),
                            tags$head(tags$style(HTML("
                            #melhor_parametro_SVM {
                              font-size: 24px;
                            }
                            "))),
                            uiOutput('mensagem_erro_svm')
                          )),
                          
                          hr(),
                          
                          fluidRow(column(
                            6,
                            
                            plotlyOutput("graficos_SVM")
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          ),
                          column(6,
                                 plotOutput("erro_SVM")
                                 %>% withSpinner(type = 8, size = 0.5))),
                          
                          hr(),
                          fluidRow(column(
                            6,
                            DT::dataTableOutput('specifics_SVM')
                            
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          ),
                          column(6,
                                 plotOutput('confusao_SVM') 
                                 %>% withSpinner(type = 8, size = 0.5)
                                 
                                 
                          )
                          
                          ),
                          hr(),
                          fluidRow(column(6,
                                          
                                          plotOutput("superficie_SVM") 
                                          %>% withSpinner(type = 8, size = 0.5)
                                          
                                          
                                          
                          ),
                          column(6,
                                 plotOutput("limite_SVM") 
                                 %>% withSpinner(type = 8, size = 0.5)
                          ))
                          
                        )
                        
                        
                        ),
               tabPanel("Realizar Previsões",
                        icon = icon('cogs'),
                        
                        
                        sidebarPanel( width = 3,
                          fileInput(
                            "arquivo_teste_SVM",
                            "Insira seu Dataset:",
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                          ),
                          
                          materialSwitch("mostrar_teste_SVM", "Mostrar Dataset", TRUE),
                          checkboxInput('opcao_prever_SVM',
                                        'Importar Modelo?',
                                        value = F),
                          
                          uiOutput('importar_SVM1'),
                          
                          actionButton("prever_SVM", "Calcular Previsão", class = 'btn-primary')
                          
                        ),
                        
                        
                        mainPanel(
                          DT::dataTableOutput('importacao_teste_SVM'),
                          uiOutput('resultados_obtidos_previsoes_SVM'),
                          DT::dataTableOutput('resultado_previsoes_SVM'),
                          downloadButton('download_SVM', "Download Resultados Completos", class = 'btn-primary')
                          
                        ))
               
               
               
             )
             
             
             ),
    
    tabPanel("Árvore de Decisão",
             tabsetPanel(
               id = "tabs_ADC",
               tabPanel("Criar Modelo",
                        icon = icon('tasks'),
                        sidebarPanel( width = 3,
                                      fileInput(
                                        "arquivo_ADC",
                                        "Insira seu Dataset:",
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                      ),
                                      materialSwitch("mostrar_ADC", "Mostrar Dataset", FALSE),
                                      uiOutput('XADC'),
                                      uiOutput('YADC'),
                                      hr(),
                                      radioButtons(
                                        inputId = "parametros_ADC",
                                        label = "Escolher Hiperparâmetro:",
                                        choices = c("Padrão" ="Padrao",
                                                    "Escolher"),
                                        selected = "Padrao",
                                        inline = T
                                      ),
                                      uiOutput('opcoes_parametros_ADC'),
                                      
                                      hr(),
                                      radioButtons(
                                        inputId = "opcao_ADC",
                                        label = "Método de Validação Cruzada:",
                                        choices = c("Padrão" = "Padrao",
                                                    "Escolher" = "Avancado"),
                                        selected = "Padrao",
                                        inline = T
                                      ),
                                      
                                      uiOutput('opcoes_treinar_ADC'),
                                      uiOutput('opcoes_treinar2_ADC'),
                                      uiOutput('opcoes_treinar3_ADC'),
                                      numericInput('treinar_ADC', 'Porcentagem de Divisão (Treino):', value = 0.8), 
                                      selectInput('metrica_ADC',
                                                  'Métrica:',
                                                  choices = c('Acurácia'='Accuracy'),
                                                              # 'Kappa' ),
                                                  selected = c('Accuracy')),
                                      hr(),
                                      actionButton("computar_ADC", "Criar Modelo", class =
                                                     'btn-primary'),
                                      downloadButton("baixar_ADC", "Exportar Modelo", class = 'btn-primary')
                                      
                        ),
                        
                        mainPanel(
                          
                          fluidRow(column(
                            12,
                            h4('Conjunto de Dados Importado'),
                            h5(
                              'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                            ),
                            DT::dataTableOutput("importacao_ADC")
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          )),
                          hr(),
                          
                          fluidRow(column(
                            12,
                            uiOutput('resultados_obtidos_ADC'),
                            DT::dataTableOutput("tabela_parametro_ADC")
                            %>% withSpinner(type = 8, size = 0.5),
                            verbatimTextOutput("melhor_parametro_ADC"),
                            verbatimTextOutput("melhor_metrica_ADC"),
                            
                            tags$head(tags$style(HTML("
                            #melhor_metrica_ADC {
                              font-size: 24px;
                            }
                            "))),
                            tags$head(tags$style(HTML("
                            #melhor_parametro_ADC {
                              font-size: 24px;
                            }
                            "))),
                            uiOutput('mensagem_erro_adc')
                          )),
                          
                          hr(),
                          
                          fluidRow(column(
                            6,
                            
                            plotlyOutput("graficos_ADC")
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          ),
                          column(6,
                                 plotOutput("erro_ADC")
                                 %>% withSpinner(type = 8, size = 0.5))),
                          
                          hr(),
                          fluidRow(column(
                            6,
                            DT::dataTableOutput('specifics_ADC')
                            
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          ),
                          column(6,
                                 plotOutput('confusao_ADC') 
                                 %>% withSpinner(type = 8, size = 0.5)
                                 
                                 
                          )
                          
                          ),
                          hr(),
                          fluidRow(column(6,
                                          
                                          plotOutput("superficie_ADC") 
                                          %>% withSpinner(type = 8, size = 0.5)
                                          
                                          
                                          
                          ),
                          column(6,
                                 plotOutput("limite_ADC") 
                                 %>% withSpinner(type = 8, size = 0.5)
                          ))
                          
                        )
                        
                        
               ),
               tabPanel("Realizar Previsões",
                        icon = icon('cogs'),
                        
                        
                        sidebarPanel( width = 3,
                                      fileInput(
                                        "arquivo_teste_ADC",
                                        "Insira seu Dataset:",
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                      ),
                                      
                                      materialSwitch("mostrar_teste_ADC", "Mostrar Dataset", TRUE),
                                      checkboxInput('opcao_prever_ADC',
                                                    'Importar Modelo?',
                                                    value = F),
                                      
                                      uiOutput('importar_ADC1'),
                                      
                                      actionButton("prever_ADC", "Calcular Previsão", class = 'btn-primary')
                                      
                        ),
                        
                        
                        mainPanel(
                          DT::dataTableOutput('importacao_teste_ADC'),
                          uiOutput('resultados_obtidos_previsoes_ADC'),
                          DT::dataTableOutput('resultado_previsoes_ADC'),
                          downloadButton('download_ADC', "Download Resultados Completos", class = 'btn-primary')
                          
                        ))
               
               
               
             )
             
             
    ),
    
    tabPanel("Floresta Aleatória (Random Forest)",
             tabsetPanel(
               id = "tabs_RFC",
               tabPanel("Criar Modelo",
                        icon = icon('tasks'),
                        sidebarPanel( width = 3,
                                      fileInput(
                                        "arquivo_RFC",
                                        "Insira seu Dataset:",
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                      ),
                                      materialSwitch("mostrar_RFC", "Mostrar Dataset", FALSE),
                                      uiOutput('XRFC'),
                                      uiOutput('YRFC'),
                                      hr(),
                                      radioButtons(
                                        inputId = "parametros_RFC",
                                        label = "Escolher Parametro:",
                                        choices = c("Padrão" = "Padrao",
                                                    "Escolher"),
                                        selected = "Padrao",
                                        inline = T
                                      ),
                                      uiOutput('opcoes_parametros_RFC'),
                                      
                                      hr(),
                                      radioButtons(
                                        inputId = "opcao_RFC",
                                        label = "Método de Validação Cruzada:",
                                        choices = c("Padrão" = "Padrao",
                                                    "Escolher" = "Avancado"),
                                        selected = "Padrao",
                                        inline = T
                                      ),
                                      
                                      uiOutput('opcoes_treinar_RFC'),
                                      uiOutput('opcoes_treinar2_RFC'),
                                      uiOutput('opcoes_treinar3_RFC'),
                                     # numericInput('n_arvores_RFC', 'Numero de Arvores:', value = 500),
                                     numericInput('treinar_RFC', 'Porcentagem de Divisão (Treino):', value = 0.8), 
                                     selectInput('metrica_RFC',
                                                 'Métrica:',
                                                 choices = c('Acurácia'='Accuracy'),
                                                             # 'Kappa' ),
                                                 selected = c('Accuracy')),
                                     hr(), 
                                     actionButton("computar_RFC", "Criar Modelo", class =
                                                     'btn-primary'),
                                     downloadButton("baixar_RFC", "Exportar Modelo", class = 'btn-primary')
                                      
                        ),
                        
                        mainPanel(
                          
                          fluidRow(column(
                            12,
                            h4('Conjunto de Dados Importado'),
                            h5(
                              'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                            ),
                            DT::dataTableOutput("importacao_RFC")
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          )),
                          hr(),
                          
                          fluidRow(column(
                            12,
                            uiOutput('resultados_obtidos_RFC'),
                            DT::dataTableOutput("tabela_parametro_RFC")
                            %>% withSpinner(type = 8, size = 0.5),
                            verbatimTextOutput("melhor_parametro_RFC"),
                            verbatimTextOutput("melhor_metrica_RFC"),
                            
                            tags$head(tags$style(HTML("
                            #melhor_metrica_RFC {
                              font-size: 24px;
                            }
                            "))),
                            tags$head(tags$style(HTML("
                            #melhor_parametro_RFC {
                              font-size: 24px;
                            }
                            "))),
                            uiOutput('mensagem_erro_rfc')
                          )),
                          
                          hr(),
                          
                          fluidRow(column(
                            6,
                            
                            plotlyOutput("graficos_RFC")
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          ),
                          column(6,
                                 plotOutput("erro_RFC")
                                 %>% withSpinner(type = 8, size = 0.5))),
                          
                          hr(),
                          fluidRow(column(
                            6,
                            DT::dataTableOutput('specifics_RFC')
                            
                            %>% withSpinner(type = 8, size = 0.5)
                            
                          ),
                          column(6,
                                 plotOutput('confusao_RFC') 
                                 %>% withSpinner(type = 8, size = 0.5)
                                 
                                 
                          )
                          
                          ),
                          hr(),
                          fluidRow(column(6,
                                          
                                          plotOutput("superficie_RFC") 
                                          %>% withSpinner(type = 8, size = 0.5)
                                          
                                          
                                          
                          ),
                          column(6,
                                 plotOutput("limite_RFC") 
                                 %>% withSpinner(type = 8, size = 0.5)
                          ))
                          
                        )
                        
                        
               ),
               tabPanel("Realizar Previsões",
                        icon = icon('cogs'),
                        
                        
                        sidebarPanel( width = 3,
                                      fileInput(
                                        "arquivo_teste_RFC",
                                        "Insira seu Dataset:",
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")
                                      ),
                                      
                                      materialSwitch("mostrar_teste_RFC", "Mostrar Dataset", TRUE),
                                      checkboxInput('opcao_prever_RFC',
                                                    'Importar Modelo?',
                                                    value = F),
                                      
                                      uiOutput('importar_RFC1'),
                                      
                                      actionButton("prever_RFC", "Calcular Previsão", class = 'btn-primary')
                                      
                        ),
                        
                        
                        mainPanel(
                          DT::dataTableOutput('importacao_teste_RFC'),
                          uiOutput('resultados_obtidos_previsoes_RFC'),
                          DT::dataTableOutput('resultado_previsoes_RFC'),
                          downloadButton('download_RFC', "Download Resultados Completos", class = 'btn-primary')
                          
                        ))
             )
    ),
    tabPanel(
      "Naive Bayes",
      tabsetPanel(
        id = "tabs_NB",
        
        tabPanel("Criar Modelo",
                 icon = icon('tasks'),
                 
                 sidebarPanel( width = 3,
                               fileInput(
                                 "arquivo_NB",
                                 "Insira seu Dataset:",
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")
                               ),
                               materialSwitch("mostrar_NB", "Mostrar Dataset", FALSE),
                               uiOutput('XNB'),
                               uiOutput('YNB'),
                               hr(),
                               radioButtons(
                                 inputId = "parametros_NB",
                                 label = "Escolher Hiperparâmetro:",
                                 choices = c("Padrão" ="Padrao",
                                             "Escolher"),
                                 selected = "Padrao",
                                 inline = T
                               ),
                               uiOutput('opcoes_parametros_NB'),
                               
                               hr(),
                               radioButtons(
                                 inputId = "opcao_NB",
                                 label = "Método de Validação Cruzada:",
                                 choices = c("Padrão" = "Padrao",
                                             "Escolher" = "Avancado"),
                                 selected = "Padrao",
                                 inline = T
                               ),
                               
                               uiOutput('opcoes_treinar_NB'),
                               uiOutput('opcoes_treinar2_NB'),
                               uiOutput('opcoes_treinar3_NB'),
                               numericInput('treinar_NB', 'Porcentagem de Divisão (Treino):', value = 0.8), 
                               selectInput('metrica_NB',
                                           'Métrica:',
                                           choices = c('Acurácia'='Accuracy'),
                                                       # 'Kappa' ),
                                           selected = c('Accuracy')),
                               hr(),
                               actionButton("computar_NB", "Criar Modelo", class =
                                              'btn-primary'),
                               downloadButton("baixar_NB", "Exportar Modelo", class = 'btn-primary')
                               
                 ),
                 
                 mainPanel(
                   
                   fluidRow(column(
                     12,
                     h4('Conjunto de Dados Importado'),
                     h5(
                       'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
                     ),
                     DT::dataTableOutput("importacao_NB")
                     %>% withSpinner(type = 8, size = 0.5)
                     
                   )),
                   hr(),
                   
                   fluidRow(column(
                     12,
                     uiOutput('resultados_obtidos_NB'),
                     DT::dataTableOutput("tabela_parametro_NB")
                     %>% withSpinner(type = 8, size = 0.5),
                     verbatimTextOutput("melhor_parametro_NB"),
                     verbatimTextOutput("melhor_metrica_NB"),
                     
                     tags$head(tags$style(HTML("
                            #melhor_metrica_NB {
                              font-size: 24px;
                            }
                            "))),
                     tags$head(tags$style(HTML("
                            #melhor_parametro_NB {
                              font-size: 24px;
                            }
                            "))),
                     uiOutput('mensagem_erro_nb')
                   )),
                   
                   hr(),
                   
                   fluidRow(column(
                     6,
                     
                     plotlyOutput("graficos_NB")
                     %>% withSpinner(type = 8, size = 0.5)
                     
                   ),
                   column(6,
                          plotOutput("erro_NB")
                          %>% withSpinner(type = 8, size = 0.5))),
                   
                   hr(),
                   fluidRow(column(
                     6,
                     DT::dataTableOutput('specifics_NB')
                     
                     %>% withSpinner(type = 8, size = 0.5)
                     
                   ),
                   column(6,
                          plotOutput('confusao_NB') 
                          %>% withSpinner(type = 8, size = 0.5)
                          
                          
                   )
                   
                   ),
                   hr(),
                   fluidRow(column(6,
                                   
                                   plotOutput("superficie_NB") 
                                   %>% withSpinner(type = 8, size = 0.5)
                                   
                                   
                                   
                   ),
                   column(6,
                          plotOutput("limite_NB") 
                          %>% withSpinner(type = 8, size = 0.5)
                   ))
                   
                   
                 )
                 
                 
        ),
        tabPanel("Realizar Previsões",
                 icon = icon('cogs'),
                 
                 
                 sidebarPanel(
                   width = 3,
                   fileInput(
                     "arquivo_teste_NB",
                     "Insira seu Dataset:",
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                   ),
                   
                   materialSwitch("mostrar_teste_NB", "Mostrar Dataset", TRUE),
                   checkboxInput('opcao_prever_NB',
                                 'Importar Modelo?',
                                 value = F),
                   
                   uiOutput('importar_NB1'),
                   
                   actionButton("prever_NB", "Calcular Previsão", class = 'btn-primary')
                   
                 ),
                 
                 
                 mainPanel(
                   DT::dataTableOutput('importacao_teste_NB'),
                   uiOutput('resultados_obtidos_previsoes_NB'),
                   DT::dataTableOutput('resultado_previsoes_NB'),
                   downloadButton('download_NB', "Download Resultados Completos", class = 'btn-primary')
                   
                 ))
        
      )
    )
    
    # tabPanel("Perceptron Multi-Camadas",
    #          tabsetPanel(
    #            id = "tabs_MLPC",
    #            tabPanel("Criar Modelo",
    #                     icon = icon('tasks'),
    #                     sidebarPanel( width = 3,
    #                                   fileInput(
    #                                     "arquivo_MLPC",
    #                                     "Insira seu Dataset:",
    #                                     accept = c("text/csv",
    #                                                "text/comma-separated-values,text/plain",
    #                                                ".csv")
    #                                   ),
    #                                   materialSwitch("mostrar_MLPC", "Mostrar Dataset", FALSE),
    #                                   uiOutput('XMLPC'),
    #                                   uiOutput('YMLPC'),
    #                                  
    #                                   hr(),
    #                                   radioButtons(
    #                                     inputId = "parametros_MLPC",
    #                                     label = "Escolher Parametro:",
    #                                     choices = c("Padrão" = "Padrao",
    #                                                 "Escolher"),
    #                                     selected = "Padrao",
    #                                     inline = T
    #                                   ),
    #                                   uiOutput('opcoes_parametros_MLPC'),
    #                                   
    #                                   hr(),
    #                                   radioButtons(
    #                                     inputId = "opcao_MLPC",
    #                                     label = "Método de Validação Cruzada:",
    #                                     choices = c("Padrão" = "Padrao",
    #                                                 "Escolher" = "Avancado"),
    #                                     selected = "Padrao",
    #                                     inline = T
    #                                   ),
    #                                   
    #                                   uiOutput('opcoes_treinar_MLPC'),
    #                                   uiOutput('opcoes_treinar2_MLPC'),
    #                                   uiOutput('opcoes_treinar3_MLPC'),
    #                                   numericInput('treinar_MLPC', 'Porcentagem de Divisão (Treino):', value = 0.8),
    #                                   selectInput('metrica_MLPC',
    #                                               'Métrica:',
    #                                               choices = c('Acurácia'='Accuracy'),
    #                                                           # 'Kappa' ),
    #                                               selected = c('Accuracy')),
    #                                   hr(),
    #                                   actionButton("computar_MLPC", "Criar Modelo", class =
    #                                                  'btn-primary'),
    #                                   downloadButton("baixar_MLPC", "Exportar Modelo", class = 'btn-primary')
    #                                   
    #                     ),
    #                     
    #                     mainPanel(
    #                       
    #                       fluidRow(column(
    #                         12,
    #                         h4('Conjunto de Dados Importado'),
    #                         h5(
    #                           'Habilite a opção Mostrar Dataset para visualizar as primeiras 5 linhas de seu arquivo importado.'
    #                         ),
    #                         DT::dataTableOutput("importacao_MLPC")
    #                         %>% withSpinner(type = 8, size = 0.5)
    #                         
    #                       )),
    #                       hr(),
    #                       
    #                       fluidRow(column(
    #                         12,
    #                         uiOutput('resultados_obtidos_MLPC'),
    #                         DT::dataTableOutput("tabela_parametro_MLPC")
    #                         %>% withSpinner(type = 8, size = 0.5),
    #                         verbatimTextOutput("melhor_parametro_MLPC"),
    #                         verbatimTextOutput("melhor_metrica_MLPC"),
    #                         
    #                         tags$head(tags$style(HTML("
    #                         #melhor_metrica_MLPC {
    #                           font-size: 24px;
    #                         }
    #                         "))),
    #                         tags$head(tags$style(HTML("
    #                         #melhor_parametro_MLPC {
    #                           font-size: 24px;
    #                         }
    #                         "))),
    #                         uiOutput('mensagem_erro_mlpc')
    #                       )),
    #                       
    #                       hr(),
    #                       
    #                       fluidRow(column(
    #                         6,
    #                         
    #                         plotlyOutput("graficos_MLPC")
    #                         %>% withSpinner(type = 8, size = 0.5)
    #                         
    #                       ),
    #                       column(6,
    #                              plotOutput("erro_MLPC")
    #                              %>% withSpinner(type = 8, size = 0.5))),
    #                       
    #                       hr(),
    #                       fluidRow(column(
    #                         6,
    #                         DT::dataTableOutput('specifics_MLPC')
    #                         
    #                         %>% withSpinner(type = 8, size = 0.5)
    #                         
    #                       ),
    #                       column(6,
    #                              plotOutput('confusao_MLPC') 
    #                              %>% withSpinner(type = 8, size = 0.5)
    #                              
    #                              
    #                       )
    #                       
    #                       ),
    #                       hr(),
    #                       fluidRow(column(6,
    #                                       
    #                                       plotOutput("superficie_MLPC") 
    #                                       %>% withSpinner(type = 8, size = 0.5)
    #                                       
    #                                       
    #                                       
    #                       ),
    #                       column(6,
    #                              plotOutput("limite_MLPC") 
    #                              %>% withSpinner(type = 8, size = 0.5)
    #                       ))
    #                       
    #                     )
    #                     
    #                     
    #            ),
    #            tabPanel("Realizar Previsões",
    #                     icon = icon('cogs'),
    #                     
    #                     
    #                     sidebarPanel( width = 3,
    #                                   fileInput(
    #                                     "arquivo_teste_MLPC",
    #                                     "Insira seu Dataset:",
    #                                     accept = c("text/csv",
    #                                                "text/comma-separated-values,text/plain",
    #                                                ".csv")
    #                                   ),
    #                                   
    #                                   materialSwitch("mostrar_teste_MLPC", "Mostrar Dataset", TRUE),
    #                                   checkboxInput('opcao_prever_MLPC',
    #                                                 'Importar Modelo?',
    #                                                 value = F),
    #                                   
    #                                   uiOutput('importar_MLPC1'),
    #                                   
    #                                   actionButton("prever_MLPC", "Calcular Previsão", class = 'btn-primary')
    #                                   
    #                     ),
    #                     
    #                     
    #                     mainPanel(
    #                       DT::dataTableOutput('importacao_teste_MLPC'),
    #                       uiOutput('resultados_obtidos_previsoes_MLPC'),
    #                       DT::dataTableOutput('resultado_previsoes_MLPC'),
    #                       downloadButton('download_MLPC', "Download Resultados Completos", class = 'btn-primary')
    #                       
    #                     ))
    #            
    #            
    #            
    #          )
    #          
    #          
    # )
    
    
  ),
  tabPanel('Sobre',
           icon = icon('info'),
           includeMarkdown('Sobre.Rmd'))
  
  )
            
            server <- function(input, output, session) {
             
              ########### REGRESSAO LINEAR ##############
              
              
              
              dados_LR1 <- reactive({
                inFile <- input$arquivo_LR
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T, stringsAsFactors = T)
                
              })
              
              dados_LR <- reactive({
                req(dados_LR1())
                dados_LR1()[complete.cases(dados_LR1()),]
              })
              
              output$importacao_LR <- DT::renderDataTable(
                if (input$mostrar_LR == TRUE) {
                  DT::datatable(dados_LR(), options = list(pageLength = 5,
                                                           scrollX = TRUE,
                                                           searching = FALSE))
                  
                
              })
              
              
              output$opcoes_treinar_LR <- renderUI({
                if (input$opcao_LR == "Avancado") {
                  selectInput(
                    "metodo_LR",
                    "Selecione o Método",
                    choices = c(
                     
                      "Validação Cruzada k-fold" = "cv",
                      "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                    ),
                    selected = "cv"
                  )
                  
                }
                
              })
              
              output$opcoes_treinar2_LR <- renderUI({
                if (input$opcao_LR == "Avancado") {
                  selectInput(
                    "numero_LR",
                    "Número K de Subconjuntos",
                    choices = c(3, 5, 7, 10),
                    selected = 10
                  )
                  
                  
                }
                
              })
              
              output$opcoes_treinar3_LR <- renderUI({
                req(input$metodo_LR)
                if (input$metodo_LR == "repeatedcv") {
                  selectInput(
                    "repeticoes_LR",
                    "Número de Repetições",
                    choices = c(3, 5, 7),
                    selected = 3
                  )
                }
                
              })
              
              output$XLR <- renderUI({
                nomes <- names(dados_LR())
                pickerInput(
                  'XLR2',
                  'Variáveis Independentes (X - Atributos)',
                  nomes,
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
                )
              })
              
              output$YLR <- renderUI({
                nomes <- names(dados_LR())
                selectInput('YLR2', 'Variável Dependente (y) - Alvo:', nomes)
              })
              
              formula_LR <- reactive({
                x <- input$XLR2
                y <- input$YLR2
                
                formula <- c(x, y)
                
                size <- length(formula)
                independente <- formula[-size]
                dependente <- formula[size]
                formula_final <-
                  reformulate(independente, response = dependente)
                formula_final
              })
              
              split_LR <- eventReactive(input$computar_LR,
                                        {
                                          set.seed(123)
                                          y <- input$YLR2
                                          createDataPartition(dados_LR()[, y], p = input$treinar_LR, list =
                                                                FALSE)
                                          
                                        })
              
              treino_LR <- eventReactive(input$computar_LR,
                                         {
                                           dados_LR()[split_LR(), ]
                                           
                                         })
              
              teste_LR <- eventReactive(input$computar_LR,
                                        {
                                          dados_LR()[-split_LR(), ]
                                          
                                        })
              
              
              
              fit_LR <- eventReactive(input$computar_LR, {
                tryCatch(
               withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
              
                
                if (input$opcao_LR == "Padrao") {
                 
                  train(
                    formula_LR(),
                    data = treino_LR(),
                    method = "lm",
                    metric = input$metrica_LR,
                    preProcess = c('center', 'scale'),
                    na.action = 'na.omit'
                  )
                  
                }
                
                else{
                  if (input$metodo_LR == "repeatedcv") {
                    ctrl <-
                      trainControl(
                        method = input$metodo_LR ,
                        number = as.numeric(input$numero_LR),
                        repeats = input$repeticoes_LR
                      )
                    
                    train(
                      formula_LR(),
                      data = treino_LR(),
                      method = "lm",
                      metric = input$metrica_LR,
                      trControl = ctrl ,
                      preProcess = c('center', 'scale'),
                      na.action = 'na.omit'
                    )
                    
                  } else{
                    ctrl <-
                      trainControl(method = input$metodo_LR ,
                                   number = as.numeric(input$numero_LR))
                    
                    train(
                      formula_LR(),
                      data = treino_LR(),
                      method = "lm",
                      metric = input$metrica_LR,
                      trControl = ctrl,
                      preProcess = c('center', 'scale'),
                      na.action = 'na.omit'
                    )
                  }
                  
                }
                }), error= function(e){return(NA)})
              })
              
              
              output$tabela_coeficientes_LR <- DT::renderDataTable({
                req(fit_LR())
                DT::datatable(gerarTabelaCoeficientes(fit_LR()) , options = list(pageLength = 5,
                                                                scrollX = TRUE,
                                                                searching = FALSE)) 
          
              })
              
              output$tabela_parametro_LR <- DT::renderDataTable({
                req(fit_LR())
                dat <- fit_LR()$results
                dat <- dat[1:(length(dat)-3)]
                DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                
              })
              
              n_independentes <- reactive({
                length(input$XLR2)
              })
              
          
         
              
              output$graficos_LR <- renderPlotly({
                
                
                req(fit_LR())
                if (n_independentes() > 1) {
                  graficarImportancia(fit_LR())
                }
                else{
                 
                   graficarTeste(fit_LR(), teste_LR(), treino_LR(), input$XLR2, input$YLR2)
                    
                  
                }
              })
              
              output$nomes_treino_LR <- renderUI({
                req(fit_LR())
                if(n_independentes() >= 2){
                selectInput("nomes_LR",
                            "Selecione a Variável:",
                            choices = input$XLR2)
                  
                }
                
              })
              
             
              output$graficos_multiplos_LR <- renderPlotly({
                req(fit_LR())
                
                if (n_independentes() > 1) {
                  #graficarMultiplos(treino_resid_LR(),x,input$YLR2,predicted,iv)
               
                  graficarTesteMultiplo(teste_LR(),  input$nomes_LR, input$YLR2)
                  
                }
               # else{ggplot() + ggtitle('Visualização disponível de 2 a 6 variáveis independentes.')}
                
              })
              
              
              # output$residuos_LR <- renderPlotly({
              #   req(fit_LR())
              #   
              #   Valor.Ajustado <- fitted(fit_LR())
              #   Valor.Residual <- resid(fit_LR())
              #   data = as.data.frame(Valor.Ajustado, Valor.Residual)
              #   
              #   p <- ggplot(data = data, aes(x = Valor.Ajustado, y = Valor.Residual)) + geom_point() +
              #     geom_smooth(aes(colour = Valor.Ajustado)) + xlab('Valores Ajustados') + ylab('Resíduos') + ggtitle('Gráfico Residual') +
              #     theme_bw() +  theme(
              #       axis.text=element_text(size=12),
              #       axis.title=element_text(size=18),
              #       plot.title = element_text(size=20, face = "bold"))
              #   
              #   ggplotly(p)
              # 
              # })
              # 
              dados_teste_LR <- reactive({
                inFile <- input$arquivo_teste_LR
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              
              output$importacao_teste_LR <- DT::renderDataTable(
                if (input$mostrar_teste_LR == TRUE) {
                  DT::datatable(dados_teste_LR(), options = list(pageLength = 5,
                                                                 scrollX = TRUE,
                                                                 searching = FALSE))
     
              })
              previsao_LR <- reactive({
                req(fit_importado_LR())
                tryCatch(
                  
                  predict(object = fit_importado_LR(), newdata = dados_teste_LR())
                  ,error = function(e) { return(rep('NA', nrow(dados_teste_LR()))) }
                )
                
              })         
              tabela_previsoes_LR <- eventReactive(input$prever_LR, {
                
                if(input$opcao_prever_LR == TRUE){
                  Resultado_Modelo = previsao_LR()
                  cbind(dados_teste_LR(), Resultado_Modelo)
                
                  
                }else{
               
                Resultado_Modelo = predict(fit_LR(), dados_teste_LR())
                data = cbind(dados_teste_LR(), Resultado_Modelo)
                data
                }
                
              })
              
              output$resultado_previsoes_LR <- DT::renderDataTable(
                DT::datatable(tabela_previsoes_LR(), options = list(pageLength = 5,
                                                                    scrollX = TRUE,
                                                                    searching = FALSE))
                
              )
              
              output$download_LR <- downloadHandler(
                filename = function() {
                  "resultados_previsoes.xlsx"
                },
                content = function(fname) {
                  write.xlsx(tabela_previsoes_LR(), fname)
                }
              )
              
              output$baixar_LR <- downloadHandler(
          
                filename = function() {
                  "modelo_reglin.rds"
                },
                content = function(fname) {
                  withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
                  saveRDS(fit_LR(), fname)
                  })
                }
              )
          
              output$importar_LR1 <- renderUI({
                if(input$opcao_prever_LR == TRUE)
                  tagList(
                fileInput(
                  "importar_LR",
                  "Carregue seu modelo:",
                  accept = c(".rds")
                ),
                helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
                  )
              })
              
              
              fit_importado_LR <- reactive({
                inFile <- input$importar_LR
                
                if (is.null(inFile))
                  return(NULL)
                
                readRDS(inFile$datapath)
                
              })
              
              output$mensagem_erro_lr <- renderUI({
                if(is.na(fit_LR()))
                  includeMarkdown('Mensagem_Erro.Rmd')
              })    
              
              output$coeficientes_LR <- renderUI({
                req(fit_LR())
                h5("Coeficientes do Modelo")
                
              })
              
              output$resultados_obtidos_LR <- renderUI({
                req(fit_LR())
                h3("Resultados e Métricas do Modelo")
                
              }
            
            )
              
              output$resultados_obtidos_previsoes_LR <- renderUI({
                req(tabela_previsoes_LR())
                h3("Resultados da Previsão")
                
              })
              
              
   #################### INICIO SUPPORT VECTOR REGRESSAO ########
              dados_SV1 <- reactive({
                inFile <- input$arquivo_SV
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              
              dados_SV <- reactive({
                req(dados_SV1())
                dados_SV1()[complete.cases(dados_SV1()),]
              })
              
              output$importacao_SV <- DT::renderDataTable({
                if (input$mostrar_SV == TRUE) {
                  DT::datatable(dados_SV(), options = list(pageLength = 5,
                                                           scrollX = TRUE,
                                                           searching = FALSE))
                  
                }
              })
              
              output$XSV <- renderUI({
                nomes <- names(dados_SV())
                pickerInput(
                  'XSV2',
                  'Variáveis Independentes (X) - Atributos',
                  nomes,
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
                )
              })
              
              output$YSV <- renderUI({
                nomes <- names(dados_SV())
                selectInput('YSV2', 'Variável Dependente (y) - Alvo:', nomes)
              })
              
              formula_SV <- reactive({
                x <- input$XSV2
                y <- input$YSV2
                
                formula <- c(x, y)
                
                size <- length(formula)
                independente <- formula[-size]
                dependente <- formula[size]
                formula_final <-
                  reformulate(independente, response = dependente)
                formula_final
              })
              
              split_SV <- eventReactive(input$computar_SV,
                                        {
                                          set.seed(123)
                                          y <- input$YSV2
                                          createDataPartition(dados_SV()[, y], p = input$treinar_SV, list =
                                                                FALSE)
                                          
                                        })
              
              treino_SV <- eventReactive(input$computar_SV,
                                         {
                                           dados_SV()[split_SV(), ]
                                           
                                         })
              
              teste_SV <- eventReactive(input$computar_SV,
                                        {
                                          dados_SV()[-split_SV(), ]
                                          
                                        })
              
              output$opcoes_treinar_SV <- renderUI({
                if (input$opcao_SV == "Avancado") {
                  selectInput(
                    "metodo_SV",
                    "Selecione o Método",
                    choices = c(
                     
                      "Validação Cruzada k-fold" = "cv",
                      "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                    ),
                    selected = "cv"
                  )
                  
                }
              })
              
              output$opcoes_treinar2_SV <- renderUI({
                if (input$opcao_SV == "Avancado") {
                  selectInput(
                    "numero_SV",
                    "Número K de Subconjuntos",
                    choices = c(3, 5, 7, 10),
                    selected = 10
                  )
                }
                
              })
              
              output$opcoes_treinar3_SV <- renderUI({
                req(input$metodo_SV)
                if (input$metodo_SV == "repeatedcv") {
                  selectInput(
                    "repeticoes_SV",
                    "Número de Repetições",
                    choices = c(3, 5, 7),
                    selected = 3
                  )
                }
                
              })
              
              output$opcoes_parametros_SV <- renderUI({
                
                if(input$parametros_SV == 'Escolher')
                {
                  if (input$kernel_SV == "svmLinear") {
                  numericInput('opcoes_parametros_SV_Linear',
                               'C:',
                               value = 0.1,
                               min = 0.1,
                               max = 10)
                }else if(input$kernel_SV == "svmRadial")
                {
                  tagList(
                  numericInput('opcoes_parametros_SV_Radial_C',
                               'C:',
                               value = 0.1,
                               min = 0.1,
                               max = 10),
                  numericInput('opcoes_parametros_SV_Radial_Sigma',
                               'Sigma:',
                               value = 0.25,
                               min = 0.1,
                               max = 2)
                  
                  
                  )
                  
                }else if(input$kernel_SV == "svmPoly"){
                  
                  tagList(
                    numericInput('opcoes_parametros_SV_Poly_C',
                                 'C:',
                                 value = 0.1,
                                 min = 0.1,
                                 max = 10),
                    numericInput('opcoes_parametros_SV_Poly_Scale',
                                 'Scale:',
                                 value = 0.25,
                                 min = 0.1,
                                 max = 2),
                    numericInput('opcoes_parametros_SV_Poly_Degree',
                                 'Degree:',
                                 value = 1,
                                 min = 1,
                                 max = 5)
                  )
                  
                }
                  
                }
              })
              
              
              grid_SV <- reactive({
                
                if(input$parametros_SV == 'Padrao'){
                  
                  if (input$kernel_SV == "svmLinear") {
                    
                    expand.grid(C = c(0.1,  0.5, 1, 1.5, 2))
                    
                    
                  }else if(input$kernel_SV == "svmRadial")
                  {
                    expand.grid(sigma = c(
                      0.25,0.5,0.75, 0.9),
                      C = c( 0.1,  0.5, 1, 1.5, 2))  
                      
                  }else if(input$kernel_SV == "svmPoly"){
                    
                    expand.grid(scale = c(
                      0.25,0.5,0.75, 0.9),
                      C = c( 0.1,  0.5, 1, 1.5, 2),
                      degree = c(1,2,3))
                    
                  }
                  
                }
                else{
                  
                  if (input$kernel_SV == "svmLinear") {
                    
                    grid <- expand.grid(C = c(input$opcoes_parametros_SV_Linear))
                    grid
                    
                  }else if(input$kernel_SV == "svmRadial")
                  {
                    expand.grid(sigma = c(
                      input$opcoes_parametros_SV_Radial_Sigma),
                      C = c(input$opcoes_parametros_SV_Radial_C))  
                    
                    
                  }else if(input$kernel_SV == "svmPoly"){
                    
                    expand.grid(scale = c(
                      input$opcoes_parametros_SV_Poly_Scale),
                      C = c(input$opcoes_parametros_SV_Poly_C),
                      degree = c(input$opcoes_parametros_SV_Poly_Degree))  
                  }
                   
                }
                
 
              })
              
              fit_SV <- eventReactive(input$computar_SV, {
                tryCatch(
               withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
          
                if (input$opcao_SV == "Padrao") {
                  if (input$kernel_SV == "svmLinear") {
                    train(
                      formula_SV(),
                      data = treino_SV(),
                      method = input$kernel_SV,
                      tuneGrid = grid_SV(),
                      metric = input$metrica_SV,
                      #tuneLength = 10,
                      preProcess = c('center', 'scale'),
                      na.action = 'na.omit'
                    )
                  }
                  else if(input$kernel_SV == "svmRadial"){
                    train(
                      formula_SV(),
                      data = treino_SV(),
                      method = input$kernel_SV,
                      tuneGrid = grid_SV(),
                      metric = input$metrica_SV,
                      #tuneLength = 10,
                      
                      preProcess = c('center', 'scale'),
                      na.action = 'na.omit'
                    )
                    
                  }
                  else{
                    train(
                      formula_SV(),
                      data = treino_SV(),
                      method = input$kernel_SV,
                      tuneGrid = grid_SV(),
                      metric = input$metrica_SV,
                      #tuneLength = 10,
                      
                      preProcess = c('center', 'scale'),
                      na.action = 'na.omit'
                    )
                    
                  }
                  
                }
                
                else{
                  if (input$metodo_SV == "repeatedcv") {
                    if (input$kernel_SV == "svmLinear") {
                      ctrl <-
                        trainControl(
                          method = input$metodo_SV ,
                          number = as.numeric(input$numero_SV),
                          repeats = input$repeticoes_SV
                        )
                      train(
                        formula_SV(),
                        data = treino_SV(),
                        method = input$kernel_SV,
                        tuneGrid = grid_SV(),
                        metric = input$metrica_SV,
                       # tuneLength = 10,
                        trControl = ctrl,
                        preProcess = c('center', 'scale'),
                        na.action = 'na.omit'
                      )
                    }
                    else if (input$kernel_SV == "svmRadial"){
                      ctrl <-
                        trainControl(
                          method = input$metodo_SV ,
                          number = as.numeric(input$numero_SV),
                          repeats = input$repeticoes_SV
                        )
                      train(
                        formula_SV(),
                        data = treino_SV(),
                        method = input$kernel_SV,
                        trControl = ctrl,
                        metric = input$metrica_SV,
                        tuneGrid = grid_SV(),
                        #tuneLength = 10,
                        
                        preProcess = c('center', 'scale'),
                        na.action = 'na.omit'
                      )
                      
                    } else {
                      
                      ctrl <-
                        trainControl(
                          method = input$metodo_SV ,
                          number = as.numeric(input$numero_SV),
                          repeats = input$repeticoes_SV)
                          
                      train(
                        formula_SV(),
                        data = treino_SV(),
                        method = input$kernel_SV,
                        tuneGrid = grid_SV(),
                        metric = input$metrica_SV,
                        #tuneLength = 10,
                        
                        preProcess = c('center', 'scale'),
                        na.action = 'na.omit'
                      )
                    }
                    
                    
                  } else{
                    if (input$kernel_SV == "svmLinear") {
                      ctrl <-
                        trainControl(method = input$metodo_SV ,
                                     number = as.numeric(input$numero_SV))
                      train(
                        formula_SV(),
                        data = treino_SV(),
                        method = input$kernel_SV,
                        tuneGrid = grid_SV(),
                        metric = input$metrica_SV,
                        #tuneLength = 10,
                        trControl = ctrl,
                        preProcess = c('center', 'scale'),
                        na.action = 'na.omit'
                      )
                    }
                    else if(input$kernel_SV == "svmRadial"){
                      ctrl <-
                        trainControl(method = input$metodo_SV ,
                                     number = as.numeric(input$numero_SV))
                      train(
                        formula_SV(),
                        data = treino_SV(),
                        method = input$kernel_SV,
                        metric = input$metrica_SV,
                        tuneGrid = grid_SV(),
                        #tuneLength = 10,
                        trControl = ctrl,
                        preProcess = c('center', 'scale'),
                        na.action = 'na.omit'
                      )
                      
                    }
                    else{
                      
                      ctrl <-
                        trainControl(method = input$metodo_SV ,
                                     number = as.numeric(input$numero_SV))
                      train(
                        formula_SV(),
                        data = treino_SV(),
                        method = input$kernel_SV,
                        metric = input$metrica_SV,
                        tuneGrid = grid_SV(),
                       
                        #tuneLength = 10,
                        
                        preProcess = c('center', 'scale'),
                        na.action = 'na.omit'
                      )
                      
                    }
                  }
                  
                }
               }), error= function(e){return(NA)})
              })
              
              n_independentes_SV <- reactive({
                length(input$XSV2)
              })
              
              
              
              output$graficos_SV <- renderPlotly({
                req(fit_SV())
                if (n_independentes_SV() > 1) {
                 graficarImportancia(fit_SV())
                }
                else{
                 
                    graficarTeste(fit_SV(), teste_SV(), treino_SV(), input$XSV2, input$YSV2)
                    
                  
                  
                }
                
              })
              
              
              # treino_resid_SV <- reactive({
              #   req(fit_SV())
              #   d <- treino_SV()
              #   predicted <- predict(fit_SV(), newdata = treino_SV())
              #   d <- as.data.frame(cbind(d, predicted))
              #   d <- gather(d, key = 'iv', value = 'x', input$XSV2)
              #   d
              # })
              
              output$nomes_treino_SV <- renderUI({
                req(fit_SV())
                if(n_independentes_SV() >= 2){
                  selectInput("nomes_SV",
                              "Selecione a Variável:",
                              choices = input$XSV2)
                  
                }
                
              })
              
              output$graficos_multiplos_SV <- renderPlotly({
               
                  req(fit_SV())
                  
                  if (n_independentes_SV() > 1) {
                    #graficarMultiplos(treino_resid_SV(),x,input$YSV2,predicted,iv)
                    
                    graficarTesteMultiplo(teste_SV(),  input$nomes_SV, input$YSV2)
                    
                  }
                  # else{ggplot() + ggtitle('Visualização disponível de 2 a 6 variáveis independentes.')}
              
                
              })

              output$residuos_SV <- renderPlot({
                req(fit_SV())
                if(input$parametros_SV == 'Padrao'){
                  plot(fit_SV(),lwd = 4 ,ylab = input$metrica_SV,main = paste0(input$metrica_SV, ' x HiperParâmetro'), cex = 2)} else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
                
              })
              
              
              output$tabela_parametro_SV <- DT::renderDataTable({
                req(fit_SV())
                dat <- round(fit_SV()$results,3)
                dat <- dat[1:(length(dat)-4)]
                DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
              })
              
              output$melhor_parametro_SV <- renderPrint({
                req(fit_SV())

                  cat(paste0(paste0("Melhor valor encontrado para ", colnames(fit_SV()$bestTune), " foi: ",fit_SV()$bestTune,"\n")) )

              })
              
              output$melhor_metrica_SV <- renderPrint({
                req(fit_SV())
                if(input$metrica_SV == "Rsquared"){
                cat(paste0( "Com R² (Rsquared) de: ",round(max(fit_SV()$results['Rsquared']),5)))
                }else{
                  cat(paste0( "Com Raíz do Erro Quadrático Médio (RMSE) de: ",round(min(fit_SV()$results['RMSE']),5)))
                }
              })
              
              dados_teste_SV <- reactive({
                inFile <- input$arquivo_teste_SV
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              output$importacao_teste_SV <- DT::renderDataTable(
                if (input$mostrar_teste_SV== TRUE) {
                  DT::datatable(dados_teste_SV(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                })
              
              previsao_SV <- reactive({
                req(fit_importado_SV())
                tryCatch(
                  
                  predict(object = fit_importado_SV(), newdata = dados_teste_SV())
                  ,error = function(e) { return(rep('NA', nrow(dados_teste_SV()))) }
                )
                
              })
              tabela_previsoes_SV <- eventReactive(input$prever_SV, {
                if(input$opcao_prever_SV == TRUE){
                  Resultado_Modelo = previsao_SV()
                  cbind(dados_teste_SV(), Resultado_Modelo)
                
                
                }else{
                  
                  Resultado_Modelo = predict(fit_SV(), dados_teste_SV())
                  data = cbind(dados_teste_SV(), Resultado_Modelo)
                  data
                }
              })
              
              output$resultado_previsoes_SV <- DT::renderDataTable(
                
                DT::datatable(tabela_previsoes_SV(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                
              )
              
              output$download_SV <- downloadHandler(
                filename = function() {
                  "resultados_previsoes.xlsx"
                },
                content = function(fname) {
                  write.xlsx(tabela_previsoes_SV(), fname)
                }
              )
              
              output$baixar_SV <- downloadHandler(
                
                filename = function() {
                  "modelo_svm_reg.rds"
                },
                content = function(fname) {
                  withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
                  incProgress(1/10)
                  saveRDS(fit_SV(), fname)
                  })
                }
              )
              
              output$importar_SV1 <- renderUI({
                if(input$opcao_prever_SV == TRUE)
                  tagList(
                  fileInput(
                    "importar_SV",
                    "Carregue seu modelo:",
                    accept = c(".rds")
                  ),
                  helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
                  )
                
              })
              
              
              fit_importado_SV <- reactive({
                inFile <- input$importar_SV
                
                if (is.null(inFile))
                  return(NULL)
                
                readRDS(inFile$datapath)
                
              })
              
              output$mensagem_erro_sv <- renderUI({
                if(is.na(fit_SV()))
                  includeMarkdown('Mensagem_Erro.Rmd')
              })
              output$resultados_obtidos_SV <- renderUI({
                req(fit_SV())
                h3("Resultados e Métricas do Modelo")
                
              })
              
              output$resultados_obtidos_previsoes_SV <- renderUI({
                req(tabela_previsoes_SV())
                h3("Resultados da Previsão")
                
              })
              
              
             
  ############################# FIM SVM REGRESSAO #############################
              
  ############### INICIO Ãrvore de Decisão REGRESSAO ##################
              dados_AD1 <- reactive({
                inFile <- input$arquivo_AD
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              
              dados_AD <- reactive({
                req(dados_AD1())
                dados_AD1()[complete.cases(dados_AD1()),]
              })
              
              output$importacao_AD <- DT::renderDataTable({
                if (input$mostrar_AD == TRUE) {
                  DT::datatable(dados_AD(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                }
              })
              
              output$XAD <- renderUI({
                nomes <- names(dados_AD())
                pickerInput(
                  'XAD2',
                  'Variáveis Independentes (X) - Atributos',
                  nomes,
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
                )
              })
              
              output$YAD <- renderUI({
                nomes <- names(dados_AD())
                selectInput('YAD2', 'Variável Dependente (y) - Alvo:', nomes)
              })
              
              formula_AD <- reactive({
                x <- input$XAD2
                y <- input$YAD2
                
                formula <- c(x, y)
                
                size <- length(formula)
                independente <- formula[-size]
                dependente <- formula[size]
                formula_final <-
                  reformulate(independente, response = dependente)
                formula_final
              })
              
              split_AD <- eventReactive(input$computar_AD,
                                        {
                                          set.seed(123)
                                          y <- input$YAD2
                                          createDataPartition(dados_AD()[, y], p = input$treinar_AD, list =
                                                                FALSE)
                                          
                                        })
              
              treino_AD <- eventReactive(input$computar_AD,
                                         {
                                           dados_AD()[split_AD(), ]
                                           
                                         })
              
              teste_AD <- eventReactive(input$computar_AD,
                                        {
                                          dados_AD()[-split_AD(), ]
                                          
                                        })
              
              output$opcoes_treinar_AD <- renderUI({
                if (input$opcao_AD == "Avancado") {
                  selectInput(
                    "metodo_AD",
                    "Selecione o Método",
                    choices = c(
                    
                      "Validação Cruzada k-fold" = "cv",
                      "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                    ),
                    selected = "cv"
                  )
                  
                }
              })
              
              output$opcoes_treinar2_AD <- renderUI({
                if (input$opcao_AD == "Avancado") {
                  selectInput(
                    "numero_AD",
                    "Número K de Subconjuntos",
                    choices = c(3, 5, 7, 10),
                    selected = 10
                  )
                }
                
              })           
              
              output$opcoes_treinar3_AD <- renderUI({
                req(input$metodo_AD)
                if (input$metodo_AD == "repeatedcv") {
                  selectInput(
                    "repeticoes_AD",
                    "Número de Repetições",
                    choices = c(3, 5, 7),
                    selected = 3
                  )
                }
                
              })
              output$opcoes_parametros_AD <- renderUI({
              
                if(input$parametros_AD == 'Escolher')
                  {
                numericInput('opcoes_parametros_AD2',
                             'Max Depth:',
                             value = 5,
                             min = 2,
                             max = 40)
                }
                  
              })
              
              
              grid_AD <- reactive({
                if(input$parametros_AD == 'Padrao'){
                  expand.grid(.maxdepth=seq(5,20,5))
                  
                }
                else{
                  expand.grid(.maxdepth=input$opcoes_parametros_AD2)
                   
                }
                
              })
              
              fit_AD <- eventReactive(input$computar_AD, {
               tryCatch(
                withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
              
                if (input$opcao_AD == "Padrao") {
                
                  train(
                    formula_AD(),
                    data = treino_AD(),
                    method = "rpart2",
                    metric = input$metrica_AD,
                    tuneGrid = grid_AD(),
                    na.action = 'na.omit'
                  )
                  
                }
                
                else{
                  if (input$metodo_AD == "repeatedcv") {
                    ctrl <-
                      trainControl(
                        method = input$metodo_AD ,
                        number = as.numeric(input$numero_AD),
                        repeats = input$repeticoes_AD
                      )
                    
                    train(
                      formula_AD(),
                      data = treino_AD(),
                      method = "rpart2",
                      trControl = ctrl ,
                      tuneGrid = grid_AD(),
                      metric = input$metrica_AD,
                      na.action = 'na.omit'
                    )
                    
                  } else{
                    ctrl <-
                      trainControl(method = input$metodo_AD ,
                                   number = as.numeric(input$numero_AD))
                    
                    train(
                      formula_AD(),
                      data = treino_AD(),
                      method = "rpart2",
                      tuneGrid = grid_AD(),
                      metric = input$metrica_AD,
                      trControl = ctrl,
                
                      na.action = 'na.omit'
                    )
                  }
                  
                }
                }), error= function(e){return(NA)})
                
              })
              
              
              n_independentes_AD <- reactive({
                length(input$XAD2)
              })
              
            
              
              output$graficos_AD <- renderPlotly({
                req(fit_AD())
                if (n_independentes_AD() > 1) {
                  graficarImportancia(fit_AD())
                }
                else{
              
                    graficarTeste(fit_AD(), teste_AD(), treino_AD(), input$XAD2, input$YAD2)
                    
                  
                  
                }
                
              })
              
              output$nomes_treino_AD <- renderUI({
                req(fit_AD())
                if(n_independentes_AD() >= 2){
                  selectInput("nomes_AD",
                              "Selecione a Variável:",
                              choices = input$XAD2)
                  
                }
                
              })
              
              # 
              # treino_resid_AD <- reactive({
              #   
              #   d <- treino_AD()
              #   predicted <- predict(fit_AD(), newdata = treino_AD())
              #   d <- as.data.frame(cbind(d, predicted))
              #   d <- gather(d, key = 'iv', value = 'x', input$XAD2)
              #   d
              # })
              
              
              output$graficos_multiplos_AD <- renderPlotly({
                req(fit_AD())
                
                if (n_independentes_AD() > 1) {
                  #graficarMultiplos(treino_resid_AD(),x,input$YAD2,predicted,iv)
                  
                  graficarTesteMultiplo(teste_AD(), input$nomes_AD, input$YAD2)
                  
                }
                # else{ggplot() + ggtitle('Visualização disponível de 2 a 6 variáveis independentes.')}
                
              })
              
              output$residuos_AD <- renderPlot({
                req(fit_AD())
                if(input$parametros_AD == 'Padrao'){
                plot(fit_AD(),lwd = 4 ,ylab = input$metrica_AD ,main = paste0(input$metrica_AD, ' x HiperParâmetro'), cex = 2)} else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
                
              })
              
              
              output$tabela_parametro_AD <- DT::renderDataTable({
                req(fit_AD())
                dat <- round(fit_AD()$results,3)
                dat <- dat[1:(length(dat)-4)]
                DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                
              })
              
              output$melhor_parametro_AD <- renderPrint({
                req(fit_AD())
                
                cat(paste0(paste0("Melhor valor encontrado para ", colnames(fit_AD()$bestTune), " foi: ",fit_AD()$bestTune,"\n")) )
                
              })
              
              output$melhor_metrica_AD <- renderPrint({
                req(fit_AD())
                if(input$metrica_AD == "Rsquared"){
                  cat(paste0( "Com R² (Rsquared) de: ",round(max(fit_AD()$results['Rsquared']),5)))
                }else{
                  cat(paste0( "Com Raíz do Erro Quadrático Médio (RMSE) de: ",round(min(fit_AD()$results['RMSE']),5)))
                }
              })
              dados_teste_AD <- reactive({
                inFile <- input$arquivo_teste_AD
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              output$importacao_teste_AD <- DT::renderDataTable(
                if (input$mostrar_teste_AD== TRUE) {
                  DT::datatable(dados_teste_AD(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                })
              previsao_AD <- reactive({
                req(fit_importado_AD())
                tryCatch(
                  
                  predict(object = fit_importado_AD(), newdata = dados_teste_AD())
                  ,error = function(e) { return(rep('NA', nrow(dados_teste_AD()))) }
                )
                
              })
              
              tabela_previsoes_AD <- eventReactive(input$prever_AD, {
                
                if(input$opcao_prever_AD == TRUE){
                 
                  Resultado_Modelo = previsao_AD()
                  cbind(dados_teste_AD(), Resultado_Modelo)
             
                  
                }else{
                  
                  Resultado_Modelo = predict(fit_AD(), dados_teste_AD())
                  data = cbind(dados_teste_AD(), Resultado_Modelo)
                  data
                }
   
              })
              
              output$resultado_previsoes_AD <- DT::renderDataTable(
                
                DT::datatable(tabela_previsoes_AD(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                
              )
              
              output$download_AD <- downloadHandler(
                filename = function() {
                  "resultados_previsoes.xlsx"
                },
                content = function(fname) {
                  write.xlsx(tabela_previsoes_AD(), fname)
                }
              )
              
              output$baixar_AD <- downloadHandler(
                
                filename = function() {
                  "modelo_adreg.rds"
                },
                content = function(fname) {
                  withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
                    incProgress(1/10)
                    saveRDS(fit_AD(), fname)
                  })
                }
              )
              
              
              output$importar_AD1 <- renderUI({
                if(input$opcao_prever_AD == TRUE)
                  tagList(
                  fileInput(
                    "importar_AD",
                    "Carregue seu modelo:",
                    accept = c(".rds")
                  ),
                  helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
                  )
                
              })
              
              
              fit_importado_AD <- reactive({
                inFile <- input$importar_AD
                
                if (is.null(inFile))
                  return(NULL)
                
                readRDS(inFile$datapath)
                
              })
              
              
              output$mensagem_erro_ad <- renderUI({
                if(is.na(fit_AD()))
                  includeMarkdown('Mensagem_Erro.Rmd')
              })
              
              output$resultados_obtidos_AD <- renderUI({
                req(fit_AD())
                h3("Resultados e Métricas do Modelo")
                
              })
              output$resultados_obtidos_previsoes_AD <- renderUI({
                req(tabela_previsoes_AD())
                h3("Resultados da Previsão")
                
              })
              
              
              
              
 ###################### FIM Ãrvore de Decisão ###################
              ########### INICIO Floresta Aleatória (Random Forest) ##############
              
              dados_RF1 <- reactive({
                inFile <- input$arquivo_RF
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              
              dados_RF <- reactive({
                req(dados_RF1())
                dados_RF1()[complete.cases(dados_RF1()),]
              })
              
              output$importacao_RF <- DT::renderDataTable({
                if (input$mostrar_RF == TRUE) {
                  DT::datatable(dados_RF(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                }
              })
              
              output$XRF <- renderUI({
                nomes <- names(dados_RF())
                pickerInput(
                  'XRF2',
                  'Variáveis Independentes (X) - Atributos',
                  nomes,
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
                )
              })
              
              output$YRF <- renderUI({
                nomes <- names(dados_RF())
                selectInput('YRF2', 'Variável Dependente (y) - Alvo:', nomes)
              })
              
              formula_RF <- reactive({
                x <- input$XRF2
                y <- input$YRF2
                
                formula <- c(x, y)
                
                size <- length(formula)
                independente <- formula[-size]
                dependente <- formula[size]
                formula_final <-
                  reformulate(independente, response = dependente)
                formula_final
              })
              
              split_RF <- eventReactive(input$computar_RF,
                                        {
                                          set.seed(123)
                                          y <- input$YRF2
                                          createDataPartition(dados_RF()[, y], p = input$treinar_RF, list =
                                                                FALSE)
                                          
                                        })
              
              treino_RF <- eventReactive(input$computar_RF,
                                         {
                                           dados_RF()[split_RF(), ]
                                           
                                         })
              
              teste_RF <- eventReactive(input$computar_RF,
                                        {
                                          dados_RF()[-split_RF(), ]
                                          
                                        })
              
              output$opcoes_treinar_RF <- renderUI({
                if (input$opcao_RF == "Avancado") {
                  selectInput(
                    "metodo_RF",
                    "Selecione o Método",
                    choices = c(
                     
                      "Validação Cruzada k-fold" = "cv",
                      "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                    ),
                    selected = "cv"
                  )
                  
                }
              })
              
              output$opcoes_treinar2_RF <- renderUI({
                if (input$opcao_RF == "Avancado") {
                  selectInput(
                    "numero_RF",
                    "Número K de Subconjuntos",
                    choices = c(3, 5, 7, 10),
                    selected = 10
                  )
                }
                
              })
              
              output$opcoes_treinar3_RF <- renderUI({
                req(input$metodo_RF)
                if (input$metodo_RF == "repeatedcv") {
                  selectInput(
                    "repeticoes_RF",
                    "Número de Repetições",
                    choices = c(3, 5, 7),
                    selected = 3
                  )
                }
                
              })
              
              output$opcoes_parametros_RF <- renderUI({
                
                if(input$parametros_RF == 'Escolher')
                {
                  numericInput('opcoes_parametros_RF2',
                               'mtry:',
                               value = 5,
                               min = 1,
                               max = 20)
                }
                
              })
              
              grid_RF <- reactive({
                if(input$parametros_RF == 'Padrao'){
                   expand.grid(.mtry=c(1:10))
                  
                }
                else{
                   expand.grid(.mtry=input$opcoes_parametros_RF2)
                  
                }
                
              })
              
              fit_RF <- eventReactive(input$computar_RF, {
               tryCatch(
                
                withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
                if (input$opcao_RF == "Padrao") {
               
                  train(
                    formula_RF(),
                    data = treino_RF(),
                    metric = input$metrica_RF,
                    method = "rf",
                    importance = T,
                    tuneGrid=grid_RF(),
                    na.action = 'na.omit'
                  )
                  
                }
                
                else{
                  if (input$metodo_RF == "repeatedcv") {
                    ctrl <-
                      trainControl(
                        method = input$metodo_RF ,
                        tuneGrid=grid_RF(),
                        importance = T,
                       
                        number = as.numeric(input$numero_RF),
                        repeats = input$repeticoes_RF
                      )
                    
                    train(
                      formula_RF(),
                      data = treino_RF(),
                      method = "rf",
                      tuneGrid=grid_RF(),
                      metric = input$metrica_RF,
                      importance = T,
                    
                      trControl = ctrl ,
                     
                      na.action = 'na.omit'
                    )
                    
                  } else{
                    ctrl <-
                      trainControl(method = input$metodo_RF ,
                                   number = as.numeric(input$numero_RF))
                    
                    train(
                      formula_RF(),
                      data = treino_RF(),
                      metric = input$metrica_RF,
                      method = "rf",
                      importance = T,
                      tuneGrid=grid_RF(),
                    
                      trControl = ctrl,
                     
                      na.action = 'na.omit'
                    )
                  }
                  
                }
                }), error= function(e){return(NA)})
                
              })
              
              n_independentes_RF <- reactive({
                length(input$XRF2)
              })
              
              
              
              output$graficos_RF <- renderPlotly({
                req(fit_RF())
                if (n_independentes_RF() > 1) {
                  graficarImportancia(fit_RF())
                }
                else{
                  
                    graficarTeste(fit_RF(), teste_RF(), treino_RF(), input$XRF2, input$YRF2)
                  
                }
                
              })
              
              
              # treino_resid_RF <- reactive({
              #   d <- treino_RF()
              #   predicted <- predict(fit_RF(), newdata = treino_RF())
              #   d <- as.data.frame(cbind(d, predicted))
              #   d <- gather(d, key = 'iv', value = 'x', input$XRF2)
              #   d
              # })
              
              output$nomes_treino_RF <- renderUI({
                req(fit_RF())
                if(n_independentes_RF() >= 2){
                  selectInput("nomes_RF",
                              "Selecione a Variável:",
                              choices = input$XRF2)
                  
                }
                
              })
              
              output$graficos_multiplos_RF <- renderPlotly({
                req(fit_RF())
                
                if (n_independentes_RF() > 1) {
                  #graficarMultiplos(treino_resid_RF(),x,input$YRF2,predicted,iv)
                  
                  graficarTesteMultiplo(teste_RF(), input$nomes_RF, input$YRF2)
                  
                }
                # else{ggplot() + ggtitle('Visualização disponível de 2 a 6 variáveis independentes.')}
                
              })
              
              output$residuos_RF <- renderPlot({
                req(fit_RF())
                if(input$parametros_RF == 'Padrao'){
                  plot(fit_RF(),lwd = 4 ,ylab = input$metrica_RF,main = paste0(input$metrica_RF, ' x HiperParâmetro'), cex = 2)} else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
                
              })
              
              
              output$tabela_parametro_RF <- DT::renderDataTable({
                req(fit_RF())
                dat <- round(fit_RF()$results,3)
                dat <- dat[1:(length(dat)-4)]
                DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
               
              })
              
              output$melhor_parametro_RF <- renderPrint({
                req(fit_RF())
                
                cat(paste0(paste0("Melhor valor encontrado para ", colnames(fit_RF()$bestTune), " foi: ",fit_RF()$bestTune,"\n")) )
                
              })
              
              output$melhor_metrica_RF <- renderPrint({
                req(fit_RF())
                if(input$metrica_RF == "Rsquared"){
                  cat(paste0( "Com R² (Rsquared) de: ",round(max(fit_RF()$results['Rsquared']),5)))
                }else{
                  cat(paste0( "Com Raíz do Erro Quadrático Médio (RMSE) de: ",round(min(fit_RF()$results['RMSE']),5)))
                }
              })
              
              dados_teste_RF <- reactive({
                inFile <- input$arquivo_teste_RF
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              output$importacao_teste_RF <- DT::renderDataTable(
              
                if (input$mostrar_teste_RF== TRUE) {
                  DT::datatable(dados_teste_RF(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                })
              
              previsao_RF <- reactive({
                req(fit_importado_RF())
                tryCatch(
                  
                  predict(object = fit_importado_RF(), newdata = dados_teste_RF())
                  ,error = function(e) { return(rep('NA', nrow(dados_teste_RF()))) }
                )
                
              })
              
              tabela_previsoes_RF <- eventReactive(input$prever_RF, {
                if(input$opcao_prever_RF == TRUE){
                  Resultado_Modelo = previsao_RF()
                  cbind(dados_teste_RF(), Resultado_Modelo)
      
                  
                }else{
                  
                  Resultado_Modelo = predict(fit_RF(), dados_teste_RF())
                  data = cbind(dados_teste_RF(), Resultado_Modelo)
                  data
                }
              })
              
              output$resultado_previsoes_RF <- DT::renderDataTable(
        
                DT::datatable(tabela_previsoes_RF(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                
              )
              
              output$download_RF <- downloadHandler(
                filename = function() {
                  "resultados_previsoes.xlsx"
                },
                content = function(fname) {
                  write.xlsx(tabela_previsoes_RF(), fname)
                }
              )
              
              output$baixar_RF <- downloadHandler(
                
                filename = function() {
                  "modelo_randfor_reg.rds"
                },
                content = function(fname) {
                  
                  withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
                  saveRDS(fit_RF(), fname)
                  })
                }
              )
              
              output$importar_RF1 <- renderUI({
                if(input$opcao_prever_RF == TRUE)
                  tagList(
                  fileInput(
                    "importar_RF",
                    "Carregue seu modelo:",
                    accept = c(".rds")
                  ),
                  helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
                  )
                
              })
              
              
              fit_importado_RF <- reactive({
                inFile <- input$importar_RF
                
                if (is.null(inFile))
                  return(NULL)
                
                readRDS(inFile$datapath)
                
              })
              
              output$mensagem_erro_rf <- renderUI({
                if(is.na(fit_RF()))
                  includeMarkdown('Mensagem_Erro.Rmd')
              })
              
              output$resultados_obtidos_RF <- renderUI({
                req(fit_RF())
                h3("Resultados e Métricas do Modelo")
                
              })
              output$resultados_obtidos_previsoes_RF <- renderUI({
                req(tabela_previsoes_RF())
                h3("Resultados da Previsão")
                
              })
              
              
  ################################## FIM RF REGRESSAO ######################
      ########### INICIO PERCEPTRON REGRESSAO ###########
              dados_MLP1 <- reactive({
                inFile <- input$arquivo_MLP
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              
              dados_MLP <- reactive({
                req(dados_MLP1())
                dados_MLP1()[complete.cases(dados_MLP1()),]
              })
              
              output$importacao_MLP <- DT::renderDataTable({
                if (input$mostrar_MLP == TRUE) {
                  DT::datatable(dados_MLP(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                }
              })
              
              output$XMLP <- renderUI({
                nomes <- names(dados_MLP())
                pickerInput(
                  'XMLP2',
                  'Variáveis Independentes (X) - Atributos',
                  nomes,
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
                )
              })
              
              output$YMLP <- renderUI({
                nomes <- names(dados_MLP())
                selectInput('YMLP2', 'Variável Dependente (y) - Alvo:', nomes)
              })
              
              formula_MLP <- reactive({
                x <- input$XMLP2
                y <- input$YMLP2
                
                formula <- c(x, y)
                
                size <- length(formula)
                independente <- formula[-size]
                dependente <- formula[size]
                formula_final <-
                  reformulate(independente, response = dependente)
                formula_final
              })
              
              split_MLP <- eventReactive(input$computar_MLP,
                                        {
                                          set.seed(123)
                                          y <- input$YMLP2
                                          createDataPartition(dados_MLP()[, y], p = input$treinar_MLP, list =
                                                                FALSE)
                                          
                                        })
              
              treino_MLP <- eventReactive(input$computar_MLP,
                                         {
                                           dados_MLP()[split_MLP(), ]
                                           
                                         })
              
              teste_MLP <- eventReactive(input$computar_MLP,
                                        {
                                          dados_MLP()[-split_MLP(), ]
                                          
                                        })
              
              output$opcoes_treinar_MLP <- renderUI({
                if (input$opcao_MLP == "Avancado") {
                  selectInput(
                    "metodo_MLP",
                    "Selecione o Método",
                    choices = c(

                      "Validação Cruzada k-fold" = "cv",
                      "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                    ),
                    selected = "cv"
                  )
                  
                }
              })
              
              output$opcoes_treinar2_MLP <- renderUI({
                if (input$opcao_MLP == "Avancado") {
                  selectInput(
                    "numero_MLP",
                    "Número K de Subconjuntos",
                    choices = c(3, 5, 7, 10),
                    selected = 10
                  )
                }
                
              })
              
              output$opcoes_treinar3_MLP <- renderUI({
                req(input$metodo_MLP)
                if (input$metodo_MLP == "repeatedcv") {
                  selectInput(
                    "repeticoes_MLP",
                    "Número de Repetições",
                    choices = c(3, 5, 7),
                    selected = 3
                  )
                }
                
              })
              
              output$opcoes_parametros_MLP <- renderUI({
                
                if(input$parametros_MLP == 'Escolher')
                {
                  tagList(
                    numericInput('layer1reg',
                                 'Camada 1:',
                                 value = 1,
                                 min = 1,
                                 max = 10),
                    numericInput('layer2reg',
                                 'Camada 2:',
                                 value = 1,
                                 min = 1,
                                 max = 10),
                    numericInput('layer3reg',
                                 'Camada 3:',
                                 value = 1,
                                 min = 1,
                                 max = 10)
                  )
                }
                
              })
              
              grid_MLP <- reactive({
                if(input$parametros_MLP == 'Padrao'){
                  expand.grid(layer1 = c(1,2,3),
                              layer2 = c(1,2,3),
                              layer3 = c(1,2,3))
                  
                }
                else{
                  expand.grid(layer1 = c(input$layer1reg),
                              layer2 = c(input$layer2reg),
                              layer3 = c(input$layer3reg))
                  
                }
                
              })
              
              fit_MLP <- eventReactive(input$computar_MLP, {
                tryCatch(
                
                withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
                  if (input$opcao_MLP == "Padrao") {
                    
                    train(
                      formula_MLP(),
                      data = treino_MLP(),
                      metric = input$metrica_MLP,
                      method = "neuralnet",
                      preProcess = c("center","scale"),
                      
                      tuneGrid=grid_MLP(),
                      na.action = 'na.omit'
                    )
                    
                  }
                  
                  else{
                    if (input$metodo_MLP == "repeatedcv") {
                      ctrl <-
                        trainControl(
                          method = input$metodo_MLP ,
                                number = as.numeric(input$numero_MLP),
                          repeats = input$repeticoes_MLP
                        )
                      
                      train(
                        formula_MLP(),
                        data = treino_MLP(),
                        method = "neuralnet",
                        tuneGrid=grid_MLP(),
                        metric = input$metrica_MLP,
                        preProcess = c("center","scale"),
                        trControl = ctrl ,
                        
                        na.action = 'na.omit'
                      )
                      
                    } else{
                      ctrl <-
                        trainControl(method = input$metodo_MLP ,
                                     number = as.numeric(input$numero_MLP))
                      
                      train(
                        formula_MLP(),
                        data = treino_MLP(),
                        method = "neuralnet",
                        tuneGrid=grid_MLP(),
                        metric = input$metrica_MLP,
                        trControl = ctrl,
                        preProcess = c("center","scale"),
                        na.action = 'na.omit'
                      )
                    }
                    
                  }
                }), error= function(e){return(NA)})
                
              })
              
              n_independentes_MLP <- reactive({
                length(input$XMLP2)
              })
              
              
              
              output$graficos_MLP <- renderPlotly({
                req(fit_MLP())
                if (n_independentes_MLP() > 1) {
                  graficarImportancia(fit_MLP())
                }
                else{
                  
                  graficarTeste(fit_MLP(), teste_MLP(), treino_MLP(), input$XMLP2, input$YMLP2)
                  
                  
                  
                }
                
              })
              
              # 
              # treino_resid_MLP <- reactive({
              #   d <- treino_MLP()
              #   predicted <- predict(fit_MLP(), newdata = treino_MLP())
              #   d <- as.data.frame(cbind(d, predicted))
              #   d <- gather(d, key = 'iv', value = 'x', input$XMLP2)
              #   d
              # })
              
              output$nomes_treino_MLP <- renderUI({
                req(fit_MLP())
                if(n_independentes_MLP() >= 2){
                  selectInput("nomes_MLP",
                              "Selecione a Variável:",
                              choices = input$XMLP2)
                  
                }
                
              })
              
              
              output$graficos_multiplos_MLP <- renderPlotly({
                req(fit_MLP())
                
                if (n_independentes_MLP() > 1) {
                  #graficarMultiplos(treino_resid_SV(),x,input$YSV2,predicted,iv)
                  
                  graficarTesteMultiplo(teste_MLP(), input$nomes_MLP, input$YMLP2)
                  
                }
                # else{ggplot() + ggtitle('Visualização disponível de 2 a 6 variáveis independentes.')}
                
              })
              
              output$residuos_MLP <- renderPlot({
                req(fit_MLP())
                if(input$parametros_MLP == 'Padrao'){
                  plot(fit_MLP(), lwd = 4 ,ylab = input$metrica_MLP ,main = paste0(input$metrica_MLP, ' x HiperParâmetro'), cex = 2)} else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
                
              })
              
              
              output$tabela_parametro_MLP <- DT::renderDataTable({
                req(fit_MLP())
                dat <- round(fit_MLP()$results,3)
                dat <- dat[1:(length(dat)-4)]
                DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
               
              })
              
              output$melhor_parametro_MLP <- renderPrint({
                req(fit_MLP())
                
                cat(paste0(paste0("Melhor valor encontrado para ", colnames(fit_MLP()$bestTune), " foi: ",fit_SV()$bestTune,"\n")) )
                
              })
              
              output$melhor_metrica_MLP <- renderPrint({
                req(fit_MLP())
                if(input$metrica_MLP == "Rsquared"){
                  cat(paste0( "Com R² (Rsquared) de: ",round(max(fit_MLP()$results['Rsquared']),5)))
                }else{
                  cat(paste0( "Com Raíz do Erro Quadrático Médio (RMSE) de: ",round(min(fit_MLP()$results['RMSE']),5)))
                }
              })
              
              dados_teste_MLP <- reactive({
                inFile <- input$arquivo_teste_MLP
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              output$importacao_teste_MLP <- DT::renderDataTable(
                
                if (input$mostrar_teste_MLP== TRUE) {
                  DT::datatable(dados_teste_MLP(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                })
              
              previsao_MLP <- reactive({
                req(fit_importado_MLP())
                tryCatch(
                  
                  predict(object = fit_importado_MLP(), newdata = dados_teste_MLP())
                  ,error = function(e) { return(rep('NA', nrow(dados_teste_MLP()))) }
                )
                
              })
              
              tabela_previsoes_MLP <- eventReactive(input$prever_MLP, {
                if(input$opcao_prever_MLP == TRUE){
                 
                  Resultado_Modelo = previsao_MLP()
                  cbind(dados_teste_MLP(), Resultado_Modelo)
                 
                  
                }else{
                  
                  Resultado_Modelo = predict(fit_MLP(), dados_teste_MLP())
                  data = cbind(dados_teste_MLP(), Resultado_Modelo)
                  data
                }
              })
              
              output$resultado_previsoes_MLP <- DT::renderDataTable(
                
                DT::datatable(tabela_previsoes_MLP(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                
              )
              
              output$download_MLP <- downloadHandler(
                filename = function() {
                  "resultados_previsoes.xlsx"
                },
                content = function(fname) {
                  write.xlsx(tabela_previsoes_MLP(), fname)
                }
              )
              
              output$baixar_MLP <- downloadHandler(
                
                filename = function() {
                  "modelo_percepreg_reg.rds"
                },
                content = function(fname) {
                  
                  withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
                    saveRDS(fit_MLP(), fname)
                  })
                }
              )
              
              output$importar_MLP1 <- renderUI({
                if(input$opcao_prever_MLP == TRUE)
                  tagList(
                  fileInput(
                    "importar_MLP",
                    "Carregue seu modelo:",
                    accept = c(".rds")
                  ),
                  helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
                  )
                
              })
              
              
              fit_importado_MLP <- reactive({
                inFile <- input$importar_MLP
                
                if (is.null(inFile))
                  return(NULL)
                
                readRDS(inFile$datapath)
                
              })
              
              output$mensagem_erro_mlp <- renderUI({
                if(is.na(fit_MLP()))
                  includeMarkdown('Mensagem_Erro.Rmd')
              })
              
              output$resultados_obtidos_MLP <- renderUI({
                req(fit_MLP())
                h3("Resultados e Métricas do Modelo")
                
              }
              
              )
              
              output$resultados_obtidos_previsoes_MLP <- renderUI({
                req(tabela_previsoes_MLP())
                h3("Resultados da Previsão")
                
              })
              
      
  ######################### INICIO KNN CLASSIFICACAO ###############
              
              dados_KNN1 <- reactive({
                inFile <- input$arquivo_KNN
                
                if (is.null(inFile))
                  return(NULL)
                
                 read.csv(inFile$datapath, header = T)
                
              })
              dados_KNN <- reactive({
                req(dados_KNN1())
                dados_KNN1()[complete.cases(dados_KNN1()),]
              })
              
              output$importacao_KNN <- DT::renderDataTable({
                if (input$mostrar_KNN == TRUE) {
                  DT::datatable(dados_KNN(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                }
              })
              
              output$XKNN <- renderUI({
                nomes <- names(dados_KNN())
                pickerInput(
                  'XKNN2',
                  'Variáveis Independentes (X) - Atributos',
                  nomes,
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
                )
              })
              
              output$YKNN <- renderUI({
                nomes <- names(dados_KNN())
                selectInput('YKNN2', 'Variável Dependente (y) - Alvo:', nomes)
              })
              
              formula_KNN <- reactive({
                x <- input$XKNN2
                y <- input$YKNN2
                
                formula <- c(x, y)
                
                size <- length(formula)
                independente <- formula[-size]
                dependente <- formula[size]
                formula_final <-
                  reformulate(independente, response = dependente)
                formula_final
              })
              
              split_KNN <- eventReactive(input$computar_KNN,
                                        {
                                          set.seed(123)
                                          y <- input$YKNN2
                                          createDataPartition(dados_KNN()[, y], p = input$treinar_KNN, list =
                                                                FALSE)
                                          
                                        })
              
              treino_KNN1 <- eventReactive(input$computar_KNN,
                                         {
                                           dados_KNN()[split_KNN(), ]
                                           
                                         })
              
              teste_KNN1 <- eventReactive(input$computar_KNN,
                                        {
                                          dados_KNN()[-split_KNN(), ]
                                          
                                        })
              
              output$opcoes_treinar_KNN <- renderUI({
                if (input$opcao_KNN == "Avancado") {
                  selectInput(
                    "metodo_KNN",
                    "Selecione o Método",
                    choices = c(
                      
                      "Validação Cruzada k-fold" = "cv",
                      "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                    ),
                    selected = "cv"
                  )
                  
                }
              })
              
              output$opcoes_treinar2_KNN <- renderUI({
                if (input$opcao_KNN == "Avancado") {
                  selectInput(
                    "numero_KNN",
                    "Número K de Subconjuntos",
                    choices = c(3, 5, 7, 10),
                    selected = 10
                  )
                }
                
              })
              
              output$opcoes_treinar3_KNN <- renderUI({
                req(input$metodo_KNN)
                if (input$metodo_KNN == "repeatedcv") {
                  selectInput(
                    "repeticoes_KNN",
                    "Número de Repetições",
                    choices = c(3, 5, 7),
                    selected = 3
                  )
                }
                
              })
              
              treino_KNN <- reactive({
                temp <- treino_KNN1()
                temp[,input$YKNN2] <- as.factor( temp[,input$YKNN2])
                temp
              })
              teste_KNN <- reactive({
                temp <- teste_KNN1()
                temp[,input$YKNN2] <- as.factor( temp[,input$YKNN2])
                temp
              })
              
              output$opcoes_parametros_KNN <- renderUI({
                
                if(input$parametros_KNN == 'Escolher')
                {
                  numericInput('opcoes_parametros_KNN2',
                               'K:',
                               value = 3,
                               min = 2,
                               max = 40)
                }
                
              })
              
              grid_KNN <- reactive({
                if(input$parametros_KNN == 'Padrao'){
                  expand.grid(k = seq(1,20,2))
                  
                }
                else{
                  expand.grid(k = c(input$opcoes_parametros_KNN2))
                  
                }
                
              })
              
              
              fit_KNN <- eventReactive(input$computar_KNN, {
                
                tryCatch(
                withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
                
                if (input$opcao_KNN == "Padrao") {
                  
                  train(
                    formula_KNN(),
                    method = "knn",
                   data = treino_KNN(),
                   metric = input$metrica_KNN,
                   tuneGrid = grid_KNN(),
                    preProcess = c('center', 'scale'),
                    na.action = 'na.omit'
                  )
                  
                }
                
                else{
                  if (input$metodo_KNN == "repeatedcv") {
                    ctrl <-
                      trainControl(
                        method = input$metodo_KNN ,
                        number = as.numeric(input$numero_KNN),
                        repeats = input$repeticoes_KNN
                      )
                    
                    train(
                      formula_KNN(),
                      data = treino_KNN(),
                      tuneGrid = grid_KNN(),
                      method = 'knn',
                      metric = input$metrica_KNN,
                      trControl = ctrl ,
                      preProcess = c('center', 'scale'),
                      na.action = 'na.omit'
                    )
                    
                  } else{
                    ctrl <-
                      trainControl(method = input$metodo_KNN ,
                                   number = as.numeric(input$numero_KNN))
                    
                    train(
                      formula_KNN(),
                      data = treino_KNN(),
                      method = 'knn',
                      metric = input$metrica_KNN,
                      trControl = ctrl,
                      tuneGrid = grid_KNN(),
                      preProcess = c('center', 'scale'),
                      na.action = 'na.omit'
                    )
                  }
                  
                }
                  
                }), error= function(e){return(NA)})
                
              })
              
              n_independentes_KNN <- reactive({
                length(input$XKNN2)
              })
          
             
              output$graficos_KNN <- renderPlotly({
                req(fit_KNN())
                if (n_independentes_KNN() > 1) {
                  
                 graficarImportanciaClassificacao(fit_KNN())
                  
                }
                
              })


              output$erro_KNN <- renderPlot({
                req(fit_KNN())
                if(input$parametros_KNN == 'Padrao'){
                  plot(fit_KNN(),lwd = 4 , main = paste0('Acurácia', ' x Hiperparâmetro'), cex = 2, ylab="Acurácia")} else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
                
              })
            

          output$confusao_KNN <- renderPlot({
            req(fit_KNN())

            knnpredict <- predict(fit_KNN(), newdata=teste_KNN())
            cm <- confusionMatrix(knnpredict, teste_KNN()[,input$YKNN2])
             ggplotConfusionMatrix(cm)

          })

          dat_KNN1 <- reactive({
            knnpredict <- predict(fit_KNN(), newdata=teste_KNN())
            cm <- confusionMatrix(knnpredict, teste_KNN()[,input$YKNN2])
            dat = round(t(as.data.frame(as.matrix(cm,what="classes"))),3)
            

          })
          
          dat_KNN <- reactive({
            
            dat_KNN1()[,c(1,2,5,6,7,11)]
            
          })
          # 
          # dat_KNN <- reactive({
          #   
          #   dat = dat_KNN1()
          #   cols = c('Sensitivity', 'Specificity', 'Precision', 'Recall', 'F1')
          #   dat = dat[,cols]
          #   
          # })

          output$specifics_KNN <- DT::renderDataTable({

            req(fit_KNN())
            DT::datatable(dat_KNN(), options = list(scrollX = TRUE,
                                                    searching = FALSE))

          })

              output$tabela_parametro_KNN <- DT::renderDataTable({
                req(fit_KNN())
                dat <- round(fit_KNN()$results,3)
                dat <- dat[1:(length(dat)-3)]
                DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              })
              
              output$melhor_parametro_KNN <- renderPrint({
                req(fit_KNN())
                cat(paste0("Melhor valor encontrado para ", colnames(fit_KNN()$bestTune), " foi: ",fit_KNN()$bestTune,"\n"))
              })
         
              output$melhor_metrica_KNN <- renderPrint({
                req(fit_KNN())
                cat(paste0("Com Acurácia de: ",round(max(fit_KNN()$results['Accuracy']),5)))
              })
              
              
              dados_teste_KNN <- reactive({
                inFile <- input$arquivo_teste_KNN
                
                if (is.null(inFile))
                  return(NULL)
                
                read.csv(inFile$datapath, header = T)
                
              })
              output$importacao_teste_KNN <- DT::renderDataTable(
                if (input$mostrar_teste_KNN== TRUE) {
                  DT::datatable(dados_teste_KNN(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                  
                })
              
              previsao_KNN <- reactive({
                req(fit_importado_KNN())
                tryCatch(
                  
                  predict(object = fit_importado_KNN(), newdata = dados_teste_KNN())
                  ,error = function(e) { return(rep('NA', nrow(dados_teste_KNN()))) }
                )
                
              })
              
              tabela_previsoes_KNN <- eventReactive(input$prever_KNN, {
                if(input$opcao_prever_KNN == TRUE){
                  
                  Resultado_Modelo = previsao_KNN()
                  cbind(dados_teste_KNN(), Resultado_Modelo)
               
                  
                }else{
                  
                  Resultado_Modelo = predict(fit_KNN(), dados_teste_KNN())
                  data = cbind(dados_teste_KNN(), Resultado_Modelo)
                  data
                }
              })
              
              output$resultado_previsoes_KNN <- DT::renderDataTable(
                DT::datatable(tabela_previsoes_KNN(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
                
              )
              
              output$download_KNN <- downloadHandler(
                filename = function() {
                  "resultados_previsoes.xlsx"
                },
                content = function(fname) {
                  write.xlsx(tabela_previsoes_KNN(), fname)
                }
              )
              
              
              X_KNN <- reactive({
                
                input$XKNN2[1]
              })
              
              Y_KNN <- reactive({
                
                input$XKNN2[2]
              })
              
              class_KNN <- reactive({
                
                input$YKNN2
              })
              
              val_X_knn <- reactive({ 
                (max(teste_KNN()[,X_KNN()]) - min(teste_KNN()[,X_KNN()])) / 150
                
                })
              
              val_Y_knn <- reactive({
                
                (max(teste_KNN()[,Y_KNN()]) - min(teste_KNN()[,Y_KNN()]))/150
                
              })
              
              
              knngrid1 <- reactive({
                pl = seq(min(teste_KNN()[,X_KNN()]), max(teste_KNN()[,X_KNN()]), by=val_X_knn())
                pw = seq(min(teste_KNN()[,Y_KNN()]), max(teste_KNN()[,Y_KNN()]), by=val_Y_knn())
                lgrid <- expand.grid(x= pl,
                                     y= pw)
                lgrid

              })


              knngrid <- reactive({
               grid = knngrid1()
               x <- X_KNN()
                y <- Y_KNN()
                colnames(grid) <- c(x,y)
                as.data.frame(grid)
 
              })

              knnPredGrid <-
                reactive({
                  req(fit_KNN())
                knnPredGrid =   predict(fit_KNN(), newdata=knngrid())
                knnPredGrid = as.numeric(knnPredGrid)
                                
                })
      
               output$mensagem_erro_knn <- renderUI({
                 if(is.na(fit_KNN()))
                includeMarkdown('Mensagem_Erro.Rmd')
               })
               
               output$resultados_obtidos_KNN <- renderUI({
                 req(fit_KNN())
                 h3("Resultados e Métricas do Modelo")
                 
               }
               
               )
               
               output$resultados_obtidos_previsoes_KNN <- renderUI({
                 req(tabela_previsoes_KNN())
                 h3("Resultados da Classificação")
                 
               })
            
              teste_Pred_KNN <-reactive({
                req(fit_KNN())
                testPred <- predict(fit_KNN(), newdata=teste_KNN())
                testPred <- as.numeric(testPred)
                
              }) 
              
              teste_KNN_Plot <- reactive({
                test <- teste_KNN()
                test$Pred <- teste_Pred_KNN()
                test$Pred <- as.factor(test$Pred)
                test
              })


          output$limite_KNN <- renderPlot({
            req(fit_KNN())

            if(n_independentes_KNN() == 2){


            ggplot(data=knngrid()) + stat_contour(aes(x=knngrid()[,X_KNN()], y=knngrid()[,Y_KNN()], z=knnPredGrid()),
                                                   bins=2) +

               geom_point(data=teste_KNN_Plot(), aes(x=teste_KNN_Plot()[,X_KNN()], y=teste_KNN_Plot()[,Y_KNN()], colour=as.factor(teste_KNN_Plot()[,input$YKNN2])),
                          size=4, alpha = 0.7)+
               theme_bw() + xlab(X_KNN()) + ylab(Y_KNN()) + ggtitle("Limite de Decisão - Treino") + labs(title = "Limite de Decisão - Treino") +
                 theme(legend.title = element_blank()) + theme(legend.position  ="bottom") + theme(
                   axis.text=element_text(size=12),
                   axis.title=element_text(size=18),
                   plot.title = element_text(size=20, face = "bold"))
              


         }
             })



           output$superficie_KNN <- renderPlot({
             req(fit_KNN())
             if(n_independentes_KNN() == 2){
              graficarSuperficie(knngrid(), X_KNN(), Y_KNN(),  knnPredGrid())
# #                          ggplot(data=knngrid()) + stat_contour(aes(x=knngrid()[,X_KNN()], y=knngrid()[,Y_KNN()], z=knnPredGrid()),
# #                                                           bins=2) +
# #                          geom_point(aes(x=knngrid()[,X_KNN()], y=knngrid()[,Y_KNN()], colour=as.factor(knnPredGrid()))) +
# #
# #                            theme_bw() + xlab(X_KNN()) + ylab(Y_KNN()) + ggtitle("Regiao de Decisão") + labs(title = "Regiao de Decisão")+
# #                 theme(legend.title = element_blank()) + theme(legend.position  ="bottom")
            }
              })
           
           output$baixar_KNN <- downloadHandler(
             
             filename = function() {
               "modelo_knn.rds"
             },
             content = function(fname) {
               withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
               saveRDS(fit_KNN(), fname)
               })
             }
           )
           
           output$importar_KNN1 <- renderUI({
             if(input$opcao_prever_KNN == TRUE)
               tagList(
               fileInput(
                 "importar_KNN",
                 "Carregue seu modelo:",
                 accept = c(".rds")
               ),
               helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
               )
             
           })
           
           
           fit_importado_KNN <- reactive({
             inFile <- input$importar_KNN
             
             if (is.null(inFile))
               return(NULL)
             
             readRDS(inFile$datapath)
             
           })
           


          ################################### FIM KNN #########################################################
          ###################################### INICIO SVM CLASSIFIER ##################################
          
          dados_SVM1 <- reactive({
            inFile <- input$arquivo_SVM
            
            if (is.null(inFile))
              return(NULL)
            
            read.csv(inFile$datapath, header = T)
            
          })
          
           dados_SVM <- reactive({
             req(dados_SVM1())
             dados_SVM1()[complete.cases(dados_SVM1()),]
           })
          
          output$importacao_SVM <- DT::renderDataTable({
            if (input$mostrar_SVM == TRUE) {
              DT::datatable(dados_SVM(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
            }
          })
          
          output$XSVM <- renderUI({
            nomes <- names(dados_SVM())
            pickerInput(
              'XSVM2',
              'Variáveis Independentes (X) - Atributos',
              nomes,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            )
          })
          
          output$YSVM <- renderUI({
            nomes <- names(dados_SVM())
            selectInput('YSVM2', 'Variável Dependente (y) - Alvo:', nomes)
          })
          
          formula_SVM <- reactive({
            x <- input$XSVM2
            y <- input$YSVM2
            
            formula <- c(x, y)
            
            size <- length(formula)
            independente <- formula[-size]
            dependente <- formula[size]
            formula_final <-
              reformulate(independente, response = dependente)
            formula_final
          })
          
          split_SVM <- eventReactive(input$computar_SVM,
                                     {
                                       set.seed(123)
                                       y <- input$YSVM2
                                       createDataPartition(dados_SVM()[, y], p = input$treinar_SVM, list =
                                                             FALSE)
                                       
                                     })
          
          treino_SVM1 <- eventReactive(input$computar_SVM,
                                       {
                                         dados_SVM()[split_SVM(), ]
                                         
                                       })
          
          teste_SVM1 <- eventReactive(input$computar_SVM,
                                      {
                                        dados_SVM()[-split_SVM(), ]
                                        
                                      })
          
          output$opcoes_treinar_SVM <- renderUI({
            if (input$opcao_SVM == "Avancado") {
              selectInput(
                "metodo_SVM",
                "Selecione o Método",
                choices = c(
                 
                  "Validação Cruzada k-fold" = "cv",
                  "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                ),
                selected = "cv"
              )
              
            }
          })
          
          output$opcoes_treinar2_SVM <- renderUI({
            if (input$opcao_SVM == "Avancado") {
              selectInput(
                "numero_SVM",
                "Número K de Subconjuntos",
                choices = c(3, 5, 7, 10),
                selected = 10
              )
            }
            
          })
          
          output$opcoes_treinar3_SVM <- renderUI({
            req(input$metodo_SVM)
            if (input$metodo_SVM == "repeatedcv") {
              selectInput(
                "repeticoes_SVM",
                "Número de Repetições",
                choices = c(3, 5, 7),
                selected = 3
              )
            }
            
          })
          
          treino_SVM <- reactive({
            temp <- treino_SVM1()
            temp[,input$YSVM2] <- as.factor( temp[,input$YSVM2])
            temp
          })
          
          teste_SVM <- reactive({
            temp <- teste_SVM1()
            temp[,input$YSVM2] <- as.factor( temp[,input$YSVM2])
            temp
          })
          
          output$opcoes_parametros_SVM <- renderUI({
            
            if(input$parametros_SVM == 'Escolher')
            {
              if (input$kernel_SVM == "svmLinear") {
                numericInput('opcoes_parametros_SVM_Linear',
                             'C:',
                             value = 0.1,
                             min = 0.1,
                             max = 10)
              }else if(input$kernel_SVM == "svmRadial")
              {
                tagList(
                  numericInput('opcoes_parametros_SVM_Radial_C',
                               'C:',
                               value = 0.1,
                               min = 0.1,
                               max = 10),
                  numericInput('opcoes_parametros_SVM_Radial_Sigma',
                               'Sigma:',
                               value = 0.25,
                               min = 0.1,
                               max = 2)
                  
                  
                )
                
              }else if(input$kernel_SVM == "svmPoly"){
                
                tagList(
                  numericInput('opcoes_parametros_SVM_Poly_C',
                               'C:',
                               value = 0.1,
                               min = 0.1,
                               max = 10),
                  numericInput('opcoes_parametros_SVM_Poly_Scale',
                               'Scale:',
                               value = 0.25,
                               min = 0.1,
                               max = 2),
                  numericInput('opcoes_parametros_SVM_Poly_Degree',
                               'Degree:',
                               value = 1,
                               min = 1,
                               max = 5)
                )
                
              }
              
            }
          })
          
          grid_SVM <- reactive({
            
            if(input$parametros_SVM == 'Padrao'){
              
              if (input$kernel_SVM == "svmLinear") {
                
                expand.grid(C = c(0.1,  0.5, 1, 1.5, 2))
                
                
              }else if(input$kernel_SVM == "svmRadial")
              {
                expand.grid(sigma = c(
                  0.25,0.5,0.75, 0.9),
                  C = c( 0.1,  0.5, 1, 1.5, 2))  
                
              }else if(input$kernel_SVM == "svmPoly"){
                
                expand.grid(scale = c(
                  0.25,0.5,0.75, 0.9),
                  C = c( 0.1,  0.5, 1, 1.5, 2),
                  degree = c(1,2,3,4))
                
              }
              
            }
            else{
              
              if (input$kernel_SVM == "svmLinear") {
                
                grid <- expand.grid(C = c(input$opcoes_parametros_SVM_Linear))
                grid
                
              }else if(input$kernel_SVM == "svmRadial")
              {
                expand.grid(sigma = c(
                  input$opcoes_parametros_SVM_Radial_Sigma),
                  C = c(input$opcoes_parametros_SVM_Radial_C))  
                
                
              }else if(input$kernel_SVM == "svmPoly"){
                
                expand.grid(scale = c(
                  input$opcoes_parametros_SVM_Poly_Scale),
                  C = c(input$opcoes_parametros_SVM_Poly_C),
                  degree = c(input$opcoes_parametros_SVM_Poly_Degree))  
              }
              
            }
            
            
          })
          
          
          fit_SVM <- eventReactive(input$computar_SVM, {
           tryCatch(
            withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
           
            if (input$opcao_SVM == "Padrao") {
              if (input$kernel_SVM == "svmLinear") {
                train(
                  formula_SVM(),
                  data = treino_SVM(),
                  method = input$kernel_SVM,
                  tuneGrid = grid_SVM(),
                  metric = input$metrica_SVM,
                  preProcess = c('center', 'scale'),
                  na.action = 'na.omit'
                )
              }
              else if(input$kernel_SVM == "svmRadial"){
                train(
                  formula_SVM(),
                  data = treino_SVM(),
                  metric = input$metrica_SVM,
                  method = input$kernel_SVM,
                  tuneGrid = grid_SVM(),
                  #tuneLength = 10,
                  
                  preProcess = c('center', 'scale'),
                  na.action = 'na.omit'
                )
                
              }
              else{
                
                train(
                  formula_SVM(),
                  data = treino_SVM(),
                  method = input$kernel_SVM,
                  tuneGrid = grid_SVM(),
                  metric = input$metrica_SVM,
                  
                  preProcess = c('center', 'scale'),
                  na.action = 'na.omit'
                )
                
                
              }
              
            }
            
            else{
              if (input$metodo_SVM == "repeatedcv") {
                if (input$kernel_SVM == "svmLinear") {
                  ctrl <-
                    trainControl(
                      method = input$metodo_SVM ,
                      number = as.numeric(input$numero_SVM),
                      repeats = input$repeticoes_SVM
                    )
                  train(
                    formula_SVM(),
                    data = treino_SVM(),
                    method = input$kernel_SVM,
                    tuneGrid = grid_SVM(),
                    metric = input$metrica_SVM,
                    trControl = ctrl,
                    preProcess = c('center', 'scale'),
                    na.action = 'na.omit'
                  )
                }
                else if (input$kernel_SVM == "svmRadial"){
                  ctrl <-
                    trainControl(
                      method = input$metodo_SVM ,
                      number = as.numeric(input$numero_SVM),
                      repeats = input$repeticoes_SVM
                    )
                  train(
                    formula_SVM(),
                    data = treino_SVM(),
                    method = input$kernel_SVM,
                    trControl = ctrl,
                    tuneGrid = grid_SVM(),
                    metric = input$metrica_SVM,
                    
                    preProcess = c('center', 'scale'),
                    na.action = 'na.omit'
                  )
                  
                } else {
                  
                  ctrl <-
                    trainControl(
                      method = input$metodo_SVM ,
                      number = as.numeric(input$numero_SVM),
                      repeats = input$repeticoes_SVM)
                  
                  train(
                    formula_SVM(),
                    data = treino_SVM(),
                    method = input$kernel_SVM,
                    tuneGrid = grid_SVM(),
                    metric = input$metrica_SVM,
                    
                    preProcess = c('center', 'scale'),
                    na.action = 'na.omit'
                  )
                }
                
                
              } else{
                if (input$kernel_SVM == "svmLinear") {
                  ctrl <-
                    trainControl(method = input$metodo_SVM ,
                                 number = as.numeric(input$numero_SVM))
                  train(
                    formula_SVM(),
                    data = treino_SVM(),
                    method = input$kernel_SVM,
                    tuneGrid = grid_SVM(),
                    metric = input$metrica_SVM,
                    trControl = ctrl,
                    preProcess = c('center', 'scale'),
                    na.action = 'na.omit'
                  )
                }
                else if(input$kernel_SVM == "svmRadial"){
                  ctrl <-
                    trainControl(method = input$metodo_SVM ,
                                 number = as.numeric(input$numero_SVM))
                  train(
                    formula_SVM(),
                    data = treino_SVM(),
                    method = input$kernel_SVM,
                    tuneGrid = grid_SVM(),
                    metric = input$metrica_SVM,
                    trControl = ctrl,
                    preProcess = c('center', 'scale'),
                    na.action = 'na.omit'
                  )
                  
                }
                else{
                  
                  ctrl <-
                    trainControl(method = input$metodo_SVM ,
                                 number = as.numeric(input$numero_SVM))
                  train(
                    formula_SVM(),
                    data = treino_SVM(),
                    method = input$kernel_SVM,
                    tuneGrid = grid_SVM(),
                    metric = input$metrica_SVM,
                    preProcess = c('center', 'scale'),
                    na.action = 'na.omit'
                  )
                  
                }
              }
              
            }
           }), error= function(e){return(NA)})
          })
          
          output$tabela_parametro_SVM <- DT::renderDataTable({
            req(fit_SVM())
            dat <- round(fit_SVM()$results,3)
            dat <- dat[1:(length(dat)-3)]
            DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
          })
          
          output$melhor_parametro_SVM <- renderPrint({
            req(fit_SVM())
            cat(paste0("Melhor valor encontrado para ", colnames(fit_SVM()$bestTune), " foi: ",fit_SVM()$bestTune,"\n"))
          })
          
          output$melhor_metrica_SVM <- renderPrint({
            req(fit_SVM())
            cat(paste0("Com Acurácia de: ",round(max(fit_SVM()$results['Accuracy']),5)))
          })
          
          n_independentes_SVM <- reactive({
            req(fit_SVM())
            length(input$XSVM2)
          })
          
          
          output$graficos_SVM <- renderPlotly({
            req(fit_SVM())
            if (n_independentes_SVM() > 1) {
              graficarImportanciaClassificacao(fit_SVM())
              
            }
            
          })
          output$erro_SVM <- renderPlot({
            req(fit_SVM())
            if(input$parametros_SVM == 'Padrao'){
              plot(fit_SVM(),lwd = 4,main = paste0('Acurácia', ' x Hiperparâmetro'), cex = 2, ylab="Acurácia")} else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
            
          })
          
          
          
          output$confusao_SVM <- renderPlot({
            req(fit_SVM())
            
            SVMpredict <- predict(fit_SVM(), newdata=teste_SVM())
            cm <- confusionMatrix(SVMpredict, teste_SVM()[,input$YSVM2])
            ggplotConfusionMatrix(cm)
            
          })
          
          dat_SVM1 <- reactive({
            SVMpredict <- predict(fit_SVM(), newdata=teste_SVM())
            cm <- confusionMatrix(SVMpredict, teste_SVM()[,input$YSVM2])
            dat = round(t(as.data.frame(as.matrix(cm,what="classes"))),3)
            
           dat
          })
          
          dat_SVM <- reactive({
            
            dat_SVM1()[,c(1,2,5,6,7,11)]
            
          })
          
          # 
          # dat_SVM <- reactive({
          #   
          #   dat = dat_SVM1()
          #   cols = c('Sensitivity', 'Specificity', 'Precision', 'Recall', 'F1')
          #   dat[,cols]
          #   
          # })
          
          output$specifics_SVM <- DT::renderDataTable({
            req(fit_SVM())
            
            DT::datatable(dat_SVM(), options = list(scrollX = TRUE,
                                                    searching = FALSE))
            
          })
          
          dados_teste_SVM <- reactive({
            inFile <- input$arquivo_teste_SVM
            
            if (is.null(inFile))
              return(NULL)
            
            read.csv(inFile$datapath, header = T)
            
          })
          output$importacao_teste_SVM <- DT::renderDataTable(
            if (input$mostrar_teste_SVM== TRUE) {
              DT::datatable(dados_teste_SVM(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
            })
          previsao_SVM <- reactive({
            req(fit_importado_SVM())
            tryCatch(
              
              predict(object = fit_importado_SVM(), newdata = dados_teste_SVM())
              ,error = function(e) { return(rep('NA', nrow(dados_teste_SVM()))) }
            )
            
          })
          
          tabela_previsoes_SVM <- eventReactive(input$prever_SVM, {
            
            if(input$opcao_prever_SVM == TRUE){
              Resultado_Modelo = previsao_SVM()
              cbind(dados_teste_SVM(), Resultado_Modelo)
              
            }else{
              
              Resultado_Modelo = predict(fit_SVM(), dados_teste_SVM())
              data = cbind(dados_teste_SVM(), Resultado_Modelo)
              data
            }
          })
          
          output$resultado_previsoes_SVM <- DT::renderDataTable(
            DT::datatable(tabela_previsoes_SVM(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
          )
          
          output$download_SVM <- downloadHandler(
            filename = function() {
              "resultados_previsoes.xlsx"
            },
            content = function(fname) {
              write.xlsx(tabela_previsoes_SVM(), fname)
            }
          )
          
          output$baixar_SVM <- downloadHandler(
            
            filename = function() {
              "modelo_svm_class.rds"
            },
            content = function(fname) {
              withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
              saveRDS(fit_SVM(), fname)
              })
            }
          )
          
          output$importar_SVM1 <- renderUI({
            if(input$opcao_prever_SVM == TRUE)
              tagList(
              fileInput(
                "importar_SVM",
                "Carregue seu modelo:",
                accept = c(".rds")
              ),
              helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
              )
            
          })
          
          
          fit_importado_SVM <- reactive({
            inFile <- input$importar_SVM
            
            if (is.null(inFile))
              return(NULL)
            
            readRDS(inFile$datapath)
            
          })
          output$mensagem_erro_svm <- renderUI({
            if(is.na(fit_SVM()))
              includeMarkdown('Mensagem_Erro.Rmd')
          })
          output$resultados_obtidos_SVM <- renderUI({
            req(fit_SVM())
            h3("Resultados e Métricas do Modelo")
            
          }
          
          )
          
          output$resultados_obtidos_previsoes_SVM <- renderUI({
            req(tabela_previsoes_SVM())
            h3("Resultados da Classificação")
            
          })
          
         
          X_SVM <- reactive({
            
            input$XSVM2[1]
          })
          
          Y_SVM <- reactive({
            
            input$XSVM2[2]
          })
          
          class_SVM <- reactive({
            
            input$YSVM2
          })
          
          val_X_svm <- reactive({ 
            (max(teste_SVM()[,X_SVM()]) - min(teste_SVM()[,X_SVM()])) / 150
            
          })
          
          val_Y_svm <- reactive({
            
            (max(teste_SVM()[,Y_SVM()]) - min(teste_SVM()[,Y_SVM()]))/150
            
          })
          
          SVMgrid1 <- reactive({
            pl = seq(min(teste_SVM()[,X_SVM()]), max(teste_SVM()[,X_SVM()]), by=val_X_svm())
            pw = seq(min(teste_SVM()[,Y_SVM()]), max(teste_SVM()[,Y_SVM()]), by=val_Y_svm())
            lgrid <- expand.grid(x= pl, 
                                 y= pw)
            lgrid
            
          })
          
          
          SVMgrid <- reactive({
            grid = SVMgrid1()
            x <-X_SVM()
            y <- Y_SVM() 
            colnames(grid) <- c(x,y)
            as.data.frame(grid)
            
          })
          
          SVMPredGrid <-
            reactive({
              SVMPredGrid =   predict(fit_SVM(), newdata=SVMgrid())
              SVMPredGrid = as.numeric(SVMPredGrid)
              SVMPredGrid
            })
          
          
          teste_Pred_SVM <-reactive({
            testPred <- predict(fit_SVM(), newdata=teste_SVM())
            testPred <- as.numeric(testPred)
            
          }) 
          
          teste_SVM_Plot <- reactive({
            test <- teste_SVM()
            test$Pred <- teste_Pred_SVM()
            test$Pred <- as.factor(test$Pred)
            test
          })
          
          
          output$limite_SVM <- renderPlot({
            req(fit_SVM())
            if(n_independentes_SVM() == 2){
             
              graficarLimite(SVMgrid(),X_SVM(),Y_SVM(),input$YSVM2, teste_SVM_Plot(),SVMPredGrid())
              
            }
          })
          
          
          
          output$superficie_SVM <- renderPlot({
            req(fit_SVM())
            if(n_independentes_SVM() == 2){
              
              graficarSuperficie(SVMgrid(), X_SVM(), Y_SVM(), SVMPredGrid())
              
#               ggplot(data=SVMgrid()) + stat_contour(aes(x=SVMgrid()[,X_SVM()], y=SVMgrid()[,Y_SVM()], z=SVMPredGrid()),
#                                                     bins=2) +
#                 geom_point(aes(x=SVMgrid()[,X_SVM()], y=SVMgrid()[,Y_SVM()], colour=as.factor(SVMPredGrid()))) +
#                 
#                 theme_bw() + xlab(X_SVM()) + ylab(Y_SVM()) + ggtitle("Regiao de Decisão") + labs(title = "Regiao de Decisão")+
#                 theme(legend.title = element_blank()) + theme(legend.position  ="bottom")
            }
          })
          
          ################################# FIM SVM CLASSIFIER #########################
          ############################## INICIO Ãrvore de Decisão CLASSIFIER #####################
          
          dados_ADC1 <- reactive({
            inFile <- input$arquivo_ADC
            
            if (is.null(inFile))
              return(NULL)
            
            read.csv(inFile$datapath, header = T)
            
          })
          dados_ADC <- reactive({
            req(dados_ADC1())
            dados_ADC1()[complete.cases(dados_ADC1()),]
          })
          
          output$importacao_ADC <- DT::renderDataTable({
            if (input$mostrar_ADC == TRUE) {
              DT::datatable(dados_ADC(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
            }
          })
          
          output$XADC <- renderUI({
            nomes <- names(dados_ADC())
            pickerInput(
              'XADC2',
              'Variáveis Independentes (X) - Atributos',
              nomes,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            )
          })
          
          output$YADC <- renderUI({
            nomes <- names(dados_ADC())
            selectInput('YADC2', 'Variável Dependente (y) - Alvo:', nomes)
          })
          
          formula_ADC <- reactive({
            x <- input$XADC2
            y <- input$YADC2
            
            formula <- c(x, y)
            
            size <- length(formula)
            independente <- formula[-size]
            dependente <- formula[size]
            formula_final <-
              reformulate(independente, response = dependente)
            formula_final
          })
          
          split_ADC <- eventReactive(input$computar_ADC,
                                     {
                                       set.seed(123)
                                       y <- input$YADC2
                                       createDataPartition(dados_ADC()[, y], p = input$treinar_ADC, list =
                                                             FALSE)
                                       
                                     })
          
          treino_ADC1 <- eventReactive(input$computar_ADC,
                                       {
                                         dados_ADC()[split_ADC(), ]
                                         
                                       })
          
          teste_ADC1 <- eventReactive(input$computar_ADC,
                                      {
                                        dados_ADC()[-split_ADC(), ]
                                        
                                      })
          
          output$opcoes_treinar_ADC <- renderUI({
            if (input$opcao_ADC == "Avancado") {
              selectInput(
                "metodo_ADC",
                "Selecione o Método",
                choices = c(
                  
                  "Validação Cruzada k-fold" = "cv",
                  "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                ),
                selected = "cv"
              )
              
            }
          })
          
          output$opcoes_treinar2_ADC <- renderUI({
            if (input$opcao_ADC == "Avancado") {
              selectInput(
                "numero_ADC",
                "Número K de Subconjuntos",
                choices = c(3, 5, 7, 10),
                selected = 10
              )
            }
            
          })
          
          
          
          treino_ADC <- reactive({
            temp <- treino_ADC1()
            temp[,input$YADC2] <- as.factor( temp[,input$YADC2])
            temp
          })
          
          teste_ADC <- reactive({
            temp <- teste_ADC1()
            temp[,input$YADC2] <- as.factor( temp[,input$YADC2])
            temp
          })
          
          output$opcoes_treinar3_ADC <- renderUI({
            req(input$metodo_ADC)
            if (input$metodo_ADC == "repeatedcv") {
              selectInput(
                "repeticoes_ADC",
                "Número de Repetições",
                choices = c(3, 5, 7),
                selected = 3
              )
            }
            
          })
          
          output$opcoes_parametros_ADC <- renderUI({
            
            if(input$parametros_ADC == 'Escolher')
            {
              numericInput('opcoes_parametros_ADC2',
                           'Max Depth:',
                           value = 5,
                           min = 2,
                           max = 40)
            }
            
          })
          
          
          grid_ADC <- reactive({
            if(input$parametros_ADC == 'Padrao'){
              expand.grid(.maxdepth=seq(5,20,5))
              
            }
            else{
              expand.grid(.maxdepth=input$opcoes_parametros_ADC2)
              
            }
            
          })
          
          
          fit_ADC <- eventReactive(input$computar_ADC, {
            tryCatch(
           
            withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
            
            if (input$opcao_ADC == "Padrao") {
              
              train(
                formula_ADC(),
                data = treino_ADC(),
                method = "rpart2",
                metric = input$metrica_ADC,
                tuneGrid = grid_ADC(),
                na.action = 'na.omit'
              )
              
            }
            
            else{
              if (input$metodo_ADC == "repeatedcv") {
                ctrl <-
                  trainControl(
                    method = input$metodo_ADC ,
                    number = as.numeric(input$numero_ADC),
                    repeats = input$repeticoes_ADC
                  )
                
                train(
                  formula_ADC(),
                  data = treino_ADC(),
                  method = "rpart2",
                  trControl = ctrl ,
                  tuneGrid = grid_ADC(),
                  metric = input$metrica_ADC,
                  na.action = 'na.omit'
                )
                
              } else{
                ctrl <-
                  trainControl(method = input$metodo_ADC ,
                               number = as.numeric(input$numero_ADC))
                
                train(
                  formula_ADC(),
                  data = treino_ADC(),
                  method = "rpart2",
                  tuneGrid = grid_ADC(),
                  trControl = ctrl,
                  metric = input$metrica_ADC,
                  na.action = 'na.omit'
                )
              }
              
            }
            }), error= function(e){return(NA)})
            
          })
          
          n_independentes_ADC <- reactive({
            length(input$XADC2)
          })
          
          
          output$graficos_ADC <- renderPlotly({
            req(fit_ADC())
            if (n_independentes_ADC() > 1) {
              
              graficarImportanciaClassificacao(fit_ADC())
              
            }
            
          })
          
          
          output$erro_ADC <- renderPlot({
            req(fit_ADC())
            if(input$parametros_ADC == 'Padrao'){
              plot(fit_ADC(),lwd = 4 ,ylab="Acurácia",main = paste0('Acurácia', ' x Hiperparâmetro'), cex = 2)} else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
            
          })
          
          
          output$confusao_ADC <- renderPlot({
            req(fit_ADC())
            
            adcpredict <- predict(fit_ADC(), newdata=teste_ADC())
            cm <- confusionMatrix(adcpredict, teste_ADC()[,input$YADC2])
            ggplotConfusionMatrix(cm)
            
          })
          
          dat_ADC1 <- reactive({
            adcpredict <- predict(fit_ADC(), newdata=teste_ADC())
            cm <- confusionMatrix(adcpredict, teste_ADC()[,input$YADC2])
            dat = round(t(as.data.frame(as.matrix(cm,what="classes"))),3)
            dat
          })
          
          dat_ADC <- reactive({
            
            dat_ADC1()[,c(1,2,5,6,7,11)]
            
          })
          
          output$specifics_ADC <- DT::renderDataTable({
            
            req(fit_ADC())
            DT::datatable(dat_ADC(), options = list(scrollX = TRUE,
                                                    searching = FALSE))
            
          })
          
          output$tabela_parametro_ADC <- DT::renderDataTable({
            req(fit_ADC())
            dat <- round(fit_ADC()$results,3)
            dat <- dat[1:(length(dat)-3)]
            DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
          })
          
          output$melhor_parametro_ADC <- renderPrint({
            req(fit_ADC())
            cat(paste0("Melhor valor encontrado para ", colnames(fit_ADC()$bestTune), " foi: ",fit_ADC()$bestTune,"\n"))
          })
          
          output$melhor_metrica_ADC <- renderPrint({
            req(fit_ADC())
            cat(paste0("Com Acurácia de: ",round(max(fit_ADC()$results['Accuracy']),5)))
          })
          
          dados_teste_ADC <- reactive({
            inFile <- input$arquivo_teste_ADC
            
            if (is.null(inFile))
              return(NULL)
            
            read.csv(inFile$datapath, header = T)
            
          })
          output$importacao_teste_ADC <- DT::renderDataTable(
            if (input$mostrar_teste_ADC== TRUE) {
              DT::datatable(dados_teste_ADC(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
            })
          previsao_ADC <- reactive({
            req(fit_importado_ADC())
            tryCatch(
              
              predict(object = fit_importado_ADC(), newdata = dados_teste_ADC())
              ,error = function(e) { return(rep('NA', nrow(dados_teste_ADC()))) }
            )
            
          })
          
          tabela_previsoes_ADC <- eventReactive(input$prever_ADC, {
            if(input$opcao_prever_ADC == TRUE){
           Resultado_Modelo = previsao_ADC()
            cbind(dados_teste_ADC(), Resultado_Modelo)
              
            }else{
              
              Resultado_Modelo = predict(fit_ADC(), dados_teste_ADC())
              data = cbind(dados_teste_ADC(), Resultado_Modelo)
              data
            }
          })
          
          output$resultado_previsoes_ADC <- DT::renderDataTable(
            DT::datatable(tabela_previsoes_ADC(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
          )
          
          output$download_ADC <- downloadHandler(
            filename = function() {
              "resultados_previsoes.xlsx"
            },
            content = function(fname) {
              write.xlsx(tabela_previsoes_ADC(), fname)
            }
          )
          
          
          
          
          X_ADC <- reactive({
            
            input$XADC2[1]
          })
          
          Y_ADC <- reactive({
            
            input$XADC2[2]
          })
          
          class_ADC <- reactive({
            
            input$YADC2
          })
          
          val_X_adc <- reactive({ 
            (max(teste_ADC()[,X_ADC()]) - min(teste_ADC()[,X_ADC()])) / 150
            
          })
          
          val_Y_adc <- reactive({
            
            (max(teste_ADC()[,Y_ADC()]) - min(teste_ADC()[,Y_ADC()]))/150
            
          })
          
          adcgrid1 <- reactive({
            pl = seq(min(teste_ADC()[,X_ADC()]), max(teste_ADC()[,X_ADC()]), by=val_X_adc())
            pw = seq(min(teste_ADC()[,Y_ADC()]), max(teste_ADC()[,Y_ADC()]), by=val_Y_adc())
            lgrid <- expand.grid(x= pl, 
                                 y= pw)
            lgrid
            
          })
          
          
          adcgrid <- reactive({
            grid = adcgrid1()
            x <-X_ADC()
            y <- Y_ADC() 
            colnames(grid) <- c(x,y)
            as.data.frame(grid)
            
          })
          
          adcPredGrid <-
            reactive({
              req(fit_ADC())
              adcPredGrid =   predict(fit_ADC(), newdata=adcgrid())
              adcPredGrid = as.numeric(adcPredGrid)
              adcPredGrid
            })
          
          
          teste_Pred_ADC <-reactive({
            req(fit_ADC())
            testPred <- predict(fit_ADC(), newdata=teste_ADC())
            testPred <- as.numeric(testPred)
            
          }) 
          
          teste_ADC_Plot <- reactive({
            test <- teste_ADC()
            test$Pred <- teste_Pred_ADC()
            test$Pred <- as.factor(test$Pred)
            test
          })
          
          
          output$limite_ADC <- renderPlot({
            req(fit_ADC())
            
            if(n_independentes_ADC() == 2){
              
              
              ggplot(data=adcgrid()) + stat_contour(aes(x=adcgrid()[,X_ADC()], y=adcgrid()[,Y_ADC()], z=adcPredGrid()),
                                                    bins=2) +
                geom_point(data=teste_ADC_Plot(), aes(x=teste_ADC_Plot()[,X_ADC()], y=teste_ADC_Plot()[,Y_ADC()], colour=as.factor(teste_ADC_Plot()[,input$YADC2])),
                           size=4, alpha = 0.7)+
                theme_bw() + xlab(X_ADC()) + ylab(Y_ADC()) + ggtitle("Limite de Decisão - Treino") + labs(title = "Limite de Decisão - Treino") +
                theme(legend.title = element_blank()) + theme(legend.position  ="bottom") + theme(
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=18),
                  plot.title = element_text(size=20, face = "bold"))
              
              
              
            }
          })
          
          
          
          output$superficie_ADC <- renderPlot({
            req(fit_ADC())
            if(n_independentes_ADC() == 2){
              graficarSuperficie(adcgrid(), X_ADC(), Y_ADC(), adcPredGrid())
              #                          ggplot(data=knngrid()) + stat_contour(aes(x=knngrid()[,X_KNN()], y=knngrid()[,Y_KNN()], z=knnPredGrid()),
              #                                                           bins=2) +
              #                          geom_point(aes(x=knngrid()[,X_KNN()], y=knngrid()[,Y_KNN()], colour=as.factor(knnPredGrid()))) +
              #                           
              #                            theme_bw() + xlab(X_KNN()) + ylab(Y_KNN()) + ggtitle("Regiao de Decisão") + labs(title = "Regiao de Decisão")+
              #                 theme(legend.title = element_blank()) + theme(legend.position  ="bottom")
            }
          })
          
          output$baixar_ADC <- downloadHandler(
            
            filename = function() {
              "modelo_arvdec_class.rds"
            },
            content = function(fname) {
              withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
              saveRDS(fit_ADC(), fname)
              })
            }
          )
          
          output$importar_ADC1 <- renderUI({
            if(input$opcao_prever_ADC == TRUE)
              tagList(
              fileInput(
                "importar_ADC",
                "Carregue seu modelo:",
                accept = c(".rds")
              ),
              helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
              )
            
          })
          
          
          fit_importado_ADC <- reactive({
            inFile <- input$importar_ADC
            
            if (is.null(inFile))
              return(NULL)
            
            readRDS(inFile$datapath)
            
          })
          output$mensagem_erro_adc <- renderUI({
            if(is.na(fit_ADC()))
              includeMarkdown('Mensagem_Erro.Rmd')
          })
          output$resultados_obtidos_ADC <- renderUI({
            req(fit_ADC())
            h3("Resultados e Métricas do Modelo")
            
          }
          
          )
          
          output$resultados_obtidos_previsoes_ADC <- renderUI({
            req(tabela_previsoes_ADC())
            h3("Resultados da Classificação")
            
          })
          
          
          ###################### FIM Ãrvore de Decisão CLASSIFIER ####################
          ######################## INICIO Floresta Aleatória (Random Forest) CLASSIFIER ###################
          
          dados_RFC1 <- reactive({
            inFile <- input$arquivo_RFC
            
            if (is.null(inFile))
              return(NULL)
            
            read.csv(inFile$datapath, header = T)
            
          })
          
          dados_RFC <- reactive({
            req(dados_RFC1())
            dados_RFC1()[complete.cases(dados_RFC1()),]
          })
          
          output$importacao_RFC <- DT::renderDataTable({
            if (input$mostrar_RFC == TRUE) {
              DT::datatable(dados_RFC(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
            }
          })
          
          output$XRFC <- renderUI({
            nomes <- names(dados_RFC())
            pickerInput(
              'XRFC2',
              'Variáveis Independentes (X) - Atributos',
              nomes,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            )
          })
          
          output$YRFC <- renderUI({
            nomes <- names(dados_RFC())
            selectInput('YRFC2', 'Variável Dependente (y) - Alvo:', nomes)
          })
          
          formula_RFC <- reactive({
            x <- input$XRFC2
            y <- input$YRFC2
            
            formula <- c(x, y)
            
            size <- length(formula)
            independente <- formula[-size]
            dependente <- formula[size]
            formula_final <-
              reformulate(independente, response = dependente)
            formula_final
          })
          
          split_RFC <- eventReactive(input$computar_RFC,
                                     {
                                       set.seed(123)
                                       y <- input$YRFC2
                                       createDataPartition(dados_RFC()[, y], p = input$treinar_RFC, list =
                                                             FALSE)
                                       
                                     })
          
          treino_RFC1 <- eventReactive(input$computar_RFC,
                                       {
                                         dados_RFC()[split_RFC(), ]
                                         
                                       })
          
          teste_RFC1 <- eventReactive(input$computar_RFC,
                                      {
                                        dados_RFC()[-split_RFC(), ]
                                        
                                      })
          
          output$opcoes_treinar_RFC <- renderUI({
            if (input$opcao_RFC == "Avancado") {
              selectInput(
                "metodo_RFC",
                "Selecione o Método",
                choices = c(
                
                  "Validação Cruzada k-fold" = "cv",
                  "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                ),
                selected = "cv"
              )
              
            }
          })
          
          output$opcoes_treinar2_RFC <- renderUI({
            if (input$opcao_RFC == "Avancado") {
              selectInput(
                "numero_RFC",
                "Número K de Subconjuntos",
                choices = c(3, 5, 7, 10),
                selected = 10
              )
            }
            
          })
          
          output$opcoes_treinar3_RFC <- renderUI({
            req(input$metodo_RFC)
            if (input$metodo_RFC == "repeatedcv") {
              selectInput(
                "repeticoes_RFC",
                "Número de Repetições",
                choices = c(3, 5, 7),
                selected = 3
              )
            }
            
          })
          
          treino_RFC <- reactive({
            temp <- treino_RFC1()
            temp[,input$YRFC2] <- as.factor( temp[,input$YRFC2])
            temp
          })
          
          teste_RFC <- reactive({
            temp <- teste_RFC1()
            temp[,input$YRFC2] <- as.factor( temp[,input$YRFC2])
            temp
          })
          
          
          output$opcoes_parametros_RFC <- renderUI({
            
            if(input$parametros_RFC == 'Escolher')
            {
              numericInput('opcoes_parametros_RFC2',
                           'mtry:',
                           value = 5,
                           min = 1,
                           max = 20)
            }
            
          })
          
          grid_RFC <- reactive({
            if(input$parametros_RFC == 'Padrao'){
              expand.grid(.mtry=c(1:10))
              
            }
            else{
              expand.grid(.mtry=input$opcoes_parametros_RFC2)
              
            }
            
          })
          
          fit_RFC <- eventReactive(input$computar_RFC, {
            tryCatch(
            withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
            
            if (input$opcao_RFC == "Padrao") {
              
              train(
                formula_RFC(),
                data = treino_RFC(),
                method = "rf",
                importance = T,
                tuneGrid=grid_RFC(),
                metric = input$metrica_RFC,
                na.action = 'na.omit'
              )
              
            }
            
            else{
              if (input$metodo_RFC == "repeatedcv") {
                ctrl <-
                  trainControl(
                    method = input$metodo_RFC ,
                    tuneGrid=grid_RFC(),
                    importance = T,
                    number = as.numeric(input$numero_RFC),
                    repeats = input$repeticoes_RFC
                  )
                
                train(
                  formula_RFC(),
                  data = treino_RFC(),
                  method = "rf",
                  tuneGrid=grid_RFC(),
                  importance = T,
                  metric = input$metrica_RFC,
                  trControl = ctrl ,
                                  na.action = 'na.omit'
                )
                
              } else{
                ctrl <-
                  trainControl(method = input$metodo_RFC ,
                               number = as.numeric(input$numero_RFC))
                
                train(
                  formula_RFC(),
                  data = treino_RFC(),
                  method = "rf",
                  importance = T,
                  tuneGrid=grid_RFC(),
                  trControl = ctrl,
                  metric = input$metrica_RFC,
                  na.action = 'na.omit'
                )
              }
              
            }
           }), error= function(e){return(NA)})
            
          })
          
          
          n_independentes_RFC <- reactive({
            length(input$XRFC2)
          })
          
          
          output$graficos_RFC <- renderPlotly({
            req(fit_RFC())
            if (n_independentes_RFC() > 1) {
              
              graficarImportanciaClassificacao(fit_RFC())
              
            }
            
          })
          
          
          output$erro_RFC <- renderPlot({
            req(fit_RFC())
            if(input$parametros_RFC == 'Padrao'){
              plot(fit_RFC(),lwd = 4 , ylab="Acurácia",main = paste0('Acurácia', ' x Hiperparâmetro'), cex = 2)} else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
            
          })
          
          
          output$confusao_RFC <- renderPlot({
            req(fit_RFC())
            
            RFCpredict <- predict(fit_RFC(), newdata=teste_RFC())
            cm <- confusionMatrix(RFCpredict, teste_RFC()[,input$YRFC2])
            ggplotConfusionMatrix(cm)
            
          })
          
          dat_RFC1 <- reactive({
            RFCpredict <- predict(fit_RFC(), newdata=teste_RFC())
            cm <- confusionMatrix(RFCpredict, teste_RFC()[,input$YRFC2])
            dat = round(t(as.data.frame(as.matrix(cm,what="classes"))),3)
            dat
          })
          
          dat_RFC <- reactive({
            
            dat_RFC1()[,c(1,2,5,6,7,11)]
            
          })
          output$specifics_RFC <- DT::renderDataTable({
            
            req(fit_RFC())
            DT::datatable(dat_RFC(), options = list(scrollX = TRUE,
                                                    searching = FALSE))
            
          })
          
          output$tabela_parametro_RFC <- DT::renderDataTable({
            req(fit_RFC())
            dat <- round(fit_RFC()$results,3)
            dat <- dat[1:(length(dat)-3)]
            DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
          })
          
          output$melhor_parametro_RFC <- renderPrint({
            req(fit_RFC())
            cat(paste0("Melhor valor encontrado para ", colnames(fit_RFC()$bestTune), " foi: ",fit_RFC()$bestTune,"\n"))
          })
          
          output$melhor_metrica_RFC <- renderPrint({
            req(fit_RFC())
            cat(paste0("Com Acurácia de: ",round(max(fit_RFC()$results['Accuracy']),5)))
          })
          
          dados_teste_RFC <- reactive({
            inFile <- input$arquivo_teste_RFC
            
            if (is.null(inFile))
              return(NULL)
            
            read.csv(inFile$datapath, header = T)
            
          })
          output$importacao_teste_RFC <- DT::renderDataTable(
            if (input$mostrar_teste_RFC== TRUE) {
              DT::datatable(dados_teste_RFC(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
            })
          previsao_RFC <- reactive({
            req(fit_importado_RFC())
            tryCatch(
              
              predict(object = fit_importado_RFC(), newdata = dados_teste_RFC())
              ,error = function(e) { return(rep('NA', nrow(dados_teste_RFC()))) }
            )
            
          })
          
          tabela_previsoes_RFC <- eventReactive(input$prever_RFC, {
            if(input$opcao_prever_RFC == TRUE){
              Resultado_Modelo = previsao_RFC()
              cbind(dados_teste_RFC(), Resultado_Modelo)
              
            }else{
              
              Resultado_Modelo = predict(fit_RFC(), dados_teste_RFC())
              data = cbind(dados_teste_RFC(), Resultado_Modelo)
              data
            }
          })
          
          output$resultado_previsoes_RFC <- DT::renderDataTable(
            DT::datatable(tabela_previsoes_RFC(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
          )
          
          output$download_RFC <- downloadHandler(
            filename = function() {
              "resultados_previsoes.xlsx"
            },
            content = function(fname) {
              write.xlsx(tabela_previsoes_RFC(), fname)
            }
          )
          
          output$baixar_RFC <- downloadHandler(
            
            filename = function() {
              "modelo_randfor_class.rds"
            },
            content = function(fname) {
              withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
              saveRDS(fit_RFC(), fname)
              })
            }
          )
          
          output$importar_RFC1 <- renderUI({
            if(input$opcao_prever_RFC == TRUE)
              tagList(
              fileInput(
                "importar_RFC",
                "Carregue seu modelo:",
                accept = c(".rds")
              ),
              helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
              )
            
          })
          
          
          fit_importado_RFC <- reactive({
            inFile <- input$importar_RFC
            
            if (is.null(inFile))
              return(NULL)
            
            readRDS(inFile$datapath)
            
          })
          
          output$mensagem_erro_rfc <- renderUI({
            if(is.na(fit_RFC()))
              includeMarkdown('Mensagem_Erro.Rmd')
          })
          
          output$resultados_obtidos_RFC <- renderUI({
            req(fit_RFC())
            h3("Resultados e Métricas do Modelo")
            
          }
          
          )
          
          output$resultados_obtidos_previsoes_RFC <- renderUI({
            req(tabela_previsoes_RFC())
            h3("Resultados da Classificação")
            
          })
          
         
          
          X_RFC <- reactive({
            
            input$XRFC2[1]
          })
          
          Y_RFC <- reactive({
            
            input$XRFC2[2]
          })
          
          class_RFC <- reactive({
            
            input$YRFC2
          })
          
          val_X_rfc <- reactive({ 
            (max(teste_RFC()[,X_RFC()]) - min(teste_RFC()[,X_RFC()])) / 150
            
          })
          
          val_Y_rfc <- reactive({
            
            (max(teste_RFC()[,Y_RFC()]) - min(teste_RFC()[,Y_RFC()]))/150
            
          })
          
          RFCgrid1 <- reactive({
            pl = seq(min(teste_RFC()[,X_RFC()]), max(teste_RFC()[,X_RFC()]), by=val_X_rfc())
            pw = seq(min(teste_RFC()[,Y_RFC()]), max(teste_RFC()[,Y_RFC()]), by=val_Y_rfc())
            lgrid <- expand.grid(x= pl, 
                                 y= pw)
            lgrid
            
          })
          
          
          RFCgrid <- reactive({
            grid = RFCgrid1()
            x <-X_RFC()
            y <- Y_RFC() 
            colnames(grid) <- c(x,y)
            as.data.frame(grid)
            
          })
          
          RFCPredGrid <-
            reactive({
              req(fit_RFC())
              RFCPredGrid =   predict(fit_RFC(), newdata=RFCgrid())
              RFCPredGrid = as.numeric(RFCPredGrid)
              RFCPredGrid
            })
          
          
          teste_Pred_RFC <-reactive({
            req(fit_RFC())
            testPred <- predict(fit_RFC(), newdata=teste_RFC())
            testPred <- as.numeric(testPred)
            
          }) 
          
          teste_RFC_Plot <- reactive({
            test <- teste_RFC()
            test$Pred <- teste_Pred_RFC()
            test$Pred <- as.factor(test$Pred)
            test
          })
          
          
          output$limite_RFC <- renderPlot({
            req(fit_RFC())
            
            if(n_independentes_RFC() == 2){
              
              
              ggplot(data=RFCgrid()) + stat_contour(aes(x=RFCgrid()[,X_RFC()], y=RFCgrid()[,Y_RFC()], z=RFCPredGrid()),
                                                    bins=2) +
                
                geom_point(data=teste_RFC_Plot(), aes(x=teste_RFC_Plot()[,X_RFC()], y=teste_RFC_Plot()[,Y_RFC()], colour=as.factor(teste_RFC_Plot()[,input$YRFC2])),
                           size=4, alpha = 0.7)+
                theme_bw() + xlab(X_RFC()) + ylab(Y_RFC()) + ggtitle("Limite de Decisão - Treino") + labs(title = "Limite de Decisão - Treino") +
                theme(legend.title = element_blank()) + theme(legend.position  ="bottom") + theme(
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=18),
                  plot.title = element_text(size=20, face = "bold"))
              
              
              
            }
          })
          
          
          
          output$superficie_RFC <- renderPlot({
            req(fit_RFC())
            if(n_independentes_RFC() == 2){
              graficarSuperficie(RFCgrid(), X_RFC(), Y_RFC(), RFCPredGrid())
              #                          ggplot(data=RFCgrid()) + stat_contour(aes(x=RFCgrid()[,X_RFC()], y=RFCgrid()[,Y_RFC()], z=RFCPredGrid()),
              #                                                           bins=2) +
              #                          geom_point(aes(x=RFCgrid()[,X_RFC()], y=RFCgrid()[,Y_RFC()], colour=as.factor(RFCPredGrid()))) +
              #                           
              #                            theme_bw() + xlab(X_RFC()) + ylab(Y_RFC()) + ggtitle("Regiao de Decisão") + labs(title = "Regiao de Decisão")+
              #                 theme(legend.title = element_blank()) + theme(legend.position  ="bottom")
            }
          })
          
          ######################### FIM Floresta Aleatória (Random Forest) CLASSIFIER ####################
          ########################## INICIO NAIVE BAYES CLASSIFIER ##################
          
          dados_NB1 <- reactive({
            inFile <- input$arquivo_NB
            
            if (is.null(inFile))
              return(NULL)
            
            read.csv(inFile$datapath, header = T)
            
          })
          
          dados_NB <- reactive({
            req(dados_NB1())
            dados_NB1()[complete.cases(dados_NB1()),]
          })
          
          
          output$importacao_NB <- DT::renderDataTable({
            if (input$mostrar_NB == TRUE) {
              DT::datatable(dados_NB(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
            }
          })
          
          output$XNB <- renderUI({
            nomes <- names(dados_NB())
            pickerInput(
              'XNB2',
              'Variáveis Independentes (X) - Atributos',
              nomes,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            )
          })
          
          output$YNB <- renderUI({
            nomes <- names(dados_NB())
            selectInput('YNB2', 'Variável Dependente (y) - Alvo:', nomes)
          })
          
          formula_NB <- reactive({
            x <- input$XNB2
            y <- input$YNB2
            
            formula <- c(x, y)
            
            size <- length(formula)
            independente <- formula[-size]
            dependente <- formula[size]
            formula_final <-
              reformulate(independente, response = dependente)
            formula_final
          })
          
          split_NB <- eventReactive(input$computar_NB,
                                     {
                                       set.seed(123)
                                       y <- input$YNB2
                                       createDataPartition(dados_NB()[, y], p = input$treinar_NB, list =
                                                             FALSE)
                                       
                                     })
          
          treino_NB1 <- eventReactive(input$computar_NB,
                                       {
                                         dados_NB()[split_NB(), ]
                                         
                                       })
          
          teste_NB1 <- eventReactive(input$computar_NB,
                                      {
                                        dados_NB()[-split_NB(), ]
                                        
                                      })
          
          output$opcoes_treinar_NB <- renderUI({
            if (input$opcao_NB == "Avancado") {
              selectInput(
                "metodo_NB",
                "Selecione o Método",
                choices = c(
                  
                  "Validação Cruzada k-fold" = "cv",
                  "Validação Cruzada k-fold com Repetição" = "repeatedcv"
                ),
                selected = "cv"
              )
              
            }
          })
          
          output$opcoes_treinar2_NB <- renderUI({
            if (input$opcao_NB == "Avancado") {
              selectInput(
                "numero_NB",
                "Número K de Subconjuntos",
                choices = c(3, 5, 7, 10),
                selected = 10
              )
            }
            
          })
          
          output$opcoes_treinar3_NB <- renderUI({
            req(input$metodo_NB)
            if (input$metodo_NB == "repeatedcv") {
              selectInput(
                "repeticoes_NB",
                "Número de Repetições",
                choices = c(3, 5, 7),
                selected = 3
              )
            }
            
          })
          
          treino_NB <- reactive({
            temp <- treino_NB1()
            temp[,input$YNB2] <- as.factor( temp[,input$YNB2])
            temp
          })
          
          teste_NB <- reactive({
            temp <- teste_NB1()
            temp[,input$YNB2] <- as.factor( temp[,input$YNB2])
            temp
          })
          
          output$opcoes_parametros_NB <- renderUI({
            
            if(input$parametros_NB == 'Escolher')
            {
              tagList(
              numericInput('opcoes_parametros_FL_NB2',
                           'fL:',
                           value = 0.5,
                           min = 0,
                           max = 3),
              selectInput('opcoes_parametros_kernel_NB2',
                          'Use Kernel',
                          choices=c('TRUE','FALSE')),
              numericInput('opcoes_parametros_adjust_NB2',
                           'Adjust:',
                           value = 1,
                           min =1,
                           max = 7)
              
              )
            }
            
          })
          
          grid_NB <- reactive({
            
              if(input$parametros_NB == 'Padrao'){
                expand.grid(fL=c(0,0.5,1.0,1.5,2), usekernel = c(TRUE,FALSE), adjust=c(1,2,3,4,5))
                
              }
              else{
                expand.grid(fL=c(input$opcoes_parametros_FL_NB2), usekernel = c(input$opcoes_parametros_kernel_NB2), adjust=c(input$opcoes_parametros_adjust_NB2))
            
              }
            
          })
          
          fit_NB <- eventReactive(input$computar_NB, {
            tryCatch(
            withProgress(message = 'Criando Modelo, aguarde ...', value =1, {
          
            if (input$opcao_NB == "Padrao") {
              
              train(
                formula_NB(),
                method = "nb",
                data = treino_NB(),
                tuneGrid = grid_NB(),
                metric = input$metrica_NB,
                na.action = 'na.omit'
              )
              
            }
            
            else{
              if (input$metodo_NB == "repeatedcv") {
                ctrl <-
                  trainControl(
                    method = input$metodo_NB ,
                    number = as.numeric(input$numero_NB),
                    repeats = input$repeticoes_NB
                  )
                
                train(
                  formula_NB(),
                  data = treino_NB(),
                  tuneGrid = grid_NB(),
                  method = 'nb',
                  metric = input$metrica_NB,
                  trControl = ctrl ,
                  
                  na.action = 'na.omit'
                )
                
              } else{
                ctrl <-
                  trainControl(method = input$metodo_NB ,
                               number = as.numeric(input$numero_NB))
                
                train(
                  formula_NB(),
                  data = treino_NB(),
                  method = 'nb',
                  trControl = ctrl,
                  tuneGrid = grid_NB(),
                  metric = input$metrica_NB,
                  na.action = 'na.omit'
                )
              }
              
            }
            }), error= function(e){return(NA)})
          })
          
          n_independentes_NB <- reactive({
            length(input$XNB2)
          })
          
          
          output$graficos_NB <- renderPlotly({
            req(fit_NB())
            if (n_independentes_NB() > 1) {
              
              graficarImportanciaClassificacao(fit_NB())
              
            }
            
          })
          
          
          output$erro_NB <- renderPlot({
            req(fit_NB())
            if(input$parametros_NB == 'Padrao'){
              plot(fit_NB(),lwd = 4 , ylab="Acurácia",main = paste0('Acurácia', ' x Hiperparâmetro'), cex = 2)
              
              } else{ggplot() + ggtitle('Visualização não disponível para esta opção.')}
            
          })
          
          
          output$confusao_NB <- renderPlot({
            req(fit_NB())
            
            NBpredict <- predict(fit_NB(), newdata=teste_NB())
            cm <- confusionMatrix(NBpredict, teste_NB()[,input$YNB2])
            ggplotConfusionMatrix(cm)
            
          })
          
          dat_NB1 <- reactive({
            NBpredict <- predict(fit_NB(), newdata=teste_NB())
            cm <- confusionMatrix(NBpredict, teste_NB()[,input$YNB2])
            dat = round(t(as.data.frame(as.matrix(cm,what="classes"))),3)
            dat
          })
          
          dat_NB <- reactive({
            
            dat_NB1()[,c(1,2,5,6,7,11)]
            
          })
          
          output$specifics_NB <- DT::renderDataTable({
            
            req(fit_NB())
            DT::datatable(dat_NB(), options = list(scrollX = TRUE,
                                                    searching = FALSE))
            
          })
          
          output$tabela_parametro_NB <- DT::renderDataTable({
            req(fit_NB())
            dat <- fit_NB()$results
            dat <- dat[1:(length(dat)-3)]
            DT::datatable(dat, options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
          })
          
          output$melhor_parametro_NB <- renderPrint({
            req(fit_NB())
            cat(paste0("Melhor valor encontrado para ", colnames(fit_NB()$bestTune), " foi: ",fit_NB()$bestTune,"\n"))
          })
          
          output$melhor_metrica_NB <- renderPrint({
            req(fit_NB())
            cat(paste0("Com Acurácia de: ",round(max(fit_NB()$results['Accuracy']),5)))
          })
          
          dados_teste_NB <- reactive({
            inFile <- input$arquivo_teste_NB
            
            if (is.null(inFile))
              return(NULL)
            
            read.csv(inFile$datapath, header = T)
            
          })
          output$importacao_teste_NB <- DT::renderDataTable(
            if (input$mostrar_teste_NB== TRUE) {
              DT::datatable(dados_teste_NB(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
              
            })
          
          previsao_NB <- reactive({
            req(fit_importado_NB())
            tryCatch(
              
              predict(object = fit_importado_NB(), newdata = dados_teste_NB())
              ,error = function(e) { return(rep('NA', nrow(dados_teste_NB()))) }
            )
            
          })
          
          tabela_previsoes_NB <- eventReactive(input$prever_NB, {
            if(input$opcao_prever_NB == TRUE){
              Resultado_Modelo = previsao_NB()
              cbind(dados_teste_NB(), Resultado_Modelo)
             
              
            }else{
              
              Resultado_Modelo = predict(fit_NB(), dados_teste_NB())
              data = cbind(dados_teste_NB(), Resultado_Modelo)
              datas
            }
          })
          
          output$resultado_previsoes_NB <- DT::renderDataTable(
            DT::datatable(tabela_previsoes_NB(), options = list(pageLength =5, scrollX = TRUE,  searching = FALSE))
            
          )
          
          output$download_NB <- downloadHandler(
            filename = function() {
              "resultados_previsoes.xlsx"
            },
            content = function(fname) {
              write.xlsx(tabela_previsoes_NB(), fname)
            }
          )
          
          
          
          X_NB <- reactive({
            
            input$XNB2[1]
          })
          
          Y_NB <- reactive({
            
            input$XNB2[2]
          })
          
          val_X_nb <- reactive({ 
            (max(teste_NB()[,X_NB()]) - min(teste_NB()[,X_NB()])) / 150
            
          })
          
          val_Y_nb <- reactive({
            
            (max(teste_NB()[,Y_NB()]) - min(teste_NB()[,Y_NB()]))/150
            
          })
          
          class_NB <- reactive({
            
            input$YNB2
          })
          
          NBgrid1 <- reactive({
            pl = seq(min(teste_NB()[,X_NB()]), max(teste_NB()[,X_NB()]), by=val_X_nb())
            pw = seq(min(teste_NB()[,Y_NB()]), max(teste_NB()[,Y_NB()]), by=val_Y_nb())
            lgrid <- expand.grid(x= pl, 
                                 y= pw)
            lgrid
            
          })
          
          
          NBgrid <- reactive({
            grid = NBgrid1()
            x <-X_NB()
            y <- Y_NB() 
            colnames(grid) <- c(x,y)
            as.data.frame(grid)
            
          })
          
          NBPredGrid <-
            reactive({
              req(fit_NB())
              NBPredGrid =   predict(fit_NB(), newdata=NBgrid())
              NBPredGrid = as.numeric(NBPredGrid)
              NBPredGrid
            })
          
          
          teste_Pred_NB <-reactive({
            req(fit_NB())
            testPred <- predict(fit_NB(), newdata=teste_NB())
            testPred <- as.numeric(testPred)
            
          }) 
          
          teste_NB_Plot <- reactive({
            test <- teste_NB()
            test$Pred <- teste_Pred_NB()
            test$Pred <- as.factor(test$Pred)
            test
          })
          
          
          output$limite_NB <- renderPlot({
            req(fit_NB())
            
            if(n_independentes_NB() == 2){
              
              
              ggplot(data=NBgrid()) + stat_contour(aes(x=NBgrid()[,X_NB()], y=NBgrid()[,Y_NB()], z=NBPredGrid()),
                                                    bins=2) +
                
                geom_point(data=teste_NB_Plot(), aes(x=teste_NB_Plot()[,X_NB()], y=teste_NB_Plot()[,Y_NB()], colour=as.factor(teste_NB_Plot()[,input$YNB2])),
                           size=4, alpha = 0.7)+
                theme_bw() + xlab(X_NB()) + ylab(Y_NB()) + ggtitle("Limite de Decisão - Treino") + labs(title = "Limite de Decisão - Treino") +
                theme(legend.title = element_blank()) + theme(legend.position  ="bottom") + theme(
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=18),
                  plot.title = element_text(size=20, face = "bold"))
              
              
              
            }
          })
          
         
          output$superficie_NB <- renderPlot({
            req(fit_NB())
            if(n_independentes_NB() == 2){
              graficarSuperficie(NBgrid(), X_NB(), Y_NB(), NBPredGrid())
              #                          ggplot(data=NBgrid()) + stat_contour(aes(x=NBgrid()[,X_NB()], y=NBgrid()[,Y_NB()], z=NBPredGrid()),
              #                                                           bins=2) +
              #                          geom_point(aes(x=NBgrid()[,X_NB()], y=NBgrid()[,Y_NB()], colour=as.factor(NBPredGrid()))) +
              #                           
              #                            theme_bw() + xlab(X_NB()) + ylab(Y_NB()) + ggtitle("Regiao de Decisão") + labs(title = "Regiao de Decisão")+
              #                 theme(legend.title = element_blank()) + theme(legend.position  ="bottom")
            }
          })
          
          output$baixar_NB <- downloadHandler(
            
            filename = function() {
              "modelo_nb.rds"
            },
            content = function(fname) {
              withProgress(message = 'Exportando modelo, aguarde ...', value=1, {
              saveRDS(fit_NB(), fname)
              })
            }
          )
          
          output$importar_NB1 <- renderUI({
            
            if(input$opcao_prever_NB == TRUE)
              tagList(
              fileInput(
                "importar_NB",
                "Carregue seu modelo:",
                accept = c(".rds")
              ),
              helpText("Garanta que seus novos dados tenham as mesmas colunas usadas para gerar o modelo importado. Caso contrário, encontrará o valor NA")
              )
            
          })
          
          
          fit_importado_NB <- reactive({
            inFile <- input$importar_NB
            
            if (is.null(inFile))
              return(NULL)
            
            readRDS(inFile$datapath)
            
          })
          
          output$mensagem_erro_nb <- renderUI({
            if(is.na(fit_NB()))
              includeMarkdown('Mensagem_Erro.Rmd')
          })
          output$resultados_obtidos_NB <- renderUI({
            req(fit_NB())
            h3("Resultados e Métricas do Modelo")
            
          }
          
          )
          
          output$resultados_obtidos_previsoes_NB <- renderUI({
            req(tabela_previsoes_NB())
            h3("Resultados da Classificação")
            
          })
       
          
  }
            
shinyApp(ui = ui, server = server)
         