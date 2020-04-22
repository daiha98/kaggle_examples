#==========Bibliotecas (Instalacao e Importacao)==========


# Instalacao dos Pacotes:

install.packages('dplyr') ## Manipulacao de Dados
install.packages('ggplot2') ## Visualizacao Grafica dos Dados
install.packages('gridExtra') ## Visualizacao em Conjunto de Graficos
install.packages('recipes') ## Adiciona PreProcessamento ao DB
install.packages('timetk') ## Adiciona features
## que auxilia nos modelos de ML para Time Series
install.packages('caret') ## Gera o Tuning dos parametros do modelo
install.packages('parsnip') ## Interface para modelos de Regressao
install.packages('glmnet') ## Modelo utilizado para ML
install.packages('workflows') ## Cruza o que foi pre processado com
## o modelo criado
install.packages('tidyverse', dependencies=TRUE, type="source", 
                 repos="https://cloud.r-project.org") 
## Auxilia na manipulacao dos dados
install.packages('vctrs') ## Inserido no 'tidyverse' que precisa ser
## atualizado
install.packages('MLmetrics') # Calcula o MSE
install.packages('forecast') # Algoritmo Holt-Winters
install.packages('prophet') #Algoritmo Prophet


# Carregando as bibliotecas:

library("dplyr", lib.loc="~/R/win-library/3.6")
library("ggplot2", lib.loc="~/R/win-library/3.6")
library("zoo", lib.loc="~/R/win-library/3.6")
library("reshape2", lib.loc = "~/R/win-library/3.6")
library("gridExtra", lib.loc="~/R/win-library/3.6")
library("recipes", lib.loc="~/R/win-library/3.6")
library("timetk", lib.loc="~/R/win-library/3.6")
library("caret", lib.loc="~/R/win-library/3.6")
library("glmnet", lib.loc="~/R/win-library/3.6")
library("parsnip", lib.loc="~/R/win-library/3.6")
library("workflows", lib.loc="~/R/win-library/3.6")
library("tidyverse", lib.loc="~/R/win-library/3.6")
library("vctrs", lib.loc="~/R/win-library/3.6")
library("MLmetrics", lib.loc = "~/R/win-library/3.6")
library("forecast", lib.loc = "~/R/win-library/3.6")
library("prophet", lib.loc = "~/R/win-library/3.6")


# Atualizando bibliotecas necessarias:

remotes::update_packages("tidyverse")
remotes::update_packages("vctrs")


#=========PreProcessamento==========


# Criando um df contendo as datas e a frequencia das ocorrencias 
# segundo essas datas

crime_dateFreq = data.frame(table(crime_preproccess$DATE))

  ## Renomeando as colunas:

  names(crime_dateFreq)[names(crime_dateFreq)=="Var1"] = "DATE"
  names(crime_dateFreq)[names(crime_dateFreq)=="Freq"] = "NUM_INCIDENTS"
  

# Alterando a coluna 'DATE' para classe 'as.Date':
  
  crime_dateFreq$DATE = as.Date(crime_dateFreq$DATE)

  
# Excluindo Outliers:
  
crime_dateFreq = crime_dateFreq %>%
    filter(DATE < '2019-09-28')


#=========Time Series - Data Analysis==========


# Fazendo um grafico de Series Temporais para analisar a curva de
# densidade das ocorrencias

ggplot(data = crime_dateFreq, aes(x = DATE, y = NUM_INCIDENTS)) +
  geom_line(colour = 'darkblue', size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  geom_smooth(method = "auto", color = 'red', size = 2) +
  xlab('Data') +
  ylab('N° Total de Ocorrências') +
  labs(title = 'Número de Ocorrências', 
       subtitle = "Boston-MA: Junho/2015 a Setembro/2019 
       (Fonte: data.boston.gov)") +
  theme_minimal()


# Mostrando graficamente a divisao de treino e teste

ggplot(data = crime_dateFreq, aes(x = DATE, y = NUM_INCIDENTS)) +
  geom_line(colour = 'darkblue', size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  geom_smooth(method = "auto", color = 'red', size = 2) +
  geom_rect(xmin = as.Date("2018-09-27"),
            xmax = as.Date("2019-12-01"),
            ymin = 0, ymax = 400,
            fill = 'lightgrey', alpha = 0.015)  +
  annotate("text", label = "Train Region", x = as.Date("2017-01-01"), 
           y = 350, color = "black") +
  annotate("text", label = "Test Region", x = as.Date("2019-04-01"), 
           y = 350, color = "black") +
  xlab('Data') +
  ylab('N° Total de Ocorrências') +
  labs(title = 'Número de Ocorrências', 
       subtitle = "Boston-MA: Junho/2015 a Setembro/2019 
       (Fonte: data.boston.gov)") +
  theme_minimal()



#=========Divisao Treino-Teste==========


train_crime = crime_dateFreq %>% 
  filter(DATE < "2018-09-27")
test_crime  = crime_dateFreq %>% 
  filter(DATE >= "2018-09-27")


#=========PreProcessamento 'RECIPE'==========


# Add 'timeseries_signature':

rcp_spec_crime = recipe(NUM_INCIDENTS ~ ., data = train_crime) %>%
  step_timeseries_signature(DATE) 
      ### Creates a a specification of a recipe step that will 
      ### convert date or date-time data into many features that can 
      ### aid in machine learning with time-series data 


# Feature Engineering com 'prep()' em 'DATE':

bake(prep(rcp_spec_crime), new_data = train_crime)
      ### Aplicando as novas features ao db:


rcp_spec_FINAL = rcp_spec_crime %>%
    step_rm(DATE) %>%
    step_rm(contains("iso"), 
            contains("second"), contains("minute"), contains("hour"),
            contains("am.pm"), contains("xts")) %>%
    step_normalize(contains("index.num"), DATE_year) %>%
    step_dummy(contains("lbl"), one_hot = TRUE)


crime_FINAL = bake(prep(rcp_spec_FINAL), new_data = train_crime)


#=========Tuning Hyperparameters 'glmnet'==========


control = trainControl(method = "repeatedcv", number = 10, 
                       repeats = 3)


glm_tune = train(NUM_INCIDENTS ~ ., data = crime_FINAL, 
                 trControl = control, method = "glmnet", 
                 tuneLenght = 10, metric = 'RMSE')
glm_tune

# glmnet 
# 
# 1566 samples
# 35 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 1410, 1410, 1408, 1408, 1409, 1409, ... 
# Resampling results across tuning parameters:
#   
# alpha  lambda      RMSE      Rsquared   MAE     
# 0.10   0.02300277  24.88867  0.3581334  19.55182
# 0.10   0.23002765  24.88288  0.3582394  19.53877
# 0.10   2.30027650  24.96812  0.3559052  19.52867
# 0.55   0.02300277  24.88688  0.3582079  19.55082
# 0.55   0.23002765  24.88123  0.3581954  19.51726
# 0.55   2.30027650  25.70585  0.3324987  20.01970
# 1.00   0.02300277  24.88671  0.3581692  19.54839
# 1.00   0.23002765  24.90101  0.3573465  19.51314
# 1.00   2.30027650  26.86925  0.2788803  20.82357
# 
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were alpha = 0.55 and lambda
# = 0.2300277.


#=========Criando o modelo de regressao c/ 'parsnip' e 'glmnet'======


crime_model = linear_reg(mode = 'regression', penalty = 0.2300277,
                         mixture = 0.55) %>%
  set_engine('glmnet')


#=========Cruzando PreProcessamento e Modelo c/ 'workflow'==========


wrkf_crimeModel = workflow() %>%
  add_recipe(rcp_spec_FINAL) %>%
  add_model(crime_model)

wrkf_crimeModel


#=========Treinando o Modelo 'glmnet' e Verificando as Previsoes==========


wrkf_train = wrkf_crimeModel %>% 
  fit(data = train_crime)


# Visualizando as previsoes na base de TREINO:

pred_train_crime = wrkf_train %>%
  predict(train_crime) %>%
  bind_cols(train_crime)


  ## Verificando metricas da base de TREINO:

  mae_train = MAE(y_pred = pred_train_crime$.pred, 
                y_true = pred_train_crime$NUM_INCIDENTS) ## 19.14774
  
  mape_train = MAPE(y_pred = pred_train_crime$.pred, 
       y_true = pred_train_crime$NUM_INCIDENTS) ## 7,83%


# Agora, analisando as previsões na base de TESTE:

pred_test_crime = wrkf_train %>%
  predict(test_crime) %>%
  bind_cols(test_crime)

  ## Verificando metricas da base de TESTE:

  mae_test = MAE(y_pred = pred_test_crime$.pred, 
               y_true = test_crime$NUM_INCIDENTS) ## 19.39575
  
  mape_train = MAPE(y_pred = pred_test_crime$.pred, 
                    y_true = test_crime$NUM_INCIDENTS) ## 8,42 %
  
  ## Analisando percentual do MAE pelo intervalo existente de preço 
  ## (max - min) na base de TESTE:
  
  mae_test/(max(test_crime$NUM_INCIDENTS) - 
               min(test_crime$NUM_INCIDENTS)) 
      ### 10,21%
  

#=========Regression to the Mean==========
  

# Checando o percentual do Regression to the Mean na base de TREINO:
  
RttM_train = 100 * (1 - cor(method = "pearson", 
                            x = pred_train_crime$NUM_INCIDENTS, 
                            y = pred_train_crime$.pred)) ## 39,68%
  
    ## If r = 1 (i.e. perfect correlation), then 1-1 = 0 and the 
    ## regression to the mean is zero. In other words, if your data 
    ## has perfect correlation, it will never regress to the mean.
    ## With an r of zero, there is 100 percent regression to the mean.
    ## In other words, data with an r of zero will always regress to 
    ## the mean.
  
#=========Visualizando no Time Series as Previsoes do 'glmnet'==========


ggplot(data = crime_dateFreq, aes(x = DATE, y = NUM_INCIDENTS)) +
  geom_line(colour = 'darkblue', size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  geom_smooth(method = "auto", color = 'red', size = 2, se = TRUE) +
  geom_rect(xmin = as.Date("2018-09-27"),
            xmax = as.Date("2019-12-01"),
            ymin = 0, ymax = 400,
            fill = 'lightgrey', alpha = 0.015) +
  annotate("text", label = "Train Region", x = as.Date("2017-01-01"), 
           y = 350, color = "black") +
  annotate("text", label = "Test Region", x = as.Date("2019-04-01"), 
           y = 350, color = "black") +
  xlab('Date') +
  ylab('N° INCIDENTS') +
  labs(title = 'Número de Ocorrências', 
       subtitle = "Boston-MA: Junho/2015 a Setembro/2019 
       (Fonte: data.boston.gov)") +
  theme_minimal() +
  geom_smooth(data = pred_train_crime, aes(x = DATE, y = .pred),
              color = 'yellow', size = 2) +
  geom_smooth(data = pred_test_crime, aes(x = DATE, y = .pred),
              color = 'green', size = 2)


#=========PreProcessamento de Dados Futuros==========  

  
# Extraindo index:
  
idx = crime_dateFreq %>% 
  tk_index()


# Criando novos dados a serem previstos a partir da ultima data do
# db crime_dateFreq:

idx_future = idx %>%
  tk_make_future_timeseries(n_future = 365)
idx_future = as.Date(idx_future)

future_crime_dateFreq = data.frame(DATE = idx_future)


#=========Prevendo Dados Futuros 'glmnet'==========  


# Visualizando as previsões FUTURAS:

pred_future_crime = wrkf_train %>%
  predict(future_crime_dateFreq) %>%
  bind_cols(future_crime_dateFreq)


# Formatando as classes:

pred_future_crime$.pred = format(pred_future_crime$.pred, digits = 2)
pred_future_crime$.pred = as.numeric(pred_future_crime$.pred)


#=========Visualizando 'glmnet' no Time Series COMPLETA==========


tsPlot1 = ggplot(data = crime_dateFreq, aes(x = DATE, y = NUM_INCIDENTS)) +
  geom_line(colour = 'darkblue', size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months",
               limits = c(min(crime_dateFreq$DATE), 
                          max(pred_future_crime$DATE))) +
  geom_smooth(method = "auto", color = 'red', size = 2, se = TRUE) +
  geom_rect(xmin = as.Date("2018-09-27"),
            xmax = as.Date("2019-09-28"),
            ymin = 0, ymax = 400,
            fill = 'lightgrey', alpha = 0.015) +
  annotate("text", label = "Train Region", x = as.Date("2017-01-01"), 
           y = 350, color = "black") +
  annotate("text", label = "Test Region", x = as.Date("2019-04-01"), 
           y = 350, color = "black") +
  xlab('Date') +
  ylab('N° INCIDENTS') +
  labs(title = 'Número de Ocorrências e Forecast c/ glmnet', 
       subtitle = "Boston-MA: Junho/2015 a Setembro/2020 
       (Fonte: data.boston.gov)") +
  theme_minimal() +
  geom_smooth(data = pred_train_crime, aes(x = DATE, y = .pred),
              color = 'yellow', size = 2, se = TRUE) +
  geom_smooth(data = pred_test_crime, aes(x = DATE, y = .pred),
              color = 'green', size = 2, se = TRUE) +
  geom_line(data = pred_future_crime, aes(x = DATE, y = .pred),
             colour = 'blue', size = 0.5) +
  geom_rect(xmin = as.Date("2019-09-28"),
            xmax = as.Date("2020-09-28"),
            ymin = 0, ymax = 400,
            fill = 'blue', alpha = 0.002) +
  geom_smooth(data = pred_future_crime, aes(x = DATE, y = .pred),
              color = 'lightgreen', size = 2, se = TRUE) +
  annotate("text", label = "Forecast Region", x = as.Date("2020-04-01"), 
           y = 350, color = "black")


#=========Decomposing Time Series========== 


# Criando um objeto tipo 'ts':

decom_crime = ts(crime_dateFreq$NUM_INCIDENTS, start = c(2015, 166),
                 end = c(2019, 270), frequency = 365.25)


# Promovendo um Seasonal Decomposition Time Series:

dts = stl(decom_crime, s.window = "per")


# Transformando em um data frame para melhor visualizacao:

tseriesCrime = as.data.frame(cbind(decom_crime, dts$time.series))
colnames(tseriesCrime) = c("OBSERVED", "SEASONAL", "TREND", 
                           "RANDOM")
tseriesCrime$DATE = crime_dateFreq$DATE


# Criando uma nova variavel para plot do 'decompose':

tseriesPLOT = melt(tseriesCrime, 'DATE')


# Decompose TS plot:

ggplot(tseriesPLOT, aes(x = DATE, y = value)) + 
  geom_line() +
  facet_free(variable~.) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months")


#=========Holt-Winters==========


# Criando um modelo Holt-Winters:

hw_crime = HoltWinters(decom_crime, start.periods = c(2015, 166))


summary(hw_crime)
      ### Error measures:
                # ME     RMSE      MAE       MPE     MAPE      MASE
## Training set 0.1410281 28.81489 22.60399 -1.322326 9.336672 0.7965082
                # ACF1
## Training set 0.1622961

## l.start = 247.6115, b.start = -4e-04, s.start = 28.8518


# Melhorando a visualizacao para um dataframe:

hw_df = data.frame("DATE" = crime_dateFreq[366:1566, 1],
                    "FITTED" = as.numeric(format(hw_crime$fitted[, 1],
                                                 digits = 2)))


# Fazendo um predict a partir do modelo Holt-Winters pra 1 ano:

forecast_crime = as.data.frame(predict(hw_crime, 
                                       n.ahead = 365)) 

# Lapidando os dados para melhor trabalho:

forecast_crime = cbind(future_crime_dateFreq$DATE, forecast_crime)
colnames(forecast_crime) = c("DATE", "PRED")


#=========TS Holt-Winters==========


# Plotando o grafico das previsoes com as ocorrencias historicas:

tsPlot2 = ggplot(data = crime_dateFreq, aes(x = DATE, y = NUM_INCIDENTS)) +
  geom_line(colour = 'darkblue', size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months",
               limits = c(min(crime_dateFreq$DATE), 
                          max(forecast_crime$DATE))) +
  geom_smooth(method = "auto", color = 'red', size = 2, se = TRUE) +
  geom_smooth(data = hw_df, aes(x = DATE, y = FITTED),
              color = 'yellow', size = 2, se = TRUE) +
  annotate("text", label = "Train Region", x = as.Date("2017-01-01"), 
           y = 350, color = "black") +
  xlab('Date') +
  ylab('N° INCIDENTS') +
  labs(title = 'Número de Ocorrências e Forecast c/ Holt-Winters', 
       subtitle = "Boston-MA: Junho/2015 a Setembro/2020 
       (Fonte: data.boston.gov)") +
  theme_minimal() +
  geom_line(data = forecast_crime, aes(x = DATE, y = PRED),
             colour = 'blue', size = 0.5) +
  geom_rect(xmin = as.Date("2019-09-28"),
            xmax = as.Date("2020-09-28"),
            ymin = 0, ymax = 400,
            fill = 'blue', alpha = 0.002) +
  geom_smooth(data = forecast_crime, aes(x = DATE, y = PRED),
               color = 'green', size = 2, se = TRUE) +
   annotate("text", label = "Forecast Region", 
            x = as.Date("2020-04-01"),  
            y = 350, color = "black")


#=========Analise Comparativa dos 2 Algoritmos==========


grid.arrange(tsPlot1, tsPlot2, ncol = 1, nrow = 2)

'''ALTERAR O INTERVALO PARA MONTH'''


#=========Tuning Hyperparameters 'svm_rbf'==========


svm_control = trainControl(method = "repeatedcv", number = 10, 
                       repeats = 3)


svm_tune = train(NUM_INCIDENTS ~ ., data = crime_FINAL, 
                 trControl = svm_control, method = "svmRadial", 
                 tuneLenght = 10, metric = 'MAE')
svm_tune

# Support Vector Machines with Radial Basis Function Kernel
# 
# 1200 samples
# 35 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times)
# Summary of sample sizes: 1080, 1080, 1080, 1080, 1080, 1080, ...
# Resampling results across tuning parameters:
# 
#   C     RMSE      Rsquared   MAE
# 0.25  24.62970  0.3670596  19.16604
# 0.50  24.39465  0.3709601  19.02896
# 1.00  24.41924  0.3680516  19.08875
# 
# Tuning parameter 'sigma' was held constant at a value of 0.01595306
# MAE was used to select the optimal model using the smallest value.
# The final values used for the model were sigma = 0.01595306 and C = 0.5.


#=========Criando o modelo de regressao c/ 'parsnip' e 'svm_rbf'======


crime_SVMmodel = svm_rbf(mode = 'regression', cost = 0.5,
                         rbf_sigma = 0.01595306) %>%
  set_engine('kernlab')


#=========Cruzando PreProcessamento e 'svm_rbf'==========


wrkf_crimeSVMModel = workflow() %>%
  add_recipe(rcp_spec_FINAL) %>%
  add_model(crime_SVMmodel)

wrkf_crimeSVMModel


#=========Treinando SVM e Verificando Previsoes==========


wrkf_SVMtrain = wrkf_crimeSVMModel %>% 
  fit(data = train_crime)


# Visualizando as previsoes na base de TREINO:

pred_SVMtrain_crime = wrkf_SVMtrain %>%
  predict(train_crime) %>%
  bind_cols(train_crime)


  ## Verificando metricas da base de TREINO:
  
  mae_SVMtrain = MAE(y_pred = pred_SVMtrain_crime$.pred, 
                  y_true = train_crime$NUM_INCIDENTS) ## 16.88825
  
  mape_SVMtrain = MAPE(y_pred = pred_SVMtrain_crime$.pred, 
                    y_true = train_crime$NUM_INCIDENTS) ## 6,92%


# Agora, analisando as previsões na base de TESTE:

pred_SVMtest_crime = wrkf_SVMtrain %>%
  predict(test_crime) %>%
  bind_cols(test_crime)

  ## Verificando metricas da base de TESTE:
  
  mae_SVMtest = MAE(y_pred = pred_SVMtest_crime$.pred, 
                 y_true = test_crime$NUM_INCIDENTS) ## 20.2142
  
  mape_SVMtrain = MAPE(y_pred = pred_SVMtest_crime$.pred, 
                    y_true = test_crime$NUM_INCIDENTS) ## 8,76 %

  
## Analisando percentual do MAE pelo intervalo existente de preço 
## (max - min) na base de TESTE:

mae_SVMtest/(max(test_crime$NUM_INCIDENTS) - 
            min(test_crime$NUM_INCIDENTS)) 
      ### 10,64%


#=========Visualizando no Time Series as Previsoes SVM==========


ggplot(data = crime_dateFreq, aes(x = DATE, y = NUM_INCIDENTS)) +
  geom_line(colour = 'darkblue', size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months") +
  geom_smooth(method = "auto", color = 'red', size = 2, se = TRUE) +
  geom_rect(xmin = as.Date("2018-09-27"),
            xmax = as.Date("2019-12-01"),
            ymin = 0, ymax = 400,
            fill = 'lightgrey', alpha = 0.015) +
  annotate("text", label = "Train Region", x = as.Date("2017-01-01"), 
           y = 350, color = "black") +
  annotate("text", label = "Test Region", x = as.Date("2019-04-01"), 
           y = 350, color = "black") +
  xlab('Date') +
  ylab('N° INCIDENTS') +
  labs(title = 'Número de Ocorrências', 
       subtitle = "Boston-MA: Junho/2015 a Setembro/2019 
       (Fonte: data.boston.gov)") +
  theme_minimal() +
  geom_smooth(data = pred_SVMtrain_crime, aes(x = DATE, y = .pred),
              color = 'yellow', size = 2) +
  geom_smooth(data = pred_SVMtest_crime, aes(x = DATE, y = .pred),
              color = 'green', size = 2)


#=========Forecast c/ SVM==========  


# Visualizando as previsões FUTURAS:

pred_SVMfuture_crime = wrkf_SVMtrain %>%
  predict(future_crime_dateFreq) %>%
  bind_cols(future_crime_dateFreq)


# Formatando as classes:

pred_SVMfuture_crime$.pred = format(
  pred_SVMfuture_crime$.pred, digits = 2)
pred_SVMfuture_crime$.pred = as.numeric(pred_SVMfuture_crime$.pred)


#=========Visualizando no Time Series SVM COMPLETA==========


tsPlot3 = ggplot(data = crime_dateFreq, aes(x = DATE, y = NUM_INCIDENTS)) +
  geom_line(colour = 'darkblue', size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months",
               limits = c(min(crime_dateFreq$DATE), 
                          max(pred_SVMfuture_crime$DATE))) +
  geom_smooth(method = "auto", color = 'red', size = 2, se = TRUE) +
  geom_rect(xmin = as.Date("2018-09-27"),
            xmax = as.Date("2019-09-28"),
            ymin = 0, ymax = 400,
            fill = 'lightgrey', alpha = 0.015) +
  annotate("text", label = "Train Region", x = as.Date("2017-01-01"), 
           y = 350, color = "black") +
  annotate("text", label = "Test Region", x = as.Date("2019-04-01"), 
           y = 350, color = "black") +
  xlab('Date') +
  ylab('N° INCIDENTS') +
  labs(title = 'Número de Ocorrências e Forecast c/ SVM', 
       subtitle = "Boston-MA: Junho/2015 a Setembro/2020 
       (Fonte: data.boston.gov)") +
  theme_minimal() +
  geom_smooth(data = pred_SVMtrain_crime, aes(x = DATE, y = .pred),
              color = 'yellow', size = 2, se = TRUE) +
  geom_smooth(data = pred_SVMtest_crime, aes(x = DATE, y = .pred),
              color = 'green', size = 2, se = TRUE) +
  geom_line(data = pred_SVMfuture_crime, aes(x = DATE, y = .pred),
            colour = 'blue', size = 0.5) +
  geom_rect(xmin = as.Date("2019-09-28"),
            xmax = as.Date("2020-09-28"),
            ymin = 0, ymax = 400,
            fill = 'blue', alpha = 0.002) +
  geom_smooth(data = pred_SVMfuture_crime, aes(x = DATE, y = .pred),
              color = 'lightgreen', size = 2, se = TRUE) +
  annotate("text", label = "Future Region", x = as.Date("2020-04-01"), 
           y = 350, color = "black")


#=========Comparando os 3 Algoritmos==========


grid.arrange(tsPlot1, tsPlot2, tsPlot3, ncol = 2, nrow = 2)


#=========Prophet Model==========


# Criando um df para ser utilizado:

df_prophetCrime = crime_dateFreq
colnames(df_prophetCrime) = c("ds", "y")


# Criando um modelo Prophet:

prophet_crime = prophet(df = df_prophetCrime, growth = "linear",
                        daily.seasonality = TRUE)


# Gerando um df com as datas futuras:

future_prophetCrime = make_future_dataframe(prophet_crime, 
                                            periods = 365, 
                                            freq = 'day')


# Fazendo um predict do df com as datas futuras:

forecast_prophet = predict(prophet_crime, future_prophetCrime)


# Dividindo a base de dados em 'fitted' e 'forecast':

fitted_prophet = forecast_prophet[1:1566, ]
forecast_prophet = forecast_prophet[1567:1931, ]


# Transformando para class 'as.Date':

fitted_prophet$ds = format(fitted_prophet$ds, "%Y-%m-%d")
fitted_prophet$ds = as.Date(fitted_prophet$ds)

forecast_prophet$ds = format(forecast_prophet$ds, "%Y-%m-%d")
forecast_prophet$ds = as.Date(forecast_prophet$ds)


#=========Analisando Metricas do Prophet==========


mae_prophet = MAE(y_pred = fitted_prophet$yhat, 
                   y_true = crime_dateFreq$NUM_INCIDENTS) ## 19.24897

mape_prophet = MAPE(y_pred = fitted_prophet$yhat, 
                    y_true = crime_dateFreq$NUM_INCIDENTS) ## 7,89%


#=========TS Prophet==========


tsPlot4 = ggplot(data = crime_dateFreq, aes(x = DATE, y = NUM_INCIDENTS)) +
  geom_line(colour = 'darkblue', size = 0.5) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 months",
               limits = c(min(crime_dateFreq$DATE), 
                          max(forecast_prophet$ds))) +
  geom_smooth(method = "auto", color = 'red', size = 2, se = TRUE) +
  geom_smooth(data = fitted_prophet, aes(x = ds, y = yhat),
              color = 'yellow', size = 2, se = TRUE) +
  annotate("text", label = "Train Region", x = as.Date("2017-01-01"), 
           y = 350, color = "black") +
  xlab('Date') +
  ylab('N° INCIDENTS') +
  labs(title = 'Número de Ocorrências e Forecast c/ Prophet', 
       subtitle = "Boston-MA: Junho/2015 a Setembro/2020 
       (Fonte: data.boston.gov)") +
  theme_minimal() +
  geom_line(data = forecast_prophet, aes(x = ds, y = yhat),
            colour = 'blue', size = 0.5) +
  geom_rect(xmin = as.Date("2019-09-28"),
            xmax = as.Date("2020-09-28"),
            ymin = 0, ymax = 400,
            fill = 'blue', alpha = 0.002) +
  geom_smooth(data = forecast_prophet, aes(x = ds, y = yhat),
              color = 'green', size = 2, se = TRUE) +
  annotate("text", label = "Forecast Region", 
           x = as.Date("2020-04-01"),  
           y = 350, color = "black")


#=========Analise em conjunto dos 4 Algoritmos==========


grid.arrange(tsPlot1, tsPlot2, 
             tsPlot3, tsPlot4, 
             ncol = 2, nrow = 2)

# 1 - glmnet                    # 2 - Holt-Winters
  ## MAE = 19.14774               ## MAE = 22.60399
  ## MAPE = 7,83%                 ## MAPE = 9.34%


# 3 - SVM                       # 4 - Prophet
  ## MAE = 16.88825               ## MAE = 19.24897
  ## MAPE = 6,92%                 ## MAPE = 7,89%

