#==========Bibliotecas (Instalacao e Carregamento)==========


# Instalacao dos Pacotes:

install.packages('caTools') ## Divisao Base de Treinamento e Teste
install.packages('ranger') ## Contem o algoritmo do Random Forest
install.packages('tuneRanger') ## Promove um tuning dos hiperparametros
install.packages('MLmetrics') # Calcula o MSE
install.packages('miscTools') # Verifica o Multiple RSquared

# Carregando as bibliotecas:

library("caTools", lib.loc = "~/R/win-library/3.6")
library("ranger", lib.loc = "~/R/win-library/3.6")
library("tuneRanger", lib.loc = "~/R/win-library/3.6")
library("MLmetrics", lib.loc = "~/R/win-library/3.6")
library("miscTools", lib.loc = "~/R/win-library/3.6")


#==========Ordinal Encoding==========


# Criando uma funcao que promovera um encoding das variaves categóricas para
# numéricas do tipo ORDINAL ENCODING.

encode_ordinal = function(x, order = unique(x)) {
  x = as.numeric(factor(x, levels = order, exclude = NULL))
  x
}


# Realizando o encoding nas variaveis: 'cut', 'color' e 'clarity'

copy_diamonds[["cut_encoded"]] = encode_ordinal(copy_diamonds[["cut"]], 
                                                 order = c('Regular', 
                                                           'Very Good',
                                                           'Premium', 
                                                           'Ideal'))
      ### Regular = 1; Very Good = 2; Premium = 3; Ideal = 4

copy_diamonds[["color_encoded"]] = encode_ordinal(copy_diamonds[["color"]], 
                                                   order = c("J", "I", "H", 
                                                             "G", "F", "E", 
                                                             "D"))
      ### J = 1; I = 2; H = 3; G = 4; F = 5; E = 6; D = 7

copy_diamonds[["clarity_encoded"]] = encode_ordinal(copy_diamonds[["clarity"]], 
                                                    order = c("I1SI2", "SI1", 
                                                              "VS2", "VS1", 
                                                              "VVS2", "VVS1", 
                                                              "IF"))
      ### I1SI2 = 1; SI1 = 2; VS2 = 3; VS1 = 4; VVS2 = 5; VVS1 = 6; IF = 7


# Verificando a natureza das classes das variaveis da base de dados:

sapply(copy_diamonds, class)


# Transformando a variavel 'price' em numeric:

copy_diamonds = transform(copy_diamonds, price = as.numeric(price))


# Criando uma copia do db para uso especifico de Machine Learning, eliminando 
# colunas que nao serao utilizadas

ml_dfdiamonds = copy_diamonds
ml_dfdiamonds[, c(3:5, 9:11, 13:17)] = NULL


#======================Divisao TREINO-TESTE================================


# Criando a "semente geradora" e promovendo a divisao da Base de Dados em 
# Treinamento e Teste:

set.seed(1)

split_cpdiamonds = sample.split(ml_dfdiamonds$X, SplitRatio = 0.70) ##Divide a
  ## base de dados aleatoriamente no percentual desejado, retornando a divisao 
  ## em TRUE (70%) e FALSE (30%) com criterio para o parâmetro 'X'

train_cpdiamonds = subset(ml_dfdiamonds, split_cpdiamonds == TRUE) ##Define 
  ## que os valores TRUE pertencem ao DB treinamento

test_cpdiamonds = subset(ml_dfdiamonds, split_cpdiamonds == FALSE) ##Define 
  ## que os valores FALSE pertencem ao DB teste


#Apagando o campo 'X', apenas utilizado para split da base de dados:

ml_dfdiamonds$X = train_cpdiamonds$X = test_cpdiamonds$X = NULL


#======================Tuning Hyperparameters===============================


# Buscando os melhores hiperparametros:

ptm = proc.time()
task = makeRegrTask(data = ml_dfdiamonds, target = "price")
estimateTimeTuneRanger(task)
tR = tuneRanger(task = task, num.trees = 200) ## measure = list(mse)
print(proc.time() - ptm)
tR

      ### Recommended parameter settings: 
      #     mtry      min.node.size     sample.fraction
      # 1    3             2               0.5971708
      #  Results: 
      #     mse     exec.time
      # 1 300686.1     5.316

tR2 = tuneRanger(task = task, num.trees = 1000, measure = list(mae))

      ### Recommended parameter settings: 
      #     mtry  min.node.size sample.fraction
      # 1    3             2       0.8321128
      # Results: 
      #     mae       exec.time
      # 1 262.6598      87.4


#==========[1] Regressao RANDOM FOREST==========


# Modelo de regressão utilizando Random Forest do pacote 'ranger':

reg.RF_cpdiamonds = ranger(formula = price ~., data = train_cpdiamonds, 
                           num.trees = 1000, mtry = tR$recommended.pars$mtry, 
                           importance = 'none', write.forest = TRUE, 
                           min.node.size = tR$recommended.pars$min.node.size, 
                           sample.fraction = 0.8321128, 
                           oob.error = TRUE, verbose = TRUE, 
                           splitrule = 'extratrees')


# Analisando a saida do modelo:

reg.RF_cpdiamonds           

      ### Ranger result

      # Call:
      # ranger(formula = price ~ ., data = train_cpdiamonds, num.trees = 1000, 
      # mtry = tR$recommended.pars$mtry, importance = "none", 
      # write.forest = TRUE, min.node.size = tR$recommended.pars$min.node.size, 
      # sample.fraction = 0.8321128, oob.error = TRUE, verbose = TRUE, 
      # splitrule = "extratrees") 
      # 
      # Type:                             Regression 
      # Number of trees:                  1000 
      # Sample size:                      35726 
      # Number of independent variables:  7 
      # Mtry:                             3 
      # Target node size:                 2 
      # Variable importance mode:         none 
      # Splitrule:                        extratrees 
      # Number of random splits:          1 
      # OOB prediction error (MSE):       307417.6 
      # R squared (OOB):                  0.9797581 


# Testando o modelo com os registros do próprio DB de TREINO e verificando o
# resultado das previsoes: 

pred_reg.RF_cpdiamonds_train = predict(object = reg.RF_cpdiamonds, 
                                       data = train_cpdiamonds[, c(1:3, 5:8)])
pred_reg.RF_cpdiamonds_train$predictions


# Calculando o Mean Absolute Error (MAE) do modelo na base de TREINO:

mae_train = MAE(y_pred = pred_reg.RF_cpdiamonds_train$predictions, 
                y_true = train_cpdiamonds$price) 
      ### MAE = U$ 142.47


# Analisando percentual do MAE pelo intervalo existente de preço (max - min)
# na base de TREINO:

mae_train/(max(train_cpdiamonds$price) - min(train_cpdiamonds$price)) 
      ### 0.77%


#Verificando o RSquared da base de TREINO:

rSquared(y = train_cpdiamonds$price, 
    resid = train_cpdiamonds$price - pred_reg.RF_cpdiamonds_train$predictions)
      ### RSquared TREINO = 0.9947795


--------------------------------------------------


# Avaliando agora as previsões para a base de TESTE:

pred_reg.RF_cpdiamonds_test = predict(object = reg.RF_cpdiamonds, 
                                    data = test_cpdiamonds[, c(1:3, 5:8)])
pred_reg.RF_cpdiamonds_test$predictions


#Calculando o Mean Absolute Error (MAE) para a base de TESTE:

mae_test = MAE(y_pred = pred_reg.RF_cpdiamonds_test$predictions, 
               y_true = test_cpdiamonds$price) 
      ### MAE = 274.8992


# Analisando percentual do MAE pelo intervalo existente de preço (max - min)
# na base de TESTE:

mae_test/(max(test_cpdiamonds$price) - min(test_cpdiamonds$price)) 
      ### 1.489%


# Verificando o RSquared da base de TESTE:

rSquared(y = test_cpdiamonds$price, 
      resid = test_cpdiamonds$price - pred_reg.RF_cpdiamonds_test$predictions)
      ### RSquared TESTE = 0.9790105


#==========[2] Modelo s/ Pre-Processamento==========


#Upload da Base de Dados:
  
SPdiamonds = read.csv("C:/Users/felip/Desktop/Cursos/Kaggle/diamonds.csv")


--------------------------------------------------


# Criando a "semente geradora" e divisao da Base de Dados em Treinamento e Teste sem
# Pre Processamento:

set.seed(12)

SPsplit_cpdiamonds = sample.split(SPdiamonds$X, SplitRatio = 0.70) ##Divide a
  ## base de dados aleatoriamente no percentual desejado, retornando a divisao 
  ## em TRUE (70%) e FALSE (30%)

SPtrain_cpdiamonds = subset(SPdiamonds, SPsplit_cpdiamonds == TRUE) ##Define 
  ## que os valores TRUE pertencem ao DB treinamento

SPtest_cpdiamonds = subset(SPdiamonds, SPsplit_cpdiamonds == FALSE) ##Define 
  ## que os valores FALSE pertencem ao DB teste


# Apagando 'X':

SPtrain_cpdiamonds$X = SPtest_cpdiamonds$X = NULL 


--------------------------------------------------


# Modelo de regressão utilizando Random Forest do pacote 'ranger' com os
# mesmos parametros ajustados do modelo original:

SPreg.RF_cpdiamonds = ranger(formula = price ~ ., data = SPtrain_cpdiamonds, 
                           num.trees = 1000, mtry = tR$recommended.pars$mtry, 
                           importance = 'none', write.forest = TRUE, 
                           min.node.size = tR$recommended.pars$min.node.size, 
                           sample.fraction = 0.8321128, 
                           oob.error = TRUE, verbose = TRUE, 
                           splitrule = 'extratrees')


# Analisando a saida:

SPreg.RF_cpdiamonds


# Testando o modelo com os registros do DB de TREINO e verificando as 
# previsoes: 

SPpred_reg.RF_cpdiamonds_train = predict(object = SPreg.RF_cpdiamonds, 
                                    data = SPtrain_cpdiamonds[, c(1:6, 8:10)])

SPpred_reg.RF_cpdiamonds_train$predictions


# Calculando o MAE do modelo na base de TREINO:

SPmae_train = MAE(y_pred = SPpred_reg.RF_cpdiamonds_train$predictions, 
                y_true = SPtrain_cpdiamonds$price) 
      ### MAE = U$ 165.92


--------------------------------------------------
  

# Avaliando agora as previsões para a base de TESTE:
  
SPpred_reg.RF_cpdiamonds_test = predict(object = SPreg.RF_cpdiamonds, 
                                      data = SPtest_cpdiamonds[, c(1:6, 8:10)])

SPpred_reg.RF_cpdiamonds_test$predictions


# Calculando o MAE para a base de TESTE:

SPmae_test = MAE(y_pred = SPpred_reg.RF_cpdiamonds_test$predictions, 
               y_true = SPtest_cpdiamonds$price) 
      ### U$ 339.1945


#======================Comparando os Resultados [1] x [2]=====================


# Analise dos resultados de TREINO:

SPmae_train - mae_train 
      ### U$ 23.45


# Analise dos resultados de TESTE:

SPmae_test - mae_test 
      ### U$ 64.30
