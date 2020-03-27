#==========Bibliotecas (Instalacao e Importacao)==========


# Instalacao dos Pacotes:

install.packages("RMySQL") ##Acesso ao Banco de Dados MySQL
install.packages('dplyr') ## Manipulacao de Dados
install.packages('ggplot2') ## Visualizacao Grafica dos Dados
install.packages('caTools') ## Divisao Base de Treinamento e Teste
install.packages('randomForest') ## Contem o algoritmo do Random Forest
install.packages('rpart') ## Contem o algoritmo de Arvore de Decisao
install.packages('rpart.plot') ## Permite visualizar a Arvore de maneira grafica
install.packages('caret') ## Gera a Matriz Confusao
install.packages('class') ## Contem o algoritmo do KNN
install.packages('e1071') ## Contem o algoritmo do SVM
install.packages('data.table') #Verifica a consistencia dos DBs Treino-Teste
install.packages('gridExtra') # Cria graficos em grid
install.packages('MLmetrics') # Calcula o MSE
install.packages('miscTools') # Verifica o Multiple RSquared da base de TESTE


# Carregando as bibliotecas:

library("RMySQL", lib.loc="~/R/win-library/3.6")
library("dplyr", lib.loc="~/R/win-library/3.6")
library("ggplot2", lib.loc="~/R/win-library/3.6")
library("caTools", lib.loc="~/R/win-library/3.6")
library("randomForest", lib.loc="~/R/win-library/3.6")
library('rpart', lib.loc="~/R/win-library/3.6") 
library('rpart.plot', lib.loc="~/R/win-library/3.6") 
library("caret", lib.loc="~/R/win-library/3.6")
library("class", lib.loc="~/R/win-library/3.6")
library("e1071", lib.loc="~/R/win-library/3.6")
library("data.table", lib.loc="~/R/win-library/3.6")
library("gridExtra", lib.loc="~/R/win-library/3.6")
library('MLmetrics', lib.loc="~/R/win-library/3.6")
library('miscTools', lib.loc="~/R/win-library/3.6")


#==========Upload da Base de Dados==========


#Conectando ao MySQL e Obtendo os Dados:

my_db = dbConnect(MySQL(), user='root', password='', dbname='dba_rocfall', 
                  host='127.0.0.1', port = 3306)
      ### Informacoes de usuario e outros requisitos do MySQL

s = paste0("select * from dados_rocfall")
      ### Comando para selecionar tudo da base de dados

rs = dbSendQuery(my_db, s)
      ### Enviando Query ao MySQL

df_rcf =  fetch(rs, n = -1)

on.exit(dbDisconnect(my_db)) 
      ### Saindo do MySQL


#==========Pre-Processamento==========


#Analisando o DB:

View(df_rcf)

#Criando uma copia do DB:

copy_df_rcf = df_rcf

# Gerando um novo intervalo de classes:

df_classify2_porc_ac = data.frame("porc_ac_start" = c(0.000, 0.500), 
                                  "porc_ac_end" = c(0.500, 1.000),
                                  "porc_ac_type" = c('Alto', 'Baixo'))
      ### Risco Alto: <0.5
      ### Risco Baixo: >= 0.5

df_cl2_porc_range = data.frame(copy_df_rcf, 
                               "porc_ac_range" = cut(copy_df_rcf$porc_ac, 
                                  breaks = df_classify2_porc_ac$porc_ac_start,
                                  right = F, include.lowest=T))

df_cl2_porc_ac_type = transform(df_cl2_porc_range, 
                                type = df_classify2_porc_ac$porc_ac_type
                                [findInterval(df_cl2_porc_range$porc_ac, 
                                df_classify2_porc_ac$porc_ac_start)])


# Criando uma copia para trabalhar exclusivamente com Machine Learning:

copy_df_rcf_ML = df_cl2_porc_ac_type


# Gerando um grafico de frequencia absoluta para verificar o 
# balanceamento das classes:

graf_porc_ac = ggplot(copy_df_rcf_ML, aes(x = type)) +
  geom_bar(width = 0.5, aes(fill = factor(type))) +
  scale_fill_manual(breaks = c("Alto", "Baixo"), 
                    values=c("blue", "lightblue")) +
  xlab('Classe') +
  ylab('Frequência') +
  labs(title = "Grafico de Frequência\n\nClassificação das Áreas Criticas por Quantidade Absoluta", 
       fill = 'Classes') +
  theme_classic()
graf_porc_ac

      ### APROVADO!


--------------------------------------------------


# Verificando valores nulos de cada campo:
  
copy_df_rcf_ML %>% 
  select(h_enc) %>% 
  filter(is.na(h_enc))

copy_df_rcf_ML %>% 
  select(ang_enc) %>% 
  filter(is.na(ang_enc))

copy_df_rcf_ML %>% 
  select(rock_mass) %>% 
  filter(is.na(rock_mass))

copy_df_rcf_ML %>% 
  select(vel_rad) %>% 
  filter(is.na(vel_rad))

copy_df_rcf_ML %>% 
  select(mean_range) %>% 
  filter(is.na(mean_range))

copy_df_rcf_ML %>% 
  select(porc_ac) %>% 
  filter(is.na(porc_ac))

copy_df_rcf_ML %>% 
  select(porc_disp) %>% 
  filter(is.na(porc_disp))

copy_df_rcf_ML %>% 
  select(porc_rock_mapped) %>% 
  filter(is.na(porc_rock_mapped))

copy_df_rcf_ML %>% 
  select(type) %>% 
  filter(is.na(type))
      ### Nao ha valores faltantes nessa base de dados!


# Visualizando valores inconsistentes em cada campo:

copy_df_rcf_ML %>% 
  select(h_enc) %>% 
  filter(h_enc < 0 | h_enc > 100)

copy_df_rcf_ML %>% 
  select(ang_enc) %>% 
  filter(ang_enc < 50 | ang_enc > 90)

copy_df_rcf_ML %>% 
  select(rock_mass) %>% 
  filter(rock_mass <  85000 | rock_mass > 3664000)

copy_df_rcf_ML %>% 
  select(vel_rad) %>% 
  filter(vel_rad < 0 | vel_rad > 5)

copy_df_rcf_ML %>% 
  select(porc_ac) %>% 
  filter(porc_ac < 0 | porc_ac > 1)

copy_df_rcf_ML %>% 
  select(porc_disp) %>% 
  filter(porc_disp < 0 | porc_disp > 1)

copy_df_rcf_ML %>% 
  select(porc_rock_mapped) %>% 
  filter(porc_rock_mapped < 0 | porc_rock_mapped > 1)

copy_df_rcf_ML %>% 
  select(type) %>% 
  filter(type == 'Alto' & type == 'Baixo')
      ### Não há valores inconsistentes nessa base de dados!


#Apagando campos desnecessarios da classificacao para ML:

copy_df_rcf_ML$id = NULL
copy_df_rcf_ML$h_rmp = NULL
copy_df_rcf_ML$ang_rmp = NULL
copy_df_rcf_ML$porc_ac_range = NULL


#==========Divisao TREINO-TESTE==========


# Criando uma "semente geradora" e promovendo a divisao da Base de Dados em 
# Treinamento e Teste:

set.seed(1)

split_df_rcf = sample.split(copy_df_rcf_ML$type, SplitRatio = 0.75) ##Divide a
  ##base de dados aleatoriamente no percentual desejado, 
  ## retornando a divisao em TRUE (75%) e FALSE (25%)

df_rcf_train = subset(copy_df_rcf_ML, split_df_rcf == TRUE) ##Define que os 
  ##valores TRUE pertencem ao DB treinamento

df_rcf_test = subset(copy_df_rcf_ML, split_df_rcf == FALSE) ##Define que os 
  ## valores FALSE pertencem ao DB teste


#Verificando se Treino-Teste esta balanceado corretamente:

check_train = setDT(df_rcf_train)[,.N/nrow(df_rcf_train),type]
check_train

          ##    type        V1
          # 1:  Baixo    0.5324015
          # 2:  Alto     0.4675985

check_test = setDT(df_rcf_test)[,.N/nrow(df_rcf_test),type]
check_test

          ##    type       V1
          # 1:  Alto    0.4676806
          # 2:  Baixo   0.5323194

      ### APROVADO!


#==========Random Forest ~APROVADO~==========


# Rodando um modelo de ML utilizando Random Forest do pacote
# 'random forest' e verificando a saida do modelo:

classify_randFor_rcf = randomForest(x = df_rcf_train[, 1:5], 
                                    y = df_rcf_train$type, 
                                    ntree = 50, mtry = 2, 
                                    classwt = c(check_train$V1[1], 
                                                check_train$V1[2]),
                                    nodesize = 2, importance = TRUE, 
                                    maxnodes = 70)

classify_randFor_rcf 
      ### OOB Estimate Error Rate: 3,30%


# Testando a Arvore com os registros do df_rcf_test e verificando
# a saida:

pred_randFor_rcf = predict(object = classify_randFor_rcf, 
                           newdata = df_rcf_test[, 1:5])
pred_randFor_rcf


#Criando uma matriz confusao para verificar o percentual de acerto:

mc_randFor_rcf = table(as.data.frame(df_rcf_test)[, 9], 
                       as.data.frame(pred_randFor_rcf)[, 1])
mc_randFor_rcf


confusionMatrix(mc_randFor_rcf) 
      ### Total Accuracy: 98,10%
      ### 95% CI : (0.9728, 0.9991)
      ### Value 'Alto' Accuracy: 96,75%
      ### Value 'Baixo' Accuracy: 99,29%


# Testando o modelo com um novo registro, criando um df com
# as features utilizadas para predicao:

nw_register = data.frame(h_enc = c(50),
                         ang_enc = c(80),
                         rock_mass = c(100000),
                         vel_rad = c(1),
                         mean_range = c(15.3813))


# Classificando o novo registro:
pred_randFor_nwR = predict(object = classify_randFor_rcf, newdata = nw_register, type = 'prob')
pred_randFor_nwR 
      ### Prob 'Alto' = 8.00% | Prob 'Baixo' = 92,00%


      ### VANTAGENS:

              #### - Algoritmo robusto (considerando Homo-Heterogeneidade) e 
              #### geralmente c/ boa precisao!
                ##### (Ideal para muitos problemas da Area academica ~POUCOS DADOS~)

              #### - Poucas etapas de preparo em relação aos demais métodos

              #### - Rápido
      

      ### DESVANTAGENS:

              #### - Limitado a dados da mesma natureza que o DB de treino.

              #### - Dificil ajuste dos parametros devido a complexidade e 
              #### não visualização gráfica da floresta.


#==========Visualizacao Grafica de uma Arvore==========


# Rodando um modelo de ML utilizando Árvore de Decisão do
# pacote 'rpart':

classify_treeDec_rcf = rpart(formula = df_rcf_train$type ~ df_rcf_train$h_enc + 
                               df_rcf_train$ang_enc + df_rcf_train$rock_mass + 
                               df_rcf_train$vel_rad + df_rcf_train$mean_range, 
                             data = df_rcf_train)


# Visualizando a Arvore Numericamente e Graficamente

print(classify_treeDec_rcf)
rpart.plot(classify_treeDec_rcf, box.palette="blue")


# Agora analisando graficamente a Feature Importance de 
# cada parametro:

varImpPlot(x = classify_randFor_rcf, main = "Feature Importance - Rocfall")


#==========Cross Validation e Tuning Hyperparameter: RF==========


# Podemos fazer um tuning dos parametros e utilizando o Cross Validation 
# do pacote 'caret':

  ## (1) 'mtry' e 'node_size'

  control_rcftrain = trainControl(method = 'repeatedcv', number = 10, 
                                  repeats = 10, search = 'grid')
      ### Criando o controle da funcao 'train', especificando que o metodo
      ### sera o CV, com 10 particoes e 10 repeticoes

  tG = expand.grid(.mtry=c(1:5), .min.node.size = c(2:6), .splitrule = "gini")
      ### Intervalo dos parametros a serem tunados

  model_ctrl_rcf = train(type ~ h_enc + ang_enc + rock_mass + vel_rad + 
                           mean_range, 
                         data = copy_df_rcf_ML, 
                         trControl = control_rcftrain, 
                         method = 'ranger', 
                         tuneGrid = tG)
      ### Construcao do CV e do Tuning Hyperparameters

  model_ctrl_rcf

        ### Random Forest 
        # 
        # 1050 samples
        # 5 predictor
        # 2 classes: 'Alto', 'Baixo' 
        # 
        # No pre-processing
        # Resampling: Cross-Validated (10 fold, repeated 3 times) 
        # Summary of sample sizes: 946, 945, 945, 945, 945, 945, ... 
        # Resampling results across tuning parameters:
        #   
        # mtry  min.node.size  Accuracy   Kappa    
        # 1     2              0.9590318  0.9177029
        # 1     3              0.9564950  0.9126856
        # 1     4              0.9561685  0.9119921
        # 1     5              0.9558570  0.9113873
        # 1     6              0.9542667  0.9081985
        # 2     2              0.9723776  0.9444588
        # 2     3              0.9717395  0.9431911
        # 2     4              0.9707781  0.9412554
        # 2     5              0.9714160  0.9425471
        # 2     6              0.9710986  0.9418946
        # 3     2              0.9717365  0.9431639
        # 3     3              0.9698318  0.9393520
        # 3     4              0.9711046  0.9419237
        # 3     5              0.9704697  0.9406263
        # 3     6              0.9704636  0.9406179
        # 4     2              0.9691999  0.9380819
        # 4     3              0.9685649  0.9368105
        # 4     4              0.9669776  0.9336110
        # 4     5              0.9669746  0.9336102
        # 4     6              0.9653842  0.9303910
        # 5     2              0.9682504  0.9361562
        # 5     3              0.9669806  0.9336153
        # 5     4              0.9660252  0.9316940
        # 5     5              0.9660191  0.9316616
        # 5     6              0.9653901  0.9304117

        # Tuning parameter 'splitrule' was held constant at a value of gini
        # Accuracy was used to select the optimal model using the largest value.
        # The final values used for the model were mtry = 2, splitrule = gini 
        # and min.node.size = 2.


--------------------------------------------------


  ## (3)'ntrees': Aqui, sera criado um loop em 'for' especificando o intervalo
  ## dos parametros a serem tunados de maneira manual:

  tG_2 = expand.grid(.mtry = 2)

  ntree_list = list()

  for (ntree in c(10, 20, 50, 100, 150, 200)) {
    set.seed(1234)
    fit = train (type ~ h_enc + ang_enc + rock_mass + vel_rad + mean_range, 
                 data = copy_df_rcf_ML, method = "rf", metric = "Accuracy", tuneGrid = tG_2, 
                 trControl = control_rcftrain, ntree = ntree, importance = TRUE, nodesize = 2)
    key = toString(ntree)
    ntree_list[[key]] = fit
  }

  results_ntree = resamples(ntree_list)

  summary(results_ntree)


      ### Call:
      #   summary.resamples(object = results_ntree)
      # 
      # Models: 10, 20, 50, 100, 150, 200 
      # Number of resamples: 30 
      # 
      # Accuracy 
      #         Min.   1st Qu.    Median      Mean   3rd Qu. Max. NA's
      # 10  0.9333333 0.9619048 0.9714286 0.9692029 0.9809524    1    0
      # 20  0.9333333 0.9619048 0.9807692 0.9742823 0.9809524    1    0
      # 50  0.9428571 0.9712225 0.9760989 0.9755642 0.9880266    1    0
      # 100 0.9428571 0.9619048 0.9714286 0.9723804 0.9809524    1    0
      # 150 0.9428571 0.9619048 0.9712912 0.9711165 0.9809524    1    0
      # 200 0.9519231 0.9619048 0.9714286 0.9720689 0.9809524    1    0
      # 
      # Kappa 
      #          Min.   1st Qu.    Median      Mean   3rd Qu. Max. NA's
      # 10  0.8655570 0.9232737 0.9425287 0.9381236 0.9618077    1    0
      # 20  0.8655570 0.9234694 0.9613239 0.9483153 0.9618321    1    0
      # 50  0.8857868 0.9422299 0.9519995 0.9509248 0.9759706    1    0
      # 100 0.8857868 0.9234694 0.9426019 0.9445089 0.9618077    1    0
      # 150 0.8846154 0.9234694 0.9421513 0.9419436 0.9617347    1    0
      # 200 0.9036323 0.9234694 0.9424551 0.9438714 0.9617833    1    0

      # You can see that the most accuracy value for ntree was perhaps 50 
      # with mean accuracy of 97,56%


--------------------------------------------------


  ## (4) 'maxnodes': Outro loop em 'for' especificando o intervalo
  ## dos parametros a serem tunados de maneira manual:

  maxnode_list = list()

  for (maxnodes in c(60:75)) {
    set.seed(12345)
    maxnode_train <- train(type ~ h_enc + ang_enc + rock_mass + vel_rad + mean_range, 
                        data = copy_df_rcf_ML, method = "rf", metric = "Accuracy",
                        tuneGrid = tG_2, importance = TRUE, trControl = control_rcftrain, nodesize = 2, 
                        maxnodes = maxnodes, ntree = 100)
    keep = toString(maxnodes)
    maxnode_list[[keep]] = maxnode_train
  }

  results_MND <- resamples(maxnode_list)

  summary(results_MND)

      ### Call:
      #   summary.resamples(object = results_MND)
      # 
      # Models: 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75 
      # Number of resamples: 30 
      # 
      # Accuracy 
      # Min.   1st Qu.    Median      Mean   3rd Qu.     Max. NA's
      # 60 0.9333333 0.9619048 0.9809524 0.9733208 0.9881402 1.000000    0
      # 61 0.9333333 0.9619048 0.9761905 0.9723805 0.9809524 1.000000    0
      # 62 0.9428571 0.9547619 0.9762337 0.9723715 0.9881402 1.000000    0
      # 63 0.9333333 0.9619048 0.9714286 0.9704636 0.9809524 1.000000    0
      # 64 0.9333333 0.9642170 0.9762337 0.9733269 0.9880952 1.000000    0
      # 65 0.9333333 0.9547619 0.9714286 0.9711046 0.9809524 1.000000    0
      # 66 0.9238095 0.9619048 0.9807692 0.9720540 0.9810872 1.000000    0
      # 67 0.9428571 0.9616300 0.9809524 0.9730064 0.9809524 1.000000    0
      # 68 0.9333333 0.9616300 0.9809524 0.9739557 0.9904762 1.000000    0
      # 69 0.9333333 0.9619048 0.9715633 0.9711076 0.9809524 0.990566    0
      # 70 0.9333333 0.9642170 0.9762337 0.9742823 0.9880952 1.000000    0
      # 71 0.9333333 0.9619048 0.9808608 0.9739617 0.9809524 1.000000    0
      # 72 0.9333333 0.9619946 0.9715633 0.9720661 0.9809524 1.000000    0
      # 73 0.9333333 0.9619048 0.9808608 0.9733268 0.9880952 1.000000    0
      # 74 0.9333333 0.9619048 0.9808608 0.9720630 0.9809524 1.000000    0
      # 75 0.9333333 0.9619048 0.9714286 0.9714190 0.9809524 1.000000    0
      # 
      # Kappa 
      # Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
      # 60 0.8655570 0.9235181 0.9616368 0.9464112 0.9761476 1.0000000    0
      # 61 0.8655570 0.9235181 0.9522288 0.9445223 0.9618077 1.0000000    0
      # 62 0.8846154 0.9093934 0.9522817 0.9444993 0.9761476 1.0000000    0
      # 63 0.8655570 0.9231750 0.9426752 0.9406454 0.9618077 1.0000000    0
      # 64 0.8655570 0.9284060 0.9522386 0.9464151 0.9760902 1.0000000    0
      # 65 0.8662420 0.9095746 0.9424551 0.9419540 0.9618077 1.0000000    0
      # 66 0.8469388 0.9234694 0.9613239 0.9438557 0.9620042 1.0000000    0
      # 67 0.8846154 0.9231110 0.9616368 0.9457576 0.9618077 1.0000000    0
      # 68 0.8655570 0.9231110 0.9616858 0.9476998 0.9808429 1.0000000    0
      # 69 0.8655570 0.9233226 0.9429143 0.9419788 0.9617347 0.9810511    0
      # 70 0.8655570 0.9283737 0.9522817 0.9483384 0.9760902 1.0000000    0
      # 71 0.8655570 0.9234694 0.9614804 0.9476755 0.9618321 1.0000000    0
      # 72 0.8655570 0.9239242 0.9429260 0.9438762 0.9617102 1.0000000    0
      # 73 0.8655570 0.9234694 0.9614804 0.9464243 0.9760902 1.0000000    0
      # 74 0.8655570 0.9234694 0.9614804 0.9438649 0.9618321 1.0000000    0
      # 75 0.8655570 0.9234694 0.9426019 0.9425743 0.9618077 1.0000000    0
      # You can see that the most accuracy value for maxnodes was 70 with mean accuracy of 97,42%


#==========K-Nearest Neighbour (KNN) ~NAO APROVADO==========


# Gerando um encoding do data frame:

transf_encod_df_rcf = copy_df_rcf_ML %>% 
  mutate_if(is.factor, as.numeric)
      ### Encoding: Alto = 1
      ###           Baixo = 2


# Realizando um Escalonamento por Padronizacao:

escalpad_transf_encod_df_rcf = transf_encod_df_rcf
escalpad_transf_encod_df_rcf[, 1:8] = scale(transf_encod_df_rcf[, 1:8])


# Fazendo uma nova divisao da base de dados em Treino e Teste:

set.seed(2)

split_df_rcf2 = sample.split(escalpad_transf_encod_df_rcf$type, 
                             SplitRatio = 0.75) 
      ### Divide a base de dados aleatoriamente no percentual desejado, 
      ### retornando a divisao em TRUE (75%) 
      ### e FALSE (25%)

df_rcf_train2 = subset(escalpad_transf_encod_df_rcf, split_df_rcf2 == TRUE) 
      ### Define que os valores TRUE pertencem ao DB treinamento

df_rcf_test2 = subset(escalpad_transf_encod_df_rcf, split_df_rcf2 == FALSE) 
      ### Define que os valores FALSE pertencem ao DB teste


# Rodando um modelo de ML utilizando KNN do pacote 'knn':

pred_KNN_rcf = knn(train = df_rcf_train2[, 1:5], test = df_rcf_test2[, 1:5], 
                   cl = df_rcf_train2[, 9], 
                   k = sqrt(nrow(df_rcf_train2)))


# Criando uma matriz confusao para verificar o percentual de acerto

mc_KNN_rcf = table(df_rcf_test2[, 9], pred_KNN_rcf)
mc_KNN_rcf


confusionMatrix(mc_KNN_rcf) 
      ### [Class 1] Total Accuracy: 87,83%
      ### 95% CI : (0.8326, 0.9153)
      ### Value 'Alto (1)' Accuracy: 92,86%
      ### Value 'Baixo (2)' Accuracy: 83,57%


      ### RANDOM FOREST > KNN!


#==========Maquina de Vetores de Suporte (SVM) ~APROVADO~==========


# Rodando um modelo de ML utilizando a funcao 'gsv' padrao do R e 
# verificando a saida do modelo:

classify_svm_rcf = svm(formula = type ~ h_enc + ang_enc + rock_mass + 
                       vel_rad + mean_range, 
                       data = df_rcf_train, type = 'C-classification', 
                       kernel = 'radial', cost = 10)

classify_svm_rcf


# Testando o modelo:

pred_svm_rcf = predict(object = classify_svm_rcf, newdata = df_rcf_test[, -9])
pred_svm_rcf


# Criando uma matriz confusao para verificar o percentual de acerto:

matriz_confusao_svm = table(as.data.frame(df_rcf_test)[, 9], 
                            as.data.frame(pred_svm_rcf)[, 1])
matriz_confusao_svm


confusionMatrix(matriz_confusao_svm) 
      ### Total Accuracy: 97,72%
      ### 95% CI : (0.951, 0.9916)
      ### Value 0 (Alto) Acuracy: 96,75%
      ### Value 1 (Baixo) Acuracy: 98,57%


      ### KNN < SVM < RANDOM FOREST! 


#==========Regression Mean_Range==========


# Rodando um modelo de regressao utilizando Random Forest
# do pacote 'random forest' e analisando a saida e o summary:

classify_reg.RF_rcf = randomForest(x = df_rcf_train[, 1:4], 
                                   y = df_rcf_train$mean_range, 
                                   ntree = 130, mtry = 4, nodesize = 1, 
                                   importance = TRUE)

classify_reg.RF_rcf #MSR = 1.474927
summary(classify_reg.RF_rcf)


# Testando o Random Forest com os registros do próprio df de TREINO:

pred_reg.RF_rcf = predict(object = classify_reg.RF_rcf, newdata = df_rcf_train[, 1:4])
pred_reg.RF_rcf


# Calculando o Mean Absolute Error (MAE):

MAE(y_pred = pred_reg.RF_rcf, y_true = df_rcf_train$mean_range) 
      ### MAE = 0.2513983m

# Verificando o RSquared da base de TREINO:

rSquared(y = df_rcf_train$mean_range, 
         resid = df_rcf_train$mean_range - pred_reg.RF_rcf)
      ### RSquared TESTE = 0.9997013


--------------------------------------------------

  
# Avaliando agora as previsões para a base de TESTE:
  
pred_reg.RF_rcf_test = predict(object = classify_reg.RF_rcf, 
                               newdata = df_rcf_test[, 1:4])
pred_reg.RF_rcf_test


# Calculando o Mean Absolute Error (MAE):

MAE(y_pred = pred_reg.RF_rcf_test, y_true = df_rcf_test$mean_range) 
      ### MAE = 0.7296671


# Verificando o RSquared da base de TESTE:

rSquared(y = df_rcf_test$mean_range, 
         resid = df_rcf_test$mean_range - pred_reg.RF_rcf_test)
      ### RSquared TESTE = 0.9979731


#==========Cross Validation e Tuning Hyperparameters: Reg.RF==========


# Validacao Cruzada e tunagem dos hiperparametros para o modelo de regressao:

  ## 'mtry':

  control_REG.rcftrain = trainControl(method = 'repeatedcv', number = 10, 
                                    repeats = 10, search = 'grid')

  tG_REG = expand.grid(.mtry = c(1:4))

  model_ctrl_REG.rcf = train(mean_range ~ h_enc + ang_enc + rock_mass + vel_rad, 
                       data = copy_df_rcf_ML, trControl = control_REG.rcftrain, 
                       method = 'rf', tuneGrid = tG_REG, ntree = 130, 
                       nodesize = 1, importance = TRUE)

  model_ctrl_REG.rcf

      ### Random Forest 
      # 1050 samples
      # 4 predictor
      # 
      # No pre-processing
      # Resampling: Cross-Validated (10 fold, repeated 3 times) 
      # Summary of sample sizes: 945, 945, 946, 946, 946, 945, ... 
      # Resampling results across tuning parameters:
      #   
      #   mtry  RMSE      Rsquared   MAE      
      # 1     9.849374  0.9476311  6.4306455
      # 2     1.980635  0.9958494  1.2720772
      # 3     1.066821  0.9983287  0.6277834
      # 4     1.005787  0.9984914  0.5817017
      # 
      # RMSE was used to select the optimal model using the smallest value.
      # The final value used for the model was mtry = 4.
      # You can see that the most accuracy value for MAE was 0.58 with 
      # mtry = 4
