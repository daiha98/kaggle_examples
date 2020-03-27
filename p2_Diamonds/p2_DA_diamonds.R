#==========Bibliotecas (Instalacao e Importacao)==========


# Instalacao dos Pacotes:

install.packages('dplyr') ## Manipulacao de Dados
install.packages('ggplot2') ## Visualização Gráfica dos Dados
install.packages('car') ## Permite criar gráficos QQplot
install.packages('gridExtra') # Cria gráficos em grid
install.packages('e1071') ## Contem algumas métricas estatísticas

# Carregando as bibliotecas:

library("dplyr", lib.loc="~/R/win-library/3.6")
library("ggplot2", lib.loc="~/R/win-library/3.6")
library("car", lib.loc="~/R/win-library/3.6")
library("gridExtra", lib.loc="~/R/win-library/3.6")
library("e1071", lib.loc="~/R/win-library/3.6")


#==========Pre Processamento==========


# Upload da Base de Dados:

diamonds = read.csv("C:/Users/felip/Desktop/Cursos/Kaggle/diamonds.csv")


# Informações sobre 'diamonds':

"Análise Primária e Resumida dos Dados, onde:

- CARAT: Weight of the diamond (0.2--5.01)
~ A metric “carat” (PT: QUILATE) is defined as 200 milligrams. Each carat is subdivided into 100 ‘points.
(Has the biggest impact on PRICE)~

- CUT: Quality of the Cut (Fair, Good, Very Good, Premium, Ideal)

- COLOR: Diamond colour, from J (worst) to D (best)

- CLARITY: A measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))

- DEPTH: Depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)

- TABLE: Width of top of diamond relative to widest point (43--95)

- PRICE: price in US dollars (\$326 -- \$18,823)
~ 4C's - 

- X: length in mm (0--10.74)

- Y: width in mm (0--58.9)

- Z: depth in mm (0--31.8)"


#Resumo estatístico:

summary(diamonds)

#            X        carat          cut                  color           clarity          depth           table      
# Min.   :    1   Min.   :0.2000   Fair     : 1610      D: 6775       SI1    :13065   Min.   :43.00   Min.   :43.00  
# 1st Qu.:13486   1st Qu.:0.4000   Good     : 4906      E: 9797       VS2    :12258   1st Qu.:61.00   1st Qu.:56.00  
# Median :26971   Median :0.7000   Very Good:12082      F: 9542       SI2    : 9194   Median :61.80   Median :57.00  
# Mean   :26971   Mean   :0.7979   Premium  :13791      G:11292       VS1    : 8171   Mean   :61.75   Mean   :57.46  
# 3rd Qu.:40455   3rd Qu.:1.0400   Ideal    :21551      H: 8304       VVS2   : 5066   3rd Qu.:62.50   3rd Qu.:59.00  
# Max.   :53940   Max.   :5.0100                        I: 5422       VVS1   : 3655   Max.   :79.00   Max.   :95.00
#                                                       J: 2808       (Other): 2531           
#
#     price             x                y                z         
# Min.   :  326   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
# 1st Qu.:  950   1st Qu.: 4.710   1st Qu.: 4.720   1st Qu.: 2.910  
# Median : 2401   Median : 5.700   Median : 5.710   Median : 3.530  
# Mean   : 3933   Mean   : 5.731   Mean   : 5.735   Mean   : 3.539  
# 3rd Qu.: 5324   3rd Qu.: 6.540   3rd Qu.: 6.540   3rd Qu.: 4.040  
# Max.   :18823   Max.   :10.740   Max.   :58.900   Max.   :31.800


      ### FEATURES 'x', 'y' e 'z' COM VALORES ZERADOS (INCONSISTENTE!)
      ### e
      ### SEM FEATURES COM VALORES NEGATIVOS!


# Verificando MISSING VALUES:

diamonds %>% 
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))    ## NÃO HÁ!
  
      ###  X   carat cut  color clarity depth table price x y z
      # 1  0     0    0     0       0     0     0    0    0 0 0


      ### Inicialmente, trataremos os valores zerados da base de dados.


#==========Análise FEATURES 'x', 'y', 'z'==========


# Criando uma cópia do db:

copy_diamonds = diamonds


# Criando a nova variavel 'Volume' (mm), onde volume = x*y*z:

diamonds$volume = diamonds$x*diamonds$y*diamonds$z
copy_diamonds$volume = copy_diamonds$x*copy_diamonds$y*copy_diamonds$z


# Relacionando volume x preco:

dps_vlxpc = ggplot(copy_diamonds, aes(x = volume, y = price)) + 
  geom_point() +
  xlab('Volume (mm)') +
  ylab('Price (U$)') +
  labs(title = "Relação - Volume x Price") +
  theme_minimal()
dps_vlxpc


        ## IDENTIFICOU-SE, APARENTEMENTE, VALORES INCONSISTENTES:


#==========Tratando Outliers de 'VOLUME' Não Nulos==========


# Fazendo a IQR Rule:

  ## Verificando os Quantiles:

  quantile(copy_diamonds$volume)    
        ###  0%         25%        50%        75%       100% 
        ### 0.00000   65.13683  114.80857  170.84245 3840.59806 

  ## Verificando o IQR:

  IQR(copy_diamonds$volume)   
        ### 105.7056
  
# Para *1.5 - NÃO SERÁ TRATADO!

left_otl = quantile(copy_diamonds$volume)[2] - 1.5*IQR(copy_diamonds$volume) ## -93.4216 
right_otl = quantile(copy_diamonds$volume)[4] + 1.5*IQR(copy_diamonds$volume) ## 329.4009

  ## Filtrando em um df apenas valores outiliers pela regra: +-1.5 * IQR

  otl_1.5df = diamonds %>% 
    select(everything()) %>% 
    filter(volume > right_otl) %>% 
    arrange(desc(volume))
  otl_1.5df
    
      
# Para *3.0 - SERÁ TRATADO!
        
extreme_left_otl = quantile(
  copy_diamonds$volume)[2] - 3*IQR(copy_diamonds$volume) ## -251.98 
extreme_right_otl = quantile(
  copy_diamonds$volume)[4] + 3*IQR(copy_diamonds$volume) ## 487.9593
  
  ## Filtrando em um df apenas valores outiliers pela regra: +-3.0 * IQR

  otl_3.0df = diamonds %>% 
    select(everything()) %>% 
    filter(volume > extreme_right_otl) %>% 
    arrange(desc(volume))
  otl_3.0df
        
        
    ## Padrões identificados: (!) Carat baixos = Erros de inserção no db... 
    ##                        (@) Carat ELEVADOS ~OUTLIERS~! 
        
  
--------------------------------------------------


# Analisando registros similares de (!):
  
copy_diamonds %>% 
  select(everything()) %>% 
  filter(volume > 800) %>% 
  arrange(desc(volume))
        
      ###  X   carat       cut       color clarity depth table price    x     y     z    volume
    # 1 24068  2.00     Premium       H     SI2   58.9  57.0 12210   8.09 58.90  8.06 3840.5981
    # 2 48411  0.51     Very Good     E     VS1   61.8  54.7  1970   5.12  5.15 31.80  838.5024
    # 3 49190  0.51     Ideal         E     VS1   61.8  55.0  2075   5.15 31.80  5.12  838.5024
  

  ## Armazenando registros similares a (X == 24068) em um df e buscando
  ## substituir esses valores inconsistentes pela media dessa feature nesse df:

  ppdm_1 = diamonds %>% 
    select(everything()) %>% 
     filter(x == 8.09) %>% 
     arrange(desc(y))
      
   ppdm_1
   ppdm_1 = ppdm_1[- c(1), ]

              ### Fazendo update de (X == 24068)
              copy_diamonds$y[copy_diamonds$X == 24068] = mean(ppdm_1$y)
              copy_diamonds$z[copy_diamonds$X == 24068] = mean(ppdm_1$z)
      
      
   ## Armazenando registros similares a (X == 48411) em um df e buscando
   ## substituir esses valores inconsistentes pela media dessa feature nesse df:
              
   ppdm_2 = diamonds %>% 
     select(everything()) %>% 
     filter(x == 5.12 & y == 5.15) %>% 
     arrange(desc(z))
      
   ppdm_2
   ppdm_2 = ppdm_2[- c(1), ]
        
              ### Fazendo update de (X == 48411)
              copy_diamonds$z[copy_diamonds$X == 48411] = mean(ppdm_2$z)
        
   ## Armazenando registros similares a (X == 49190) em um df e buscando
   ## substituir esses valores inconsistentes pela media dessa feature nesse df:
              
   ppdm_3 = diamonds %>% 
     select(everything()) %>% 
     filter(x == 5.15) %>% 
     arrange(desc(y))
      
   ppdm_3
   ppdm_3 = ppdm_3[- c(1), ]
        
              ### Fazendo update de (X == 49190)
              copy_diamonds$y[copy_diamonds$X == 49190] = mean(ppdm_3$y)
              copy_diamonds$z[copy_diamonds$X == 49190] = mean(ppdm_3$z)
              
              
# Fazendo update agora da variável VOLUME:
              
copy_diamonds$volume = copy_diamonds$x*copy_diamonds$y*copy_diamonds$z

  
# Verificando se o ajuste foi feito:

copy_diamonds %>% 
  select(everything()) %>% 
  filter(volume > 800) %>% 
  arrange(desc(volume))
      ###  X       carat   cut     color   clarity depth   table   price   x       y       z       volume 
      ### <0 linhas> (ou row.names de comprimento 0)


--------------------------------------------------

  
# AGORA, tratando os valores associados a (@):
  
  
    ## Eliminando registros já tratados da base de dados 'otl_3.0df':
  
    otl_3.0df = otl_3.0df[- c(1:3), ]
    

    ## Fazendo um 'Capping' desses dados: 
'Replacing those observations outside the extreme upper limit 
 with the value of carat = 3.00'

    capp_carat = copy_diamonds %>% 
      select(everything()) %>% 
      filter(carat == 3.00) %>% 
      arrange(desc(volume))
  
    ## Trocando o carat de 'otl_3.0df' para 3.00:

    otl_3.0df$carat[1:18] = 3.00
    
    
    ## Criando valores randômicos para 'x', 'y' e 'z' utilizando os 
    ## limites de 'capp_carat':
    
    replace_df = data.frame(x = runif(18, min = min(capp_carat$x), 
                                      max = max(capp_carat$x)),
                            y = runif(18, min = min(capp_carat$y), 
                                      max = max(capp_carat$y)),
                            z = runif(18, min = min(capp_carat$z), 
                                      max = max(capp_carat$z)))
    
    
          ### Substituindo no db 'otl_3.0df':
    
          otl_3.0df[, 9:11] = replace_df[1:3]
          
          
          ### Arredondando para 2 casas decimais:
          
          otl_3.0df[, 9:11] = format(otl_3.0df[, 9:11], digits = 3)
          
          
          ### Transformando as variáveis 'char' para 'numeric':
          
          otl_3.0df = transform(otl_3.0df, x = as.numeric(x),
                                           y = as.numeric(y), 
                                           z = as.numeric(z))
    
          
# Fazendo update agora da variável VOLUME:
          
otl_3.0df$volume = otl_3.0df$x * otl_3.0df$y * otl_3.0df$z
    
    
# Organizar os registros para inserção das CORREÇÕES:

copy_diamonds = copy_diamonds %>% 
  select(everything()) %>% 
  arrange(desc(volume))
    

# Inserindo os novos dados:

copy_diamonds[1:18, ] = otl_3.0df[1:18, ]
    

# Reorganizando...

copy_diamonds = copy_diamonds %>% 
  select(everything()) %>%  
  arrange((X))
  copy_diamonds
    
  
# Verificando o novo dispersograma 'Volume x Price':
  
ggplot(copy_diamonds, aes(x = volume, y = price)) + 
  geom_point() +
  xlab('Volume (mm)') +
  ylab('Price (U$)') +
  labs(title = "Relação - Volume x Price") +
  theme_minimal()


#==========Outliers de 'VOLUME' Nulos==========  

    
# Identificando esses registros pelo Dispersograma, analisando outros 
# OUTLIERS e verificando também alguma relação entre as FEATURES:
  
  ## Relação x~y (1):

  dps_xy = ggplot(data = copy_diamonds, aes(x = x, y = y)) +
    geom_point()
  dps_xy
  
  
  ## Relação x~z (2):
  
  dps_xz = ggplot(data = copy_diamonds, aes(x = x, y = z)) +
    geom_point()
  dps_xz
  
  
  ## Relação z~y (3):
  
  dps_zy = ggplot(data = copy_diamonds, aes(x = z, y = y)) +
    geom_point()
  dps_zy
      
        ### Organizando para visualização em conjunto:
  
        gg_xyz = grid.arrange(dps_xy, dps_xz, dps_zy,
                              ncol = 3, nrow = 1)  
                #### APARENTEMENTE, HÁ UMA RELAÇÃO DE CRESCIMENTO LINEAR ENTRE AS VARIÁVEIS!

        
# Para (1):
        
mod_1 = lm(y ~ x, data = copy_diamonds)
summary(mod_1)
            
    ## Calculando os residuals...
    copy_diamonds$residuals = residuals(mod_1)
    outliers_inf1 = copy_diamonds[abs(copy_diamonds$residuals) > 0.3, ]
    outliers_inf1


--------------------------------------------------

    
# Para (2):
mod_2 = lm(z ~ x, data = copy_diamonds)
summary(mod_2)
            
    ## Calculando os residuals...
    copy_diamonds$residuals2 = residuals(mod_2)
    outliers_inf2 = copy_diamonds[abs(copy_diamonds$residuals2) > 0.3, ]
    outliers_inf2


--------------------------------------------------

            
# Para (3):
mod_3 = lm(y ~ z, data = copy_diamonds)
summary(mod_3)
            
    ## Calculando os residuals...
    copy_diamonds$residuals3 = residuals(mod_3)
    outliers_inf3 = copy_diamonds[abs(copy_diamonds$residuals3) > 0.3, ]
    outliers_inf3


--------------------------------------------------
          
      
# Eliminação dos Outliers:
copy_diamonds = anti_join(copy_diamonds, outliers_inf1)
copy_diamonds = anti_join(copy_diamonds, outliers_inf2)
copy_diamonds = anti_join(copy_diamonds, outliers_inf3)


# Mudando o nome das colunas:
colnames(copy_diamonds)[13] = "residuals_yx"
colnames(copy_diamonds)[14] = "residuals_zx"
colnames(copy_diamonds)[15] = "residuals_yz"


--------------------------------------------------
  
  
# Tratando registros com VOLUME NULOS:
  
    ## Quantificando esses registros em um df:
  
    dms_tt = copy_diamonds %>% 
      select(everything()) %>% 
      filter(volume == 0) %>% 
      arrange(desc(x))
    dms_tt

    #     X    carat  cut       color  clarity depth   table price  x y z   volume   residuals  residuals2  residuals3
    # 1 11964  1.00 Very Good     H     VS2     63.3    53    5139  0 0 0      0    -0.05488132 -0.03768361 -0.1414183
    # 2 15952  1.14      Fair     G     VS1     57.5    67    6381  0 0 0      0    -0.05488132 -0.03768361 -0.1414183
    # 3 24521  1.56     Ideal     G     VS2     62.2    54    12800 0 0 0      0    -0.05488132 -0.03768361 -0.1414183
    # 4 26244  1.20   Premium     D    VVS1     62.1    59    15686 0 0 0      0    -0.05488132 -0.03768361 -0.1414183
    # 5 27430  2.25   Premium     H     SI2     62.8    59    18034 0 0 0      0    -0.05488132 -0.03768361 -0.1414183
    # 6 49557  0.71      Good     F     SI2     64.1    60    2130  0 0 0      0    -0.05488132 -0.03768361 -0.1414183
    # 7 49558  0.71      Good     F     SI2     64.1    60    2130  0 0 0      0    -0.05488132 -0.03768361 -0.1414183
    
    
# Eliminando registros impossíveis de previsão:

copy_diamonds = copy_diamonds %>% 
  select(everything()) %>%  
  arrange((x))
copy_diamonds = copy_diamonds[- c(1:7), ]
    

# Reorganizando...

copy_diamonds = copy_diamonds %>% 
  select(everything()) %>%  
  arrange((X))
  copy_diamonds
          
  
# Verificando a nova distribuição dos dados:
          
    ## Relação x~y (1):
  
    dps_xy = ggplot(data = copy_diamonds, aes(x = x, y = y)) +
              geom_point()
    dps_xy
           
                 
    ## Relação x~z (2):
              
    dps_xz = ggplot(data = copy_diamonds, aes(x = x, y = z)) +
                geom_point()
    dps_xz
    
    
    ## Relação z~y (3):
    
    dps_zy = ggplot(data = copy_diamonds, aes(x = z, y = y)) +
                geom_point()
    dps_zy
          
    
    ## Organizando para visualização em conjunto:
    gg_xyz = grid.arrange(dps_xy, dps_xz, dps_zy,
                          ncol = 3, nrow = 1)


#==========Outliers e Scatterplots (CONTINUOS)==========


# Dispersogramas: N x Price

  ## Carat - QUANTO MAIOR O CARAT, MAIOR O PREÇO 
  ## (mas não é o único fator influente...)
    
  dps_carat = ggplot(data = copy_diamonds, aes(x = carat, y = price)) +
    geom_point() +
    xlab('Carat (Weight: 1.0 Carat = 200mg)') +
    ylab('Price (U$)') +
    labs(title = "Dispersograma - Carat") +
    theme_minimal()
  dps_carat
  
  
  ## Depth
  
  dps_depth = ggplot(data = copy_diamonds, aes(x = depth, price)) +
    geom_point() +
    xlab('Depth Percentage (z / mean(x, y) = 2 * z / (x + y))') +
    ylab('Price (U$)') +
    labs(title = "Dispersograma - Depth") +
    theme_minimal()
  dps_depth
  
  
  ## Table
  
  dps_table = ggplot(data = copy_diamonds, aes(x = table, y = price)) +
    geom_point() +
    xlab('Table: Width of Top of diamond relative to Widest point') +
    ylab('Price (U$)') +
    labs(title = "Dispersograma - Table") +
    theme_minimal()
  dps_table
  
  
  ## Verificando as Variáveis Contínuas em conjunto: 
  dps_par_cont = grid.arrange(dps_carat, dps_depth, dps_table,
                              ncol = 3, nrow = 1)
  dps_par_cont
  
  
# Analisando os registros com aspecto de Outliers:

  ## Carat - JÁ FOI FEITO!
  
  ------------------------------------------------------------
  
  ## Depth: QUANTO MAIOR O DEPTH, APARENTEMENTE PIOR É O CORTE
  
      ### Fazendo a IQR Rule:
    
      quantile(copy_diamonds$depth) #0%  25%  50%  75% 100% 
                                  # 43.0 61.1 61.9 62.5 68.4 
        
        
      IQR(copy_diamonds$depth)   ##  1.4
        
        
      ### Para *3.0
      
      depth_left_otl = quantile(copy_diamonds$depth)[2] - 3.0*IQR(copy_diamonds$depth) ## 56.9 
      depth_right_otl = quantile(copy_diamonds$depth)[4] + 3.0*IQR(copy_diamonds$depth) ## 66.7
      
      
      depth_otl_3.0df = copy_diamonds %>% 
        select(everything()) %>% 
        filter(depth > depth_right_otl | depth < depth_left_otl) %>% 
        arrange(desc(depth))
      depth_otl_3.0df
        
      
      ### Removendo esses dados do db:
      
      copy_diamonds = anti_join(copy_diamonds, depth_otl_3.0df)
   
      
  ---------------------------------------------------------------     
      
        
  ## Table: QUANTO MAIOR O TABLE, APARENTEMENTE PIOR O CORTE
        
      ### Fazendo a IQR Rule:
        
      quantile(copy_diamonds$table)    #0%   25%  50%  75%  100% 
                                      # 43   56   57   59    73
        
      IQR(copy_diamonds$table)   ## 3.0
        
      
      ### Para *3.0
      
      table_left_otl = quantile(copy_diamonds$table)[2] - 3.0*IQR(copy_diamonds$table) ## 47.0
      table_right_otl = quantile(copy_diamonds$table)[4] + 3.0*IQR(copy_diamonds$table) ## 68.0
        
      table_otl_3.0df = copy_diamonds %>% 
        select(everything()) %>% 
        filter(table > table_right_otl | table < table_left_otl) %>% 
        arrange(desc(table))
      table_otl_3.0df
        
      
        ### Removendo esses dados do db:
      
        copy_diamonds = anti_join(copy_diamonds, table_otl_3.0df)
        
        
--------------------------------------------------------------------------
        
          
# Verificando como ficou a nova distribuição:
        
  ## Carat
          
  dps_carat = ggplot(data = copy_diamonds, aes(x = carat, y = price)) +
    geom_point() +
    xlab('Carat (Weight: 1.0 Carat = 200mg)') +
    ylab('Price (U$)') +
    labs(title = "Dispersograma - Carat") +
    theme_minimal()
  dps_carat
        
  
  ## Depth
  
  dps_depth = ggplot(data = copy_diamonds, aes(x = depth, price)) +
    geom_point() +
    xlab('Depth Percentage (z / mean(x, y) = 2 * z / (x + y))') +
    ylab('Price (U$)') +
    labs(title = "Dispersograma - Depth") +
    theme_minimal()
  dps_depth
        
  
  ## Table
  
  dps_table = ggplot(data = copy_diamonds, aes(x = table, y = price)) +
    geom_point() +
    xlab('Table: Width of Top of diamond relative to Widest point') +
    ylab('Price (U$)') +
    labs(title = "Dispersograma - Table") +
    theme_minimal()
  dps_table
        
  
    ## Verificando as Variáveis Contínuas em conjunto: 
  
        dps_par_cont = grid.arrange(dps_carat, dps_depth, dps_table,
                                    ncol = 3, nrow = 1)


#==========Distribuicao das Variaveis (TODAS)==========

  
# Analisando GRAFICAMENTE (Histograma e Barplot) a distribuição das variáveis:
        
          ### OBS: bins = Sturges' formula, where: ceiling(log2(length(x)) + 1)

  ## Carat - SEGUE UMA SÉRIE DE DISTRIBUIÇÕES EXPONENCIAIS NEGATIVAS 
  ## DE 0.5 EM 0.5 CARAT
        
  bp_carat = ggplot(data = copy_diamonds, aes(x = carat)) +
    geom_histogram(aes(y = ..density..), 
                   bins = ceiling(log2(length(copy_diamonds$carat)) + 1)) +
    geom_density(size = 1, colour = 'darkblue') +
    xlab('Carat (Weight: 1.0 Carat = 200mg)') +
    ylab('Frequência Relativa') +
    labs(title = "Histograma - Carat") +
    theme_minimal()
  bp_carat

  
  ## Cut - "SEGUE UMA DISTRIBUIÇÃO EXPONENCIAL POSITIVA"
  
  bp_cut = ggplot(data = copy_diamonds, aes(x = cut)) +
    geom_bar(width = 0.75) +
    xlab('Cut (Quality of Cut)') +
    ylab('Frequência Absoluta') +
    labs(title = "Barplot - Cut") +
    scale_x_discrete(limits = c("Fair", "Good", "Very Good", "Premium", "Ideal")) +
    theme_minimal()
  bp_cut
  
  
        ### Transformando registros 'Fair' e 'Good' em um só: 'Regular'
  
        regular_cutdf = copy_diamonds %>% 
          select(everything()) %>% 
          filter(cut == 'Fair' | cut == 'Good')
        regular_cutdf
        
        copy_diamonds = anti_join(copy_diamonds, regular_cutdf)
        
        regular_cutdf$cut = 'Regular'
        
        copy_diamonds = full_join(copy_diamonds, regular_cutdf)
        
        
        ### Transformando as variáveis 'char' para 'factor':
        
        copy_diamonds = transform(copy_diamonds, cut = as.factor(cut))
                                  
                      
        ### Verificando o novo barplot:
        
        
        ## Cut - "SEGUE UMA DISTRIBUIÇÃO EXPONENCIAL POSITIVA"
        bp_cut = ggplot(data = copy_diamonds, aes(x = cut)) +
          geom_bar(width = 0.75) +
          xlab('Cut (Quality of Cut)') +
          ylab('Frequência Absoluta') +
          labs(title = "Barplot - Cut") +
          scale_x_discrete(limits = c("Regular", "Very Good", "Premium", 
                                      "Ideal")) +
          theme_minimal()
        bp_cut
        
  
  ## Color
        
  bp_color = ggplot(data = copy_diamonds, aes(x = color)) +
    geom_bar(width = 0.75) +
    xlab('Color (From J (worst) to D (best))') +
    ylab('Frequência Absoluta') +
    labs(title = "Barplot - Color") +
    scale_x_discrete(limits = c("J", "I", "H", "G", "F", "E", "D")) +
    theme_minimal()
  bp_color

  
  ## Clarity - "PARECE SER UMA DISTRIBUIÇÃO NORMAL..."
  bp_clarity = ggplot(data = copy_diamonds, aes(x = clarity)) +
    geom_bar(width = 0.75) +
    xlab('Clarity (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best)))') +
    ylab('Frequência Absoluta') +
    labs(title = "Barplot - Clarity") +
    scale_x_discrete(limits = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", 
                                "VVS1", "IF")) +
    theme_minimal()
  bp_clarity

  
        ### Transformando registros 'I1' e 'SI2' em um só: 'I1SI2'
  
        i1si2_claritydf = copy_diamonds %>% 
          select(everything()) %>% 
          filter(clarity == 'I1' | clarity == 'SI2')
        i1si2_claritydf
        
        copy_diamonds = anti_join(copy_diamonds, i1si2_claritydf)
        
        i1si2_claritydf$clarity = 'I1SI2'
        
        copy_diamonds = full_join(copy_diamonds, i1si2_claritydf)
        
        
        ### Transformando as variáveis 'char' para 'factor':
        
        copy_diamonds = transform(copy_diamonds, clarity = as.factor(clarity))
        
        
        ### Clarity - "PARECE SER UMA DISTRIBUIÇÃO NORMAL..."
        
        bp_clarity = ggplot(data = copy_diamonds, aes(x = clarity)) +
          geom_bar(width = 0.75) +
          xlab('Clarity (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, 
               IF (best)))') +
          ylab('Frequência Absoluta') +
          labs(title = "Barplot - Clarity") +
          scale_x_discrete(limits = c("I1SI2", "SI1", "VS2", "VS1", "VVS2", 
                                      "VVS1", "IF")) +
          theme_minimal()
        bp_clarity
  
  
  ## Depth - SEGUE UMA DISTRIBUIÇÃO NORMAL
        
  bp_depth = ggplot(data = copy_diamonds, aes(x = depth)) +
    geom_histogram(aes(y = ..density..), bins = ceiling(log2(length(copy_diamonds$depth)) + 1)) +
    geom_density(size = 1, colour = 'darkblue') +
    xlab('Depth Percentage (z / mean(x, y) = 2 * z / (x + y))') +
    ylab('Frequência Relativa') +
    labs(title = "Histograma - Depth") +
    theme_minimal()
  bp_depth

  
  ## Table - APARENTEMENTE PARECIA UMA DISTRIBUIÇÃO NORMAL, MAS NÃO É...
  bp_table = ggplot(data = copy_diamonds, aes(x = table)) +
    geom_histogram(aes(y = ..density..), 
                   bins = ceiling(log2(length(copy_diamonds$table)) + 1)) +
    geom_density(size = 1, colour = 'darkblue') +
    xlab('Table: Width of Top of diamond relative to Widest point') +
    ylab('Frequência Relativa') +
    labs(title = "Histograma - Table") +
    theme_minimal()
  bp_table

  
  ## Price - SEGUE UMA DISTRIBUIÇÃO EXPONENCIAL NEGATIVA
  
  bp_price = ggplot(data = copy_diamonds, aes(x = price)) +
    geom_histogram(aes(y = ..density..), bins = ceiling(log2(length(copy_diamonds$price)) + 1)) +
    geom_density(size = 1, colour = 'darkblue') +
    xlab('Price (U$)') +
    ylab('Frequência Relativa') +
    labs(title = "Histograma - Price") +
    theme_minimal()
  bp_price

  ## Verificando as Variáveis Contínuas em conjunto: 
  
  par_cont = grid.arrange(bp_carat, bp_depth, 
                          bp_table, bp_price,
                          ncol = 2, nrow = 2)

  
  ## Verificando as Variáveis Discretas em conjunto: 
  
  par_disc = grid.arrange(bp_cut, bp_color, bp_clarity,
                          ncol = 3, nrow = 1)

  
-----------------------------------------------------------------------------
  
    
# Analisando as variáveis pelo QQplot:

  ## Depth - SEGUE UMA DISTRIBUIÇÃO NORMAL!
    
  qqPlot(copy_diamonds$depth, main = "Distribuição das Variáveis - Depth", 
         xlab = 'Quantiles', 
         ylab = 'Valores de Depth')
  
  skewness(copy_diamonds$depth) 
  
  ## --0.3026153 --> A symmetrical distribution will have a skewness of 0!
  ## Usually mean your data have more extreme values than would be expected if 
  ## they truly came from a Normal distribution.
  
  
  kurtosis(copy_diamonds$depth) ##  0.3044868

  
  ## Table
  
  qqPlot(copy_diamonds$table, main = "Distribuição das Variáveis - Table", 
         xlab = 'Quantiles', 
         ylab = 'Valores de Table')
  
  skewness(copy_diamonds$table) ## 0.5939399
  
  kurtosis(copy_diamonds$table) 
  
  ## 0.6175618 ---> Platykurtic: (Kurtosis < 3): Distribution is shorter, 
  ## tails are thinner than the normal distribution. The peak is lower and broader 
  ## than Mesokurtic, which means that data are light-tailed or lack of outliers.
  
  
#==========Bubble Plots (Count ~DISCRETE~)==========

  
# Relacao Variaveis Discretas
  
  ## Cut x Color
  
  cnt_cclr = ggplot(copy_diamonds, aes(x = cut, y = color)) + 
    geom_count() +
    xlab('Cut (Quality of Cut)') +
    ylab('Color (From J (worst) to D (best))') +
    labs(title = "Relação - Cut x Color", size = 'Total de Registros') +
    scale_x_discrete(limits = c("Regular", "Very Good", "Premium", "Ideal")) +
    scale_y_discrete(limits = c("J", "I", "H", "G", "F", "E", "D")) +
    theme_classic()
  cnt_cclr

  
  ## Cut x Clarity
  
  cnt_cct = ggplot(copy_diamonds, aes(x = cut, y = clarity)) + 
    geom_count() +
    xlab('Cut (Quality of Cut)') +
    ylab('Clarity (I1SI2 (worst), SI1, VS2, VS1, VVS2, VVS1, IF (best)))') +
    labs(title = "Relação - Cut x Clarity", size = 'Total de Registros') +
    scale_x_discrete(limits = c("Regular", "Very Good", "Premium", "Ideal")) +
    scale_y_discrete(limits = c("I1SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")) +
    theme_classic()
  cnt_cct

  
  ## Color x Clarity
  
  cnt_clrct = ggplot(copy_diamonds, aes(x = color, y = clarity)) + 
    geom_count() +
    xlab('Color (From J (worst) to D (best))') +
    ylab('Clarity (I1SI2 (worst), SI1, VS2, VS1, VVS2, VVS1, IF (best)))') +
    labs(title = "Relação - Color x Clarity", size = 'Total de Registros') +
    scale_x_discrete(limits = c("J", "I", "H", "G", "F", "E", "D")) +
    scale_y_discrete(limits = c("I1SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")) +
    theme_classic()
  cnt_clrct

  
  ## Verificando as Variáveis Discretas em conjunto: 
  cnt_par_disc = grid.arrange(cnt_cclr, cnt_cct, cnt_clrct,
                            ncol = 3, nrow = 1)


#==========Box Plots (N x Price)==========


# Construindo gráficos de área para analisar QUAL VARIAVEL DISCRETA MAIS IMPACTUA NO PRECO:

  ## Cut
  
  bxp_cutprice = ggplot(copy_diamonds, aes(x = cut, y = price)) +
  geom_boxplot(colour = 'Black') +
  xlab('Cut') +
  ylab('Preço (U$)') +
  labs(title = "Boxplot - Cut x Price", 
       subtitle = "Quality of the Cut: Regular (Worst), Very Good, Premium, 
       Ideal (Best)") +
  scale_x_discrete(limits = c("Regular", "Very Good", "Premium", "Ideal")) +
  theme_grey()
  bxp_cutprice
  
  
  ## Clarity
  
  bxp_cltprice = ggplot(copy_diamonds, aes(x = clarity, y = price)) +
  geom_boxplot(colour = 'Black') +
  xlab('Clarity') +
  ylab('Preço (U$)') +
  labs(title = "Boxplot - Clarity x Price", 
      subtitle = "Clarity: I1SI2 (Worst), SI1, VS2, VS1, VVS2, VVS1, 
      IF (Best)") +
  scale_x_discrete(limits = c("I1SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", 
                              "IF")) +
  theme_grey()
  bxp_cltprice

  
  ## Color
  
  bxp_clrprice = ggplot(copy_diamonds, aes(x = color, y = price)) +
  geom_boxplot(colour = 'Black') +
  xlab('Color') +
  ylab('Preço (U$)') +
  labs(title = "Boxplot - Color x Price", 
      subtitle = "Color: J (Worst), I, H, G, F, E, D (Best)") +
  scale_x_discrete(limits = c("J", "I", "H", "G", "F", "E", "D")) +
  theme_grey()
  bxp_clrprice
  
  
# Criar um intervalo para transformar 'Carat' em uma variável discreta:
  
  df_carat = data.frame("carat_start" = c(0.00, 0.61, 1.22, 1.83, 2.44), 
                        "carat_end" = c(0.61, 1.22, 1.83, 2.44, 3.05),
                        "carat_class" = c('c0', 'c1', 'c2', 'c3', 'c4'))
  
  df_carat_range = data.frame(copy_diamonds, 
                                "carat_range" = cut(copy_diamonds$carat, 
                                                breaks = df_carat$carat_start,
                                                right = F, include.lowest=T))
  
  copy_diamonds = transform(df_carat_range, type = df_carat$carat_class
                                 [findInterval(df_carat_range$carat, 
                                               df_carat$carat_start)])
  
  
  ## Carat
  
  bxp_crtprice = ggplot(copy_diamonds, aes(x = type, y = price)) +
    geom_boxplot(colour = 'Black') +
    xlab('Carat Class') +
    ylab('Preço (U$)') +
    labs(title = "Boxplot - Carat x Price", 
         subtitle = "Carat Class: C0 (0.00 - 0.61),C1, C2, C3, C4, C5(2.44 - 3.05)") +
    theme_grey()
  bxp_crtprice
  
  
# Verificando as Variáveis Discretas em conjunto: 
  
  bxp_par_disc = grid.arrange(bxp_cutprice, bxp_cltprice, 
                              bxp_clrprice, bxp_crtprice,
                              ncol = 2, nrow = 2)
