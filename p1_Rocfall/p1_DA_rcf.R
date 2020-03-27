#==========Bibliotecas (Instalacao e Carregamento)==========

# Instalacao dos Pacotes necessarios:

install.packages("RMySQL") ##Acesso ao Banco de Dados MySQL
install.packages('dplyr') ##Manipulacao de Dados
install.packages("ggplot2") ##Visualizacao de Dados


# Acessando os Pacotes:

library(RMySQL)
library("ggplot2")
library('dplyr')


#==========Conexao ao DB no MySQL==========


# Conectando ao MySQL e Obtendo os Dados:

my_db = dbConnect(MySQL(), user = 'root', password = '', dbname = 'dba_rocfall', 
                  host = '127.0.0.1', port = 3306) 
      ### Informacoes sobre o usuario e outros requisitos do MySQL 

s = paste0("select * from dados_rocfall") 
      ### Comando para selecionar tudo da base de dados

rs = dbSendQuery(my_db, s)
       ### Enviando Query ao MySQL

df_rcf =  fetch(rs, n = -1)

on.exit(dbDisconnect(my_db)) 
      ###Saindo do MySQL


#=====================Analise Estatistica e Descritiva dos Resultados=========


# Describe

summary(df_rcf)          ## Médias: - Area Critica = 52,12% ; 
                         ## Dispersao = 35,61% ; 
                         ## Mapeadas = 87,64%

                         ## Distribuicao Normal das Variáveis


#==========Correlacao Numerica Alcance/Velocidade Angular==========


# DataFrames por Alturas:

valores_h5 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, porc_disp, porc_rock_mapped) %>% 
  filter(h_enc == 5)

valores_h10 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, porc_disp, porc_rock_mapped) %>% 
  filter(h_enc == 10)

valores_h20 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, porc_disp, porc_rock_mapped) %>% 
  filter(h_enc == 20)

valores_h50 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, porc_disp, porc_rock_mapped) %>% 
  filter(h_enc == 50)

valores_h100 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, porc_disp, porc_rock_mapped) %>% 
  filter(h_enc == 100)


  ## Correlacao de Pearson Alcance/Velocidade Angular - Alturas
  
  cr_prs_h5 = cor(valores_h5$mean_range, valores_h5$vel_rad) #0.6642546
  cr_prs_h10 = cor(valores_h10$mean_range, valores_h10$vel_rad) #0.5419937
  cr_prs_h20 = cor(valores_h20$mean_range, valores_h20$vel_rad) #0.3509454
  cr_prs_h50 = cor(valores_h50$mean_range, valores_h50$vel_rad) #0.150241
  cr_prs_h100 = cor(valores_h100$mean_range, valores_h100$vel_rad) #0.07200185

        ### A relação entre as variáveis vai diminuindo a medida que a 
        ### altura aumenta


  ## Unindo as correlacoes em um único DF:
  cr_prs_hs = c(cr_prs_h5, cr_prs_h10, cr_prs_h20, cr_prs_h50, cr_prs_h100)
  
  hs = c(5, 10, 20, 50, 100)
  
  df_crhs = data.frame(cr_prs_hs, hs)


  ## Verificando a disposição em um gráfico de pontos e a possivel curva:
  
  gcurv_cr_prs_hs = ggplot(data = df_crhs, aes(x = cr_prs_hs, y = hs)) + 
    geom_path(linetype = 'dashed', colour = 'red', size = 1.25) + 
    geom_point(colour = 'black', size = 2) +
    labs(title = "Curva Estimada da Correlação de Pearson\n\n
         Alcance/Velocidade Angular\n\nCritério por Altura") +
    scale_x_continuous("Valor das Correlações") + 
    scale_y_continuous("Altura da Encosta (m)", limits = c(0,100), 
                       breaks = c(0,10, 20, 50, 100)) + 
    theme_minimal()
  gcurv_cr_prs_hs

  
--------------------------------------------------
  

#DataFrames por Angulo da Encosta:
    
valores_angenc50 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(ang_enc == 50)

valores_angenc60 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(ang_enc == 60)

valores_angenc70 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(ang_enc == 70)

valores_angenc80 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(ang_enc == 80)

valores_angenc90 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(ang_enc == 90)


  ## Correlação de Pearson Alcance/Velocidade Angular - Ângulo da Encosta
  
  cr_prs_angenc50 = cor(valores_angenc50$mean_range, 
                        valores_angenc50$vel_rad) #0.06101697
  cr_prs_angenc60 = cor(valores_angenc60$mean_range, 
                        valores_angenc60$vel_rad) #0.07412709
  cr_prs_angenc70 = cor(valores_angenc70$mean_range, 
                        valores_angenc70$vel_rad) #0.09483567
  cr_prs_angenc80 = cor(valores_angenc80$mean_range, 
                        valores_angenc80$vel_rad) #0.1643499
  cr_prs_angenc90 = cor(valores_angenc90$mean_range, 
                        valores_angenc90$vel_rad) #0.6946701

        ### A relação entre as variáveis vai aumentando a medida que o 
        ### ângulo aumenta


  ## Unindo as correlacoes em um único DF:

  cr_prs_angencs = c(cr_prs_angenc50, cr_prs_angenc60, cr_prs_angenc70, 
                     cr_prs_angenc80, cr_prs_angenc90)
  
  angencs = c(50, 60, 70, 80, 90)
  
  df_angencs = data.frame(cr_prs_angencs, angencs)

  
  ## Verificando a disposição em um gráfico de pontos e a possivel curva:
  
  gcurv_cr_prs_angencs = ggplot(data = df_angencs, aes(x = cr_prs_angencs, 
                                                       y = angencs)) + 
    geom_path(linetype = 'dashed', colour = 'blue', size = 1.25) + 
    geom_point(colour = 'black', size = 2) +
    labs(title = "Curva Estimada da Correlação de Pearson\n\n
         Alcance/Velocidade Angular\n\nCritério por Ângulo") +
    xlab("Valor das Correlações") + 
    ylab("Ângulo da Encosta (°)") + 
    theme_minimal()
  gcurv_cr_prs_angencs

  
--------------------------------------------------
  

# DataFrames por Massa do Bloco:
  
valores_mass85000 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 85000)

valores_mass167000 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>%
  filter(rock_mass == 167000)

valores_mass288000 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>%
  filter(rock_mass == 288000)

valores_mass458000 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>%
  filter(rock_mass == 458000)

valores_mass973000 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>%
  filter(rock_mass == 973000)

valores_mass2307000 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 2307000)

valores_mass3664000 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 3664000)


  ## Correlação de Pearson Alcance/Velocidade Angular - Massa do Bloco

  cr_prs_mass85000 = cor(valores_mass85000$mean_range, 
                         valores_mass85000$vel_rad) #0.03292435
  cr_prs_mass167000 = cor(valores_mass167000$mean_range, 
                          valores_mass167000$vel_rad) #0.04497623
  cr_prs_mass288000 = cor(valores_mass288000$mean_range, 
                          valores_mass288000$vel_rad) #0.05911093
  cr_prs_mass458000 = cor(valores_mass458000$mean_range, 
                          valores_mass458000$vel_rad) #0.07406367
  cr_prs_mass973000 = cor(valores_mass973000$mean_range, 
                          valores_mass973000$vel_rad) #0.1087721
  cr_prs_mass2307000 = cor(valores_mass2307000$mean_range, 
                           valores_mass2307000$vel_rad) #0.1620004
  cr_prs_mass3664000 = cor(valores_mass3664000$mean_range, 
                           valores_mass3664000$vel_rad) #0.2055921

        ### A relação entre as variáveis vai aumentando a medida que a massa do 
        ### bloco aumenta, mas ainda permanece muito baixo!


  ## Unindo as correlacoes em um único DF:

  cr_prs_masses = c(cr_prs_mass85000, cr_prs_mass167000, cr_prs_mass288000, 
                    cr_prs_mass458000, cr_prs_mass973000, cr_prs_mass2307000, 
                    cr_prs_mass3664000)

  masses = c(85000, 167000, 288000, 458000, 973000, 2307000, 3664000)

  df_masses = data.frame(cr_prs_masses, masses)


  ## Verificando a disposição em um gráfico de pontos e a possivel curva:
  
  gcurv_cr_prs_masses = ggplot(data = df_masses, aes(x = cr_prs_masses, 
                                                     y = masses)) + 
    geom_path(linetype = 'dashed', colour = 'darkgreen', size = 1.25) + 
    geom_point(colour = 'black', size = 2) +
    labs(title = "Curva Estimada da Correlação de Pearson\n\n
         Alcance/Velocidade Angular\n\nCritério por Massa") +
    scale_x_continuous("Valor das Correlações") + 
    scale_y_continuous("Massa do Bloco (kg)", limits = c(85000,3664000), 
         breaks = c(85000, 167000, 288000, 458000, 973000, 2307000, 3664000)) + 
    theme_minimal()
  gcurv_cr_prs_masses


#==========Correlacao Grafica: Alcance/Velocidade Angular==========

 
# Dispersograma relacionando Alcance com a Velocidade Angular - Altura 5m
  
disp_a_va_h5 = ggplot(data = valores_h5, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance Médio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular\n\nAltura 5m") +
  geom_smooth(method = "lm", se = FALSE, colour = 'red') +
  theme_minimal()
disp_a_va_h5


# Dispersograma relacionando Alcance com a Velocidade Angular - Altura 10m

disp_a_va_h10 = ggplot(data = valores_h10, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance Médio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular\n\nAltura 10m") +
  geom_smooth(method = "lm", se = FALSE, colour = 'red') +
  theme_minimal()
disp_a_va_h10


# Dispersograma relacionando Alcance com a Velocidade Angular - Altura 20m

disp_a_va_h20 = ggplot(data = valores_h20, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance Médio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular\n\nAltura 20m") +
  geom_smooth(method = "lm", se = FALSE, colour = 'red') +
  theme_minimal()
disp_a_va_h20


# Dispersograma relacionando Alcance com a Velocidade Angular - Altura 50m

disp_a_va_h50 = ggplot(data = valores_h50, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance Médio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular\n\nAltura 50m") +
  geom_smooth(method = "lm", se = FALSE, colour = 'red') +
  theme_minimal()
disp_a_va_h50


# Dispersograma relacionando Alcance com a Velocidade Angular - Altura 100m

disp_a_va_h100 = ggplot(data = valores_h100, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance Médio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular\n\nAltura 100m") +
  geom_smooth(method = "lm", se = FALSE, colour = 'red') +
  theme_minimal()
disp_a_va_h100


--------------------------------------------------


# Dispersograma relacionando Alcance com a Velocidade Angular - Ângulo 50°
  
disp_a_va_angenc50 = ggplot(data = valores_angenc50, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance Médio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular - Ângulo 50°") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
disp_a_va_angenc50


# Dispersograma relacionando Alcance com a Velocidade Angular - Ângulo 60°

disp_a_va_angenc60 = ggplot(data = valores_angenc60, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance Médio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular - Ângulo 60°") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
disp_a_va_angenc60


# Dispersograma relacionando Alcance com a Velocidade Angular - Ângulo 70°

disp_a_va_angenc70 = ggplot(data = valores_angenc70, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance M?dio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular - Ângulo 70°") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
disp_a_va_angenc70


# Dispersograma relacionando Alcance com a Velocidade Angular - Ângulo 80°

disp_a_va_angenc80 = ggplot(data = valores_angenc80, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance M?dio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular - Ângulo 80°") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
disp_a_va_angenc80


# Dispersograma relacionando Alcance com a Velocidade Angular - Ângulo 90°

disp_a_va_angenc90 = ggplot(data = valores_angenc90, aes(x = mean_range, y = vel_rad)) +
  geom_jitter(height = 2, width = 2) +
  xlab('Alcance M?dio dos Blocos (m)') +
  ylab('Velocidade Angular (rad/s)') +
  labs(title = "Dispersograma da Relação Alcance/Velocidade Angular - Ângulo 90°") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
disp_a_va_angenc90


#==========Grafico de Barra Áreas Crítica/Massa dos Blocos==========


# DataFrames por Massa do Bloco e Velocidade Angular:

valores_mass85000_velrad0 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 85000, vel_rad == 0)

valores_mass85000_velrad1 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 85000, vel_rad == 1)

valores_mass85000_velrad2 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 85000, vel_rad == 2)

valores_mass85000_velrad3 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 85000, vel_rad == 3)

valores_mass85000_velrad4 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 85000, vel_rad == 4)

valores_mass85000_velrad5 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 85000, vel_rad == 5)


# Gráficos de Barra para Velocidade Angular 0rad/s

gbar_mass85000_velrad0 = ggplot(data = valores_mass85000_velrad0, 
                                aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass85000_velrad0$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 85000kg e Velocidade Angular 0rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass85000_velrad0


# Gráficos de Barra para Velocidade Angular 1rad/s

gbar_mass85000_velrad1 = ggplot(data = valores_mass85000_velrad1, 
                                aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass85000_velrad1$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 85000kg e Velocidade Angular 1rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass85000_velrad1


# Gráficos de Barra para Velocidade Angular 2rad/s

gbar_mass85000_velrad2 = ggplot(data = valores_mass85000_velrad2, 
                                aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass85000_velrad2$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 85000kg e Velocidade Angular 2rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass85000_velrad2


# Gráficos de Barra para Velocidade Angular 3rad/s

gbar_mass85000_velrad3 = ggplot(data = valores_mass85000_velrad3, 
                                aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass85000_velrad3$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 85000kg e Velocidade Angular 3rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass85000_velrad3


# Gráficos de Barra para Velocidade Angular 4rad/s

gbar_mass85000_velrad4 = ggplot(data = valores_mass85000_velrad4, 
                                aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass85000_velrad4$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 85000kg e Velocidade Angular 4rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass85000_velrad4


# Gráficos de Barra para Velocidade Angular 5rad/s

gbar_mass85000_velrad5 = ggplot(data = valores_mass85000_velrad5, 
                                aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass85000_velrad5$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área Crítica\nBlocos de 85000kg e Velocidade Angular 5rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass85000_velrad5


      ### A medida que vai aumentando a velocidade angular, menos blocos caem na 
      ### área crítica quando aumentamos a altura da encosta e seu ângulo. 
      ### De 50° a 70° há uma linearidade. Quando analisamos os ângulos a partir 
      ### de 80° há uma variação devido ao modelo de definição de área crítica 
      ### segundo o manual da CPRM.


--------------------------------------------------


# DataFrames por Massa do Bloco diferentes e Velocidade Angular = 0
  
valores_mass167000_velrad0 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 167000, vel_rad == 0)

valores_mass288000_velrad0 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 288000, vel_rad == 0)

valores_mass458000_velrad0 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 458000, vel_rad == 0)

valores_mass973000_velrad0 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 973000, vel_rad == 0)

valores_mass2307000_velrad0 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 2307000, vel_rad == 0)

valores_mass3664000_velrad0 = df_rcf %>% 
  select(h_enc, ang_enc, rock_mass, mean_range, vel_rad, porc_ac, 
         porc_disp, porc_rock_mapped) %>% 
  filter(rock_mass == 3664000, vel_rad == 0)


# Gráficos de Barra - Massa = 85000kg

gbar_mass85000_velrad0 = ggplot(data = valores_mass85000_velrad0, 
                                aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass85000_velrad0$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área
       Crítica\nBlocos de 85000kg e Velocidade Angular 0rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass85000_velrad0


# Gráficos de Barra - Massa = 167000kg

gbar_mass167000_velrad0 = ggplot(data = valores_mass167000_velrad0, 
                                 aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass167000_velrad0$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 167000kg e Velocidade Angular 0rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass167000_velrad0


#Gráficos de Barra - Massa = 288000kg

gbar_mass288000_velrad0 = ggplot(data = valores_mass288000_velrad0, 
                                 aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass288000_velrad0$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 288000kg e Velocidade Angular 0rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass288000_velrad0


#Gráficos de Barra - Massa = 458000kg

gbar_mass458000_velrad0 = ggplot(data = valores_mass458000_velrad0, 
                                 aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass458000_velrad0$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 458000kg e Velocidade Angular 0rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass458000_velrad0


#Gráficos de Barra - Massa = 973000kg

gbar_mass973000_velrad0 = ggplot(data = valores_mass973000_velrad0, 
                                 aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass973000_velrad0$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 973000kg e Velocidade Angular 0rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass973000_velrad0


#Gráficos de Barra - Massa = 2307000kg

gbar_mass2307000_velrad0 = ggplot(data = valores_mass2307000_velrad0, 
                                  aes(x = ang_enc, y = porc_ac, 
                                  fill = as.factor(
                                    valores_mass2307000_velrad0$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 2307000kg e Velocidade Angular 0rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass2307000_velrad0


#Gráficos de Barra - Massa = 3664000kg

gbar_mass3664000_velrad0 = ggplot(data = valores_mass3664000_velrad0, 
                                  aes(x = ang_enc, y = porc_ac, 
                                fill = as.factor(
                                  valores_mass3664000_velrad0$h_enc))) +
  geom_bar(stat = 'identity', color = 'black', position=position_dodge()) +
  scale_x_continuous("Ângulo da Encosta (°)") +
  scale_y_continuous("Porcentagem de Blocos na AC") +
  labs(title = 'Gráfico de Barras \nRelação Topografia/Velocidade Angular/Área 
       Crítica\nBlocos de 3664000kg e Velocidade Angular 0rad/s', 
       fill = 'Altura da Encosta (m)') +
  scale_fill_brewer(palette="YlOrRd") +
  theme_minimal()
gbar_mass3664000_velrad0


      ### No fim das contas, esses gráficos provam que a massa não é um grande 
      ### fator que condiciona a quantidade de blocos que cai na área critica, visto 
      ### que a variação entre as diferentes massas é muito pequena.


#==========Grafico de Frequencia Areas Critica==========


# Promovendo uma classificacao para as Areas Criticas:

df_classify_porc_ac = data.frame(
  "porc_ac_start" = c(0.000, 0.250, 0.500, 0.750), 
  "porc_ac_end" = c(0.250, 0.500, 0.750, 1.000),
  "porc_ac_type" = c('Ruim', 'Regular', 'Bom', 'Excelente')) 
      ### Criando um df que define 4 intervalos de 0.250 em 0.250:
            #### - Ruim
            #### - Regular
            #### - Bom
            #### - Excelente

df_cl_porc_range = data.frame(df_rcf, 
  "porc_ac_range" = cut(df_rcf$porc_ac, 
                        breaks = df_classify_porc_ac$porc_ac_start,
                        right = F, include.lowest=T))
      ### Estabelecendo esses intervalos em 'df_rcf', criando uma feature
      ### nomeada "porc_ac_range"

df_cl_porc_ac_type = transform(df_cl_porc_range, 
                               type = df_classify_porc_ac$porc_ac_type
                               [findInterval(df_cl_porc_range$porc_ac, 
                                          df_classify_porc_ac$porc_ac_start)])
      ### Gerando um novo df, onde os ajustes foram feitos e as classes foram
      ### criadas conforme o intervalo estabelecido


--------------------------------------------------


#Gerando os gráficos:

graf_fpol_class_porc_ac = ggplot(df_cl_porc_ac_type, aes(x = ang_enc, 
                                                         colour = type)) +
  geom_freqpoly(size = 1.05) +
  xlab('Ângulo da Encosta (°)') +
  ylab('Frequência') +
  labs(title = "Gráfico de Frequência\n\nClassificação das Áreas Críticas por 
       Quantidade Absoluta", 
       colour = 'Classificações') +
  theme_bw()
graf_fpol_class_porc_ac


      ### Esse gráfico mostra a quantidade de blocos que caem na área crítica em 
      ### frequência absoluta. As variações nas velocidades angulares são muito 
      ### radicais, o que faz muitos blocos se concentrarem nas classificações de 
      ### "Excelente" e "Ruim". ## A medida que o Ângulo aumenta, a tendencia é 
      ### diminuir os blocos que se concentram nas áreas "Excelente" e aumentar os 
      ### blocos na area "Ruim". Quando chega no angulo de 80°, quando muda de grupo de 
      ### encosta, a classificacao piora demasiadamente! Muitos blocos fogem da area critica. 
      ### O angulo de 90° tem equilibrio de areas "Excelente" e "Ruim"
      ### Também prova que a medida que o ângulo aumenta, a correlação 
      ### alcance/velocidade angular aumenta, comprovada pelo aumento da classe "Ruim", 
      ### representando proporção positiva entre os parâmetros.
