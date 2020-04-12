#==========Bibliotecas (Instalacao e Importacao)==========


# Instalacao dos Pacotes:

install.packages('dplyr') ## Manipulacao de Dados
install.packages('ggplot2') ## Visualizacao Grafica dos Dados
install.packages('RgoogleMaps') ## Visualizacao Espacial c/ Google Maps
install.packages('raster') ## Ferramentas para trabalhar c/ Shapefiles


# Carregando as bibliotecas:

library("dplyr", lib.loc="~/R/win-library/3.6")
library("ggplot2", lib.loc="~/R/win-library/3.6")
library("RgoogleMaps", lib.loc="~/R/win-library/3.6")
library("raster", lib.loc="~/R/win-library/3.6")


#==========Upload da Base de Dados==========

crime = read.csv(
  "C:/Users/felip/Desktop/Cursos/Kaggle/bostonCrimes_kgl/crime.csv")


# Mostrando os primeiros registros da base de dados:

head(crime)


#==========Pre-Processamento==========

# Mostrando um resumo estatistico dos dados:

summary(crime)


# Tratando NA's:

  ## Tratando "" por NA:

  crime = crime %>% 
    mutate_all(na_if, "")


  ## Substituindo NA 'Shooting' por "N":

  crime$SHOOTING = as.character(crime$SHOOTING)
  
  crime = crime %>%
    mutate(SHOOTING = replace(SHOOTING, which(is.na(SHOOTING)), "N"))
  
  crime$SHOOTING = as.factor(crime$SHOOTING)


# Verificando MISSING VALUES:

crime %>% 
  select_all %>%
  summarise_all(funs(sum(is.na(.))))


  ## Removendo MISSING VALUES do db:
  
  crime_preproccess = crime %>%
    select_all %>%
    na.omit()


# Tratando 'Locations' (-1, -1)

otl_crime_locations = crime_preproccess %>% 
  select_all %>% 
  filter(Lat ==  -1.00000000 | Long == -1.00000000)

  ## Removendo esses dados do db:

  crime_preproccess = anti_join(crime_preproccess, otl_crime_locations)
  

# Analisando novamente o 'summary':

summary(crime_preproccess)




#==========Mapa de Boston==========


# Analisando o Mapa de Boston retirado do 'Google Maps':

coord_boston = GetMap(center = c(lat = 42.36025, lon = -71.05829), 
                      destfile = tempfile("boston_map", fileext = ".png"), 
                      zoom = 11, type = 'google-m')

boston_map = PlotOnStaticMap(coord_boston)


#==========Ocorrencia dos Crimes no Mapa==========


# Colocando os pontos de Ocorrencia dos crimes na cidade de Boston:

crime_occ_map = PlotOnStaticMap(boston_map, lon = crime_preproccess$Long, 
                                lat = crime_preproccess$Lat, destfile = 'crime_occ_map.png',
                                FUN = points, col = "red", add = T)


#==========Contorno dos Bairros==========


# Carregando Shapefile com os poligonos da cidade e dos bairros:

  ## Shapefile dos Bairros:
  
  shp_nB = shapefile(
  "C:/Users/felip/Desktop/Cursos/Kaggle/bostonCrimes_kgl/Boston_Neighborhoods.shp")

  df_shp_nB = as.data.frame(shp_nB)
  
    ### Colocando na mesma projecao do Google Maps:
    crs = CRS("+proj=longlat +datum=WGS84")
    shp_nB = spTransform(shp_nB, crs)
  
    ### Transformando para 'SpatialPolygons' que e o formato que o
    ### 'PlotPolysOnStaticMap' aceita o poliogono:
    
    shp_nB = SpatialPolygons(Srl = shp_nB@polygons)
  
    
# Importando os poligonos para nosso mapa:
    
boston_map = PlotOnStaticMap(coord_boston)
crime_occ_map = PlotOnStaticMap(boston_map, lon = crime_preproccess$Long, 
                                lat = crime_preproccess$Lat, 
                                destfile = 'crime_occ_map.png',
                                FUN = points, col = "red", add = T)
PlotPolysOnStaticMap(boston_map, shp_nB, add = T)



#==========Crimes Existentes==========


# Analisando os Crimes Existentes:
levels.default(sort(crime_preproccess[["OFFENSE_CODE_GROUP"]]))


#==========Encoding dos Crimes==========

# Criando uma funcao que ira fazer um encoding das variaveis do tipo 'factor':

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}


# Encoding Ordinal da feature 'OFFENSE_CODE_GROUP':

crime_preproccess[["OFFENSE_SORT_ENCODED"]] = 
  encode_ordinal(crime_preproccess[["OFFENSE_CODE_GROUP"]],
    order = levels.default(sort(crime_preproccess[["OFFENSE_CODE_GROUP"]])))


#==========Grafico de Barras - Crimes==========


# Criando um grafico de barras para verificar a frequência dos crimes mais
# cometidos:

ggplot(data = crime_preproccess, aes(x = OFFENSE_SORT_ENCODED)) +
  geom_bar(aes(y = (..count..)), position = 'dodge', width = 0.5, fill = 'blue') +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1, size = 3) +
  xlab('Código dos Crimes') +
  ylab('Frequência Absoluta') +
  labs(title = "Tipos de Ocorrências", 
       subtitle = "Crimes cometidos na cidade de Boston-MA: 
Junho/2015 a Outubro/2019 (Fonte: data.boston.gov)") +
  scale_x_discrete(limits = c(1:67)) +
  theme_classic()

#==========Grafico de Pizza - Shooting==========


n = sum(crime_preproccess$SHOOTING == 'N')
y = sum(crime_preproccess$SHOOTING == 'Y')

shoot_perc = c(n, y)

piepercent = paste(round((100 * shoot_perc)/(sum(shoot_perc)), 2), 
                   "%", sep="")


# Fazendo um grafico do tipo pizza para verificar se as ocorrencias cometidas
# tiveram envolvimento de tiro ou nao:

pie(shoot_perc, labels = piepercent, col = c('darkgrey', 'white'),
    main = 'Porcentagem de Crimes\nEnvolvimento com Arma de Fogo\nBoston-MA Crimes', border = 'black')
legend("bottomright", c('No', 'Yes'), cex = 0.9, fill = c('darkgrey', 'white'))
text(0, 1, "Junho/2015 a Outubro/2019 (Fonte: data.boston.gov)", col = "black")


#==========Time Series - PreProcess==========


# Criando a coluna 'DATE' para trabalhar com time series:

  ## Copiando dados de 'OCCURRED_ON_DATE':

  crime_preproccess$DATE = crime_preproccess$OCCURRED_ON_DATE

  ## Transformando para class 'character':

  crime_preproccess$DATE = as.character(crime_preproccess$DATE)

  ## Transformando para class 'POSIXlt':
  
  crime_preproccess$DATE = strptime(crime_preproccess$DATE, 
                                    format = "%Y-%m-%d %H:%M:%S")
  
  ## Alterando para class 'date':
  
  crime_preproccess$DATE = format(crime_preproccess$DATE, "%Y-%m-%d")
  crime_preproccess$DATE = as.Date.character(crime_preproccess$DATE)

  
#==========Time Series - Graficos==========


# Gerando um grafico Time Series das ocorrencias entre Junho/2015 a 
# Setembro/2019

  # 1 - Por Mes/Ano:

  ggplot(data = crime_preproccess, aes(x = MONTH)) +
    geom_line(stat = "count", colour = 'darkblue', size = 0.5) +
    facet_grid(YEAR ~.) +
    geom_text(stat = 'count', aes(label = ..count..), 
              vjust = -1, size = 3) +
    scale_x_continuous(labels = c(1:12), 
                       breaks = c(1:12)) +
    scale_y_continuous(limits = c(4000, 10000),
                       breaks = c(seq(4000, 10000, by = 2000))) +
    xlab('Mês') +
    ylab('N° Total de Crimes') +
    labs(title = 'Número de Crimes por Mes-Ano', 
         subtitle = "Boston-MA: Junho/2015 a Setembro/2019 
         (Fonte: data.boston.gov)") +
    theme_minimal()
    ### Como a pesquisa termina no início de Outubro/2019, nao foi 
    ### posta no grafico por não apresentar dados relativos do mes 
    ### todo.
  
  
  # 2 - Por Hora/Ano:
  ggplot(data = crime_preproccess, aes(x = HOUR)) +
    geom_line(stat = "count", colour = 'darkgrey', size = 1) +
    facet_grid(YEAR ~.) +
    geom_text(stat = 'count', aes(label = ..count..), 
              vjust = -1, size = 3) +
    scale_x_continuous(labels = c(0:23), 
                       breaks = c(0:23)) +
    scale_y_continuous(limits = c(0, 7500),
                       breaks = c(seq(0, 7500, by = 2500))) +
    xlab('Hora') +
    ylab('N° Total de Crimes') +
    labs(title = 'Número de Crimes por Hora-Ano', 
         subtitle = "Boston-MA: Junho/2015 a Setembro/2019 
         (Fonte: data.boston.gov)") +
    theme_minimal()



#==========Heat Map - PreProcess==========

  
# Utilizando a funcao 'fortify' para transformar o shapefile em 
# dataframe e pegar as coordenadas dos poligonos:
  
shp_nB.fort = shp_nB
shp_nB.fort = fortify(shp_nB.fort)


#==========Heat Map - Crimes==========
  

# Plotando um Heat Map das ocorrencias ao longo da cidade:

  ## 1 - Geral

  ggplot(shp_nB.fort, aes(x = long, y = lat, group = group)) +
    geom_polygon(colour = 'black', fill = 'white') +
    stat_density2d(data = crime_preproccess, aes(x = Long, y = Lat, 
                                                 fill = ..level..), 
                   alpha = 0.5, inherit.aes = FALSE, geom = "polygon") +
    scale_fill_distiller(palette = "Spectral") +
    theme_minimal()
  
  ## 2 - Por ano:
  
  ggplot(shp_nB.fort, aes(x = long, y = lat, group = group)) +
    geom_polygon(colour = 'black', fill = 'white') +
    stat_density2d(data = crime_preproccess, aes(x = Long, y = Lat, 
                                                 fill = ..level..), 
                   alpha = 0.5, inherit.aes = FALSE, geom = "polygon") +
    facet_wrap(~YEAR) +
    scale_fill_distiller(palette = "Spectral") +
    theme_minimal()
  
