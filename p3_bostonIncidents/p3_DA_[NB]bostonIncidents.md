p3\_DA\_\[NB\]bostonIncidents
================

Configurando para Apresentacao no Github

``` r
knitr::opts_chunk$set(echo = TRUE)
```

Carregando Pacotes

``` r
library("dplyr", lib.loc="~/R/win-library/3.6")
```

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("ggplot2", lib.loc="~/R/win-library/3.6")
library("RgoogleMaps", lib.loc="~/R/win-library/3.6")
```

    ## Warning: package 'RgoogleMaps' was built under R version 3.6.3

``` r
library("raster", lib.loc="~/R/win-library/3.6")
```

    ## Warning: package 'raster' was built under R version 3.6.3

    ## Loading required package: sp

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

Upload do DB

``` r
crime = read.csv(
  "C:/Users/felip/Desktop/Cursos/Kaggle/bostonCrimes_kgl/crime.csv")
```

Primeiras Linhas do DB

``` r
head(crime)
```

    ##   INCIDENT_NUMBER OFFENSE_CODE   OFFENSE_CODE_GROUP
    ## 1      I192082859          724           Auto Theft
    ## 2      I192082751          724           Auto Theft
    ## 3      I192082680          727           Auto Theft
    ## 4      I192082577          724           Auto Theft
    ## 5      I192079582          727           Auto Theft
    ## 6      I192078648         3114 Investigate Property
    ##                  OFFENSE_DESCRIPTION DISTRICT REPORTING_AREA SHOOTING
    ## 1                         AUTO THEFT      E18            519         
    ## 2                         AUTO THEFT      E18            493         
    ## 3 AUTO THEFT - LEASED/RENTED VEHICLE      D14            794         
    ## 4                         AUTO THEFT       D4            130         
    ## 5 AUTO THEFT - LEASED/RENTED VEHICLE      A15             47         
    ## 6               INVESTIGATE PROPERTY       B3            427         
    ##      OCCURRED_ON_DATE YEAR MONTH DAY_OF_WEEK HOUR   UCR_PART           STREET
    ## 1 2019-10-13 09:28:24 2019    10      Sunday    9   Part One       LINCOLN ST
    ## 2 2019-10-12 20:11:26 2019    10    Saturday   20   Part One METROPOLITAN AVE
    ## 3 2019-10-12 15:12:43 2019    10    Saturday   15   Part One       ALLSTON ST
    ## 4 2019-10-12 04:41:52 2019    10    Saturday    4   Part One  SAINT JAMES AVE
    ## 5 2019-10-02 08:08:49 2019    10   Wednesday    8   Part One        N MEAD ST
    ## 6 2019-09-29 06:39:00 2019     9      Sunday    6 Part Three       WILMORE ST
    ##        Lat      Long                    Location
    ## 1 42.25952 -71.12156 (42.25951765, -71.12156299)
    ## 2 42.26209 -71.11671 (42.26209214, -71.11670964)
    ## 3 42.35237 -71.13510 (42.35237455, -71.13509584)
    ## 4 42.34948 -71.07640 (42.34947586, -71.07640150)
    ## 5 42.38185 -71.06655 (42.38184582, -71.06655134)
    ## 6 42.27796 -71.09246 (42.27796370, -71.09246318)

Pre Processamento

``` r
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
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

    ##   INCIDENT_NUMBER OFFENSE_CODE OFFENSE_CODE_GROUP OFFENSE_DESCRIPTION DISTRICT
    ## 1               0            0                  0                   0     2169
    ##   REPORTING_AREA SHOOTING OCCURRED_ON_DATE YEAR MONTH DAY_OF_WEEK HOUR UCR_PART
    ## 1          27253        0                0    0     0           0    0      110
    ##   STREET   Lat  Long Location
    ## 1  12391 27204 27204        0

``` r
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
```

    ## Joining, by = c("INCIDENT_NUMBER", "OFFENSE_CODE", "OFFENSE_CODE_GROUP",
    ## "OFFENSE_DESCRIPTION", "DISTRICT", "REPORTING_AREA", "SHOOTING",
    ## "OCCURRED_ON_DATE", "YEAR", "MONTH", "DAY_OF_WEEK", "HOUR", "UCR_PART",
    ## "STREET", "Lat", "Long", "Location")

Summary dos Dados Pos-Processamento

``` r
summary(crime_preproccess)
```

    ##       INCIDENT_NUMBER    OFFENSE_CODE                        OFFENSE_CODE_GROUP
    ##  I152071596   :    20   Min.   : 111   Motor Vehicle Accident Response: 40374  
    ##  I172053750   :    18   1st Qu.:1001   Larceny                        : 33569  
    ##  I192025403   :    15   Median :2907   Medical Assistance             : 31053  
    ##  I162067346   :    14   Mean   :2303   Investigate Person             : 23679  
    ##  I182051210   :    14   3rd Qu.:3201   Other                          : 22561  
    ##  I130041200-00:    13   Max.   :3831   Simple Assault                 : 19985  
    ##  (Other)      :395609                  (Other)                        :224482  
    ##                             OFFENSE_DESCRIPTION    DISTRICT      REPORTING_AREA
    ##  SICK/INJURED/MEDICAL - PERSON        : 24966   B2     : 62808   Min.   :  1   
    ##  INVESTIGATE PERSON                   : 23679   C11    : 54880   1st Qu.:178   
    ##  M/V - LEAVING SCENE - PROPERTY DAMAGE: 19026   D4     : 50026   Median :345   
    ##  VANDALISM                            : 18988   B3     : 45948   Mean   :385   
    ##  ASSAULT SIMPLE - BATTERY             : 18594   A1     : 41990   3rd Qu.:545   
    ##  VERBAL DISPUTE                       : 17535   C6     : 28531   Max.   :962   
    ##  (Other)                              :272915   (Other):111520                 
    ##  SHOOTING              OCCURRED_ON_DATE       YEAR          MONTH       
    ##  N:394041   2017-06-01 00:00:00:    32   Min.   :2015   Min.   : 1.000  
    ##  Y:  1662   2015-07-01 00:00:00:    26   1st Qu.:2016   1st Qu.: 4.000  
    ##             2016-08-01 00:00:00:    26   Median :2017   Median : 7.000  
    ##             2015-12-07 11:38:00:    25   Mean   :2017   Mean   : 6.647  
    ##             2017-08-01 00:00:00:    24   3rd Qu.:2018   3rd Qu.: 9.000  
    ##             2017-01-01 00:00:00:    23   Max.   :2019   Max.   :12.000  
    ##             (Other)            :395547                                  
    ##     DAY_OF_WEEK         HOUR             UCR_PART                 STREET      
    ##  Friday   :60246   Min.   : 0.00             :     0   WASHINGTON ST : 18766  
    ##  Monday   :56618   1st Qu.: 9.00   Other     :  1520   BLUE HILL AVE :  9408  
    ##  Saturday :55610   Median :14.00   Part One  : 76900   BOYLSTON ST   :  9061  
    ##  Sunday   :50061   Mean   :13.12   Part Three:196676   DORCHESTER AVE:  6495  
    ##  Thursday :57800   3rd Qu.:18.00   Part Two  :120607   TREMONT ST    :  6410  
    ##  Tuesday  :57190   Max.   :23.00                       HARRISON AVE  :  6083  
    ##  Wednesday:58178                                       (Other)       :339480  
    ##       Lat             Long                               Location     
    ##  Min.   :42.23   Min.   :-71.18   (42.34862382, -71.08277637):  1671  
    ##  1st Qu.:42.30   1st Qu.:-71.10   (42.36183857, -71.05976489):  1651  
    ##  Median :42.33   Median :-71.08   (42.28482577, -71.09137369):  1462  
    ##  Mean   :42.32   Mean   :-71.08   (42.32866284, -71.08563401):  1337  
    ##  3rd Qu.:42.35   3rd Qu.:-71.06   (42.25621592, -71.12401947):  1239  
    ##  Max.   :42.40   Max.   :-71.00   (42.29755533, -71.05970910):  1153  
    ##                                   (Other)                    :387190

Armazenando o Mapa de Boston no Google Maps

``` r
coord_boston = GetMap(center = c(lat = 42.36025, lon = -71.05829), 
                    destfile = tempfile("boston_map", fileext = ".png"), 
                    zoom = 11, type = 'google-m')
```

Plotando o Mapa da Cidade

``` r
boston_map = PlotOnStaticMap(coord_boston)
```

![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/Plotando%20o%20Mapa%20da%20Cidade-1.png)

Ocorrências dos Crimes Distribuidos no Mapa

``` r
boston_map = PlotOnStaticMap(coord_boston)
crime_occ_map = PlotOnStaticMap(boston_map, lon = crime_preproccess$Long, 
                lat = crime_preproccess$Lat, destfile = 'crime_occ_map.png',
                FUN = points, col = "red", add = T)
```

![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/Ocorrências%20dos%20Crimes%20Distribuidos%20no%20Mapa-1.png)

Colocando o Contorno dos Bairros

``` r
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
                lat = crime_preproccess$Lat, destfile = 'crime_occ_map.png',
                FUN = points, col = "red", add = T)
PlotPolysOnStaticMap(MyMap = crime_occ_map, polys = shp_nB, add = T)
```

![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/Colocando%20o%20Contorno%20dos%20Bairros-1.png)

Crimes Existentes

``` r
levels.default(sort(crime_preproccess[["OFFENSE_CODE_GROUP"]]))
```

    ##  [1] "Aggravated Assault"                       
    ##  [2] "Aircraft"                                 
    ##  [3] "Arson"                                    
    ##  [4] "Assembly or Gathering Violations"         
    ##  [5] "Auto Theft"                               
    ##  [6] "Auto Theft Recovery"                      
    ##  [7] "Ballistics"                               
    ##  [8] "Biological Threat"                        
    ##  [9] "Bomb Hoax"                                
    ## [10] "Burglary - No Property Taken"             
    ## [11] "Commercial Burglary"                      
    ## [12] "Confidence Games"                         
    ## [13] "Counterfeiting"                           
    ## [14] "Criminal Harassment"                      
    ## [15] "Disorderly Conduct"                       
    ## [16] "Drug Violation"                           
    ## [17] "Embezzlement"                             
    ## [18] "Evading Fare"                             
    ## [19] "Explosives"                               
    ## [20] "Fire Related Reports"                     
    ## [21] "Firearm Discovery"                        
    ## [22] "Firearm Violations"                       
    ## [23] "Fraud"                                    
    ## [24] "Gambling"                                 
    ## [25] "Harassment"                               
    ## [26] "Harbor Related Incidents"                 
    ## [27] "HOME INVASION"                            
    ## [28] "Homicide"                                 
    ## [29] "HUMAN TRAFFICKING"                        
    ## [30] "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE"
    ## [31] "Investigate Person"                       
    ## [32] "INVESTIGATE PERSON"                       
    ## [33] "Investigate Property"                     
    ## [34] "Landlord/Tenant Disputes"                 
    ## [35] "Larceny"                                  
    ## [36] "Larceny From Motor Vehicle"               
    ## [37] "License Plate Related Incidents"          
    ## [38] "License Violation"                        
    ## [39] "Liquor Violation"                         
    ## [40] "Manslaughter"                             
    ## [41] "Medical Assistance"                       
    ## [42] "Missing Person Located"                   
    ## [43] "Missing Person Reported"                  
    ## [44] "Motor Vehicle Accident Response"          
    ## [45] "Offenses Against Child / Family"          
    ## [46] "Operating Under the Influence"            
    ## [47] "Other"                                    
    ## [48] "Other Burglary"                           
    ## [49] "Phone Call Complaints"                    
    ## [50] "Police Service Incidents"                 
    ## [51] "Prisoner Related Incidents"               
    ## [52] "Property Found"                           
    ## [53] "Property Lost"                            
    ## [54] "Property Related Damage"                  
    ## [55] "Prostitution"                             
    ## [56] "Recovered Stolen Property"                
    ## [57] "Residential Burglary"                     
    ## [58] "Restraining Order Violations"             
    ## [59] "Robbery"                                  
    ## [60] "Search Warrants"                          
    ## [61] "Service"                                  
    ## [62] "Simple Assault"                           
    ## [63] "Towed"                                    
    ## [64] "Vandalism"                                
    ## [65] "Verbal Disputes"                          
    ## [66] "Violations"                               
    ## [67] "Warrant Arrests"

Encoding dos Crimes

``` r
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}


# Encoding Ordinal da feature 'OFFENSE_CODE_GROUP':

crime_preproccess[["OFFENSE_SORT_ENCODED"]] = 
  encode_ordinal(crime_preproccess[["OFFENSE_CODE_GROUP"]],
    order = levels.default(sort(crime_preproccess[["OFFENSE_CODE_GROUP"]])))
```

Barplot - Crimes

``` r
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
```

![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/Barplot%20-%20Crimes-1.png)

Pieplot - Envolvimento c/ Arma de Fogo

``` r
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
```

![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/Pieplot%20-%20Envolvimento%20c/%20Arma%20de%20Fogo-1.png)

PreProcess - Time Series

``` r
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
```

Time Series - Ocorrencias por Mes/Ano

``` r
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
       subtitle = "Boston-MA: Junho/2015 a Setembro/2019 (Fonte: data.boston.gov)") +
  theme_minimal()
```

    ## Warning: Removed 1 rows containing missing values (geom_path).

    ## Warning: Removed 1 rows containing missing values (geom_text).

![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/Time%20Series%20-%20Ocorrencias%20por%20Mes/Ano-1.png)

``` r
      ### Como a pesquisa termina no início de Outubro/2019, nao foi posta no grafico por não apresentar dados relativos do mes todo.
```

Time Series - Ocorrencias por Hora/Ano

``` r
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
```

![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/Time%20Series%20-%20Ocorrencias%20por%20Hora/Ano-1.png)

Heat Map - PreProccess

``` r
# Utilizando a funcao 'fortify' para transformar o shapefile em 
# dataframe e pegar as coordenadas dos poligonos:
  
shp_nB.fort = shp_nB
shp_nB.fort = fortify(shp_nB.fort)
```

\[1\] Heat Map - Crimes em Geral

``` r
ggplot(shp_nB.fort, aes(x = long, y = lat, group = group)) +
    geom_polygon(colour = 'black', fill = 'white') +
    stat_density2d(data = crime_preproccess, aes(x = Long, y = Lat, 
                                                 fill = ..level..), 
                   alpha = 0.5, inherit.aes = FALSE, geom = "polygon") +
    scale_fill_distiller(palette = "Spectral") +
    theme_minimal()
```

![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/%5B1%5D%20Heat%20Map%20-%20Crimes%20em%20Geral-1.png)

\[2\] Heat Map - Crimes por Ano ![](p3_DA_%5BNB%5DbostonIncidents_files/figure-markdown_github/%5B2%5D%20Heat%20Map%20-%20Crimes%20por%20Ano-1.png)
