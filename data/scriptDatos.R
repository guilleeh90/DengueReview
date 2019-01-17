# Librerias
library(ggplot2)

#############################################################
# Carga de datos
eagypti <- read.csv('aegypti_albopictus.csv', sep = ',')

# Numero de casos por pais
numCasos1 <- function(pais, datos = eagypti) {
  return(dim(datos[datos$COUNTRY == pais, ])[1])
}

paises <- unique(as.character(eagypti$COUNTRY))
sapply(paises, numCasos1)

# Ploteo de casos por anio
plotCases1 <- function(pais, datos = eagypti) {
  
  datos <- datos[datos$COUNTRY == pais, ]
  plot(table(datos$YEAR), type = 'l',
       xlab = 'Casos de mosquito',
       ylab = 'Anio',
       main = pais)
  
}

plotCases1('Brazil')
plotCases1('United States of America')

#############################################################
# Carga de datos
gbif <- read.csv('0025895-181108115102211.csv', sep = '\t')

# Numero de casos por pais
numCasos2 <- function(codePais, datos = gbif) {
  return(dim(datos[datos$countryCode == codePais, ])[1])
}

codePaises <- unique(as.character(gbif$countryCode))
sapply(codePaises, numCasos2)

# Ploteo de casos por anio
plotCases2 <- function(codePais, datos = gbif) {
  
  datos <- datos[datos$countryCode == codePais, ]
  plot(table(datos$year), type = 'l',
       xlab = 'Casos de mosquito',
       ylab = 'Anio',
       main = codePais)
  
}

plotCases2('BR') # Brazil
plotCases2('US') # United States
#############################################################

# Carga de datos
casesBrazil <- read.csv('W_Table_data.csv', sep = ',')
casesUS <- read.csv('W_Table_data_2.csv', sep = ',')

# Plot de casos
plotDengue <- function(datos, elements = c('D', 'DG', 'M')) {
  
  p <- ggplot()
  
  if('D' %in% elements) {
    p <- p +
      geom_line(aes(x = datos$Year, y = datos$Dengue,
                    colour = 'Dengue'))
  }
  
  if('DG' %in% elements) {
    p <- p + 
      geom_line(aes(x = datos$Year, y = datos$Dengue.Grave,
                    colour = 'Dengue Grave'))
  }
  
  if('M' %in% elements) {
    p <- p +
      geom_line(aes(x = datos$Year, y = datos$Muertes,
                    colour = 'Muertes'))
  }
  
  print(p)
  
}

plotDengue(casesBrazil, elements = c('M'))
plotDengue(casesUS, elements = c('M'))
#############################################################



