#Descriptivos utilizados para hacer el seguimiento del sindicalismo 2018 hasta el 2024

## p3i por rama_est1 (tarda 23 min. aprox)

tbls <- list() ## Sólo correr una vez

library("dplyr")
library("survey")
library("gtsummary")

theme_gtsummary_language(
  language = "es",
  decimal.mark = NULL,
  big.mark = NULL,
  iqr.sep = NULL,
  ci.sep = NULL,
  set_theme = TRUE
)

theme_gtsummary_compact(set_theme = TRUE, font_size = 8)

##
local({
library("dplyr")
tbl <- des.1 %>%
tbl_strata(
strata = "year",
.tbl_fun =
~ .x %>%
tbl_svysummary(
  by = rama_est1,
  statistic = list(p3i ~ "{n} ({p})"),
  #percent = "row",
  label= p3i~rk.get.label(des.1$variables$p3i), #Las etiquetas desde el survey design original.
  include = "p3i")
)
rk.print(tbl)
.GlobalEnv$tbls$p3i_rama_est1 <- tbl
})


local({
## Computar
setwd()
})

# rama_est1 por población sindicalizada (p3i)
# Dura aproximadamente 3 min con gpu dedicada

local({
library(survey)
library(dplyr)
library(splitstackshape)

# Con el objeto 'des.1' de clase survey.design2 y la variable 'p3i' categórica

# Calcular el total ponderado por año y ent.z
est <- svyby(~rama_est1, ~year + p3i, des.1, svytotal, na.rm=TRUE)

columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
# Eliminamos las columnas especificadas
est <- est[, -which(names(est) %in% columnas_a_eliminar)]

library(tidyr)
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
library(tidyr)
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")

#Use
piv2 <- cSplit(piv2, "variable", ".")
#piv3 <- bind_cols(piv1,piv2)

piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "p3i" = "p3i","respuesta" = "variable_2"))

columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado","variable_1")

# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]
local({
## Computar
input <- piv3[["respuesta"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (piv3[["respuesta"]])
recoded[input == "rama_est1Primario"] <- "Primario"
recoded[input == "rama_est1Secundario"] <- "Secundario"
recoded[input == "rama_est1Terciario"] <- "Terciario"
recoded[input == "rama_est1No especificado"] <- "No especificado"
piv3[["respuesta"]] <- as.factor (recoded)
## Imprimir el resultado
rk.header ("Re-codificar datos categóricos", parameters=list("Variable de entrada"="piv3[[\"respuesta\"]]",
	"Variable de salida"="piv3[[\"respuesta\"]]",
	"Número de diferencias después de re-codificar"=sum (piv3[["respuesta"]] != piv3[["respuesta"]], na.rm=TRUE) + sum (is.na (piv3[["respuesta"]]) != is.na (piv3[["respuesta"]]))))

.GlobalEnv$estim$rama_est1_y_p3i  <-piv3

})

# Crear el gráfico
local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
piv3 <- estim$rama_est1_y_p3i
# Crear el gráfico de línea con facetas por respuesta y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = p3i, group = p3i)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~respuesta, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(des.1[["variables"]][["rama_est1"]]),
  x = "Año", y = "Total Ponderado", color = "Nivel de respuesta",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

### Crear MAPA de la república Mexicana por tres zonas

#Algoritmo de descarga de los datos .zip
url<-"http://internet.contenidos.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463674658_s.zip"
if(!file.exists("areas_geoestadisticas_estatales.zip"){
  download.file(url,"areas_geoestadisticas_estatales.zip",mode="wb")
}
if(!file.exists("conjunto_de_datos/areas_geoestadisticas_basicas_rurales.dbf")){
  unzip("areas_geoestadisticas_estatales.zip")
}

#Cargar el paquete terra
library("terra")
#Cargar sólo los polígonos de los estados
shape_estados <- vect("conjunto de datos", layer="01_32_ent")

#Cargar librerías para trabajar con los mapas
library("ggplot2")
library("tidyterra")

# Crear el gráfico para hacer recodificación y colocar colores discretos
 mex  <- shape_estados %>%
   autoplot()
   print(mex)

# Recodiciación de nombres por ZONAS en mex[["data"]][["NOMGEO"]]
local({
## Computar
input <- mex[["data"]][["NOMGEO"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles, se movió ,"Veracruz de Ignacio de la Llave" a sur-este
recoded <- as.character (mex[["data"]][["NOMGEO"]])
recoded[input %in% c("Baja California","Baja California Sur","Sonora","Coahuila de Zaragoza","Chihuahua","Nuevo León","Tamaulipas","Sinaloa","Durango","Zacatecas","Aguascalientes","Nayarit")] <- "Norte"
recoded[input %in% c("Ciudad de México","México","Querétaro","Jalisco","Colima","Michoacán de Ocampo","San Luis Potosí","Guanajuato","Hidalgo")] <- "Centro"
recoded[input %in% c("Yucatán","Quintana Roo","Tabasco","Campeche","Guerrero","Chiapas","Morelos", "Oaxaca","Puebla","Tlaxcala","Veracruz de Ignacio de la Llave")] <- "Sur-Este"
.GlobalEnv$mex[["data"]][["ZONA"]] <- as.factor (recoded)
## Imprimir el resultado
rk.header ("Re-codificar datos categóricos", parameters=list("Variable de entrada"="mex[[\"data\"]][[\"NOM_ENT\"]]",
	"Variable de salida"="mex[[\"data\"]][[\"ZONA\"]]",
	"Número de diferencias después de re-codificar"=sum (mex[["data"]][["NOMGEO"]] != mex[["data"]][["ZONA"]], na.rm=TRUE) + sum (is.na (mex[["data"]][["NOMGEO"]]) != is.na (mex[["data"]][["ZONA"]]))))
})

### Gráfico con todos los elementos
local({
values <- c('wheat3','tomato1','green3')
plot <- mex + aes(fill= ZONA) +
theme_minimal() + # ¿theme_minimal() o theme_void() ?
            theme(plot.title = element_text(size=22),
                  legend.key.size = unit(0.5, "cm"),
                  legend.position = "inside",
                  legend.position.inside = c(0.8, 0.7)) +
                  scale_fill_manual(values=values) +
                scale_color_manual(values=c("black")) +
                  guides(color = FALSE) +
             labs(title = "Agrupación por Zonas de Estudio",
             #fill = "ZONA", # En este caso es el mismo nombre de la variable
             caption = "Fuente: Datos Geográficos 2018 (INEGI),\n Zonas de análisis")
rk.graph.on("PNG",1344, 960, pointsize=10.0, res=125, bg = "transparent")
try ({
	print(plot)
})
rk.graph.off()
})

### Filtrar por PEA

nrow(des.1[["variables"]])

local({
## Computar
.GlobalEnv$pea <- subset(des.1,subset=clase1%in%"Población económicamente activa")
for(i in 1:length(names(pea$variables))){
	 attr(.GlobalEnv$pea$variables[[names(pea$variables)[i]]],".rk.meta") = attr(des.1$variables[[names(pea$variables)[i]]],".rk.meta")
} })

nrow(pea[["variables"]])

Se crea la lista para albergar estimaciones
estim <- list()

### Proporción de la población sindicalizada sobre la PEA

pea.ps <- data.frame(cbind(
"PEA"=c(53936667,55578352,57014967,55385133,58085314,60089308,60663120),
"Población.Sindicalizada"=c(4336688,4525422,4677481,4753995,4869045,5099794,5237925),
"A.o.1t"=c(2018,2019,2020,2021,2022,2023,2024),
"Proporción.PS.PEA"=c(0.0804033367504892,0.0814241847257364,0.0820395283224491,0.0858352186316859,0.0838257498272283,0.0848702401432215,0.0863444709075300)
))
data.frame(cbind(
"PEA"=c(53936667,55578352,57014967,55385133,58085314,60089308,60663120),
"Población Sindicalizada"=c(4336688,4525422,4677481,4753995,4869045,5099794,5237925),
"Año.1t"=c(2018,2019,2020,2021,2022,2023,2024),
"Proporción PS/PEA"=c(0.080,0.081,0.082,0.086,0.084,0.085,0.086)
))

Etiqueta <- c("PEA","Población Sindicalizada","1t Año","Proporción PS/PEA")

local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(pea.ps, aes(x = A.o.1t, y = Proporción.PS.PEA)) +
  geom_line(linewidth=2,color = "deeppink4") +
  geom_point(color = "firebrick4",
  #fill = "firebrick3",
  size=8)+
 #scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= "Proporción de la población sindicalizada sobre la PEA, 1t 2018-2024",
  x = "Año",
  y = "Proporción")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## Por Rama rama_est1

library(forcats)
pea[["variables"]][["rama_est1"]] <- fct_drop(pea[["variables"]][["rama_est1"]])
Población ocupada

local({
library(survey)
library(dplyr)
library(splitstackshape)

# Con el objeto 'pea' de clase survey.design2 y la variable 'p3i' categórica
# Calcular el total ponderado por año y ent.z
est <- svyby(~rama_est1, ~year + p3i, pea, svytotal, na.rm=TRUE)

columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
# Eliminamos las columnas especificadas
est <- est[, -which(names(est) %in% columnas_a_eliminar)]

library(tidyr)
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
library(tidyr)
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")

#Use
piv2 <- cSplit(piv2, "variable", ".")
#piv3 <- bind_cols(piv1,piv2)

piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "p3i" = "p3i","respuesta" = "variable_2"))

columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado","variable_1")

# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]

## Computar
input <- piv3[["respuesta"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (piv3[["respuesta"]])
recoded[input == "rama_est1Primario"] <- "Primario"
recoded[input == "rama_est1Secundario"] <- "Secundario"
recoded[input == "rama_est1Terciario"] <- "Terciario"
recoded[input == "rama_est1No especificado"] <- "No especificado"
piv3[["respuesta"]] <- as.factor (recoded)
## Imprimir el resultado
rk.header ("Re-codificar datos categóricos", parameters=list("Variable de entrada"="piv3[[\"respuesta\"]]",
	"Variable de salida"="piv3[[\"respuesta\"]]",
	"Número de diferencias después de re-codificar"=sum (piv3[["respuesta"]] != piv3[["respuesta"]], na.rm=TRUE) + sum (is.na (piv3[["respuesta"]]) != is.na (piv3[["respuesta"]]))))
.GlobalEnv$estim$rama_est1_y_p3i  <-piv3
})

# Crear el gráfico
local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
piv3 <- estim$rama_est1_y_p3i
# Crear el gráfico de línea con facetas por respuesta y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = p3i, group = p3i)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~respuesta, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(pea[["variables"]][["rama_est1"]]),
  x = "Año", y = "Total Ponderado", color = "Nivel de respuesta",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## p3i por rama_est1 (tarda 23 min. aprox)

# tbls <- list()

library("dplyr")
library("survey")
library("gtsummary")

theme_gtsummary_language(
  language = "es",
  decimal.mark = NULL,
  big.mark = NULL,
  iqr.sep = NULL,
  ci.sep = NULL,
  set_theme = TRUE
)

theme_gtsummary_compact(set_theme = TRUE, font_size = 8)

##Tabla de PEA por año, rama y p3i

##
local({
library("dplyr")
tbl <- pea %>%
tbl_strata(
strata = "year",
.tbl_fun =
~ .x %>%
tbl_svysummary(
  by = rama_est1,
  statistic = list(p3i ~ "{n} ({p})"),
  digits = list(p3i ~ c(0, 1)),
  #percent = "row",
  label= p3i~rk.get.label(pea$variables$p3i), #Las etiquetas desde el survey design original.
  include = "p3i")
)
rk.print(tbl)
.GlobalEnv$tbls$p3i_rama_est1 <- tbl
})

## Filtrar p3i con "Sí"
local({
## Computar
.GlobalEnv$p3i_si  <- subset(pea,subset=p3i%in%"Sí")
for(i in 1:length(names(p3i_si$variables))){
	 attr(.GlobalEnv$p3i_si$variables[[names(p3i_si$variables)[i]]],".rk.meta") = attr(pea$variables[[names(p3i_si$variables)[i]]],".rk.meta")
} })

nrow(p3i_si[["variables"]])

## SVYby SEX

local({
library(survey)
library(dplyr)
library(splitstackshape)

# Con el objeto 'pea' de clase survey.design2 y la variable 'p3i' categórica

# Calcular el total ponderado por año y ent.z
est <- svyby(~sex, ~year , p3i_si, svytotal, na.rm=TRUE)

# columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
# # Eliminamos las columnas especificadas
# est <- est[, -which(names(est) %in% columnas_a_eliminar)]

library(tidyr)
piv1<- pivot_longer(est, 2:3, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
library(tidyr)
piv2<- pivot_longer(est, 4:5, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")

#Use
piv2 <- cSplit(piv2, "variable", ".")
#piv3 <- bind_cols(piv1,piv2)

piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year","respuesta" = "variable_2"))

columnas_a_eliminar <- c("sexHombre","sexMujer","se.sexHombre",	"se.sexMujer","variable_1")

# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]

## Computar
input <- piv3[["respuesta"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (piv3[["respuesta"]])
recoded[input == "sexHombre"] <- "Hombre"
recoded[input == "sexMujer"] <- "Mujer"
piv3[["respuesta"]] <- as.factor (recoded)
## Imprimir el resultado
rk.header ("Re-codificar datos categóricos", parameters=list("Variable de entrada"="piv3[[\"respuesta\"]]",
	"Variable de salida"="piv3[[\"respuesta\"]]",
	"Número de diferencias después de re-codificar"=sum (piv3[["respuesta"]] != piv3[["respuesta"]], na.rm=TRUE) + sum (is.na (piv3[["respuesta"]]) != is.na (piv3[["respuesta"]]))))

.GlobalEnv$estim$sex_p3i_si  <-piv3

})

library(forcats)
estim[["sex_p3i_si"]][["respuesta"]] <- fct_rev(estim[["sex_p3i_si"]][["respuesta"]] )

# Crear el gráfico
local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
piv3 <- estim$sex_p3i_si
# Crear el gráfico de línea con facetas por respuesta y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  #facet_wrap(~respuesta, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(p3i_si[["variables"]][["sex"]]),
  x = "Año", y = "Total Ponderado", color = "Nivel de sex",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## p3i por rama_est1 (tarda 23 min. aprox)

# tbls <- list()

library("dplyr")
library("survey")
library("gtsummary")


theme_gtsummary_language(
  language = "es",
  decimal.mark = NULL,
  big.mark = NULL,
  iqr.sep = NULL,
  ci.sep = NULL,
  set_theme = TRUE
)

theme_gtsummary_compact(set_theme = TRUE, font_size = 8)

local({
library("dplyr")
tbl <- tbl_svysummary(p3i_si,
  by = year,
  statistic = list(sex ~ "{n} ({p})"),
  digits = list(sex ~ c(0, 1)),
  #percent = "row",
  label= sex~rk.get.label(p3i_si$variables$sex), #Las etiquetas desde el survey design original.
  include = "sex")
rk.print(tbl)
.GlobalEnv$tbls$sex_p3i_si <- tbl
})

local({
library("dplyr")
tbl <- p3i_si %>%
tbl_strata(
strata = "year",
.tbl_fun =
~ .x %>%
tbl_svysummary(
  by = sex,
  statistic = list(rama_est1 ~ "{n} ({p})"),
  digits = list(rama_est1 ~ c(0, 1)),
  #percent = "row",
  label= rama_est1~rk.get.label(p3i_si$variables$rama_est1), #Las etiquetas desde el survey design original.
  include = "rama_est1")
)
rk.print(tbl)
.GlobalEnv$tbls$p3i_rama_est1 <- tbl
})

local({
library(survey)
library(dplyr)
library(splitstackshape)

# Con el objeto 'pea' de clase survey.design2 y la variable 'p3i' categórica


# Calcular el total ponderado por año y ent.z
est <- svyby(~sex, ~year+rama_est1 , p3i_si, svytotal, na.rm=TRUE)

# columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
# # Eliminamos las columnas especificadas
# est <- est[, -which(names(est) %in% columnas_a_eliminar)]

library(tidyr)
piv1<- pivot_longer(est, 3:4, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
library(tidyr)
piv2<- pivot_longer(est, 5:6, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")

#Use
piv2 <- cSplit(piv2, "variable", ".")
#piv3 <- bind_cols(piv1,piv2)

piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year","rama_est1" = "rama_est1","respuesta" = "variable_2"))

columnas_a_eliminar <- c("sexHombre","sexMujer","se.sexHombre",	"se.sexMujer","variable_1")

# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]

## Computar
input <- piv3[["respuesta"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (piv3[["respuesta"]])
recoded[input == "sexHombre"] <- "Hombre"
recoded[input == "sexMujer"] <- "Mujer"
piv3[["respuesta"]] <- as.factor (recoded)
## Imprimir el resultado
rk.header ("Re-codificar datos categóricos", parameters=list("Variable de entrada"="piv3[[\"respuesta\"]]",
	"Variable de salida"="piv3[[\"respuesta\"]]",
	"Número de diferencias después de re-codificar"=sum (piv3[["respuesta"]] != piv3[["respuesta"]], na.rm=TRUE) + sum (is.na (piv3[["respuesta"]]) != is.na (piv3[["respuesta"]]))))

.GlobalEnv$estim$sex_p3i_si_est1  <-piv3

})

library(forcats)
estim[["sex_p3i_si_est1"]][["respuesta"]] <- fct_rev(estim[["sex_p3i_si_est1"]][["respuesta"]] )

# Crear el gráfico
local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
piv3 <- estim$sex_p3i_si_est1
# Crear el gráfico de línea con facetas por respuesta y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~rama_est1, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(p3i_si[["variables"]][["sex"]]),
  x = "Año", y = "Total Ponderado", color = "Nivel de sex",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## EDAD


local({
library("dplyr")
tbl <- p3i_si %>%
tbl_svysummary(
  by = year,
  statistic = list(eda ~ "{mean}±{sd} ({p25}, {median}, {p75})"),
  #percent = "row",
  label= eda~rk.get.label(p3i_si$variables$eda), #Las etiquetas desde el survey design original.
  include = "eda")
rk.print(tbl)
.GlobalEnv$tbls$eda_p3i_si <- tbl
})

local({
library("dplyr")
tbl <- p3i_si %>%
tbl_strata(
strata = "year",
.tbl_fun =
~ .x %>%
tbl_svysummary(
  by = sex,
  statistic = list(eda ~ "{mean}±{sd} ({p25}, {median}, {p75})"),
  digits = list(eda ~ c(1,0,0,0,0)),
  #percent = "row",
  label= eda~rk.get.label(p3i_si$variables$eda), #Las etiquetas desde el survey design original.
  include = "eda")
)
rk.print(tbl)
.GlobalEnv$tbls$eda_sex_p3i_si <- tbl
})

# Histograma por edad

    local({
 p3i_si_2018  <-  subset(subset= year%in%"2018")
    p <- svyhist(~eda, p3i_si_2018,
    breaks = "Sturges",
    xlab="Edad",
    ylab="Frecuencia",
    main="Distribución de Edades Ponderadas",
    col="lightblue",
    probability=FALSE)
    rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
        try({
            print(p)
    })
    rk.graph.off()
    })
###
local({
library(ggplot2)
library(questionr)
    p <- p3i_si %>%
subset(subset= year%in%"2018") %>%
    ggsurvey() +
    aes(x = eda) +
    geom_histogram(
            aes(y = after_stat(count)),
            binwidth = 5,
            fill = "lightblue",
            color = "black"
        ) +
        labs(
            x = "Edad",
            y = "Frecuencia",
            title = "Distribución de Edades en la Muestra en 1t 2018"
             )
    rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
        try({
            print(p)
        })
    rk.graph.off()
    })

local({
library(ggplot2)
library(questionr)
    p <- p3i_si %>%
subset(subset= year%in%"2021") %>%
    ggsurvey() +
    aes(x = eda) +
    geom_histogram(
            aes(y = after_stat(count)),
            binwidth = 5,
            fill = "lightblue",
            color = "black"
        ) +
        labs(
            x = "Edad",
            y = "Frecuencia",
            title = "Distribución de Edades en la Muestra en 1t 2021"
             )
    rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
        try({
            print(p)
        })
    rk.graph.off()
    })


local({
library(ggplot2)
library(questionr)
p <- p3i_si %>%
subset(subset= year%in%"2024") %>%
    ggsurvey() +
    aes(x = eda) +
    geom_histogram(
            aes(y = after_stat(count)),
            binwidth = 5,
            fill = "lightblue",
            color = "black"
        ) +
        labs(
            x = "Edad",
            y = "Frecuencia",
            title = "Distribución de Edades en la Muestra en 1t 2024"
             )
    rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
        try({
            print(p)
        })
    rk.graph.off()
    })

## Crear estadísticos con la media de la edad

local({
library(survey)
#library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(forcats)

# Con el objeto 'des.1' de clase survey.design2 y la variable 'p3i' categórica

# Calcular el total ponderado por año y ent.z
est <- svyby(~eda, ~year + sex, des.1 , svymean, na.rm=TRUE)
est$sex <- fct_rev(est$sex)
# Crear el gráfico
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(est, aes(x = year, y = eda, color = sex, group = sex)) +
  geom_line() +
  geom_errorbar(aes(ymin = eda - 2*se, ymax = eda + 2*se), width = 0.2) +
  #facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(des.1[["variables"]][["sex"]]),
  x = "Año", y = "Media estimada", color = "Nivel de sex",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

local({
library(survey)
#library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(forcats)

# Con el objeto 'des.1' de clase survey.design2 y la variable 'p3i' categórica

# Calcular el total ponderado por año y ent.z
est3 <- svyby(~eda, ~year + sex, pea , svymean, na.rm=TRUE)
est3$sex <- fct_rev(est3$sex)
# Crear el gráfico
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(est3, aes(x = year, y = eda, color = sex, group = sex)) +
  geom_line() +
  geom_errorbar(aes(ymin = eda - 2*se, ymax = eda + 2*se), width = 0.2) +
  #facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(des.1[["variables"]][["sex"]]),
  x = "Año", y = "Media estimada", color = "Nivel de sex",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

local({
library(survey)
#library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(forcats)

# Con el objeto 'des.1' de clase survey.design2 y la variable 'p3i' categórica

# Calcular el total ponderado por año y ent.z
library("survey")
est2 <- subset(pea, clase2 %in% "Población ocupada")
est3 <- svyby(~eda, ~year + sex, est2 , svymean, na.rm=TRUE)
library("forcats")
est3$sex <- fct_rev(est3$sex)
# Crear el gráfico
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(est3, aes(x = year, y = eda, color = sex, group = sex)) +
  geom_line() +
  geom_errorbar(aes(ymin = eda - 2*se, ymax = eda + 2*se), width = 0.2) +
  #facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
 # labs(title= rk.get.label(pea[["variables"]][["sex"]]),
 labs(title= "Media de edad de la población ocupada por sexo",
  x = "Año",
  y = "Media estimada",
  color = "Nivel de sex",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

library("survey")
library("dplyr")
library("gtsummary")

theme_gtsummary_language(
  language = "es",
  decimal.mark = NULL,
  big.mark = NULL,
  iqr.sep = NULL,
  ci.sep = NULL,
  set_theme = TRUE
)

theme_gtsummary_compact(set_theme = TRUE, font_size = 8)

local({
tbl <- p3i_si %>%
tbl_svysummary(
  by = year,
  statistic = list(ambito2 ~ "{n} ({p})"),
  percent = "column",
  label= ambito2~rk.get.label(p3i_si$variables$ambito2), #Las etiquetas desde el survey design original.
  include = "ambito2")
rk.print(tbl)
.GlobalEnv$tbls$p3i_ambito2 <- tbl
})

local({
tbl <- des_1t_18_24 %>%
tbl_strata(
strata = "year",
.tbl_fun =
~ .x %>%
tbl_svysummary(
  by = sex,
  statistic = list(ambito2 ~ "{n} ({p})"),
  percent = "row",
  label= ambito2~rk.get.label(des_1t_18_24$variables$ambito2), #Las etiquetas desde el survey design original.
  include = "ambito2")
)
rk.print(tbl)
.GlobalEnv$tbls$ambito2_sx <- tbl
})

####
estim_tam <- list()

local({
library(survey)
library(dplyr)
  library(splitstackshape)
est <- svyby(~rama_est1, ~year + ent.z, p3i_si , svytotal, na.rm=TRUE)


# Eliminamos las columnas especificadas
columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
est <- est[, -which(names(est) %in% columnas_a_eliminar)]

library(tidyr)
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
library(tidyr)
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")

#Use
piv2 <- cSplit(piv2, "variable", ".")
#piv3 <- bind_cols(piv1,piv2)

piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "ent.z" = "ent.z","respuesta" = "variable_2"))

columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","variable_1","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado")

# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]

## Computar
input <- piv3[["respuesta"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (piv3[["respuesta"]])
recoded[input == "rama_est1Primario"] <- "Primario"
recoded[input == "rama_est1Secundario"] <- "Secundario"
recoded[input == "rama_est1Terciario"] <- "Terciario"
recoded[input == "rama_est1No especificado"] <- "No especificado"
piv3[["respuesta"]] <- as.factor (recoded)
## Imprimir el resultado
#rk.header ("Re-codificar datos categóricos", parameters=list("Variable de #entrada"="piv3[[\"respuesta\"]]",
#	"Variable de salida"="piv3[[\"respuesta\"]]",
#	"Número de diferencias después de re-codificar"=sum (piv3[["respuesta"]] != piv3[["respuesta"]], na.rm=TRUE) + sum (is.na (piv3[["respuesta"]]) != is.na (piv3[["respuesta"]]))))
.GlobalEnv$estim_tam$rama_est1_ent.z_y  <-piv3
})

  # Crear el gráfico
local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
piv3 <- estim_tam$rama_est1_ent.z_y
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(p3i_si[["variables"]][["rama_est1"]]),
  x = "Año", y = "Total Ponderado", color = "Nivel de rama_est1",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})


#### Hombre


local({
library(survey)
library(dplyr)
  library(splitstackshape)

hombres <- subset(p3i_si, subset=sex%in% "Hombre")

est <- svyby(~rama_est1, ~year + ent.z, hombres , svytotal, na.rm=TRUE)


# Eliminamos las columnas especificadas
columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
est <- est[, -which(names(est) %in% columnas_a_eliminar)]

library(tidyr)
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
library(tidyr)
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")

#Use
piv2 <- cSplit(piv2, "variable", ".")
#piv3 <- bind_cols(piv1,piv2)

piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "ent.z" = "ent.z","respuesta" = "variable_2"))

columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","variable_1","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado")

# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]

## Computar
input <- piv3[["respuesta"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (piv3[["respuesta"]])
recoded[input == "rama_est1Primario"] <- "Primario"
recoded[input == "rama_est1Secundario"] <- "Secundario"
recoded[input == "rama_est1Terciario"] <- "Terciario"
recoded[input == "rama_est1No especificado"] <- "No especificado"
piv3[["respuesta"]] <- as.factor (recoded)
## Imprimir el resultado
#rk.header ("Re-codificar datos categóricos", parameters=list("Variable de #entrada"="piv3[[\"respuesta\"]]",
#	"Variable de salida"="piv3[[\"respuesta\"]]",
#	"Número de diferencias después de re-codificar"=sum (piv3[["respuesta"]] != piv3[["respuesta"]], na.rm=TRUE) + sum (is.na (piv3[["respuesta"]]) != is.na (piv3[["respuesta"]]))))

.GlobalEnv$estim_tam$rama_est1_ent.z_y_hm  <-piv3

})
  # Crear el gráfico
local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
piv3 <- estim_tam$rama_est1_ent.z_y_hm
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(p3i_si[["variables"]][["rama_est1"]]),
  x = "Año", y = "Total Ponderado", color = "Nivel de rama_est1",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## Mujer

local({
library(survey)
library(dplyr)
  library(splitstackshape)

mujeres <- subset(p3i_si, subset=sex%in% "Mujer")

est <- svyby(~rama_est1, ~year + ent.z, mujeres , svytotal, na.rm=TRUE)


# Eliminamos las columnas especificadas
columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
est <- est[, -which(names(est) %in% columnas_a_eliminar)]

library(tidyr)
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
library(tidyr)
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")

#Use
piv2 <- cSplit(piv2, "variable", ".")
#piv3 <- bind_cols(piv1,piv2)

piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "ent.z" = "ent.z","respuesta" = "variable_2"))

columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","variable_1","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado")

# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]

## Computar
input <- piv3[["respuesta"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (piv3[["respuesta"]])
recoded[input == "rama_est1Primario"] <- "Primario"
recoded[input == "rama_est1Secundario"] <- "Secundario"
recoded[input == "rama_est1Terciario"] <- "Terciario"
recoded[input == "rama_est1No especificado"] <- "No especificado"
piv3[["respuesta"]] <- as.factor (recoded)
## Imprimir el resultado
#rk.header ("Re-codificar datos categóricos", parameters=list("Variable de #entrada"="piv3[[\"respuesta\"]]",
#	"Variable de salida"="piv3[[\"respuesta\"]]",
#	"Número de diferencias después de re-codificar"=sum (piv3[["respuesta"]] != piv3[["respuesta"]], na.rm=TRUE) + sum (is.na (piv3[["respuesta"]]) != is.na (piv3[["respuesta"]]))))

.GlobalEnv$estim_tam$rama_est1_ent.z_y_hm  <-piv3

})

# Crear el gráfico
local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
piv3 <- estim_tam$rama_est1_ent.z_y_hm
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(title= rk.get.label(p3i_si[["variables"]][["rama_est1"]]),
  x = "Año", y = "Total Ponderado", color = "Nivel de rama_est1",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

### FIN
