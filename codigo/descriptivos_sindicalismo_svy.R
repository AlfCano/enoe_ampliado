## Estadísticos descriptivos de la adscipción al sindicalismo en la ENOE del 2018 al 2024.

# Revisar si se tienen los paquetes necesarios para ejecutar este script:
require("librarian") # Instala el paquete "librarian" si no está previamente instalado y lo carga en la sesión.
shelf(dplyr, survey, gtsummary, survey, ggplot2, RColorBrewer,stringr, splitstackshape, forcats, tidyr, questionr, terra, tidyterra) # Ejecuta la función shelf del paquete librarian. Revisa si los paquetes ya están instalados, si no lo están los instala y los carga en la sesión.


## Descriptivos utilizados para hacer el seguimiento del sindicalismo 2018 hasta el 2024 en el cuestionario apliado (1t)

#Establecer el directorio de trabajo
local({
## Computar
setwd("/dirigir/al/directorio/con/des1.RData") #Producto del escript "import_svy.R" en el repositorio: "https://github.com/AlfCano/enoe_ampliado"
})

## Prepara el entorno
library("dplyr")
library("survey")
library("gtsummary")

# Configura el idioma del paquete "gtsummary" en español.
theme_gtsummary_language(
  language = "es",
  set_theme = TRUE
)
# Configura el tamaño de las tablas creadas por el paquete "gtsummary" en tamaño compacto y en tamaño 8.
theme_gtsummary_compact(
  set_theme = TRUE,
  font_size = 8
)

tbls <- list() ## Crea la lista en blanco "tbls" en la que se alojarán las tablas.

## Tabla 2. Población Económicamente Activa (PEA) estimada para 1t 2018-2024
# Número de personas en la PEA por sexo
local({
library("dplyr") # Carga el paquete "dplyr".
tbl <- subset(des.1, clase1 %in% "Población económicamente activa") %>% # Crea el objeto "pea", con  "Población económicamente activa" (PEA) en la variable "clase1".
tbl_svysummary(
  by = year,
  statistic = list(sex ~ "{n} ({p})"),
  percent = "column",
  label= sex~rk.get.label(des.1$variables$sex), #Las etiquetas desde el survey design original.
  include = "sex")
rk.print(tbl)
.GlobalEnv$tbls$pea_sex <- tbl
})

## Tabla 3. Distribución de la población sindicalizada por sexo 1t 2018 a 2024
local({
library("dplyr")
tbl <- subset(des.1,
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí")) %>%
tbl_svysummary(
  by = year,
  statistic = list(sex ~ "{n} ({p})"),
  digits = list(sex ~ c(0, 1)),
  percent = "column",
  label= sex~rk.get.label(des.1$variables$sex), #Las etiquetas desde el survey design original.
  include = "sex")
rk.print(tbl)
.GlobalEnv$tbls$sex_p3i_si <- tbl
})

## Gráfica 1. Proporción de la población sindicalizada sobre la PEA, 1t 2018 a 2024
### Proporción de la población sindicalizada sobre la PEA

pea.ps <- data.frame(cbind(
"PEA"=c(53936667,55578352,57014967,55385133,58085314,60089308,60663120),
"Población.Sindicalizada"=c(4336688,4525422,4677481,4753995,4869045,5099794,5237925),
"A.o.1t"=c(2018,2019,2020,2021,2022,2023,2024),
"Proporción.PS.PEA"=c(0.0804033367504892,0.0814241847257364,0.0820395283224491,0.0858352186316859,0.0838257498272283,0.0848702401432215,0.0863444709075300)
))

# "Proporción de la población sindicalizada sobre la PEA, 1t 2018-2024"
  #title= "Proporción de la población sindicalizada sobre la PEA, 1t 2018-2024",

local({
library(survey)
library(ggplot2)
library(RColorBrewer)
library(stringr)
# Crear el gráfico de línea
p <- ggplot(pea.ps, aes(x = A.o.1t, y = Proporción.PS.PEA)) +
  geom_line(linewidth=2,color = "deeppink4") +
  geom_point(color = "firebrick4",
  #fill = "firebrick3",
  size=8)+
  labs(
  x = "Año",
  y = "Proporción")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
         print(p)
    })
  rk.graph.off()
})

## Gráfica 2. Población sindicalizada por sexo, 1t 2018 a 2024
## SVYby SEX

local({
library("survey")
library("dplyr")
library("splitstackshape")
library("forcats")
library("tidyr")
# Con el objeto 'p3i_si' de clase survey.design2 y la variable 'p3i' categórica
# Calcular el total ponderado por año
est <- subset(des.1,
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí")) %>%
svyby(~sex, ~year , ., svytotal, na.rm=TRUE)
piv1<- pivot_longer(est, 2:3,
names_to = "respuesta", values_to = "recuento",
values_drop_na = FALSE,
names_repair="universal")
piv2<- pivot_longer(est, 4:5, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")
piv2 <- cSplit(piv2, "variable", ".")
piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year","respuesta" = "variable_2"))
columnas_a_eliminar <- c("sexHombre","sexMujer","se.sexHombre",	"se.sexMujer","variable_1")
# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]
piv3[["respuesta"]] <- gsub("sex", "", piv3[["respuesta"]] )
piv3[["respuesta"]] <- fct_rev(piv3[["respuesta"]])
.GlobalEnv$estim$sex_p3i_si  <-piv3
})

# Crear el gráfico
  #title= rk.get.label(p3i_si[["variables"]][["sex"]]),
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
piv3 <- estim$sex_p3i_si
# Crear el gráfico de línea con facetas por respuesta y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  #facet_wrap(~respuesta, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(
  x = "Año", y = "Total Ponderado", color = "Nivel de sex",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
         print(p)
    })
   rk.graph.off()
})

## Gráfica 3. Clasificación de la población ocupada según sector de actividades-Totales, 1t 2018 a 2024
# [1] "Clasificación de la población ocuapda según sector de actividades-Totales"
# rama_est1 por población sindicalizada (p3i)
# Tarda aproximadamente 3 min con gpu dedicada

local({
library("survey")
library("dplyr")
library("splitstackshape")
library("tidyr")
# Con el objeto 'des.1' de clase survey.design2  por la variable "rama_est1".
# Calcular el total ponderado por año y la variable 'p3i' categórica
est <- svyby(~rama_est1, ~year + p3i, des.1, svytotal, na.rm=TRUE)
columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
# Eliminamos las columnas especificadas
est <- est[, -which(names(est) %in% columnas_a_eliminar)]
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")
piv2 <- cSplit(piv2, "variable", ".")
piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "p3i" = "p3i","respuesta" = "variable_2"))
columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado","variable_1")
# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]
## Computar
piv3[["respuesta"]] <- gsub("rama_est1", "", piv3[["respuesta"]] )
.GlobalEnv$estim$rama_est1_y_p3i  <- piv3
})

# Crear el gráfico
  #title= rk.get.label(des.1[["variables"]][["rama_est1"]]),
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
piv3 <- estim$rama_est1_y_p3i
# Crear el gráfico de línea con facetas por respuesta y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = p3i, group = p3i)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~respuesta, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(
  x = "Año",
  y = "Total Ponderado",
  color = "Nivel de respuesta",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## Gráfica 4. Clasificación de la población ocupada sindicalizada según sector de actividades-Totales 1t 2018-2024

local({
#Preparar
library("survey")
library("dplyr")
library("splitstackshape")
library("tidyr")
library("forcats")
# Con el objeto 'pea' de clase survey.design2 y la variable 'p3i' categórica
# Calcular el total ponderado por año y
est <- subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí")) %>%
svyby(~sex, ~year+rama_est1 , ., svytotal, na.rm=TRUE)
piv1<- pivot_longer(est, 3:4, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
piv2<- pivot_longer(est, 5:6, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")
piv2 <- cSplit(piv2, "variable", ".")
piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year","rama_est1" = "rama_est1","respuesta" = "variable_2"))
columnas_a_eliminar <- c("sexHombre","sexMujer","se.sexHombre",	"se.sexMujer","variable_1")
# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]
# Sustituir sex con ""
piv3[["respuesta"]] <- gsub("sex", "", piv3[["respuesta"]] )
piv3[["respuesta"]] <- fct_rev(piv3[["respuesta"]] )
.GlobalEnv$estim$sex_p3i_si_est1 <- piv3
})

# Crear el gráfico
  #title= rk.get.label(p3i_si[["variables"]][["sex"]]),
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
piv3 <- estim$sex_p3i_si_est1
# Crear el gráfico de línea con facetas por respuesta y color por nivel de variable
p <- ggplot(piv3, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~rama_est1, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(
  x = "Año", y = "Total Ponderado", color = "Nivel de sex",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
   })
  rk.graph.off()
})

## EDAD
## Gráfica 5. Histograma de Distribución de Edades en el primer trimestre de 2018
# Histograma por edad
            #title = "Distribución de Edades en la Muestra en 1t 2018"
###
local({
library("dplyr")
library("survey")
library("ggplot2")
library("questionr")
    p <- subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí")) %>%
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
             )
    rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
        try({
            print(p)
        })
    rk.graph.off()
    })

##Gráfica 6. Histograma de Distribución de Edades en el primer trimestre de 2021
            #title = "Distribución de Edades en la Muestra en 1t 2021"
local({
library("dplyr")
library("survey")
library("ggplot2")
library("questionr")
    p <- subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí")) %>%
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
             )
    rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
        try({
             print(p)
        })
    rk.graph.off()
    })

## Gráfica 7. Histograma de Distribución de Edades en el primer trimestre de 2024
            #title = "Distribución de Edades en la Muestra en 1t 2024"
local({
library("dplyr")
library("survey")
library("ggplot2")
library("questionr")
    p <-  subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí")) %>%
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
             )
    rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
        try({
             print(p)
        })
    rk.graph.off()
    })



##Tabla 4. Estadísticos descriptivos de la Edad para la Población Sindicalizada en el periodo 2018-2024
## Crear estadísticos con la media de la edad
local({
library("dplyr")
tbl <- subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí"))%>%
tbl_svysummary(
  by = year,
  statistic = list(eda ~ "{mean}±{sd} ({p25}, {median}, {p75})"),
  label= eda~rk.get.label(des.1$variables$eda), #Las etiquetas desde el survey design original.
  include = "eda")
rk.print(tbl)
.GlobalEnv$tbls$eda_p3i_si <- tbl
})


## Tabla 5. Estadísticos descriptivos de la Edad para la Población Sindicalizada por Sexo en el periodo 2018-2024
local({
library("dplyr")
tbl <- subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí")) %>%
tbl_strata(
strata = "year",
.tbl_fun =
~ .x %>%
tbl_svysummary(
  by = sex,
  statistic = list(eda ~ "{mean}±{sd} ({p25}, {median}, {p75})"),
  digits = list(eda ~ c(1,0,0,0,0)),
  label= eda~rk.get.label(des.1$variables$eda), #Las etiquetas desde el survey design original.
  include = "eda")
)
rk.print(tbl)
.GlobalEnv$tbls$eda_sex_p3i_si <- tbl
})

##Gráfica 8. Media de la edad de la población mayor de 14 años por sexo
local({
library("survey")
library("forcats")
est <- svyby(~eda, ~year + sex, des.1 , svymean, na.rm=TRUE)
est$sex <- fct_rev(est$sex)
.GlobalEnv$estim$eda_svymean_y_sex <- est
})


#Crear el gráfico
  #title= rk.get.label(des.1[["variables"]][["sex"]]), # Se omite el título.
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
# Con el objeto la tabla 'eda_svymean_y_sex'.
est <- estim$eda_svymean_y_sex
# Crear el gráfico de línea.
p <- ggplot(est, aes(x = year, y = eda, color = sex, group = sex)) +
  geom_line() +
  geom_errorbar(aes(ymin = eda - 2*se, ymax = eda + 2*se), width = 0.2) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(x = "Año",
  y = "Media estimada",
  color = "Nivel de sex",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## Gráfica 9. Media de la edad de la población mayor de 14 años económicamente activa por sexo
local({
library("dplyr")
library("survey")
library("forcats")
est <- subset(des.1, clase1 %in% "Población económicamente activa") %>%
svyby(~eda, ~year + sex, pea , svymean, na.rm=TRUE)
est$sex <- fct_rev(est$sex)
.GlobalEnv$estim$pea_eda_svymean_y_sex <- est
})

#Se crea el gráfico
  #title= rk.get.label(pea[["variables"]][["sex"]]),
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
library("forcats")
# Con la tabla 'pea_eda_svymean_y_sex'
# Con la tabla de la media ponderada pea_eda_svymean_y_sex
est <- estim$pea_eda_svymean_y_sex
# Crear el gráfico de línea
p <- ggplot(est, aes(x = year, y = eda, color = sex, group = sex)) +
  geom_line() +
  geom_errorbar(aes(ymin = eda - 2*se, ymax = eda + 2*se), width = 0.2) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(
    x = "Año",
    y = "Media estimada",
    color = "Nivel de sex",
    caption = "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## Gráfica 10. Media de edad de la población ocpada por sexo
local({
library("dplyr")
library("survey")
library("forcats")
est <- subset(des.1, clase1 %in% "Población económicamente activa" &
clase2 %in% "Población ocupada") %>%
svyby(~eda, ~year + sex, ., svymean, na.rm=TRUE)
est$sex <- fct_rev(est$sex)
.GlobalEnv$estim$po_eda_svymean_y_sex <- est
})

# Crear el gráfico
 #title= "Media de edad de la población ocupada por sexo",
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
library("forcats")
# Calcular el total ponderado por año
est <- estim$po_eda_svymean_y_sex
# Crear el gráfico
# Crear el gráfico de línea con color por nivel de variable
p <- ggplot(est, aes(x = year, y = eda, color = sex, group = sex)) +
  geom_line() +
  geom_errorbar(aes(ymin = eda - 2*se, ymax = eda + 2*se), width = 0.2) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
 labs(
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

## Gráfica 11. Media de la edad de la población sindicalizada por sexo
## Filtrar p3i con "Sí"
# Con el objeto 'des.1' de clase survey.design2 y la variable 'p3i' categórica
local({
library("dplyr")
library("survey")
library("forcats")
est <- subset(des.1, clase1 %in% "Población económicamente activa" &
clase2 %in% "Población ocupada" &
p3i%in%"Sí") %>%
svyby(~eda, ~year + sex, ., svymean, na.rm=TRUE)
est$sex <- fct_rev(est$sex)
.GlobalEnv$estim$p3i_sex_svymean_eda_y <- est
}
)

# Crear el gráfico
 #title= "Media de la edad de la población sindicalizada por sexo",
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
library("forcats")
# Con el objeto 'des.1' de clase survey.design2 y la variable 'p3i' categórica
# Calcular el total ponderado por año
est <- estim$p3i_sex_svymean_eda_y
# Crear el gráfico de línea
p <- ggplot(est, aes(x = year, y = eda, color = sex, group = sex)) +
  geom_line() +
  geom_errorbar(aes(ymin = eda - 2*se, ymax = eda + 2*se), width = 0.2) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
 labs(
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


## Tabla 6. Clasificación de la población sindicalizada ocupada por tamaño de la unidad económica-Subtotales 1er trimestre 2018 a 2024
local({
tbl <- subset(des.1, clase1 %in% "Población económicamente activa" &
clase2 %in% "Población ocupada" &
p3i%in%"Sí") %>%
tbl_svysummary(
  by = year,
  statistic = list(ambito2 ~ "{n} ({p})"),
  percent = "column",
  label= ambito2~rk.get.label(des.1$variables$ambito2), #Las etiquetas desde el survey design original.
  include = "ambito2")
rk.print(tbl)
.GlobalEnv$tbls$p3i_ambito2 <- tbl
})

## Gráfica 12. Clasificación de las Zonas de Análisis 1t 2018-2024
# Crear MAPA de la república Mexicana por tres zonas

#Algoritmo de descarga de los datos .zip
url<-"http://internet.contenidos.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463674658_s.zip"
if(!file.exists("areas_geoestadisticas_estatales.zip"){
  download.file(url,"areas_geoestadisticas_estatales.zip",mode="wb")
}
if(!file.exists("conjunto_de_datos/areas_geoestadisticas_basicas_rurales.dbf")){
  unzip("areas_geoestadisticas_estatales.zip")
}

local({
## Computar
setwd("/direccionar/al/directorio/areas_geoestadisticas_estatales")
})

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

rm(shape_estados)

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


# Gráfico con todos los elementos
             #title = "Agrupación por Zonas de Estudio",
local({
library("ggplot2")
library("tidyterra")
values <- c('wheat3','tomato1','green3')
plot <- mex + aes(fill= ZONA) +
theme_minimal() + # Se escogió "theme_minimal()", también se puede usar "theme_void()".
            theme(plot.title = element_text(size=22),
                  legend.key.size = unit(0.5, "cm"),
                  legend.position = "inside",
                  legend.position.inside = c(0.8, 0.7)) +
                  scale_fill_manual(values=values) +
                scale_color_manual(values=c("black")) +
                  guides(color = "none") +
             labs(
             #fill = "ZONA", # En este caso es el mismo nombre de la variable
             caption = "Fuente: Datos Geográficos 2018 (INEGI),\n Zonas de análisis")
rk.graph.on("PNG",1344, 960, pointsize=10.0, res=125, bg = "transparent")
try ({
 	print(plot)
})
rk.graph.off()
})

## Gráfica 13. Clasificación de la población ocupada sindicalizada según sector de actividades-Totales 1t 2018-2024
####

local({
library("survey")
library("dplyr")
library("splitstackshape")
library("tidyr")
est <-  subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i%in%"Sí")) %>%
svyby(~rama_est1, ~year + ent.z, . , svytotal, na.rm=TRUE)
# Eliminamos las columnas especificadas
columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
est <- est[, -which(names(est) %in% columnas_a_eliminar)]
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")
piv2 <- cSplit(piv2, "variable", ".")
piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "ent.z" = "ent.z","respuesta" = "variable_2"))
columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","variable_1","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado")
# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]
#Sustituir "rama_est1" con "".
piv3[["respuesta"]] <- gsub("rama_est1", "", piv3[["respuesta"]])
# Guardar el resultado en la lista estim_tam
.GlobalEnv$estim$rama_est1_ent.z_y  <-piv3
})

  # Crear el gráfico
  #title= rk.get.label(p3i_si[["variables"]][["rama_est1"]]),
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
est <- estim$rama_est1_ent.z_y
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(est, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(
  x = "Año", y = "Total Ponderado",
  color = "Nivel de rama_est1",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## Gráfica 14
##Clasificación de la población ocupada sindicalizada masculina según sector de actividades-Totales 1t 2018-2024
#### Hombre
local({
## Preparar
library("survey")
library("dplyr")
library("tidyr")
library("splitstackshape")
##Computar
est <- subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i %in% "Sí") &
(sex %in% "Hombre")) %>%
svyby(~rama_est1, ~year + ent.z, . , svytotal, na.rm=TRUE)
# Eliminamos las columnas especificadas
columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
est <- est[, -which(names(est) %in% columnas_a_eliminar)]
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")
#Use
piv2 <- cSplit(piv2, "variable", ".")
piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "ent.z" = "ent.z","respuesta" = "variable_2"))
columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","variable_1","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado")
# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]
#Sustituir "rama_est1" con "".
piv3[["respuesta"]] <- gsub("rama_est1", "", piv3[["respuesta"]])
##Asignar el resultado
# En la lista estim_tam
.GlobalEnv$estim$rama_est1_ent.z_y_hm  <- piv3
})

# Crear el gráfico
  # title= rk.get.label(p3i_si[["variables"]][["rama_est1"]]),
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
est <- estim$rama_est1_ent.z_y_hm
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(est, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(
  x = "Año", y = "Total Ponderado", color = "Nivel de rama_est1",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

## Gráfica 15
##Clasificación de la población ocupada sindicalizada femenina según sector de actividades-Totales 1t 2018-2024
## Mujer
local({
library("survey")
library("dplyr")
library("splitstackshape")
library("tidyr")
est <- subset(des.1,                              #Filtrar
(clase1 %in% "Población económicamente activa") &
(p3i %in% "Sí") &
(sex %in% "Mujer")) %>%
svyby(~rama_est1, ~year + ent.z, . , svytotal, na.rm=TRUE)
# Eliminamos las columnas especificadas
columnas_a_eliminar <- c("rama_est1No aplica","se.rama_est1No aplica")
est <- est[, -which(names(est) %in% columnas_a_eliminar)]
piv1<- pivot_longer(est, 3:6, names_to = "respuesta", values_to = "recuento", values_drop_na = FALSE,names_repair="universal")
piv2<- pivot_longer(est, 7:10, names_to = "variable", values_to = "se", values_drop_na = FALSE,names_repair="universal")
#Use
piv2 <- cSplit(piv2, "variable", ".")
#piv3 <- bind_cols(piv1,piv2)
piv3 <- left_join(x=piv1, y=piv2, by = c("year" = "year", "ent.z" = "ent.z","respuesta" = "variable_2"))
columnas_a_eliminar <- c("se.rama_est1Primario","se.rama_est1Secundario","se.rama_est1Terciario","se.rama_est1No.especificado","variable_1","rama_est1Primario","rama_est1Secundario","rama_est1Terciario","rama_est1No.especificado")
# Eliminamos las columnas especificadas
piv3 <- piv3[, -which(names(piv3) %in% columnas_a_eliminar)]
#Sustituir "rama_est1" con "".
piv3[["respuesta"]] <- gsub("rama_est1", "", piv3[["respuesta"]])
# Guardar el resultado en la lista estim_tam
.GlobalEnv$estim$rama_est1_ent.z_y_mj  <-piv3
})

# Crear el gráfico
  #title= rk.get.label(p3i_si[["variables"]][["rama_est1"]]),
local({
library("survey")
library("ggplot2")
library("RColorBrewer")
library("stringr")
est <- estim$rama_est1_ent.z_y_mj
# Crear el gráfico de línea con facetas por ent.z y color por nivel de variable
p <- ggplot(est, aes(x = year, y = recuento, color = respuesta, group = respuesta)) +
  geom_line() +
  geom_errorbar(aes(ymin = recuento - 2*se, ymax = recuento + 2*se), width = 0.2) +
  facet_wrap(~ent.z, nrow = 3) +
 scale_color_brewer(palette = "Set1", labels = function(x) str_wrap(x, width = 20)) +  # Usar paleta Set1
  labs(
  x = "Año", y = "Total Ponderado", color = "Nivel de rama_est1",
  caption= "Las barras de error se calcularon con ± 2 veces el error estándar.")
rk.graph.on("PNG",1224, 720, pointsize=10.0, res=125, bg = "transparent")
    try({
        print(p)
    })
  rk.graph.off()
})

### FIN
