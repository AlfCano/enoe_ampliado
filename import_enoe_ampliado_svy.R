
## Construir el objeto para asistir la importación
require("librarian") # Instala el paquete "librarian" si no está previamente instalado y lo carga en la sesión.
shelf(rkward, rio, lookup, dplyr, survey) # Ejecuta la función shelf del paquete librarian. Revisa si los paquetes ya están instalados, si no lo están los instala y los carga en la sesión.
## Establecer el directorio de trabajo
# setwd() # Se asume que se ha designado el directorio de trabajo.


# Requiere crear un objeto para repetir el proceso de maneara reiterativa.
local({
##Preparar
library("rio")
## Computar
url_usr <- "https://github.com/AlfCano"
ruta    <- "enoe_ampliado/raw/main/datos/datos_auxiliares"
arch    <- "caja.RData"
list    <- import(file.path(url_usr,ruta,arch), trust = TRUE)
##Asignar
.GlobalEnv$caja <- list
})

## Elementos para importar tablas

## Importar
#Importar archivos *.RData en la lista "sdem_coes"

local({
##Preparar
sdem_coes <- list()
arch <- caja[["import"]][["arch"]]
## Computar
for(e in arch) {
data <- rio::import(e, trust=TRUE)
.GlobalEnv$sdem_coes[[e]] <- data #Guarda cada tabla con el nombre del enlace de github.
}
})

## Una vez que se ha importado todos los elementos que compodrán el análisis.
#Cambia el nombre de cada archivo por el nombre en la variable nom "año".
library("lookup")
names(sdem_coes) <- vlookup(names(sdem_coes),
caja$import,
"arch",
"nom")

## Crear la variable "year" con el año corrspondiente a cada set de datos.
local({
library("lookup")
years <- caja[["import"]]
for(i in years$nom) {
df <- sdem_coes[[i]]
df[["year"]] <- vlookup(i, years,"nom","value")
.GlobalEnv$sdem_coes[[i]] <- df
}
})

## Comprobar número de columnas.
lapply(sdem_coes, ncol)
# Se espera el siguiente número de columnas para cada data.frame:
# $t1_2018.df
# [1] 385
#
# $t1_2019.df
# [1] 385
#
# $t1_2020.df
# [1] 385
#
# $t1_2021.df
# [1] 390
#
# $t1_2022.df
# [1] 403
#
# $t1_2023.df
# [1] 403
#
# $t1_2024.df
# [1] 403

## Comprobar número de filas.
lapply(sdem_coes, nrow)
# Se espera el siguiente número de filas para cada data.frame:
# $t1_2018.df
# [1] 287004
#
# $t1_2019.df
# [1] 300514
#
# $t1_2020.df
# [1] 311756
#
# $t1_2021.df
# [1] 264449
#
# $t1_2022.df
# [1] 306126
#
# $t1_2023.df
# [1] 344205
#
# $t1_2024.df
# [1] 326632

# Problema 2n 2022 Se subsanó con las observaciones hechas al 3t_2022t

# c("p6e","p6f","p6g","p6h","p6i")


## Para proceder a la unificación se deben atender discrepancias en las etiquetas de valor de las diferentes tablas.

# Para homologar la codificación en la ENOE 2018 y 2019 con las subsecuentes del 2020 a la feecha en la variable rama_est2.
local({
##Preparar
nom <- c("t1_2018.df","t1_2019.df")
## Computar
for(e in nom) {
df <- sdem_coes[[e]]
levels(df[["rama_est2"]])[match("Gobierno y organismos internacion",    # En la tabla  2018 y 2019 se cambia "Gobierno y organismos internacion" por:
levels(df[["rama_est2"]]))] <- "Gobierno y organismos internacionales"  # "Gobierno y organismos internacionales".
.GlobalEnv$sdem_coes[[e]] <- df
}
})

#Zona y Ur son variables 43 y 44. Se eecodifian en las tablas correspondientes al 1t 2018, 2019 y 2020, para que todas las variables tengan los mismos niveles (etiquetas de valor) en la variable de tipo factor "ur".
local({
lista <- c("t1_2018.df","t1_2019.df","t1_2020.df")
## Computar
for(e in lista) {
input <- sdem_coes[[e]][["ur"]]
# Usa "as.character()" como un formato de datos intermedio, para poder añadir o quitar niveles.
recoded <- as.character (sdem_coes[[e]][["ur"]])
recoded[input == "Urbano"] <- "Muestra urbana" # "Urbano" se cambia por la etiqueta de valor "Muestra urbana".
recoded[input == "Rural"] <- "Muestra complemento y rural" # "Rural" se cambia por la etiqueta de valor "Muestra complemento y rural".
## Asignar el resultado
.GlobalEnv$sdem_coes[[e]][["ur"]] <- as.factor (recoded)
}
})


# Se homologa la etiqueta para la variable "zona"
local({
lista <- c("t1_2018.df","t1_2019.df","t1_2020.df")
## Computar
for(e in lista) {
input <- sdem_coes[[e]][["zona"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (sdem_coes[[e]][["zona"]])
recoded[input == 'Zona ""A""'] <- "Zona 2 Resto del paí­s" # 'Zona ""A""' se cambia por la etiqueta de valor "Zona 2 Resto del paí­s"
recoded[input == 'Zona ""B""'] <- "Zona 1 Frontera norte" # 'Zona ""B""' se cambia por la etiqueta de valor "Zona 1 Frontera norte"
## Asignar el resultado
.GlobalEnv$sdem_coes[[e]][["zona"]] <- as.factor (recoded)
}
	})

# En el 1t 2019 Una de las tablas coe/sdem tiene Ciudad de México y/o Distrito federal se reparó a nivel de import desde datos abiertos en la carpeta "datos_limpios".


#Para unir las tablas con el paquete "dplyr", se cualrequiere que las variables tengan los mismos nombres, por lo que hay que homologar los nombres de los diseños anteriores al 2021 y actualizarlos los nombres equivalentes actuales, en este caso las variables: "est_d",  "t_loc", "cs_p20_des", "cs_p22_des".

local({
sc <- c("t1_2018.df","t1_2019.df","t1_2020.df")
for(i in sc) {
df <- sdem_coes[[i]]
df[["est_d_tri"]] <- df[["est_d"]]
df[["t_loc_tri"]] <- df[["t_loc"]]
df[["cs_p21_des"]] <- df[["cs_p20_des"]]
df[["cs_p23_des"]] <- df[["cs_p22_des"]]
.GlobalEnv$sdem_coes[[i]] <- df
}
})

## Con el comando siguiente se importa desde github la tabla completa con etiquetas y sociodemográficos que se utilizará para aplicar los metadatos a la unión de las bases de datos que se descargaron anteriormente.

#Se importa el listado de etiquetas de valor para el set de datos-
sd.cs <- rio::import(caja[["sd_cs"]][["arch"]], trust = TRUE)
#Actualizar caja con enoe_ampliado

# Se cohersionan en variables tipo "factor" las variables en marco de datos "df.factors" dentro de la lista "caja".
local({
library("dplyr")
library("lookup")
sct <- caja[["df.factors"]][["df"]]
## Selecciona las tablas en sct
for(e in sct){
f <- sdem_coes[[e]]
r <- caja[["df.factors"]] %>% subset(df == e)
list_names <- r[["vars"]]#enlista las variables deseadas
## Crear Variables de factor
for (i in list_names)  {
f[[i]] <- as.factor(f[[i]]) # Es el mismo resultado pues las variables son numéricas
}
# Se asignan etiquetas de valor a factores (niveles)
for (i in list_names) {
fa <- sd.cs[[i]]
v <- f[[i]]
levels(v) <- vlookup(levels(v),
fa,
"CVE",
"DESCRIP",
nomatch = NA)
f[[i]] <- v
}
.GlobalEnv$sdem_coes[[e]] <- f
}
})

## Se combinan las tablas.
## Preparar.
library("dplyr")
##Computar y asignar
attach(sdem_coes) # Adjunta el nombre de la lista sdem_coes a la ruta de la sesión para obtener acceso directo a los nombres de objetos que contiene.
sc <- t1_2018.df %>%
        bind_rows(t1_2019.df) %>%  # Se usa la función bind_rows para obtener una tabla producto de colocar una encima de la otra con los mismo nombres de columnas.
        bind_rows(t1_2020.df) %>%
        bind_rows(t1_2021.df) %>%
        bind_rows(t1_2022.df) %>%
        bind_rows(t1_2023.df) %>%
        bind_rows(t1_2024.df)
detach(sdem_coes) #Retiramos de la ruta global el contenido de la lista "sdem_coes".

if(nrow(sc) == 2140686) {       # Si el objeto sc contiene el número de filas esperado,
  rm(sdem_coes, sd.cs)          # Entonces se elimina la lista "sdem_coes" y "sd.cs".
}

#Recodificar  la variable "p4d2" #crear hojas de cálculo con los vectores.
local({
## Computar
input <- sc[["p4d2"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (sc[["p4d2"]])
attach(caja[["p4d"]])
recoded[input %in% organismo] <- "Organismo autónomo (IFE, Institutos Estatales Electorales, Comisiones Nacionales o Estatales de Derechos Humanos)" #Se conserva la etiqueta oficial
recoded[input %in% iglesia] <- "Iglesia, asociación profesional, cámara o sindicato"
recoded[input %in% no.sabe] <- "No sabe"
recoded[input %in% ninguna] <- "Ninguna de las anteriores"
.GlobalEnv$sc[["p4d2_1"]] <- as.factor (recoded)
detach(caja[["p4d"]])
})

#Se eliminan las variables ca, fac, est_d, t_loc, cs_p20_des y cs_p22_des.
local({
df <- sc
df <- subset(df, select = -c(ca, fac, est_d, t_loc, cs_p20_des, cs_p22_des))
.GlobalEnv$sc <- df
})

#Se crea variabe Zonas de Entidad Federativa
local({
## Computar
input <- sc[["ent"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (sc[["ent"]])
recoded[input %in% c("Aguascalientes","Baja California","Baja California Sur","Coahuila","Chihuahua","Durango","Nayarit","Nuevo León","Sinaloa","Sonora","Tamaulipas","Zacatecas")] <- "Norte"
recoded[input %in% c("Colima","Ciudad de México","Guanajuato","Hidalgo","Jalisco","México","Michoacán","Querétaro","San Luis Potosí")] <- "Centro"
recoded[input %in% c("Campeche","Chiapas","Guerrero","Oaxaca","Puebla","Quintana Roo","Tabasco","Tlaxcala","Morelos","Veracruz","Yucatán")] <- "Sur-Este"
## Asignar el resultado
.GlobalEnv$sc[["ent.z"]] <- as.factor (recoded) # Los conjuntos de niveles de los factores son diferentes.
})

## Crear una variable numérica para contar totales cuando sea creado el objeto "survey.design".
sc[["n"]] <- 1

## Asigna las etiquetas en el formato de RKWard.
local({
## Preparar
library("lookup")
## Computar
list_nem <- caja[["labels"]][["vars"]] # Para simplificar la notación se copia la variable "vars" del data.frame "labels" en la lista "caja" al objeto "list_nem".
dic <- caja[["labels"]] # Asigna la tabla "labels" al objeto "dic".
for (i in list_nem) {
  rk.set.label(
    sc[[i]], # Asigna las etiquetas de valor en el objeto "d" a cada variable "i".
      vlookup(i,    # Busca los valores "i" en
        dic,  # la tabla seleccionada "dic",
        "vars",       # en la variable "nemonico" y
        "label"))     # devuelve la cadena correspondiente en la variable "nombre_campo".
      }
## Asignar el resultado
.GlobalEnv$sc <- sc
})

## Crear el objeto clase de "diseño de encuesta 2" ("survey.design2")

local({
library("survey")
.GlobalEnv$des.1 <- svydesign( id = ~upm,
strata = ~est_d_tri,
weights = ~fac_tri,
data = sc, nest=TRUE )
})

if(nrow(sc) == nrow(des.1[["variables"]])) {     # Si el objeto sc contiene el número de filas al que se encuentra en el diseño de encuesta:
 rm(sc, caja)                                    # Entonces se elimina el objeto "sc" y la lista "caja".
}


## Ahora se tiene disponible el objeto des.1 en el área de trabajo que cotiene la muestra de los 1t del 2018 al 2024.
# Puede proceder a ubicar su directorio de trabajo en la carpeta "/codigo" para realizar los procedimientos en el script "descriptivos_sindicalismo_svy.R" que se pueden consultar en la carpeta "/salida".
