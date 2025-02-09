
## Construir el objeto para asistir la importación

# Requiere crear un objeto para repetir el proceso de maneara reiterativa.

local({
##Preparar
library("rio")
## Computar
url_usr <- "https://github.com/AlfCano"
ruta <- "enoe_ampliado/raw/main/caja"
arch <- "caja.RData"
list <- import(file.path(url_usr,ruta,arch), trust = TRUE)
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

#Cambia el nombre de cada archivo por el nombre en la variable nom"año".

library(lookup)
names(sdem_coes) <- vlookup(names(sdem_coes),
caja$import,
"arch",
"nom")

#Create a Year variable
local({
library("lookup")
years <- caja[["import"]]
for(i in years$nom) {
df <- sdem_coes[[i]]
df[["year"]] <- vlookup(i, years,"nom","value")
.GlobalEnv$sdem_coes[[i]] <- df
}
})

lapply(sdem_coes, ncol)

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

lapply(sdem_coes, nrow)

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


## Se debe limpiar la codificación


Se homologa la codificación
local({
##Preparar
nom <- c("t1_2018.df","t1_2019.df")
## Computar
for(e in nom) {
df <- sdem_coes[[e]]
levels(df[["rama_est2"]])[match("Gobierno y organismos internacion",    # En la tabla  2017 y 2018 cambiar "Gobierno y organismos internacion" por:
levels(df[["rama_est2"]]))] <- "Gobierno y organismos internacionales"  # "Gobierno y organismos internacionales"
.GlobalEnv$sdem_coes[[e]] <- df
}
})

#Zona y Ur son variables 43 y 44. Recodifian en 2018, 2019, 2020

local({
lista <- c("t1_2018.df","t1_2019.df","t1_2020.df")
## Computar
for(e in lista) {
input <- sdem_coes[[e]][["ur"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (sdem_coes[[e]][["ur"]])
recoded[input == "Urbano"] <- "Muestra urbana"
recoded[input == "Rural"] <- "Muestra complemento y rural"
.GlobalEnv$sdem_coes[[e]][["ur"]] <- as.factor (recoded)
## Imprimir el resultado
rk.header ("Re-codificar datos categóricos", parameters=list("Variable de entrada"="sdem_coes[[e]][[\"ur\"]]",
	"Variable de salida"="sdem_coes[[e]][[\"ur\"]]",
	"Número de diferencias después de re-codificar"=sum (sdem_coes[[e]][["ur"]] != sdem_coes[[e]][["ur"]], na.rm=TRUE) + sum (is.na (sdem_coes[[e]][["ur"]]) != is.na (sdem_coes[[e]][["ur"]]))))
}
})


# Se homologa la etiqueta para la variable zona

local({
lista <- c("t1_2018.df","t1_2019.df","t1_2020.df")
## Computar
for(e in lista) {
input <- sdem_coes[[e]][["zona"]]
# Use as.character() como un formato de datos intermedio, para poder añadir o quitar niveles
recoded <- as.character (sdem_coes[[e]][["zona"]])
recoded[input == 'Zona ""A""'] <- "Zona 2 Resto del paí­s"
recoded[input == 'Zona ""B""'] <- "Zona 1 Frontera norte"

.GlobalEnv$sdem_coes[[e]][["zona"]] <- as.factor (recoded)
## Imprimir el resultado
rk.header ("Re-codificar datos categóricos", parameters=list("Variable de entrada"="sdem_coes[[e]][[\"zona\"]]",
	"Variable de salida"="sdem_coes[[e]][[\"zona\"]]",
	"Número de diferencias después de re-codificar"=sum (sdem_coes[[e]][["zona"]] != sdem_coes[[e]][["zona"]], na.rm=TRUE) + sum (is.na (sdem_coes[[e]][["zona"]]) != is.na (sdem_coes[[e]][["zona"]]))))
}
	})

# 2019 Una de las tablas coe/sdem tiene Ciudad de México y/o Distrito federal se repara a nivel de import desde datos abiertos.

# Crear variables ponderadas
#Para unir las bases de datos a continuación se usará el paquete dplyr, el cualrequiere

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

##Con el comando siguiente se importa desde github la tabla completa con etiquetas y sociodemográficos que se utilizará para etiquetar la unión de las bases de datos que se descargaron anteriormente.

Se importa el listado de etiquetas de valor para el set de datos-
sd.cs <- rio::import(caja[["sd_cs"]][["arch"]], trust = TRUE)
#Actualizar caja con enoe_ampliado

#Convertir variables de factor las variables en la tabla "df.factors"
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
f[[i]] <- as.factor(f[[i]]) #es el mismo resultado pues las variables son numéricas
}

##Asignar etiquetas de valor a factores (niveles)
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

library("dplyr")
attach(sdem_coes)
sc <- t1_2018.df %>%
        bind_rows(t1_2019.df) %>%
        bind_rows(t1_2020.df) %>%
        bind_rows(t1_2021.df) %>%
        bind_rows(t1_2022.df) %>%
        bind_rows(t1_2023.df) %>%
        bind_rows(t1_2024.df)
detach(sdem_coes)

nrow(sc) #2140686

rm(sdem_coes)
rm(sd.cs)

#Recodificar "p4d2" #crear hojas de cálculo con los vectores ¿para subset?
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

#Se eliminan las variables
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

#warning ("Algunos valores de entrada se han especificado más de una vez: ", "\"Morelos\"")
.GlobalEnv$sc[["ent.z"]] <- as.factor (recoded)
## Imprimir el resultado
rk.header ("Re-codificar datos categóricos", parameters=list("Variable de entrada"="des_1t_18_24[[\"variables\"]][[\"ent\"]]",
	"Variable de salida"="des_1t_18_24[[\"variables\"]][[\"ent.z\"]]",
	"Número de diferencias después de re-codificar"=sum (sc[["ent"]] != sc[["ent.z"]], na.rm=TRUE) + sum (is.na (sc[["ent"]]) != is.na (sc[["ent.z"]]))))
})

#Se ignora: "Error en Ops.factor(sc[["ent"]], sc[["ent.z"]]):   los conjuntos de niveles de los factores son diferentes"

##Crear una variable numérica para contar totales cuando sea creado el diseño muestral.
sc[["n"]] <- 1


local({
## Preparar
library("lookup")
## Computar
list_nem <- caja[["labels"]][["vars"]] # Para simplificar la notación se copia la variable "nemonico" del data.frame "dic" en la lista "meta.sdem" al objeto "list_nem"
for (i in list_nem) {
rk.set.label(sc[[i]], # Asigna las etiquetas de valor en el objeto "d" a cada variable "i".
vlookup(i,        # Busca los valores "i" en
caja$labels,    # la tabla seleccionada "dic",
"vars",       # en la variable "nemonico" y
"label"))  # devuelve la cadena correspondiente en la variable "nombre_campo".
                }
## Asignar el resultado
.GlobalEnv$sc <- sc
})


 ##Crear el diseño muestral

local({
library("survey")
.GlobalEnv$des.1 <- svydesign( id = ~upm,
strata = ~est_d_tri,
weights = ~fac_tri,
data = sc, nest=TRUE )
})

rm(sc)
