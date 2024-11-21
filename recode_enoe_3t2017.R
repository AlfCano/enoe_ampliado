## Importar ENOE desde datos abiertos 3t2017

## Instalar paquetes necesarios
install.packages(c("stringr", "rio", "janitor", "tidyr", "tibble", "dplyr", "lookup", "readr"))  # Se ha cambiado "uchardet" por "readr". El paquete "uchardet" no se encuentra más en CRAN, se ha sustituido por 'readr'.

##Crear la función iconv.recursive
# La función "iconv.recursive" fue elaborada por los contribuidores de RKWard, y se encuentra disponible en su menú de importación genérica "Archivo->Importar->Formato de Importación->Importación genérica (basada en rio)"
# Crea la función auxiliar "iconv.recursive" para convertir todas las cadenas a la codificación actual.
iconv.recursive <- function (x, from) { # Asigna la función subsecuente al objeto llamado "iconv.recursive".
	attribs <- attributes (x); # Accede a la lista de los atributos de un objeto.
	if (is.character (x)) {    # Si es de tipo character...
		x <- iconv (x, from=from, to="", sub="") # aplica la función "iconv()" para convertir vectores de caracteres entre codificaciones.
	} else if (is.list (x)) {    # Si es una lista.
		x <- lapply (x, function (sub) iconv.recursive (sub, from)) # Se aplica para cada elemento de la lista con la función "lapply()".
	}
	attributes (x) <- lapply (attribs, function (sub) iconv.recursive (sub, from)) # Crea los atributos para todos los elementos de la lista.
	x
} # Fin de la declaración de la función. 

## Crear ruta de acceso al directorio extraído desde DA
# se debe indicar la dirección en la que se ha extraído el directorio, desde el archivador:  "> ~2017_trim3_enoe_csv"

local({                           # Inicia el ambiente local.
fp   <- list()                    # Crea la lista para asignar las rutas a los archivos.
dir  <-  "/dirección/al/directorio/con/2017_trim3_enoe_csv" # Se coloca aquí la dirección al directorio que contiene las carpetas "conjunto de datos_...". 
cd   <-  "conjunto_de_datos"      # Cadena constante en los nombres de las tablas.
tip  <- (c("sdem","coe1","coe2")) # Combina cadenas en una columna (vector) con los nombres de las tablas a importar.
prog <-  "enoe"                   # Programa de información que se importará.
year <-  "2017"                   # Periodo.
t	 <-  "3t"                     # Trimestre.
fp$files <- list()                # Crea la lista para asignar el resultado en el objeto "tab" del loop siguiente (CONTENEDOR).
for(i in tip) {                   # Inicia el loop con "for(){}", para cada el elemento en el puesto "i" (i-ésimo) en la columna "tip" (SECUENCIA).
tab <- paste(cd, i, prog, year, t,sep = "_") # Pega los nombres para cada puesto "i" (OPERACIONES).
r   <- file.path (dir, tab, fsep = .Platform$file.sep) # Crea la ruta de archivos con el separador de la plataforma "/" ó "\".
.GlobalEnv$fp[[i]] <- r           # Asigna el resultado de la ruta de archivo al ambiente global.
.GlobalEnv$fp$files[[i]] <- tab   # Asigna la cadena con el nombre del directorio a la lista "files" en el ambiente global.
.GlobalEnv$fp$dir <- dir          # Asigna el objeto dir al entorno global. Éste aloja la ruta que servirá para guardar el resultado, la tabla con las tres tablas unidas y sus metadatos.
}  # Cierra el loop.
}) # Cierra el ambiente local.


## Dirección donde se alojará la tabla al final del guion  
# Designación del directorio de trabajo con la función `setwd`
local({
setwd(fp$dir) # Ejecuta la función "setwd". 
})

# ### SDEM ###

## Importar la lista "cat" para tabla "sdem"
# Designación del directorio de trabajo con la función `setwd` Catálogo de datos: "catalogos"

local({
## Computar 
setwd(file.path(fp$sdem,"catalogos", fsep = .Platform$file.sep)) 
}) 

## Importar la lista "cat" para tabla "sdem"

local({
## Preparar
library("rio")       # Carga el paquete "rio".
##Computar
sdem_files <- list.files(pattern = "\\.csv$") # Enlista los archivos en directorio con extensión csv.
.GlobalEnv$meta.sdem <- list()        # Crea la lista "meta.sdem" en el Ambiente Global (".GlobalEnv").        
.GlobalEnv$meta.sdem$cat <- list()    # Crea la lista "cat" dentro de "meta.sdem" que guardará el resultado del loop.
for (i in sdem_files) {         
data <- import(i)               # Importa los archivos listados en el directorio.
data <- iconv.recursive (data, from= "latin1") # Convierte todas las cadenas de "latin1" a la actual.
##Asignar el resultado
.GlobalEnv$meta.sdem$cat[[i]] <- data # Asigna el resultado a la lista "cat" en el Ambiente Global dentro de la lista "meta.sdem".
}
})

# Para limpiar los nombres usamos de la cadena ".csv" de "stringr".

## Preparar
library("stringr")
##Computar
names(meta.sdem$cat) <- str_replace(names(meta.sdem$cat), pattern = ".csv", replacement = "") # Para eliminar la cadena ".csv", de los nombres se aplica la función"str_replace()" para reemplazar: ".csv" dentro de los nombres ("names()") de la lista ("meta.sdem$cat") que contiene las tablas correspondientes a los archivos importados y reemplazar por: "", es decir, por cadena vacía.

## Diccionario de datos "sdem"

#Importar diccionario de datos. Al final se limpian los nombres con la función `clean_names()` (de `janitor`).

local({                
## Preparar
library("rio")
library("janitor")
## Computar
setwd(file.path(fp$sdem,"diccionario_de_datos", fsep = .Platform$file.sep))
file <- list.files(pattern = "\\.csv$")
data <- import(file) # Importa el archivo al objeto "data".
data <- iconv.recursive (data, from="latin1")
data <- clean_names (data) # Se limpian los nombres del "data.frame".
## Asignar el resultado  
.GlobalEnv$meta.sdem$dic <- data
})

## Creación del data.frame "noms", para seleccionar variables a convertir en factor.

# Se crea un marco de datos para seleccionar las variables cuyos valores deberán ser etiquetados.

local({     
## Preparar
library("tidyr")
library("tibble")
## Computar
noms <- data.frame(cbind("catalogo"=meta.sdem$dic[["catalogo"]], "nombre_campo"=meta.sdem$dic[["nombre_campo"]])) # Crea el data.frame "noms" con las columnas con las variables "catalogo" y "nombre_campo" de la tabla "dic" .
noms[["catalogo"]] <- replace(noms[["catalogo"]], noms[["catalogo"]]=='', NA) # Cambia celdas en blanco por NA.
noms <- drop_na(noms) 
noms  <- rownames_to_column(noms, var="id")
## Asignar el resultado  
.GlobalEnv$meta.sdem$noms <- noms
})


# Variables que se removerán se encuentran en la lista "rem": `"r_def","loc","mun","v_sel","n_hog","h_mud","n_ent","n_ren","eda","nac_dia","nac_mes" y "cs_p13_2"`.
local({
## Preparar
library ("dplyr")
## Computar
df <- meta.sdem$noms #Copia el data.frame nomms al objeto df dentro del Ambiente Local.
rem <- list("r_def","loc","mun","v_sel","n_hog","h_mud","n_ent","n_ren","eda","nac_dia","nac_mes","cs_p13_2") # Crea la lista "rem" con los nombres de las variables numéricas que no se convertirán en tipo factor.
df <- df %>%
  filter(!catalogo %in% rem) # Elimina ("!") las filas de la variable "catalogo" que contienen elementos en ("%in%") la lista "rem".
## Asignar el resultado
.GlobalEnv$meta.sdem$noms <- df
})

# Por último, se elimina el registro con "l_nac" en la variable "nemonico" de la tabla "dic". La variable "l_nac" no tiene contraparte con una tabla de etiquetas de valor, ni con una variable en el conjunto de datos que se importará en el siguiente apartado. En cambio, **"l_nac_c"** si tiene una variable homónima en el conjunto de datos y etiquetas en la lista "cat".

local({
## Preparar
library("dplyr")
## Computar
df <- meta.sdem[["dic"]] %>%
  filter(!nemonico %in% "l_nac")
## Asignar el resultado
.GlobalEnv$meta.sdem[["dic"]] <- df
})

#Se obtiene un marco de datos sin el registro l_nac con ciento cuatro filas.

## Importación del conjunto de datos y asignación de metadatos

#Importamos los datos sociodemográficos "sdem".
local({
## Computar
setwd(file.path(fp$sdem,"conjunto_de_datos", fsep = .Platform$file.sep))
sdem <- fp$files$sdem
file <- paste(sdem,"csv",sep = ".")
data <- read.csv (file=file, na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, strip.white = FALSE, blank.lines.skip = TRUE)
## Asignar el resultado
.GlobalEnv$meta.sdem$data <- data
})

# Debido a que `R` distingue entre mayúsculas y minúsculas, el tipo de letra en las variables "nemonico" y "catalogo" dentro del marco de datos "dic" debe ser igual a los nombres de las tablas dentro de la lista "cat" y al tipo de letrea en los nombres de las variables dentro del conjunto de datos.
# Es posible convertir mayúsculas o minúsculas según sea el caso para que coincidan los nombres que se encuentran en la lista "cat" y los nombres en el . Se puede utilizar las funciones `str_to_upper( )` o ` str_to_lower( )` del paquete `stringr`. En el caso "3t2017" los nombres de los "data.frames" contenidos dentro de `meta.sdem[["cat"]]`se encuentran en minúsculas. Sin embargo, si fuera necesario, un ejemplo de lo anterior sería:  

#library(stringr)
#names(meta.sdem$data) <- str_to_lower(names(meta.sdem$data)) ## Revisar si es necesario cambiar de mayúsculas a minúsculas. 

# Cuando se tiene el conjunto de datos importado, es posible convertir en variables de factor aquellas cuyos nombres se encuentran en la columna "catalogos" del marco de datos "noms".

local({
## Computar
list_names <- meta.sdem$noms[["catalogo"]] # Copia la columna con los nombres de las variables deseadas al objeto "list_names".
for (i in list_names)  {
f <- meta.sdem$data # Copia el objeto "data" de la lista "meta.sdem" al objeto "f".
f[[i]]<- as.factor(f[[i]]) # Con la función "as.factor()" del paquete "base".
## Asignar el resultado
.GlobalEnv$meta.sdem$data <- f
}
})

#Ahora se asignan las etiquetas de valor en el formato `RKWard` con la función `rk.set.label()`. Para aplicar la etiqueta correspondiente a cada una de las variables del conjunto de datos, se ha construido un loop con la función `vlookup()` del paquete `lookup`.

local({
## Preparar
library("lookup")
## Computar
d <- meta.sdem$data  # Para simplificar la notación asignamos el objeto "data" de la lista "meta.sdem" al objeto "d".
list_nem <- meta.sdem$dic$nemonico # Para simplificar la notación se copia la variable "nemonico" del data.frame "dic" en la lista "meta.sdem" al objeto "list_nem"
for (i in list_nem) {
rk.set.label(d[[i]], # Asigna las etiquetas de valor en el objeto "d" a cada variable "i".
vlookup(i,        # Busca los valores "i" en
meta.sdem$dic,    # la tabla seleccionada "dic",  
"nemonico",       # en la variable "nemonico" y
"nombre_campo"))  # devuelve la cadena correspondiente en la variable "nombre_campo".
                }
## Asignar el resultado
.GlobalEnv$meta.sdem$data <- d
})

# Finalmente, se colocan las etiquetas de valor a cada nivel. Esto se logra con la aplicación del resultado a los niveles (con la función `levels()`) de cada una de las variables de factor.  

local({
## Preparar
library("lookup")
## Computar
list_names <- meta.sdem$noms[["catalogo"]]  # Crea la lista de nombres "list_names" con la variable "catalogo"
d <- meta.sdem$data  
for (i in list_names){
f <- meta.sdem$cat[[i]] # Crea el objeto "f" que contiene las categorías a colocar que itera por cada observación "i" contenida en "list_names".
v <- d[[i]]             # Luego crea el objeto "v" que contiene la variable a la que se asignarán los valores conforme a la siguiente búsqueda.
levels(v) <- vlookup(levels(v),     # Pega las categorías en los niveles de "v" con la función "levels", tras comparar con la función "lookup",los valores en los niveles de "v" también a través de la función "levels",
f,          # en el contenido de cada tabla "f".
"CVE",      # En el data.frame "f", busca por parejas el valor de la clave en la variable "CVE" de cada nivel con
"DESCRIP")  # la etiqueta de valor en la variable "DESCRIP".
## Asignar el resultado
.GlobalEnv$meta.sdem$data[[i]] <- v
}
nrow(meta.sdem$data) # Para obtener el recuento: "[1] 392178"
})   

# Hasta este punto, se han obtenido en la tabla "data" `[1] 392178` registros, ciento cuatro columnas etiquetadas y setenta y tres variables de factor con etiquetas de valor a través de los metadatos proporcionados en linea con el estándar DA. Para obtener el recuento anterior se puede ejecturar:

 # En la ENOEN 3t 2021 no existe archivo de etiquetas para la variable "l_nac_c" ("Pregunta 11 Lugar de nacimiento 30"). Y en las versiones del 2017 al 2020 la variable "cs_p14_c" destacó que sus etiquetas de valor sólo aparecen en mayúsculas, mientras que en la 2021 y 2022 aparecen en altas y bajas con uso de tilde. 
 
# En la importación de la ENOEN 3t 2021 se observó que no existe archivo de etiquetas para la variable "l_nac_c" ("Pregunta 11 Lugar de nacimiento 30"). Por otro lado, en las versiones del 2017 a la 2020 la variable "cs_p14_c" destacó que sus etiquetas de valor sólo aparecen en mayúsculas, mientras que en la 2021 y 2022 aparecen en altas y bajas con uso de tilde. También para esta variable en el caso del 3t 2022 se observó que su archivo de categorías no tiene nombres de columnas "CVE" ni "DESCRIP". Para corregir eso se hizo: 

# {r cs_p14_c, eval=F, echo=T, purl = TRUE} 
# local({
# p <- meta.sdem$cat$cs_p14_c
# p$CVE <- p$V1
# p <- within(p,rm(V1))
# p$DESCRIP <- p$V2
# p <- within(p,rm(V2))
# .GlobalEnv$meta.sdem$cat$cs_p14_c <- p
# })

# Se encuentra en la columna 33 del conjunto de datos. 

## Filtrar casos de la tabla "sdem"

#Para unir las tres tablas ("sdem" + "coe1" + "coe2") con base en las instrucciones en INEGI [-@inegiConociendoBaseDatos2010], se procede al filtrado de la tabla de la tabla "sdem". Primeramente se deben eliminar los registros de la variable "r_def" ("Resultado definitivo de la entrevista") diferentes de 00, las cuales corresponden a entrevistas incompletas o no logradas. También los registros con condición de residencia ausente (c_res="Ausente definitivo"). Así mismo, se debe suprimir a los menores de 12 años `(eda > 11)`, ya que las tablas "coe1" y "coe2" sólo incluyen a personas de 12 años y más. Finalmente, se eliminan los registros que tienen registros iguales a 99 en edad `(eda != 99)`, y que corresponden a "Años no especificados de menores de 12"^[Las personas con `eda == 98` se etiquetan como "Años no especificados de 12 años y más" [@inegiEncuestaNacionalOcupacion2020]]. Por lo anterior, se aplicó la siguiente condición de selección: `(r_def == 0) & (c_res != "Ausente definitivo") & (eda > 11) & (eda != 99)")`. El filtro se aplica a través de la función `filter()` (de `dplyr`) y asigna el resultado a la nueva lista "count.df":

local({
library("dplyr")
## Computar
.GlobalEnv$count.df <- list()
.GlobalEnv$count.df$sdem <- filter(meta.sdem$data, (r_def == 0) & (c_res != "Ausente definitivo") & (eda >11) & (eda!=99))
nrow(count.df$sdem)
})
	 
#Se obrienen, `[1] 307117` registros en la tabla "sdem". Este número deberá coincidir con el obtenido de las tablas "coe1" y "coe2". 
# Contar los registros que se tienen en la tabla "sdem".


# Ahora que cuenta con el marco de datos "sdem" en la lista recién creada "count.df " y se ha comprobado el número de registros puede considerar remover la base de datos "data" de a lista "meta.sdem" con:

# meta.sdem <- within(meta.sdem, rm(data))

# ### COE1 ###

## Importar la lista "cat" para tabla "COE1"

local({
## Computar
setwd(file.path(fp$coe1,"catalogos", fsep = .Platform$file.sep))
})

## Importar catálogos

local({
## Preparar
library("rio")
## Computar
cat_files <- list.files(pattern = "\\.csv$")
.GlobalEnv$meta.coe1 <- list()
.GlobalEnv$meta.coe1$cat <- list()
for (i in cat_files) {
data <- import(i)
data <- iconv.recursive (data, from="latin1") # Se asume que se ha guardado la función "icon.recursive()", en el espacio de trabajo.
## Asignar el resultado
.GlobalEnv$meta.coe1$cat[[i]] <- data
}
})

# Para limpiar los nombres usamos de la cadena ".csv" de "stringr".

library("stringr")
names(meta.coe1$cat) <- str_replace(names(meta.coe1$cat), pattern = ".csv", replacement = "")

# Las etiquetas en la tabla "p4d2" en la lista "cat" presenta dos valores en el mismo campo para cada registro. Para asignar las etiquetas de manera correcta, estos deben ser limpiados y separados en dos columnas, recodificados condicionalmente y el marco de datos debe ser reformado a formato "longer" construir las variables de índice y asignación necesarias. La solución para dividir, limpiar las etiquetas y reconstruir el marco de datos de la tabla "p4d2" se presenta en las líneas 296-348 del guion en extenso. Por otro lado en las líneas 431-435 para realizar la recodificación correspondiente en el marco de datos en la variable homónima. 

# Luego de la importación de los catálogos ("cat") en la lista "meta.coe1" se realizan las siguientes operaciones en la tabla "p4d2". Primero se eliminan las cadenas "P4d=1 " y ": " con la función `sub()` combinado con el operador `%>%` :  

local({
## Preparar
library("dplyr")
## Computar
d <- meta.coe1[["cat"]][["p4d2"]][["DESCRIP"]]
d1 <- d %>%
          sub("P4d=1: ","", .  # Usamos la función "sub()" para sustituir "P4d=1: " por cadena vacía '""'. Y se usa el comodín para colocar el resultado.
          ) %>% sub(": ", "", .)    # Nuevamente se encadena para substituir la cadena ": " por cadena vacía '""'. E igualmente se coloca "." para indicar la posición del resultado anterior.
## Asignar el resultado
.GlobalEnv$meta.coe1[["cat"]][["p4d2"]][["DESCRIP1"]] <- d1
})

# Después, se emplea la función `separate()` del paquete `tidyr` para dividir texto en las columnas "p4d.eq1" y "p4d.eq2" separadas por "P4d=2". 

local({
## Preparar
library("tidyr")
## Computar
p  <- meta.coe1[["cat"]][["p4d2"]]
vars <- c("p4d.eq1","p4d.eq2") 
p <-   p %>%
          separate("DESCRIP1", vars, sep = "P4d=2", remove = FALSE)
## Asignar el resultado  
.GlobalEnv$meta.coe1[["cat"]][["p4d2"]] <- p
})

# Ahora se eliminan espacios con la función `str_squish()` del paquete `stringr` de esas columnas. 

local({
## Preparar
library("stringr")
## Computar
vars <- c("p4d.eq1","p4d.eq2")
for (i in vars) {
eq <- meta.coe1[["cat"]][["p4d2"]][[i]]
eq <- str_squish(eq)
## Asignar el resultado
.GlobalEnv$meta.coe1[["cat"]][["p4d2"]][[i]]<-eq}
})

# Finalmente, se usa la función `gather()` (también de `tidyr`), para transformar en formato "longer" y posteriormente se utiliza la función `ifelse()` para realizar la codificación condicionada.

local({
## Preparar
library("tidyr")
## Computar
dt_l <- meta.coe1$cat$p4d2 %>% gather(var, etqt, p4d.eq1:p4d.eq2, factor_key=TRUE)
dt_l$CVR <- ifelse(dt_l$var=="p4d.eq1", dt_l$CVE+10, dt_l$CVE+20) # La función "ifelse()", crea la variable "CVR" con valores únicos si cumple la condición de que "var" sea exactamente igual a "p4d.eq1". Si cumple suma 10 al valor en CVE, sino le suma 20 a esa misma columna.
dt_l$DESCRIP <- dt_l$etqt # Copia la columna "etqt" en "DESCRIP".
dt_l$CVE <- dt_l$CVR  # Copia la columna "CVR" en "CVE".
## Asignar el resultado
.GlobalEnv$meta.coe1$cat$p4d2 <- dt_l
})

## Diccionario de datos

#Importar diccionario de datos. Al final se limpian los nombres con la función `clean_names()` (de `janitor`).

local({
## Preparar
library("rio") 
library("janitor")
## Computar
setwd(file.path(fp$coe1,"diccionario_de_datos", fsep = .Platform$file.sep))
file <- list.files(pattern = "\\.csv$")
data <- import(file)
data <- iconv.recursive (data, from="latin1")
data <- clean_names (data)
## Asignar el resultado
.GlobalEnv$meta.coe1$dic <- data
})

##Creación del data.frame "noms", para seleccionar variables a convertir en factor.

#Creamos un marco de datos para seleccionar las variables cuyos valores deberán ser etiquetados.

local({     
## Preparar
library("tidyr")
library("tibble")
## Computar
noms <- data.frame(cbind("catalogo"=meta.coe1$dic[["catalogo"]], "nombre_campo"=meta.coe1$dic[["nombre_campo"]]))
noms[["catalogo"]] <- replace(noms[["catalogo"]], noms[["catalogo"]]=='', NA)
noms <- drop_na(noms)
noms <- rownames_to_column(noms,var="id")
## Asignar el resultado  
.GlobalEnv$meta.coe1$noms <- noms
})

# Variables que se removerán se encuentran en la lista "rem": "r_def","v_sel","n_hog","h_mud","n_ent","n_ren","eda","n_inf","p3f2","p4d3"

# Con especial atención en las variable "p3f2" y "p4d3". Con las etiquetas "Pregunta 3f ¿Más de uno?, ¿Cuántos?" y Pregunta 4d Campo exclusivo del sistema. Que no tienen etiquetas de valor y deben ser filtradas en el data.frame "noms" de la lista "meta.coe1".

local({
library (dplyr)
df <- meta.coe1$noms
rem <- list("r_def","v_sel","n_hog","h_mud","n_ent","n_ren","eda","n_inf","p3f2","p4d3")
df <- df %>%
  filter(!catalogo %in% rem)
.GlobalEnv$meta.coe1$noms <- df
})

# 1     r_def	Resultado definitivo de la entrevista
# 4     v_sel	Vivienda Seleccionada
# 5     n_hog	Número de hogar
# 6     h_mud	Hogar mudado
# 7     n_ent	Número de entrevista o visita
# 8     n_ren	Número de renglón
# 9     eda	    Edad
# 10    n_inf	Renglón del Informante
# 68    p3f2    Pregunta 3f ¿Más de uno?, ¿Cuántos?
# 91    p4d3    Exclusivo del sistema

## Importar el conjunto de datos
local({
setwd(file.path(fp$coe1,"conjunto_de_datos", fsep = .Platform$file.sep))
## Computar
coe1 <- fp$files$coe1
file <- paste(coe1,"csv",sep = ".")
data <- read.csv (file=file,  na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, strip.white = FALSE, blank.lines.skip = TRUE)
# copiar del entorno local a globalenv()
.GlobalEnv$meta.coe1[["data"]] <- data
})

# Debido a que `R` distingue entre mayúsculas y minúsculas, el tipo de letra en las variables "nemonico" y "catalogo" dentro del marco de datos "dic" debe ser igual a los nombres de las tablas dentro de la lista "cat" y al tipo de letrea en los nombres de las variables dentro del conjunto de datos.
# Es posible convertir mayúsculas o minúsculas según sea el caso para que coincidan los nombres que se encuentran en la lista "cat" y los nombres en el . Se puede utilizar las funciones `str_to_upper( )` o ` str_to_lower( )` del paquete `stringr`. En el caso "3t2017" los nombres de los "data.frames" contenidos dentro de `meta.sdem[["cat"]]`se encuentran en minúsculas. Sin embargo, si fuera necesario, un ejemplo de lo anterior sería:

# library(stringr)
# names(meta.coe1$data) <- str_to_lower(names(meta.coe1$data))

# Recodificación en la variable "p4d2"
local({
p <- with (meta.coe1$data, ((p4d1*10) + p4d2))
.GlobalEnv$meta.coe1$data$p4d2 <- p
})


# Transformar las variables que se encuentran en tipo numérico a factores
local({
list_names <- meta.coe1$noms[["catalogo"]]
for (i in list_names)  {
f <- meta.coe1$data
f[[i]]<- as.factor(f[[i]])
.GlobalEnv$meta.coe1$data <-f
}
})

# Para etiquetar la variable "p5d_thrs" se requiere corregir la cadena en el diccionario de datos. La cadena que se encuentra dentro de `meta.coe1$dic$nemonico` es "p5d_thr" pero la variable en "data" se llama "p5d_thrs". Para corregirlo sustituimos con la función `str_replace()` (de `stringr`).

library("stringr")
meta.coe1$dic$nemonico <- str_replace(meta.coe1$dic$nemonico, "p5d_thr", "p5d_thrs")

# Colocar etiquetas de variable
local({
library("lookup")
d <- meta.coe1$data
list_nem <- meta.coe1$dic$nemonico
for (i in list_nem) 
{
rk.set.label(d[[i]],
vlookup(i, 
meta.coe1$dic,
"nemonico",
"nombre_campo",
nomatch = NA)
)
}
.GlobalEnv$meta.coe1$data <- d
})

# Colocar niveles a las variables de tipo factor.
local({
#library(tidyverse)
library("lookup")
list_names <- meta.coe1[["noms"]][["catalogo"]]
d <- meta.coe1$data
for (i in list_names){
f <- meta.coe1$cat[[i]]
v <- d[[i]]
levels(v) <- vlookup(levels(v),
f,
"CVE",
"DESCRIP",
nomatch = NA)
.GlobalEnv$meta.coe1$data[[i]] <- v
}
.GlobalEnv$count.df$coe1  <- .GlobalEnv$meta.coe1$data
})

lapply(count.df, nrow)

# Considerar remover:
# meta.coe1 <- within(meta.coe1, rm(data))

# ### COE2 ###

## Importar la lista "cat" para tabla "coe2"

local({
## Computar
setwd(file.path(fp$coe2,"catalogos", fsep = .Platform$file.sep))
})


local({
## Preparar
library(rio)
cat_files <- list.files(pattern = "\\.csv$")
.GlobalEnv$meta.coe2 <- list()
.GlobalEnv$meta.coe2$cat <- list()
for (i in cat_files) {
data <- import(i)
data <- iconv.recursive (data, from="latin1")
.GlobalEnv$meta.coe2$cat[[i]] <- data
}
})

# En el diseño ENOE^N 2022, los siguientes conjuntos se han encontrado en otra codificación las variables: p6e, p6f, p6g, p6h, p6i. El siguiente loop se ha creado para detectar la codificación de caracteres. Estas variables no se encuentran en el conjunto de datos correspondienta al 2017.
# local({
# library(readr)
# p6 <- c("p6e.csv", "p6f.csv", "p6g.csv", "p6h.csv", "p6i.csv")
# for(i in p6) {
# v <- read_csv(i)
# .GlobalEnv$meta.coe2$cat_coe2[[i]] <- v
# }
# })

# Para limpiar los nombres usamos de la cadena ".csv" de "stringr".
## Preparar
library("stringr")
##Computar
names(meta.coe2$cat) <- str_replace(names(meta.coe2$cat), pattern = ".csv", replacement = "")

## Diccionario de datos "coe2"

local({
## Preparar
library("rio")
library("janitor")
## Computar
setwd(file.path(fp$coe2,"diccionario_de_datos", fsep = .Platform$file.sep))
file <- list.files(pattern = "\\.csv$")
data <- import(file)
data <- iconv.recursive (data, from="latin1")
data <- clean_names (data)
## Asignar el resultado  
.GlobalEnv$meta.coe2$dic <- data
})

## Creación del data.frame "noms", para seleccionar variables a convertir en factor.

# Se crea un marco de datos para seleccionar las variables cuyos valores deberán ser etiquetados.

local({     
## Preparar
library("tidyr")
library("tibble")
## Computar
noms <- data.frame(cbind("catalogo"=meta.coe2$dic[["catalogo"]], "nombre_campo"=meta.coe2$dic[["nombre_campo"]]))
noms[["catalogo"]] <- replace(noms[["catalogo"]], noms[["catalogo"]]=='', NA)
noms <- drop_na(noms)
noms <- rownames_to_column(noms,var="id")
## Asignar el resultado  
.GlobalEnv$meta.coe2$noms <- noms
})

# Variables que se removerán se encuentran en la lista "rem": "v_sel","n_hog","h_mud","n_ent","n_ren","eda","n_inf","p8_9"

local({
## Preparar
library ("dplyr")
## Computar
df <- meta.coe2$noms
rem <- list("v_sel","n_hog","h_mud","n_ent","n_ren","eda","n_inf","p8_9")
df <- df %>%
  filter(!catalogo %in% rem)
## Asignar el resultado  
.GlobalEnv$meta.coe2$noms <- df
})


# Se usan los nombres. La variable "p8_9" (col 42) no tiene categorías en los csv y no está en el 2018.
#3  v_sel	Vivienda Seleccionada
#4  n_hog	Número de hogar
#5  h_mud	Hogar mudado
#6  n_ent	Numero de entrevista o visita
#7  n_ren	Número de renglón
#8  eda    Edad
#9  n_inf	Renglón del Informante
#37	p8_9	Pregunta 8 Opción 9: no sabe ###No tiene archivo de categorías

## Importación del conjunto de datos y asignación de metadatos

#Importamos los datos sociodemográficos "coe2"
local({
## Computar
setwd(file.path(fp$coe2,"conjunto_de_datos", fsep = .Platform$file.sep))
coe2 <- fp$files$coe2
file <- paste(coe2,"csv",sep = ".")
data <- read.csv (file=file,  na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, strip.white = FALSE, blank.lines.skip = TRUE) #cuidado con la N y el número de año
## Asignar el resultado  
.GlobalEnv$meta.coe2$data <- data
})

# Debido a que `R` distingue entre mayúsculas y minúsculas, el tipo de letra en las variables "nemonico" y "catalogo" dentro del marco de datos "dic" debe ser igual a los nombres de las tablas dentro de la lista "cat" y al tipo de letrea en los nombres de las variables dentro del conjunto de datos.
# Es posible convertir mayúsculas o minúsculas según sea el caso para que coincidan los nombres que se encuentran en la lista "cat" y los nombres en el . Se puede utilizar las funciones `str_to_upper( )` o ` str_to_lower( )` del paquete `stringr`. En el caso "3t2017" los nombres de los "data.frames" contenidos dentro de `meta.sdem[["cat"]]`se encuentran en minúsculas. Sin embargo, si fuera necesario, un ejemplo de lo anterior sería:  

# library(stringr)
# names(meta.coe2$data) <- str_to_lower(names(meta.coe2$data))

local({
## Computar
list_names <- meta.coe2$noms[["catalogo"]]
for (i in list_names)  {
f <- meta.coe2$data
f[[i]]<- as.factor(f[[i]])
## Asignar el resultado  
.GlobalEnv$meta.coe2$data <-f
}
})

#Ahora se asignan las etiquetas de valor en el formato `RKWard` con la función `rk.set.label()`. Para aplicar la etiqueta correspondiente a cada una de las variables del conjunto de datos, se ha construido un loop con la función `vlookup()` del paquete `lookup`.

local({
## Preparar
library("lookup")
## Computar
d <- meta.coe2$data
for (i in meta.coe2$dic$nemonico) 
{
rk.set.label(d[[i]],
vlookup(i, 
meta.coe2$dic,
"nemonico",
"nombre_campo",
nomatch = NA)
)
}
## Asignar el resultado
.GlobalEnv$meta.coe2$data <- d
})

# Finalmente, se colocan las etiquetas de valor a cada nivel. Esto se logra con la aplicación del resultado a los niveles (con la función `levels()`) de cada una de las variables de factor. 

local({
## Preparar
library("lookup")
## Computar
list_names <- meta.coe2$noms[["catalogo"]] 
d <- meta.coe2$data
for (i in list_names){
f <- meta.coe2$cat[[i]]
v <- d[[i]]
levels(v) <- vlookup(levels(v),
f,
"CVE",
"DESCRIP",
nomatch = NA)
## Asignar el resultado
.GlobalEnv$meta.coe2$data[[i]] <- v
}
})

###Copiar de coe1 a coe2 r_def
#  Observar que en la tabla "coe2" no existe variable "r_def", si se utiliza como índice para la unión con las otras tablas.

nrow(meta.coe2[["data"]])
count.df$coe2 <- meta.coe2[["data"]]

count.df$sdem[["fac_tri"]] <- count.df$sdem[["fac"]]

### Revisar `[1] 307117` para las tres tablas

lapply(count.df, nrow)

#meta.coe1 <- within(meta.coe1, rm(data))
#meta.coe2 <- within(meta.coe2, rm(data))

##Crear una sola tabla con SDEM_PEA, COE1 y COE2 y guardar el resultado.

# Se usa unión "NATURAL" con el paquete `dplyr`.
local({
##Preparar
library("dplyr")
library("lookup")
## Computar
attach(count.df) #Añadir la lista al directorio de búsqueda de `R`.
df <-right_join(sdem, coe1, by=NULL, copy=FALSE) %>% 
      right_join(coe2, by=NULL, copy=FALSE)
etq <- data.frame(cbind("nemonico"=c("cd_a","ent","ur"), "descrip"=c("Ciudad autorrepresentada","Entidad","Urbano/Rural")))
for (i in etq$nemonico) {
rk.set.label(df[[i]], vlookup(i, etq, "nemonico", "descrip",nomatch = NA))}
## Asignar el resultado
.GlobalEnv[["sdem_coes.df"]] <- df
detach(count.df)
nrow(sdem_coes.df) # Recuento [1] 307117
})

# Joining with `by = join_by(r_def, cd_a, ent, con, upm, d_sem, n_pro_viv, v_sel,
# n_hog, h_mud, n_ent, per, n_ren, eda, ur, fac)`
# Joining with `by = join_by(cd_a, ent, con, upm, d_sem, n_pro_viv, v_sel, n_hog,
# h_mud, n_ent, per, n_ren, eda, ur, fac, n_inf)`
# Filtrar menores de 15 años

local({
##Preparar
library("dplyr")
## Computar
.GlobalEnv$sdem_coes_14.df <- filter(sdem_coes.df, eda>14) #filtrar menores de 15 años
nrow(sdem_coes_14.df)
})

##Revisar `[1] 286294`

# Remover
# rm(sdem_coes.df)

#Ahora se puede guardar el archivo si se obtiene `[1] 286294` del conteo en el marco de datos "sdem_coes_14.df", tras filtrar a los menores de catorce años al directorio en el que se descomprimieron los datos con:

## Guardar
local({
## Computar
save(sdem_coes_14.df,
	file=file.path(fp$dir,"sdem_coes_14.RData",fsep = .Platform$file.sep))
## Imprimir el resultado
rk.header ("Guardar objetos R", parameters=list("Nombre de archivo"=file.path(fp$dir,"sdem_coes_14.RData",fsep = .Platform$file.sep),
	"Objeto"="sdem_coes_14.df"))
})




