## Código R para generar precisiones estadísticas de totales, para las principales estimaciones obtenidas
##con la Encuesta Telefónica de Ocupación y Empleo 2020 (ENOE).
# I. Seleccionar el directorio de trabajo, donde se aloja el archivo descargado desde: desde : https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/enoe_n_2022_trim3_dbf.zip.
# en este caso: enoe_n_2022_trim3_dbf.zip
local({
## Computar
setwd()
})
# Librería para lectura y escritura de archivos con extensión DBF 
library( rio ) #
install_formats()
# Librería para configurar el diseño complejo de la encuesta y obtener precisiones estadísticas con el método
# de conglomerados últimos.
library( survey )
# II. Selección por parte del usuario del archivo ENOEN_SDEMT322.dbf,
local({
## Preparar
require(rio)
## Computar
data <- import("ENOEN_SDEMT322.dbf")
.GlobalEnv$ENOEN_SDEMT322.data <- data  # asignar a globalenv()
rk.edit(.GlobalEnv$ENOEN_SDEMT322.data)
## Imprimir el resultado
rk.header ("Importar datos genéricos", parameters=list("Nombre de archivo"="ENOEN_SDEMT322.dbf",
	"Objeto en el que guardar"="ENOEN_SDEMT322.data"))
})


#con los campos de las variables
# precodificadas de la ETOE 2020, a las que se les calcularan precisiones estadísticas (CLASE1 y CLASE2 ). Se
# obtendrá un archivo inicial con los registros que cumplan con la condición de residencia.
# Nota: si no se selecciona adecuadamente el archivo, ejecutar nuevamente esta sección.
sdem <- ENOEN_SDEMT322.data[which((ENOEN_SDEMT322.data$C_RES == 1 | ENOEN_SDEMT322.data$C_RES == 3) & (ENOEN_SDEMT322.data$R_DEF == "00")),
names(ENOEN_SDEMT322.data) %in% c("EST_D","UPM","FAC","EDA","CLASE1","CLASE2")]
#rm( ENOEN_SDEMT322.data)
# Visualización de 6 registros del archivo inicial con la función head.
head(sdem)
# III. A partir de los campos CLASE1 y CLASE2, obtención de las variables de empleo:
# - Población Económicamente Activa (PEA)
# - Población Ocupada (PO)
# - Población Desocupada Abierta (PDA)
# - Población No Económicamente Activa (PNEA)
# - Población No Económicamente Activa Disponible (PNEA_D)
# - Población No Económicamente Activa No Disponible (PNEA_ND)
# Adecuación del campo EDA
sdem.1 <- cbind( sdem[,c("EST_D_TRI","UPM","FAC_TRI","CLASE1","CLASE2")], as.numeric(substr(sdem$EDA,1,2)) )
colnames(sdem.1)[6] <- "EDA"
#rm( sdem )
# Obtención de las variables de empleo PEA, PO, PDA, PNEA, PNEA_D y PNEA_ND.
PEA <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE1 == 1,1,0))
PO <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE2 == 1,1,0))
PDA <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE2 == 2,1,0))
PNEA <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE1 == 2,1,0))
PNEA_D <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE2 == 3,1,0))
PNEA_ND <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE2 == 4,1,0))
# IV. Obtención del archivo con las variables de diseño y análisis, adecuado para obtener precisiones estadísticas.
sdem.2 <- cbind( sdem.1[,c("EST_D_TRI","UPM","FAC_TRI")], PEA, PO, PDA, PNEA, PNEA_D, PNEA_ND )
#rm( sdem.1 )
# Visualización de 5 registros del archivo final para el análisis.
head(sdem.2)
# V. Configuración del diseño muestral de la encuesta compleja y especificación del archivo a cargar
# análisis, en este caso sdem.2.
design.1 <- svydesign( id = ~UPM, strata = ~EST_D, weights = ~FAC, data = sdem.2, nest=TRUE )
# VI. Obtención para las variables de empleo de sus estimaciones puntuales y por intervalo,
# estándar y coeficientes de variación.
Totales <- svytotal( ~PEA+PO+PDA+PNEA+PNEA_D+PNEA_ND, design.1 )
# Visualización de estimaciones puntuales y sus errores estándar.
Totales
est_ee <- data.frame(Totales)
p_cu <- cbind( est_ee[1], round(100*cv(Totales),3), round(SE(Totales),0),
round(est_ee[1]-(est_ee[2]*1.645),0), round(est_ee[1]+(est_ee[2]*1.645),0) )
p_cu <- cbind( c(“PEA”,”PO”,”PDA”,”PNEA”,”PNEA_D”,”PNEA_ND”), p_cu )
colnames(p_cu) <- c(“Variable”,”Estimacion”,”cv (%)”,”ee”,”LIIC”,”LSIC”)
# Visualización final de estimaciones, cv, ee e intervalos de confianza al 90%.
p_cu
# VII. Almacenamiento en disco de la tabla que contiene las precisiones estadísticas calculadas.
# Nota: esta tabla se almacenara en la misma carpeta del archivo de trabajo.
#Formato dbf
destino <- paste( rutaO, “PREC_EST_1.dbf”, sep = “” )
write.dbf( p_cu, destino )
#Formato csv
destino <- paste( rutaO, “PREC_EST_1.csv”, sep = “” )
write.csv( p_cu, destino )
