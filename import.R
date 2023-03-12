## Código R para generar precisiones estadísticas de totales, para las principales estimaciones obtenidas
##con la Encuesta Telefónica de Ocupación y Empleo 2020 (ETOE).
# I. Activación de librerías
rm( list=ls() )
# Librería para lectura y escritura de archivos con extensión DBF.
library( foreign )
# Librería para configurar el diseño complejo de la encuesta y obtener precisiones estadísticas con el método
# de conglomerados últimos.
library( survey )
# II. Selección por parte del usuario del archivo SDEMT0420.DBF, con los campos de las variables
# precodificadas de la ETOE 2020, a las que se les calcularan precisiones estadísticas (CLASE1 y CLASE2 ). Se
# obtendrá un archivo inicial con los registros que cumplan con la condición de residencia.
# Nota: si no se selecciona adecuadamente el archivo, ejecutar nuevamente esta sección.
rm( list=ls() )
arch_dbf <- file.choose(new = FALSE)
rutaO = paste(dirname(arch_dbf),”/”, sep = “”)
archO = basename(arch_dbf)
archOrigen = paste(rutaO, archO, sep = “”)
sdem.0 <- read.dbf( archOrigen, as.is = FALSE )
sdem <- sdem.0[which((sdem.0$C_RES == 1 | sdem.0$C_RES == 3) & (sdem.0$R_DEF == “00”)),
names(sdem.0) %in% c(“EST_D”,”UPM”,”FAC”,”EDA”,”CLASE1”,”CLASE2”)]
rm( sdem.0 )
# Visualización de 5 registros del archivo inicial.
sdem[1:5,]
# III. A partir de los campos CLASE1 y CLASE2, obtención de las variables de empleo:
# - Población Económicamente Activa (PEA)
# - Población Ocupada (PO)
# - Población Desocupada Abierta (PDA)
# - Población No Económicamente Activa (PNEA)
# - Población No Económicamente Activa Disponible (PNEA_D)
# - Población No Económicamente Activa No Disponible (PNEA_ND)
# Adecuación del campo EDA
sdem.1 <- cbind( sdem[,c(“EST_D”,”UPM”,”FAC”,”CLASE1”,”CLASE2”)], as.numeric(substr(sdem$EDA,1,2)) )
colnames(sdem.1)[6] <- “EDA”
rm( sdem )
# Obtención de las variables de empleo PEA, PO, PDA, PNEA, PNEA_D y PNEA_ND.
PEA <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE1 == 1,1,0))
PO <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE2 == 1,1,0))
PDA <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE2 == 2,1,0))
PNEA <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE1 == 2,1,0))
PNEA_D <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE2 == 3,1,0))
PNEA_ND <- as.numeric(ifelse((sdem.1$EDA >= 15 & sdem.1$EDA <= 98) & sdem.1$CLASE2 == 4,1,0))
# IV. Obtención del archivo con las variables de diseño y análisis, adecuado para obtener precisiones estadísticas.
sdem.2 <- cbind( sdem.1[,c(“EST_D”,”UPM”,”FAC”)], PEA, PO, PDA, PNEA, PNEA_D, PNEA_ND )
rm( sdem.1 )
# Visualización de 5 registros del archivo final para el análisis.
sdem.2[1:5,]
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
