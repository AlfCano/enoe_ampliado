

## Directorio de trabajo

local({
## Computar
setwd("/directorio/usuario/git/enoe/Insumos")
})

## Plantilla

local({
## Preparar
require(knitr)
require(rmarkdown)
## Computar
render(
  input="Plantilla.Rmd",
  output_format="all",
  #output_dir="/enoe/Insumos",
  quiet=TRUE
)
## Imprimir el resultado
rk.header ("Exportar archivo RMarkdown", parameters=list("Archivo RMarkdown"="enoe/Insumos/Plantilla.Rmd",
	"Formato de destino"="Todos los formatos definidos en el documento",
	"Guardar en"="/enoe/Insumos"))
})
