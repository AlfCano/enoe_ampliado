# Script de Procesamiento Automatizado de Datos ENOE (2018-2025)

## 1. Descripción General

Este script de R está diseñado para automatizar por completo el proceso de descarga, limpieza, armonización y combinación de múltiples años de la **Encuesta Nacional de Ocupación y Empleo (ENOE)**. El script obtiene los datos directamente del repositorio de GitHub [AlfCano/enoe_ampliado](https://github.com/AlfCano/enoe_ampliado).

El objetivo principal es producir un único `data.frame` listo para el análisis (`combined_df`), que contiene los datos integrados de todos los años especificados, con una estructura de columnas homogénea y con todas las etiquetas de variables originales preservadas.

## 2. Características Principales

-   **Totalmente Automatizado**: Ejecuta todo el flujo de trabajo con un solo comando, desde la descarga de los datos hasta la creación del `data.frame` final.
-   **Autocontenido**: No requiere archivos externos de configuración (como `caja.RData`). Toda la lógica, incluyendo URLs, cambios de nombres de variables y recodificaciones, está contenida dentro del propio script.
-   **Preservación de Metadatos**: Implementa una estrategia robusta que primero extrae y almacena todas las etiquetas de variables (`rk.get.label`) en un diccionario, realiza las transformaciones de datos que de otro modo las eliminarían, y finalmente las reaplica al `data.frame` combinado.
-   **Manejo de Inconsistencias**: Detecta y corrige automáticamente las inconsistencias en los tipos de datos entre diferentes años (ej. `factor` vs. `integer` vs. `character`), asegurando que la combinación de los `data.frames` no falle.
-   **Espacio de Trabajo Limpio**: Todo el proceso se ejecuta dentro de un entorno `local({})`, por lo que no se crean objetos intermedios en el espacio de trabajo global. El único resultado es el `data.frame` final `combined_df`, que se asigna explícitamente al entorno global.
-   **Fácilmente Configurable**: El usuario puede modificar fácilmente el rango de años a importar simplemente cambiando una variable al inicio del script.

## 3. Requisitos

-   **R**: Se recomienda una versión reciente (4.0.0 o superior).
-   **Paquetes de R**: El script utiliza el paquete `librarian` para gestionar e instalar automáticamente las siguientes dependencias:
    -   `rio`: Para importar datos desde URLs.
    -   `dplyr`: Para la manipulación de datos.
    -   `stringr`: Para la manipulación de cadenas de texto.
    -   `purrr`: Para la programación funcional y la iteración.
    -   `rkward`: Para la gestión de etiquetas de variables.

## 4. Cómo Utilizar el Script

1.  **Abrir el Script**: Abra el archivo `.R` en su entorno de R (como RStudio).
2.  **(Opcional) Configurar los Años**: Si desea importar un rango de años diferente, modifique la siguiente línea al inicio del script:
    ```r
    years_to_import <- 2020:2024 # Ejemplo para importar solo de 2020 a 2024
    ```
3.  **Ejecutar el Script**: Ejecute el script completo. El paquete `librarian` se encargará de instalar cualquier paquete que falte.
4.  **Verificar el Resultado**: Una vez que el script finalice, un `data.frame` llamado `combined_df` estará disponible en su entorno global, listo para ser analizado.

## 5. Lógica del Proceso (Fases)

El script está organizado en fases claras y comentadas:

-   **Fase 1: Configuración**: Carga los paquetes necesarios y define las variables de configuración principales (rango de años, URLs).
-   **Fase 2: Importación de Datos**: Construye dinámicamente las URLs para cada año y descarga los archivos `.RData` en una lista.
-   **Fase 3: Armonización y Extracción de Etiquetas**:
    -   Itera sobre cada `data.frame` anual para estandarizar los nombres de las columnas según una "receta maestra" (`master_recipe`).
    -   Aplica recodificaciones específicas para los años que lo requieran.
    -   **Paso clave**: Extrae las etiquetas de todas las variables de cada `data.frame` ya armonizado y las almacena en un diccionario central (`master_label_dictionary`).
-   **Fase 4: Combinación, Transformación y Re-etiquetado**:
    -   Detecta y estandariza columnas con tipos de datos inconsistentes para prevenir errores durante la combinación.
    -   Combina todos los `data.frames` de la lista en uno solo usando `dplyr::bind_rows()`.
    -   Realiza las transformaciones finales, como la creación de las variables `p4d2_1` y `ent.z`.
    -   **Paso clave**: Re-aplica las etiquetas a cada columna del `data.frame` final utilizando el diccionario creado en la fase anterior.
-   **Fase 5: Asignación Global**: Asigna el `data.frame` final y limpio al entorno global (`.GlobalEnv$combined_df`).
