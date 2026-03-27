# -----------------------------------------------------------------------------
# SCRIPT DE IMPORTACIÓN, LIMPIEZA Y PREPARACIÓN DE DATOS DE LA ENCUESTA ENOE
# -----------------------------------------------------------------------------
# Autor: [Your Name]
# Fecha: [Current Date]
#
# Descripción:
# Este script realiza un proceso completo para trabajar con múltiples años de
# la encuesta ENOE. Todo el proceso se ejecuta dentro de un entorno local para
# no contaminar el espacio de trabajo global. El único objeto guardado en el
# entorno global es el data frame final 'combined_df'.
# Se implementa una estrategia de extracción y reaplicación de etiquetas para
# asegurar que no se pierda metadatos durante las transformaciones.
# -----------------------------------------------------------------------------

# El script completo se ejecuta dentro de un bloque local.
local({

  # =============================================================================
  # FASE 1: CONFIGURACIÓN Y GESTIÓN DE PAQUETES
  # =============================================================================
  cat("--- Fase 1: Configuración y Gestión de Paquetes ---\n")

  if (!require("librarian")) install.packages("librarian")
  librarian::shelf(rio, dplyr, stringr, purrr, rkward)

  years_to_import <- 2018:2025
  base_url <- "https://github.com/AlfCano/enoe_ampliado/raw/main/datos/datos_limpios/"
  file_pattern <- "sdem_coes_14_1t_{year}.RData"


  # =============================================================================
  # FASE 2: DESCARGA E IMPORTACIÓN DE DATOS ANUALES
  # =============================================================================
  cat("--- Fase 2: Descarga e Importación de Datos ---\n")

  full_urls <- stringr::str_glue(file_pattern, year = years_to_import) %>%
    paste0(base_url, .)

  enoe_list <- purrr::map(full_urls, function(url) {
    cat("Importando:", url, "\n")
    rio::import(url, trust = TRUE)
  })

  names(enoe_list) <- paste0("enoe_", years_to_import)


  # =============================================================================
  # FASE 3: ARMONIZACIÓN DE DATOS Y EXTRACCIÓN DE ETIQUETAS
  # =============================================================================
  cat("--- Fase 3: Armonización y Extracción de Etiquetas ---\n")

  master_recipe <- tibble::tribble(
    ~year_range,      ~original_name, ~final_name,   ~final_label,
    2018:2020,        "est_d",        "est_d_tri",   "Estrato de diseño trimestral",
    2018:2020,        "t_loc",        "t_loc_tri",   "Tamaño de localidad trimestral",
    2018:2020,        "cs_p20_des",   "cs_p21_des",  "Clave de desagregación (CS_P21)",
    2018:2020,        "cs_p22_des",   "cs_p23_des",  "Clave de desagregación (CS_P23)"
  )

  # Se crea un diccionario para almacenar todas las etiquetas.
  master_label_dictionary <- list()

  harmonized_list <- purrr::imap(enoe_list, function(df, name) {
    year <- as.numeric(stringr::str_extract(name, "\\d{4}"))
    cat("Procesando y limpiando datos para el año:", year, "\n")

    recipe_for_year <- master_recipe %>% filter(purrr::map_lgl(year_range, ~ year %in% .x))

    if (nrow(recipe_for_year) > 0) {
      rename_map <- setNames(recipe_for_year$original_name, recipe_for_year$final_name)
      df <- dplyr::rename(df, !!!rename_map)
    }

    if (year %in% 2018:2020) {
      df <- df %>% mutate(
        ur = recode(ur, "Urbano" = "Muestra urbana", "Rural" = "Muestra complemento y rural"),
        zona = recode(zona, 'Zona ""A""' = "Zona 2 Resto del paí­s", 'Zona ""B""' = "Zona 1 Frontera norte")
      )
    }

    if (year %in% 2018:2019) {
      df <- df %>% mutate(rama_est2 = recode(rama_est2, "Gobierno y organismos internacion" = "Gobierno y organismos internacionales"))
    }

    # ** INICIO DE LA EXTRACCIÓN DE ETIQUETAS **
    # Después de renombrar, se extraen las etiquetas y se guardan en el diccionario.
    original_labels <- sapply(df, rk.get.label)
    for (col_name in names(original_labels)) {
      label <- original_labels[[col_name]]
      # Solo se añade la etiqueta si no es NULL y no existe previamente,
      # para evitar sobrescribir una etiqueta válida con una vacía.
      if (!is.null(label) && !is.null(col_name) && !(col_name %in% names(master_label_dictionary))) {
        master_label_dictionary[[col_name]] <<- label
      }
    }
    # ** FIN DE LA EXTRACCIÓN DE ETIQUETAS **

    df %>% mutate(year = year)
  })

  # Se adjunta el diccionario de etiquetas a la lista como un atributo para pasarlo a la siguiente fase.
  attr(harmonized_list, "master_labels") <- master_label_dictionary
  rm(enoe_list)


  # =============================================================================
  # FASE 4: COMBINACIÓN, TRANSFORMACIÓN FINAL Y REAPLICACIÓN DE ETIQUETAS
  # =============================================================================
  cat("--- Fase 4: Combinando, Transformando y Re-etiquetando ---\n")

  # Se recupera el diccionario de etiquetas desde el atributo de la lista.
  final_labels_dictionary <- attr(harmonized_list, "master_labels")

  # Se estandarizan los tipos de datos para una combinación segura.
  all_cols <- purrr::map(harmonized_list, names) %>% unlist() %>% unique()
  cols_to_standardize <- purrr::map_lgl(all_cols, function(col) {
    types <- purrr::map_chr(harmonized_list, ~ class(.x[[col]])[1])
    length(unique(na.omit(types))) > 1
  }) %>% all_cols[.]

  if(length(cols_to_standardize) > 0) cat("  Estandarizando columnas:", paste(cols_to_standardize, collapse = ", "), "\n")

  consistent_list <- purrr::map(harmonized_list, ~ dplyr::mutate(.x, across(any_of(cols_to_standardize), as.character)))
  rm(harmonized_list)

  # Se combinan los datos.
  cat("  Combinando los datos...\n")
  final_df <- dplyr::bind_rows(consistent_list)
  rm(consistent_list)

  # Se realizan las transformaciones finales.
  cat("  Aplicando transformaciones finales...\n")
  p4d_codes <- list(organismo = 1:3, iglesia = 4:7, no_sabe = 8, ninguna = 9)
  final_df <- final_df %>%
    mutate(
      p4d2_numeric = as.numeric(p4d2),
      p4d2_1 = as.factor(case_when(
        p4d2_numeric %in% p4d_codes$organismo ~ "Organismo autónomo (IFE, Institutos Estatales Electorales, Comisiones Nacionales o Estatales de Derechos Humanos)",
        p4d2_numeric %in% p4d_codes$iglesia   ~ "Iglesia, asociación profesional, cámara o sindicato",
        p4d2_numeric %in% p4d_codes$no_sabe   ~ "No sabe",
        p4d2_numeric %in% p4d_codes$ninguna   ~ "Ninguna de las anteriores",
        TRUE ~ NA_character_
      )),
      ent.z = as.factor(case_when(
        ent %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Coahuila", "Chihuahua", "Durango", "Nayarit", "Nuevo León", "Sinaloa", "Sonora", "Tamaulipas", "Zacatecas") ~ "Norte",
        ent %in% c("Colima", "Ciudad de México", "Guanajuato", "Hidalgo", "Jalisco", "México", "Michoacán", "Querétaro", "San Luis Potosí") ~ "Centro",
        TRUE ~ "Sur-Este"
      )),
      n = 1
    ) %>%
    select(-any_of(c("ca", "fac", "est_d", "t_loc", "cs_p20_des", "cs_p22_des", "p4d2_numeric")))

  # Se reaplican TODAS las etiquetas guardadas.
  cat("  Re-aplicando todas las etiquetas de variables...\n")

  # Añadir etiquetas de variables nuevas o renombradas al diccionario para asegurar que se apliquen.
  recipe_labels <- setNames(master_recipe$final_label, master_recipe$final_name)
  new_labels <- list("year" = "Año de la encuesta", "p4d2_1" = "Institución recodificada", "ent.z" = "Zona de Entidad Federativa", "n" = "Conteo para encuestas")
  final_labels_dictionary <- c(final_labels_dictionary, recipe_labels, new_labels)

  for (col_name in names(final_df)) {
    if (col_name %in% names(final_labels_dictionary)) {
      rk.set.label(final_df[[col_name]], final_labels_dictionary[[col_name]])
    }
  }

  # =============================================================================
  # FASE 5: ASIGNACIÓN AL ENTORNO GLOBAL
  # =============================================================================
  .GlobalEnv$combined_df <- final_df

})

cat("\n--- Proceso Completado Exitosamente ---\n")
cat("El data frame 'combined_df' ha sido creado en el entorno global con todas las etiquetas preservadas.\n")


