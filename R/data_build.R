#' @keywords internal
# perutimber_especies list ------------------------------------------------
#' lista de especies presentes en la base de datos perutimber
#' se debe conciderar que las combinaciones de input_genus, input_epitheton,
#' input_subspecies_epitheton son unicos,
#' los valores de  accepted_name pueden repetirse
#'
# comentario ##     ds_df <- readxl::read_xlsx("data/data_ds043_2006.xlsx") |>
# comentario ##      dplyr::mutate(id_cat = paste0( dplyr::row_number(),
# comentario ##                                     stringr::str_sub(input_genus, 1, 2),
# comentario ##                                     stringr::str_sub(input_epitheton, 1, 2)) |>
# comentario ##                      toupper()) |>
# comentario ##      dplyr::mutate_all(~stringr::str_trim(.) |>  stringr::str_squish()) |>
# comentario ##      dplyr::mutate(especie = dplyr::if_else(is.na(especie),
# comentario ##                                               tnrs_especies,
# comentario ##                                               especie))
# comentario ##
# comentario ##     ds_df |>  dplyr::distinct(fuente)
# comentario ##  ds_df |>  dim()
# comentario ##  # revisa que los nombres completos sean unicos
# comentario ##  ds_df |>
# comentario ##   dplyr::filter(!stringr::str_detect(especie, "forma")) |>
# comentario ##   #dplyr::filter(fuente == "original") |>
# comentario ##   dplyr::add_count(scientific_name) |>
# comentario ##   dplyr::filter(n > 1) |>
# comentario ##   dplyr::distinct(scientific_name, categoria, n, fuente, especie)
# comentario ##  # para tener registros unicos se retiran los registros de las especies que
# comentario ##  # se listan como formas
# comentario ##  library(tidyverse)
# comentario ##  ds_df <- ds_df |>
# comentario ##   dplyr::filter(!stringr::str_detect(especie, "forma")) |>
# comentario ##   dplyr::rename(accepted_family = tnrs_familia,
# comentario ##                 rank = accepted_name_rank,
# comentario ##                 accepted_name = tnrs_especies)
# comentario ##  ds_df |>
# comentario ##  distinct(fuente)
# comentario ##  # TAB_PERUTIMBER ----------------------------------------------------------
# comentario ##  #' esta base de datos debe ser guardada en una base de datos de tipo
# comentario ##  #' data.frame
# comentario ##  names_tab_ds43 <- c("id_cat", "input_genus", "input_epitheton",
# comentario ##                           "rank", "input_subspecies_epitheton", "taxonomic_status",
# comentario ##                           "accepted_name", "accepted_family",
# comentario ##                           "accepted_name_author")
# comentario ##  names_tab_ds43|> length()
# comentario ##  tab_ds43_2006 <- ds_df |>
# comentario ##   dplyr::select( dplyr::all_of(names_tab_ds43)) |>
# comentario ##   as.data.frame()
# comentario ##  tab_ds43_2006 |>
# comentario ##   pesa::check_na()
# comentario ##  # PERUTIMBER SPS CLASS ----------------------------------------------------
# comentario ##  #' esta debe almacenarce como matrx o array
# comentario ##  names_sps_class <- c("species", "genus", "epithet", "author",
# comentario ##                      "subspecies", "variety",
# comentario ##                      "subvariety", "forma", "subforma", "id")
# comentario ##  ds43_2006_sps_class <-
# comentario ##  ds_df |>
# comentario ##   dplyr::rename(species = scientific_name,
# comentario ##                 genus = input_genus,
# comentario ##                 epithet = input_epitheton,
# comentario ##                 author = accepted_name_author,
# comentario ##                 subspecies = subspecie,
# comentario ##                 variety = variedad) |>
# comentario ##   dplyr::mutate(subvariety = "",
# comentario ##                 forma = "",
# comentario ##                 subforma = "",
# comentario ##                 id =  dplyr::row_number()) |>
# comentario ##   dplyr::select( dplyr::all_of(names_sps_class)) |>
# comentario ##   dplyr::mutate_all(~as.character(.) |>
# comentario ##                       toupper()) |>
# comentario ##   dplyr::mutate(subspecies  = dplyr::if_else(is.na(subspecies),
# comentario ##                                              "",
# comentario ##                                              subspecies),
# comentario ##                 variety  = dplyr::if_else(is.na(variety),
# comentario ##                                              "",
# comentario ##                                              variety)) |>
# comentario ##   as.matrix.data.frame()
# comentario ##  ds43_2006_sps_class |>
# comentario ##   head()
# comentario ##   #class()
# comentario ##
# comentario ##  # -------------------------------------------------------------------------
# comentario ##  ds_043_2006_ag <- ds_df |>
# comentario ##    dplyr::select(categoria,
# comentario ##                  accepted_name = tnrs_especies,
# comentario ##                  accepted_family = tnrs_familia) |>
# comentario ##    dplyr::distinct() |>
# comentario ##    as.data.frame()
# comentario ##
# comentario ##  ds_043_2006_ag
# comentario ##
# comentario ##  #'
# comentario ##  #'
# comentario ##  #'
# comentario ##  # TAB POSSITION -----------------------------------------------------------
# comentario ##  #' esta debe ser guardada como data.frame
# comentario ##  #' perutimber::tab_perutimber_position
# comentario ##  names_posistion = c("position", "triphthong", "genus")
# comentario ##  tab_ds43_2006_position <- ds_df |>
# comentario ##   dplyr::select(genus = input_genus) |>
# comentario ##   dplyr::mutate(id =  dplyr::row_number(),
# comentario ##                 triphthong = stringr::str_sub(genus, 1, 3)) |>
# comentario ##   dplyr::mutate_if(is.character, ~toupper(.)) |>
# comentario ##   dplyr::group_by(genus, triphthong) |>
# comentario ##   dplyr::summarise(position = min(id),
# comentario ##                    .groups = "drop") |>
# comentario ##   dplyr::arrange(position) |>
# comentario ##   as.data.frame() |>
# comentario ##   dplyr::select(dplyr::all_of(names_posistion))
# comentario ##  tab_ds43_2006_position |>  head()
# comentario ##  tab_ds43_2006_position |>  dim()
# comentario ##  # save clean data ---------------------------------------------------------
# comentario ##  tab_ds43_2006 |>
# comentario ##   save(file = "data/tab_ds43_2006.rda")
# comentario ##  ds43_2006_sps_class |>
# comentario ##   save(file = "data/ds43_2006_sps_class.rda")
# comentario ##  tab_ds43_2006_position |>
# comentario ##  save(file = "data/tab_ds43_2006_position.rda")
# comentario ##  ds_043_2006_ag |>
# comentario ##    save(file = "data/ds_043_2006_ag.rda")
