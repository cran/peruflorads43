#' Species names list from DS043-2006-AG Aprueban Categorizacion de Especies
#' Amenazadas de Flora Silvestre
#' @name ds_043_2006_ag
#' @docType data
#' @format A tibble with the following columns:
#'   \describe{\item{categoria}{A character vector.}
#'   \item{accepted_name}{A character vector. The list of the accepted plant taxa names according to the Taxonomic Name Resolution Service - TNRS.}
#'   \item{accepted_family}{A character vector. The corresponding family name of the accepted_name.}}
#'
#' @references
#' DS043-2006-AG: Aprueban Categorizacion de Especies Amenazadas de Flora Silvestre. 13-07-2006
#'
#' @keywords datasets
#' @examples
#'
#' data(ds_043_2006_ag)
#' str(ds_043_2006_ag)
#'
"ds_043_2006_ag"


#' List of Plant Species Name accordingly with the DS043-200-AG.
#'
#' The 'tab_ds43_2006' contains records belonging to all the species DS043-200-AG.
#'
#' @name tab_ds43_2006
#' @docType data
#' @format A tibble with the following columns:
#'   \describe{\item{id_cat}{The fixed species id of the input taxon.}
#'   \item{input_genus}{A character vector. The input genus of the corresponding species name.}
#'   \item{input_epitheton}{A character vector. The specific epithet of the corresponding species name.}
#'   \item{rank}{A character vector. The taxonomic rank: "species","subspecies", "variety", of the corresponding species name.}
#'   \item{input_subspecies_epitheton}{A character vector. If the indicated rank is below species, the subspecies epithet input of the corresponding species name.}
#'   \item{taxonomic_status}{A character vector. description if a taxon is classified as ‘accepted’, ‘synonym’, ‘no opinion’. According to the Taxonomic Name Resolution Service - TNRS.}
#'   \item{accepted_name}{A character vector. The list of the accepted plant taxa names according to the Taxonomic Name Resolution Service - TNRS.}
#'   \item{accepted_family}{A character vector. The corresponding family name of the accepted_name.}
#'   \item{accepted_name_author}{A character vector. The corresponding author name of the accepted_name, staying empty if the taxonomic_status is "Synonym" or "No opinion".}}
#'
#' @references
#' DS043-2006-AG: Aprueban Categorizacion de Especies Amenazadas de Flora Silvestre. 13-07-2006
#'
#' @keywords datasets
#' @examples
#'
#' data(tab_ds43_2006)
#' str(tab_ds43_2006)
#'
"tab_ds43_2006"

#' List of species name in tab_ds43_2006 separeted by category
#'
#' The 'ds43_2006_sps_class' includes all species separeted by genus, epithet, author,
#' subspecies, variety, and id (position in the
#' \code{tab_ds43_2006}).
#'
#' @name ds43_2006_sps_class
#' @docType data
#' @format A data.frame.
#' @keywords datasets
#' @examples
#'
#' data(ds43_2006_sps_class)
#'
"ds43_2006_sps_class"

#' List of the number positions of the first 3 letters of the species name in
#' the tab_ds43_2006
#'
#' The 'tab_ds43_2006_position' reports the
#' position (in term of number of rows) of the first three letters (triphthong)
#' for the plant names stored in the variable 'accepted_name' of the table
#' 'tab_ds43_2006'. This indexing system speeds up of the search on the
#' largest list using the package.
#'
#'
#' @name tab_ds43_2006_position
#' @docType data
#' @format A data frame with 305 observations on the following 3 variables.
#' \describe{
#' \item{position}{A character vector. It is the position of the first 3 letters of the species name in the tab_ds43_2006.}
#' \item{triphthong}{A character vector. First 3 letters of the species name in the tab_ds43_2006.}
#' \item{genus}{A character vector. Corresponding Genus name.} }
#'
#' @keywords datasets
#' @examples
#'
#' data(tab_ds43_2006_position)
#' str(tab_ds43_2006_position)
"tab_ds43_2006_position"

