#' @title Get DS043-2006-AG: Aprueban Categorizacion de Especies Amenazadas de Flora Silvestre. 13-07-2006 data
#'
#' @description
#' This function takes a species list and tries to match a name in the "DS043-2006-AG:
#'  Aprueban Categorizacion de Especies Amenazadas de Flora Silvestre", subseting
#' information for each species. If the name_submitted is a valid name, it will
#' be the duplicated in accepted_name column, else the accepted_name column will
#' display the closest name given the maximum distance defined in `max_distance`
#'
#' @param splist A character vector specifying the input taxon, each element
#' including genus and specific epithet and, potentially, infraspecific rank,
#' infraspecific name and author name.
#' Only valid characters are allowed (see \code{\link[base:validEnc]{base::validEnc}}).
#'
#' @param max_distance match when comparing the submitted name with the closest
#' name matches in the species listed in the "DS043-2006-AG: Aprueban Categorizacion de
#'  Especies Amenazadas de Flora Silvestre". The distance used is a generalized
#' Levenshtein distance that indicates the total number of insertions, deletions,
#' and substitutions allowed to match the two names. It can be expressed as an
#' integer or as the fraction of the binomial name.
#' For example, a name with length 10, and a max_distance = 0.1, allow only one
#' change (insertion, deletion, or substitution). A max_distance = 2, allows two
#' changes.
#'
#' @return A table with the accepted name and catalog data of the species.
#'
#' @export
#'
#' @examples
#'
#' splist <- c("Cleistocactus clavispinus",
#'              "Welfia alfredi",
#'              "Matucana haynei")
#' category_ds043_2006(splist)
#'
#'
category_ds043_2006 <- function(splist, max_distance = 0.2){
  sps_result <- search_sps(splist = splist, max_distance = max_distance)
  submitted_names = sps_result[!is.na(sps_result$name_submitted), 1]#,
  names_accepted <-  sps_result[, 8]#
  output_matrix <- matrix(nrow = length(submitted_names), ncol = 4 )
  for (i in seq_along(names_accepted)) {
    if(!is.na(names_accepted[i])){

      sps_x <- cbind(matrix(submitted_names[i], ncol = 1),
                     as.matrix(peruflorads43::ds_043_2006_ag[peruflorads43::ds_043_2006_ag$accepted_name == names_accepted[i],]))
      output_matrix[i,] <- sps_x
    }
    else {
      sps_x <- cbind(matrix(submitted_names[i], ncol = 1),
                     matrix(rep(NA_character_, 3), ncol = 3))
      output_matrix[i,] <- sps_x
    }
  }
  colnames(output_matrix) <- c("name_submitted", "category", "accepted_name", "accepted_family")
  rownames(output_matrix) <- NULL
  return(as.data.frame(output_matrix))
}
