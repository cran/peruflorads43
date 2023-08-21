#' The matching algorithm
#' @keywords internal
.match_algorithm  <- function(splist_class,
                              max_distance,
                              progress_bar = FALSE,
                              keep_closest = TRUE,
                              genus_fuzzy = TRUE,
                              grammar_check = FALSE) {
  # N species
  n_sps <- nrow(splist_class)

  # N classes
  n_class <- ncol(peruflorads43::ds43_2006_sps_class)

  # Save results
  exact <- matrix(ncol = n_class + 1, nrow = n_sps)


  # Loop across species
  if (progress_bar) {
    pb <- utils::txtProgressBar(min = 0,
                                max = n_sps,
                                style = 3)
  }
  for (i in seq_len(n_sps)) {
    splist_class_i <- splist_class[i, ]
    check_non_defined <-
      splist_class_i[3] %in% c("SP", "SP.",
                               "SPEC.", "AGG.")
    if (!check_non_defined) {
      # Search genus position
      max_distance2 <- ifelse(genus_fuzzy, max_distance, 0)
      pos_genus_pre <- .group_ind(
        group_name = splist_class_i[2],
        group_ref = peruflorads43::tab_ds43_2006_position$genus,
        max_distance2,
        only_one = FALSE,
        closest = TRUE
      )
      pos_genus <- .genus_search(pos_genus_pre)

      if (!any(is.na(pos_genus))) {
        # Try exact match first
        exact[i,] <- .exact_match(splist_class_i,
                                  pos_genus,
                                  n_class)
        # Try common grammar errors
        if (any(is.na(exact[i, ])) & grammar_check) {
          epis <- .sub_common(splist_class_i[3])
          if (length(epis) > 0) {
            names_grammar <- paste(splist_class_i[2],
                                   epis)
            splist_class_i_mult <- .splist_classify(names_grammar)
            n_gram <- length(names_grammar)
            temp <- matrix(nrow = n_gram,
                           ncol = length(exact[i, ]))
            for (k in seq_len(n_gram)) {
              temp[k, ] <- .exact_match(splist_class_i_mult[k, ],
                                        pos_genus,
                                        n_class)
            }
            pos_gram <- apply(temp, 1, function(x) {all(!is.na(x))})
            n_pos_gram <- sum(pos_gram)
            if (n_pos_gram > 1) {
              pos_gram_t <- pos_gram == TRUE
              pos_gram[pos_gram_t] <- c(TRUE, rep(FALSE, (n_pos_gram - 1)))
              temp[pos_gram, ncol(temp)] <- TRUE
            }
            if (any(pos_gram)) {
              exact[i,] <- temp[pos_gram, ]
            }
          }
        }

        # Try fuzzy
        if (any(is.na(exact[i, ])) & max_distance > 0) {
          exact[i,] <- .fuzzy_match(splist_class_i,
                                    pos_genus,
                                    max_distance,
                                    n_class,
                                    keep_closest = keep_closest,
                                    max_distance2 = max_distance2)
        }
      }
    }
    if (progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  if (progress_bar) {
    close(pb)
  }
  return(exact)
}




#-------------------------------------------------------#
# Exact match function
#' @keywords internal
.exact_match <- function(splist_class_i,
                         pos_genus,
                         n_class,
                         fuzzy = FALSE) {
  # Look the categories that are equal
  sp_pos <- apply(peruflorads43::ds43_2006_sps_class[pos_genus, -n_class,
                                                   drop = FALSE],
                  1,
                  function(x) {
                    x == splist_class_i
                  })

  # Identify the actual number positions
  choosen <- which(sp_pos[3, ])

  # Set homonyms FALSE
  homonyms <- FALSE


  # Work when fuzzy
  if (fuzzy) {
    choosen <- seq_len(ncol(sp_pos))
  }


  n_choosen <- length(choosen)

  # IF NEEDED INSERT COMMON ERRORS HERE
  if (n_choosen == 0) {
    # No match found
    return(rep(NA, n_class + 1))
  } else {
    # if there is more than one matched genus and epithet
    # keep the one with more matches across subcategories
    if (n_choosen > 1) {
      sum_equals <- colSums(sp_pos[, choosen])
      pos_equals <- sum_equals == max(sum_equals)
      choosen <- choosen[pos_equals]
      if (length(choosen) > 1) {
        matched_sp <-
          peruflorads43::ds43_2006_sps_class[pos_genus, , drop = FALSE][choosen, "id"]
       # status_pos <-
       #  peruflorads43::tab_ds43_2006[as.numeric(matched_sp), "taxonomic_status"]
       # test_accepted <- status_pos == "Accepted"
       # if (any(test_accepted)) {
       #   choosen <- choosen[test_accepted][1]
       #    homonyms <- TRUE
       #  } else {
       #    homonyms <- TRUE
       #    choosen <- choosen[1]
       #  }
      }
    }


    # Pick matched species ID
    matched_sp <-
      peruflorads43::ds43_2006_sps_class[pos_genus, , drop = FALSE][choosen, "id"]

    # Concatenate with the matching info
    matched_result <- c(matched_sp, sp_pos[, choosen], homonyms)

    return(matched_result)
  }
}

#-------------------------------------------------------#
# Fuzzy matching function
#' @keywords internal
.fuzzy_match <- function(splist_class_i,
                         pos_genus = NULL,
                         max_distance,
                         n_class,
                         return_all = FALSE,
                         keep_closest = TRUE,
                         max_distance2 = max_distance) {
  # If we did not find an approximation of the genus
  fuzzy_match <- NULL
  if (!is.null(pos_genus)) {
    # Use the `.agrep_whole` function with the max_distance parameter
    name1 <- paste(splist_class_i[2], splist_class_i[3])
    name2 <- paste(peruflorads43::ds43_2006_sps_class[pos_genus, 2],
                   peruflorads43::ds43_2006_sps_class[pos_genus, 3])
    fuzzy_match <- .agrep_whole(name1,
                                name2,
                                max_distance = max_distance)
  }
  if (is.null(pos_genus) | length(fuzzy_match) == 0) {
    pos_genus <- seq_len(nrow(peruflorads43::ds43_2006_sps_class))
    # Use the `.agrep_whole` function with the max_distance parameter
    name1 <- paste(splist_class_i[2], splist_class_i[3])
    name2 <- paste(peruflorads43::ds43_2006_sps_class[, 2],
                   peruflorads43::ds43_2006_sps_class[, 3])
    fuzzy_match <- .agrep_whole(name1,
                                name2,
                                max_distance = max_distance2)
  }

  if (length(fuzzy_match) == 0) {
    # No match found
    return(rep(NA, n_class + 1))
  } else {
    if (keep_closest) {
      # Keep the closest
      dist_names <- utils::adist(name1, name2[fuzzy_match])
      which_closest <- which(dist_names == min(dist_names))
      fuzzy_match <- fuzzy_match[which_closest]
    }
    # Reuse the exact_match function, but look only for fuzzy matches
    pos_genus <-
      as.numeric(peruflorads43::ds43_2006_sps_class[pos_genus, "id"][fuzzy_match])
    n_pos_genus <- length(pos_genus)

    res_fuzzy <- matrix(nrow = n_pos_genus, ncol = n_class + 1)

    for (i in seq_len(n_pos_genus)) {
      res_fuzzy[i, ] <- .exact_match(splist_class_i,
                                     pos_genus[i],
                                     n_class,
                                     fuzzy = TRUE)
    }
    # keep only the ones with highest number of classes matches
    rights <- apply(res_fuzzy[, -1, drop = FALSE],
                    1,
                    function(x) {
                      sum(x == "TRUE")
                    })
    pos_genus2 <- which(rights == max(rights))
    # If more than one
    if (!return_all) {
      if (length(pos_genus2) > 1) {
        sub_tab <- peruflorads43::tab_ds43_2006[res_fuzzy[pos_genus2, 1], ]
        #pos_genus2 <- which(sub_tab$taxonomic_status == "Accepted")
        res_fuzzy[, n_class + 1] <- TRUE # homonyms to TRUE
        #if (length(pos_genus2) == 0) {
        #  pos_genus2 <- 1
        #}
      }
      return(res_fuzzy[pos_genus2[1], ])
    }
    if (return_all) {
      return(res_fuzzy[pos_genus2, 1])
    }
  }
}
# -------------------------------------------------------------------------
# search genus
#-------------------------------------------------------#
# Function to wrap .genus_search for multiple genus names
#' @keywords internal
.genus_search_multiple <- function(gen_pos) {
  # Length of genus positions
  n_positions <- length(gen_pos)
  # List to result
  gen_pos_mult <- list()
  # Loop to apply the individual functions
  for (i in seq_len(n_positions)) {
    gen_pos_mult[[i]] <- .genus_search(gen_pos[i])
  }
  if(!all(is.na(gen_pos))) {
    # Genus names in the list
    names(gen_pos_mult) <- peruflorads43::tab_ds43_2006_position$genus[gen_pos]
  }
  # Return the list with the positions
  return(gen_pos_mult)
}

#-------------------------------------------------------#
# Transform group match into actual genus positions
#' @keywords internal
.genus_search <- function(group_pos) {
  #group_pos = result from group_search
  if (all(is.na(group_pos))) {
    return(NA)
  } else {
    # Identify their actual positions
    gen_pos <- NULL
    for (k in seq_along(group_pos)) {
      # Get the genus start and end position
      genus_sequence <- c(group_pos[k], group_pos[k] + 1)
      tab_gen_pos <- peruflorads43::tab_ds43_2006_position[genus_sequence, 1]
      # For the last one sequence to the end of the table
      if (is.na(tab_gen_pos[2])) {
        gen_pos <- c(gen_pos, tab_gen_pos[1]:nrow(peruflorads43::tab_ds43_2006))
      } else {
        # Now sequence over it
        gen_pos <- c(gen_pos, tab_gen_pos[1]:(tab_gen_pos[2] - 1))
      }
    }
    # Generate a vector to use for searching
    return(gen_pos)
  }
}

# -------------------------------------------------------------------------
# group_search
#-------------------------------------------------------#
# Function to wrap .lcvp_group_ind for multiple names
.group_pt <- function(group_names,
                      group_ref,
                      max_distance) {
  # group_names = list of names to be searched
  # group_ref = reference species name
  # max_distance = fuzzy match distance allowed

  # Length of group names
  n_groups <- length(group_names)
  # Object to keep the results
  groups_pos <- numeric(n_groups)
  # Loop over all names applying the individual function
  for (i in 1:n_groups) {
    groups_pos[i] <- .group_ind(group_names[i],
                                group_ref,
                                max_distance)
  }
  # Result the position in the list
  return(groups_pos)
}


#-------------------------------------------------------#
# Function to search species names,
# based on group (genus, family, order)
#' @keywords internal
.group_ind <- function(group_name,
                       group_ref,
                       max_distance,
                       only_one = TRUE,
                       closest = FALSE) {
  # Get the position
  group_pos <- which(group_ref == group_name)
  # Fuzzy match if it did not work
  if (length(group_pos) == 0) {
    if (max_distance > 0) {
      group_pos <- .agrep_whole(group_name,
                                group_ref,
                                max_distance = max_distance)
      closest1 <- utils::adist(group_name, group_ref[group_pos])

      if (closest & length(group_pos) > 0) {
        which_closest1 <- which(closest1 == min(closest1))
        group_pos <- group_pos[which_closest1]
      }
      n_temp <- length(group_pos)

      if (n_temp > 1 & only_one) {
        # If more than one, get the closest
        which_closest <- which.min(utils::adist(group_name,
                                                group_ref[group_pos]))
        group_pos <- group_pos[which_closest] # choose more than one
      }
      if (n_temp == 0) {
        # if no match is found, return NA
        group_pos <- NA
      }
    } else {
      group_pos <- NA
    }
  }

  return(group_pos)
}
# -------------------------------------------------------------------------
# search excat names
search_sps <- function(splist,
                      max_distance = 0.2,
                      show_correct = FALSE,
                      genus_fuzzy = FALSE,
                      grammar_check = FALSE,
                      progress_bar = FALSE) {
  #hasData() # Check if LCVP is installed
  # Defensive function here, check for user input errors
  if (is.factor(splist)) {
    splist <- as.character(splist)
  }
  .names_check(splist, "splist")

  # Fix species name
  splist_std <- .names_standardize(splist)

  # Classify splist
  splist_class <- .splist_classify(splist_std)

  # Check binomial
  .check_binomial(splist_class, splist)

  # Now match
  matching <- .match_algorithm(splist_class,
                               max_distance,
                               progress_bar = progress_bar,
                               genus_fuzzy = genus_fuzzy,
                               grammar_check = grammar_check)

  # Elaborate the return object
  ## Return Null if it did not find anything
  if (all(is.na(matching))) {
    result_final <- NULL
    ## Return the matrix with matched species
  } else {
    comb_match <- matching[, -(1:2), drop = FALSE]
    # keep homonyms to the warning
    ho_pos <- ncol(comb_match)
    homonyms <- as.logical(comb_match[, ho_pos])
    homonyms[is.na(homonyms)] <- FALSE
    comb_match <- comb_match[, -ho_pos, drop = FALSE]

    comb_match <- as.matrix(apply(comb_match, 2, as.logical))

    if (ncol(comb_match) == 1) { # If only one column, need to be transposed
      comb_match <- t(comb_match)
    }
    # Transform in data.frame
    comb_match <- as.data.frame(comb_match)
    names_col <-
      colnames(peruflorads43::ds43_2006_sps_class)[-c(1,
                                                    ncol(peruflorads43::ds43_2006_sps_class))]

    colnames(comb_match) <- paste(names_col, "match", sep = "_")

    result_final <- data.frame("name_submitted" = splist,
                               peruflorads43::tab_ds43_2006[matching[, 1], , drop = FALSE])

    # Add whether the searched name matched each class,
    # will be used in the summary function
    attributes(result_final)$match.names <- comb_match
    # Remove row names
    rownames(result_final) <- NULL
    # Warning more than one match
    if (any(homonyms)) {
      warning(
        paste0(
          "More than one name was matched for some species. ",
          "Only the first 'Accepted' (if present) name was returned. ",
          "Consider using the function fuzzy_search ",
          "to return all names for these species:\n",
          paste(result_final[homonyms, 1], collapse = ", ")
        ),
        call. = FALSE
      )
      attributes(result_final)$matched_mult <- result_final[homonyms, 1]
    }
  }

  # If no match, give a warning
  if (is.null(result_final)) {
    warning(paste0("No match found for the species list provided.",
                   " Try increasing the 'max_distance' argument."))
  } else {
    if (show_correct) {

      result_final$Correct <- rowSums(comb_match[, 1:2, drop = FALSE]) == 2
    }
  }
  return(result_final)
}
# -------------------------------------------------------------------------
# search fuzzy names
#' @keywords internal
search_fuzzy_sps <- function(splist,
                            max_distance = 0.2,
                            genus_fuzzy = FALSE,
                            bind_result = TRUE,
                            keep_closest = TRUE) {
  #hasData() # Check if LCVP is installed
  # Defensive functions, check for user input errors
  ## Change factors in characters
  if (is.factor(splist)) {
    splist <- as.character(splist)
  }
  .names_check(splist, "splist")
 # .check_status(status)

  # Fix species name
  species_std <- .names_standardize(splist)

  # Classify species
  species_class <- .splist_classify(species_std)

  # Check binomial
  .check_binomial(species_class, splist)

  # Run individual algorithm to multiple species
  n_sps <- length(splist)
  result <- list()
  for (i in 1:n_sps) {
    result[[i]] <-
      .fuzzy_search_ind(species_class[i, , drop = FALSE],
                        max_distance,
                        keep_closest,
                        genus_fuzzy = genus_fuzzy)
  }

  # If need to bind the results
  if (bind_result) {
    result <- do.call(rbind, result)
    result <- result[!is.na(result[, 1]), , drop = FALSE]
    if (nrow(result) == 0) {
      return(NULL)
    }
  } else {
    names(result) <- splist
  }
  return(result)
}

#----------------------------------------------------
#' @keywords internal
.fuzzy_search_ind <- function(species_class,
                              max_distance,
                              keep_closest,
                              genus_fuzzy) {


  if (is.na(species_class[, 3])) {
    warning(paste0("'", species_class[, 1], "' does not include an epithet."),
            call. = FALSE)
    return(NA)
  } else {
    # Now match
    ## Get the genus  first
    max_distance2 <- ifelse(genus_fuzzy, max_distance, 0)
    gen_number <- .group_ind(species_class[1, 2],
                             peruflorads43::tab_ds43_2006_position$genus,
                             max_distance = max_distance2,
                             FALSE)
    pos_genus <- unlist(.genus_search_multiple(gen_number))
    n_class <- ncol(peruflorads43::ds43_2006_sps_class)

    if (!any(is.na(pos_genus))) {
      # Try fuzzy
      pos_res <- .fuzzy_match(species_class[1,],
                              pos_genus,
                              max_distance,
                              n_class,
                              return_all = TRUE,
                              keep_closest = keep_closest,
                              max_distance2 = max_distance2)
    } else {
      # Fuzzy if did not find the genus
      pos_res <- NULL
    }
    if (length(pos_res) > 0 & !all(is.na(pos_res))) {
      # Result
      result <- peruflorads43::tab_ds43_2006[pos_res, , drop = FALSE]
      ## names 1 and 2
      name1 <- paste(species_class[1, 2], species_class[1, 3])
      name2 <- paste(peruflorads43::ds43_2006_sps_class[as.numeric(pos_res), 2],
                     peruflorads43::ds43_2006_sps_class[as.numeric(pos_res), 3])

      # Add a column indicating the distance
      Name.Distance <- t(utils::adist(name1, name2))
      result <- cbind(result, Name.Distance)###add name_submited
      result <- result[order(Name.Distance), , drop = FALSE]
      #if (!all(c( "Accepted", "Synonym", "No opinion") %in% status)) {
      #  result <- result[result$taxonomic_status %in% status, , drop = FALSE]
      #}

      rownames(result) <- NULL
      return(result)
    } else {
      warning(paste0("No match found for ", "'", species_class[, 1], "'."),
              call. = FALSE)
      return(NA)
    }
  }
}


# -------------------------------------------------------------------------
#' @keywords internal

# .check_status <- function(status) {
#
#   status_valid <- c( "Accepted", "Synonym", "No opinion")
#   check <- all(status %in% status_valid)
#   if (!check) {
#     stop(paste0("status argument should be one of the following: ",
#                 paste0("'", status_valid, "'", collapse = ", "), ". Not ",
#                 paste0("'", status, "'", collapse = ", ")),
#          call. = FALSE)
#   }
# }



