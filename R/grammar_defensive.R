# Find a pattern at the end of the character
#' @keywords internal
.find_mat <- function(x, pattern) {
  n_c <- nchar(x)
  n_p <- nchar(pattern) - 1
  return(substr(x, n_c - n_p, n_c) == pattern)
}

# find lati or late
.sub_lat <- function(x) {
  if (all(substr(x, 1, 4) %in% c("LATE", "LATI"))) {
    substring(x, 1, 4) <- "LATE"
    x2 <- x
    substring(x2, 1, 4) <- "LATI"
    return(c(x, x2))
  } else {
    return(x)
  }
}

# Find which pattern matched
#' @keywords internal
.find_common <- function(x) {
  ei <- .find_mat(x, "EI")
  ii <- .find_mat(x, "II")
  i <- .find_mat(x, "I") & !ii & !ei
  iae <- .find_mat(x, "IAE")
  ae <- .find_mat(x, "AE") & !iae
  iifolia <- .find_mat(x, "IIFOLIA")
  iiflora <- .find_mat(x, "IIFLORA")
  ifolia <- .find_mat(x, "IFOLIA") & !iifolia
  iflora <- .find_mat(x, "IFLORA") & !iiflora
  iodes <- .find_mat(x, "IODES")
  oides <- .find_mat(x, "OIDES")
  odes <- .find_mat(x, "ODES") & !iodes
  stats::setNames(
    c(ei, ii, i, iae, ae, iifolia, iiflora, ifolia, iflora,
      iodes, oides, odes),
    c("ei", "ii", "i", "iae", "ae", "iifolia", "iiflora",
      "ifolia", "iflora", "iodes", "oides", "odes")
  )
}

# Substitute
#' @keywords internal
.sub_common <- function(x) {
  x0 <- x
  commons <- which(.find_common(x))
  n_c <- nchar(x)
  n_p <- nchar(names(commons))
  if (length(n_p) != 0) {
    base_str <- substr(x, 1, n_c - n_p)
    sub_str <-
      list(
        "EI" = 1:3,
        "II" = 2:3,
        "I" = 2:3,
        "IAE" = 2:5,
        "AE" = 2:5,
        "IIFOLIA" = c(6, 8),
        "IIFLORA" = c(7, 9),
        "IFOLIA" = c(6, 8),
        "IFLORA" = c(7, 9),
        "IODES" = 10:12,
        "OIDES" = 10:12,
        "ODES" = 10:12
      )
    x <- paste0(base_str, names(sub_str)[sub_str[[commons]]])
  }
  result <- .sub_lat(x)
  return(result[result != x0])
}



#-------------------------------------------------------#
# Function to check list of names input
#' @keywords internal
.names_check <- function(splist,
                         argument_name) {

  # Check if it is a character
  if (!is.character(splist) | !is.vector(splist)) {
    stop(paste0(argument_name,
                " should be a character vector, not '",
                paste(class(splist), collapse = " "), "'"),
         call. = FALSE)
  }
  enc_valid <- !validEnc(splist)

  # Check if it has invalid encoding
  if (any(enc_valid)) {
    stop(paste(argument_name,
               "should include only valid characters,",
               "please check the name(s) at position(s):",
               paste(which(enc_valid), collapse = ", ")),
         call. = FALSE)
  }
}


#-------------------------------------------------------#
# Check if names are binomial
#' @keywords internal
.check_binomial <- function(splist_class, splist) {

  missing_bino <- which(apply(splist_class[, 2:3, drop = FALSE],
                              1,
                              function(x) {any(is.na(x))}))
  if (length(missing_bino) > 0) {
    stop(paste0("splist should include only binomial names,",
                " please check the following names: ",
                paste(paste0("'", splist[missing_bino], "'"), collapse = ", ")),
         call. = FALSE)

  }
}
#-------------------------------------------------------#
# Make names standard
#' @keywords internal
.names_standardize <- function(splist) {
  fixed1 <- toupper(splist) # all up
  fixed2 <- gsub("CF\\.", "", fixed1)
  fixed3 <- gsub("AFF\\.", "", fixed2)
  fixed4 <- trimws(fixed3) # remove trailing and leading space
  fixed5 <- gsub("_", " ", fixed4) # change names separated by _ to space

  # Hybrids
  fixed6 <- gsub("(^X )|( X$)|( X )", " ", fixed5)
  hybrids <- fixed5 == fixed6
  if (!all(hybrids)) {
    sp_hybrids <- splist[!hybrids]
    warning(paste("The 'x' sign indicating hybrids have been removed in the",
                  "following names before search:",
                  paste(paste0("'", sp_hybrids, "'"), collapse = ", ")),
            immediate. = TRUE, call. = FALSE)
  }
  # Merge multiple spaces
  fixed7 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", fixed6, perl = TRUE)
  return(fixed7)
}

#-------------------------------------------------------#
# Function to match the closest fuzzy name
#' @keywords internal
.agrep_whole <- function(x, y, max_distance) {
  if (max_distance < 1 & max_distance > 0) {
    max_distance <- ceiling(nchar(x) * max_distance)
  }
  a <- utils::adist(x, y)
  return(which(a <= max_distance))
}
