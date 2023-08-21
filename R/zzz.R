
.onAttach <- function(lib, pkg) {
  packageStartupMessage("This is peruflorads043_2006_ag ",
                        utils::packageDescription("peruflorads43",
                                                  fields = "Version"
                        ),
                        appendLF = TRUE
  )
}


# -------------------------------------------------------------------------

show_progress <- function() {
  isTRUE(getOption("peruflorads043.show_progress")) && # user disables progress bar
    interactive() # Not actively knitting a document
}



.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_peruflorads043 <- list(
    peruflorads043.show_progress = TRUE
  )
  to_set <- !(names(opt_peruflorads043) %in% names(opt))
  if (any(to_set)) options(opt_peruflorads043[to_set])
  invisible()
}


# -------------------------------------------------------------------------
