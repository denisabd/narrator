#' Return a Table of all Templates Currently Available in the Package
#'
#' @return a tibble with all templates and functions in the packages
#' @export
#'
#' @examples list_templates()
list_templates <- function() {

  class(args(narrator::narrate_descriptive))

  pkg_funs <- ls("package:narrator")
  narrate_funs <- pkg_funs[!is.na(stringr::str_extract(pkg_funs, "narrate_"))]

  templates <- tibble::tibble(
    fun = character(),
    type = character(),
    name = character(),
    template = character()
  )

  for (fun in narrate_funs) {

    # Get all arguments from the function
    fun_args <- as.list(args(fun))

    # Identifying template functions
    nms <- names(fun_args)
    funs <- nms[!is.na(stringr::str_extract(nms, "template_"))]

    templates <- templates %>%
      dplyr::bind_rows(
        tibble::tibble(
          fun = fun,
          type = stringr::str_replace(fun, "narrate_", ""),
          name = funs,
          template = as.character(fun_args[funs])
        )
      )
  }

  return(templates)
}


