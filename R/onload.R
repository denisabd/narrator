# holding place for things we want to keep around but not visible
ct <- NULL

#' @import V8
.onLoad <- function(libname, pkgname) {

  # read in the suffixes using V8
  ct <<- V8::v8()

  ct$source(system.file("js/pluralize.js", package="narrator"))

}

utils::globalVariables(c("cum_share", "dep_rel", "feats", "head_token_id", "lag",
                         "plural", "share", "token", "token_fixed", "token_id",
                         "upos", "value"))
