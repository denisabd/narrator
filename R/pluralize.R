#' Pluralize a word
#'
#' @param x character vector of words to make plural
#' @param n number of plural (some plurals change by amount and this also impacts
#'        the \code{prepend} functionality)
#' @param prepend should we prepend the value of \code{n} to the output? Default: \code{FALSE}
#' @return character vector of modified pluralized words
#' @export
#'
#' @examples
#' pluralize('test')
#' pluralize('test', 5)
#' pluralize('test', 5, TRUE)
#' pluralize('regex')
pluralize <- function(x, n=2, prepend=FALSE) {

  unname(sapply(x, function(y) {
    ct$call("pluralize", y, n, prepend)
  }
  )
  )
}

#' Singularize a word
#'
#'
#' @param x vector of words to make singular
#' @return modified character vector of singularized words
#' @export
#' @examples
#' singularize('test')
#' singularize(c("boats", "houses", "cats", "rivers"))
#' pluralize(singularize(c("boats", "houses", "cats", "rivers")))
#' singularize(c("buses", "wishes", "pitches", "boxexs"))
#' pluralize(singularize(c("buses", "wishes", "pitches", "boxexs")))
#' singularize(c("pennies", "spies", "babies", "cities", "daisies"))
#' pluralize(singularize(c("pennies", "spies", "babies", "cities", "daisies")))
#' singularize(c("sheep", "fish", "deer", "species", "aircraft"))
#' pluralize(singularize(c("sheep", "fish", "deer", "species", "aircraft")))
singularize <- function(x) {
  unname(sapply(x, function(y) {
    ct$call("pluralize", y, 1, FALSE)
  }
  ))
}

#' Test plural state of a word
#'
#' @param x vector of words to test
#' @return logical vector
#' @export
#' @examples
#' is_singular(c("boats", "house", "cats", "river"))
#' is_plural(c("boats", "house", "cats", "river"))
is_plural <- function(x) {
  unname(
    sapply(x, function(y) {
      ct$call("pluralize.isPlural", y)
    })
  )
}

#' @rdname is_plural
#' @export
is_singular <- function(x) {
  unname(
    sapply(x, function(y) {
      ct$call("pluralize.isSingular", y)
    })
  )
}
