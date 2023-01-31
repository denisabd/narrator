#' Correct the sentence using udpipe
#'
#' @param text Text string for correction
#' @param language Language for udpipe model, defaults to 'english'
#' @param measure Measure for the template
#'
#' @return Text string
#' @export
#'
#' @examples correct_text("In 2020 total Sales across all Regions (EMEA, NA, ASPAC) is equal to 23.5 M")
#' correct_text("Total Profit across all Regions are 85 M")
correct_text <- function(text, language = "english", measure = "") {

  # Split into sentences
  #unlist(strsplit(text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl = TRUE))

  udpipe_available <- require("udpipe")

  # If packages isn't available
  if (udpipe_available == FALSE) {
    warning("Function correct_text() requires udpipe package to run.
Please install udpipe package using 'install.packages('udpipe')'")
    return(text)
  }

  tryCatch(
    annotation <- udpipe::udpipe(text, language) %>%
      tibble::as_tibble(),

    error = function(e) {
      model_en <- udpipe_download_model(language = language)
      annotation <- udpipe::udpipe(text, "english") %>%
        tibble::as_tibble()
    }
  )

  subtable <- annotation %>%
    dplyr::select(token, token_id, upos, feats, head_token_id, dep_rel) %>%
    dplyr::mutate(plural = dplyr::case_when(stringr::str_detect(feats, "Number=Plur") ~ "plural",
                                            stringr::str_detect(feats, "Number=Sing") ~ "singular",
                                            upos %in% c("VERB", "AUX") & is_plural(token) ~ "plural",
                                            upos %in% c("VERB", "AUX") & is_singular(token) ~ "singular",
                                            token == measure & is_plural(token) ~ "plural",
                                            token == measure & is_singular(token) ~ "singular",
                                            TRUE ~ NA_character_)
    ) %>%
    dplyr::filter(upos %in% c("NOUN", "AUX", "VERB")) %>% # Filtering nouns and verbs
    dplyr::group_by(head_token_id) %>%
    dplyr::filter(dplyr::n() > 1 | dep_rel == "root") %>% # Only duplicated values of head_token_id, pointing to a single term
    dplyr::filter(dplyr::n_distinct(plural) > 1 | dep_rel == "root")

  if (nrow(subtable) == 0) return(text)

  for (i in unique(subtable$head_token_id)) {

    form <- subtable %>%
      dplyr::filter(head_token_id == i,
                    upos == "NOUN") %>%
      dplyr::pull(var = plural)

    if (length(form) == 0) {
      next
    } else {
      form <- form[1]
    }

    if (form == "plural") {
      subtable <- subtable %>%
        dplyr::mutate(token_fixed = pluralize(token))
    } else {
      subtable <- subtable %>%
        dplyr::mutate(token_fixed = singularize(token))
    }

    # If root is available
    if (i == "0") {
      text <- annotation %>%
        dplyr::left_join(subtable) %>%
        suppressMessages() %>%
        dplyr::mutate(token_fixed = ifelse(is.na(token_fixed), token, token_fixed)) %>%
        dplyr::select(token_fixed) %>%
        as.matrix() %>%
        as.character() %>%
        paste(collapse = " ")

      return(text)
    }
  }

  text <- annotation %>%
    dplyr::left_join(subtable) %>%
    suppressMessages() %>%
    dplyr::mutate(token_fixed = ifelse(is.na(token_fixed), token, token_fixed)) %>%
    dplyr::select(token_fixed) %>%
    as.matrix() %>%
    as.character() %>%
    paste(collapse = " ")

  return(text)
}
