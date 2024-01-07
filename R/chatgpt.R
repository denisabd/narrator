#' Check Internet Connection
#'
#' @noRd
internet_available <- function() {
  response <- tryCatch(
    httr::GET("http://google.com"),
    error = function(e) {
      return(NULL)
    })

  if (is.null(response)) {
    return(FALSE)
  }

  status <- httr::status_code(response)

  if (status == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Enhance the narrative output with ChatGPT
#'
#' @inheritParams narrate_descriptive
#' @param narrative List of narratives returned by narrate_* function, character vector or string that will be enhanced by ChatGPT
#' @param prompt Prompt to send to OpenAI API
#'
#' @return [character()] of narratives enhanced by ChatGPT
#' @export
#'
#' @examples
#' \dontrun{
#' narrative <- sales %>%
#' dplyr::filter(Product %in% c("Tools", "Clothing", "Home")) %>%
#'   dplyr::group_by(Product, Region)  %>%
#'   dplyr::summarise(Sales = sum(Sales)) %>%
#'   narrate_descriptive()
#'
#' enhance_narrative(narrative)
#' }
enhance_narrative <- function(
    narrative,
    prompt = "Improve the written narrative by adding better business language for the following:",
    openai_api_key = Sys.getenv("OPENAI_API_KEY"),
    max_tokens = 1024,
    temperature = 0,
    top_p = 1,
    frequency_penalty = 0,
    presence_penalty = 0
) {

  if (openai_api_key == "" | is.null(openai_api_key)) stop("Open AI API key is required to run the function")

  if (!internet_available()) {
    message("No internet connection")
    return(narrative)
  }

  if ("list" %in% class(narrative)) {
    narrative <- as.character(narrative)
  }

  narrative <- paste(narrative, collapse = " ")

  output <- paste0(prompt, narrative) %>%
    get_completions(
      openai_api_key = openai_api_key,
      max_tokens = max_tokens,
      temperature = temperature,
      top_p = top_p,
      frequency_penalty = frequency_penalty,
      presence_penalty = presence_penalty
    ) %>%
    parse_response()

  return(output)
}

#' Translate the narrative output with ChatGPT
#'
#' @inheritParams enhance_narrative
#' @param language Write a language to translate narrative to, please use English here, ex. "Spanish" not "Español"
#'
#' @return [character()] of narratives enhanced by ChatGPT
#' @export
#'
#' @examples
#' \dontrun{
#' narrative <- sales %>%
#' dplyr::filter(Product %in% c("Tools", "Clothing", "Home")) %>%
#'   dplyr::group_by(Product, Region)  %>%
#'   dplyr::summarise(Sales = sum(Sales)) %>%
#'   narrate_descriptive()
#'
#' narrative <- enhance_narrative(narrative)
#' translate_narrative(narrative, "Spanish")
#' }
translate_narrative <- function(
    narrative,
    prompt = "Using professional language translate the following text to",
    language,
    openai_api_key = Sys.getenv("OPENAI_API_KEY"),
    max_tokens = 1024,
    temperature = 0,
    top_p = 1,
    frequency_penalty = 0,
    presence_penalty = 0
) {

  if (openai_api_key == "" | is.null(openai_api_key)) stop("Open AI API key is required to run the function")

  if (!internet_available()) {
    message("No internet connection")
    return(narrative)
  }

  if ("list" %in% class(narrative)) {
    narrative <- as.character(narrative)
  }

  narrative <- paste(narrative, collapse = " ")

  output <- paste(prompt, language, ':', narrative) %>%
    get_completions(
      openai_api_key = openai_api_key,
      max_tokens = max_tokens,
      temperature = temperature,
      top_p = top_p,
      frequency_penalty = frequency_penalty,
      presence_penalty = presence_penalty
    ) %>%
    parse_response()

  return(output)
}

#' Summarize the narrative output with ChatGPT
#'
#' @inheritParams enhance_narrative
#'
#' @return [character()] of narratives enhanced by ChatGPT
#' @export
#'
#' @examples
#' \dontrun{
#' narrative <- sales %>%
#' dplyr::filter(Product %in% c("Tools", "Clothing", "Home")) %>%
#'   dplyr::group_by(Product, Region)  %>%
#'   dplyr::summarise(Sales = sum(Sales)) %>%
#'   narrate_descriptive()
#'
#' narrative <- enhance_narrative(narrative)
#' summarize_narrative(narrative)
#' }
summarize_narrative <- function(
    narrative,
    prompt = "Summarize the following narrative to make it shorter:",
    openai_api_key = Sys.getenv("OPENAI_API_KEY"),
    max_tokens = 1024,
    temperature = 0,
    top_p = 1,
    frequency_penalty = 0,
    presence_penalty = 0
) {

  if (openai_api_key == "" | is.null(openai_api_key)) stop("Open AI API key is required to run the function")

  if (!internet_available()) {
    message("No internet connection")
    return(narrative)
  }

  if ("list" %in% class(narrative)) {
    narrative <- as.character(narrative)
  }

  narrative <- paste(narrative, collapse = " ")

  output <- paste0(
    prompt,
    narrative
  ) %>%
    get_completions(
      openai_api_key = openai_api_key,
      max_tokens = max_tokens,
      temperature = temperature,
      top_p = top_p,
      frequency_penalty = frequency_penalty,
      presence_penalty = presence_penalty
    ) %>%
    parse_response()

  return(output)
}

#' Get GPT Completions Endpoint
#'
#' @param prompt The prompt to generate completions for.
#' @param openai_api_key OpenAI's API key.
#' @param max_tokens The maximum number of tokens to generate in the chat completion.
#' @param temperature What sampling temperature to use, between 0 and 2. Higher
#' values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic.
#' @param top_p An alternative to sampling with temperature, called nucleus sampling, where the model considers
#' the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#' @param frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far,
#' decreasing the model's likelihood to repeat the same line verbatim.
#' @param presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far,
#' increasing the model's likelihood to talk about new topics.
#'
#' @noRd
get_completions <- function(
    prompt,
    openai_api_key = Sys.getenv("OPENAI_API_KEY"),
    max_tokens = 1024,
    temperature = 0,
    top_p = 1,
    frequency_penalty = 0,
    presence_penalty = 0) {

  if (nchar(openai_api_key) == 0) {
    stop("`OPENAI_API_KEY` not provided.")
  }

  if (!internet_available()) {
    message("No internet connection")
    return(narrative)
  }

  # See https://platform.openai.com/docs/api-reference/chat
  # and https://beta.openai.com/docs/api-reference/completions/create
  model <- Sys.getenv("OPENAI_MODEL", "gpt-3.5-turbo")
  params <- list(
    model = model,
    max_tokens = as.numeric(Sys.getenv("OPENAI_MAX_TOKENS", max_tokens)),
    temperature = as.numeric(Sys.getenv("OPENAI_TEMPERATURE", temperature)),
    top_p = as.numeric(Sys.getenv("OPENAI_TOP_P", top_p)),
    frequency_penalty = as.numeric(Sys.getenv("OPENAI_FREQUENCY_PENALTY", frequency_penalty)),
    presence_penalty = as.numeric(Sys.getenv("OPENAI_PRESENCE_PENALTY", presence_penalty))
  )

  if (as.logical(Sys.getenv("OPENAI_VERBOSE", FALSE))) {
    cat(paste0("\n*** ChatGPT input:\n\n", prompt, "\n"))
  }

  if (grepl("gpt-3.5-turbo", model)) {
    return_language <- Sys.getenv("OPENAI_RETURN_LANGUAGE")

    if (nchar(return_language) > 0) {
      return_language <- paste0("You return all your replies in ", return_language, ".")
    }

    messages <- list(
      list(
        role = "system",
        content = paste(
          "You are a helpful assistant with extensive knowledge of professional and business language, experienced in creating text headlines and presentations.",
          return_language
        )
      ),
      list(role = "user", content = prompt)
    )

    output <- httr::content(httr::POST(
      "https://api.openai.com/v1/chat/completions",
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
      httr::content_type_json(),
      body = jsonlite::toJSON(c(params, list(messages = messages)), auto_unbox = TRUE)
    ))

  } else {
    output <- httr::content(httr::POST(
      "https://api.openai.com/v1/completions",
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
      httr::content_type_json(),
      body = jsonlite::toJSON(c(params, list(prompt = prompt)), auto_unbox = TRUE)
    ))
  }

  return(output)
}

#' Parse OpenAI API Response
#'
#' Takes the raw response from the OpenAI API and extracts the text content from it.
#' This function is currently designed to differentiate between gpt-3.5-turbo and others.
#'
#' @param raw_response The raw response object returned by the OpenAI API.
#'
#' @noRd
parse_response <- function(raw_response) {
  # If the model is from the `gpt-3.5-turbo` family, it parses in a different way.
  if (grepl("gpt-3.5-turbo", Sys.getenv("OPENAI_MODEL", "gpt-3.5-turbo"))) {
    trimws(sapply(raw_response$choices, function(x) x$message$content))
  } else {
    trimws(sapply(raw_response$choices, function(x) x$text))
  }
}
