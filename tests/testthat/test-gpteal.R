test_that("internet_available works",{
  expect_equal(
    internet_available(),
    TRUE
  )
})

test_that("get_completions work",{
  out <- get_completions(
    prompt = "What is the capital of Great Britain?",
    max_tokens = 256
  )
  expect_type(
    out$choices[[1]]$message$content,
    "character"
  )
  expect_gt(
    out$usage$total_tokens,
    42
  )
  expect_equal(
    out$usage$prompt_tokens,
    42
  )
  expect_length(
    out,
    7
  )
})

test_that("enhance/translate/summarize narrative works", {
  out <- sales %>%
    dplyr::filter(Product %in% c("Tools", "Clothing", "Home")) %>%
    dplyr::group_by(Product, Region)  %>%
    dplyr::summarise(Sales = sum(Sales)) %>%
    narrate_descriptive() %>%
    enhance_narrative()

  expect_type(
    out,
    "character"
  )

  expect_length(
    out,
    1
  )

  out_translated <- translate_narrative(out, language = "Spanish")

  expect_type(
    out_translated,
    "character"
  )
  expect_length(
    out_translated,
    1
  )

  out_summarized <- summarize_narrative(out)

  expect_type(
    out_summarized,
    "character"
  )
  expect_length(
    out_summarized,
    1
  )
})
