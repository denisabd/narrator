test_that("V8 during .onLoad works", {
  expect_no_error({
    ct <<- V8::v8()
    ct$source(system.file("js/pluralize.js", package="narrator"))
  })
})
