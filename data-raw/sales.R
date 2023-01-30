# Generating Synthetic Dataset
sales_date <- seq(as.Date('2019-01-01'), by = 'day', length.out = 365*3 + 1)

date <- sample(sales_date, 1000, replace = TRUE)
promotion <- rbinom(n=length(date), size = 1, p = 0.15)
price <- round(runif(n=length(date), min = 5.9, max = 280),1)
quantity <- sample(1:30, length(date), replace = TRUE)
territory <- sample(c("EMEA", "NA", "ASPAC", "LATAM"), length(date), replace = TRUE, prob = c(0.3, 0.45, 0.15, 0.1))
product <- sample(c("Product A", "Product B", "Product C", "Product D",
                    "Product E", "Product F", "Product H", "Product I"), length(date), replace = TRUE, prob = c(0.05, 0.2, 0.1, 0.05, 0.25, 0.15, 0.1, 0.1))

sales <- tibble::tibble(
  Date = date,
  Territory = territory,
  Product = product,
  Promotion = promotion,
  Price = price,
  Quantity = quantity) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(`Order ID` = paste0("QR", (dplyr::row_number() + 10000))) %>%
  dplyr::mutate(Sales = ifelse(Promotion == 1, Price*0.8*Quantity, Price*Quantity)) %>%
  dplyr::relocate(`Order ID`, .before = 1)

# Save Data
sales %>%
  readr::write_csv("data-raw/sales.csv")

usethis::use_data(sales, overwrite = TRUE)