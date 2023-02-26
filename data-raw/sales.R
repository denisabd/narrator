# Generating Synthetic Dataset
sales_date <- seq(as.Date('2019-01-01'), by = 'day', length.out = 365*3 + 1)

date <- sample(sales_date, 10000, replace = TRUE)
promotion <- rbinom(n=length(date), size = 1, p = 0.15)
price <- round(runif(n=length(date), min = 5.9, max = 280),1)
quantity <- sample(1:30, length(date), replace = TRUE)
Region <- sample(c("EMEA", "NA", "ASPAC", "LATAM"),
                 length(date), replace = TRUE,
                 prob = c(0.3, 0.4, 0.16, 0.14))
Store <- sample(c("Big", "Medium", "Small"),
                 length(date),
                 replace = TRUE,
                 prob = c(0.65, 0.25, 0.1))
product <- sample(c("Baby", "Electronics", "Food & Beverage", "Clothing", "Home", "Tools"),
                  length(date),
                  replace = TRUE,
                  prob = c(0.08, 0.2, 0.35, 0.07, 0.15, 0.15))

sales <- tibble::tibble(
  Date = date,
  Region = Region,
  Product = product,
  Store = Store,
  Promotion = promotion,
  Price = price,
  Quantity = quantity) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(`Order ID` = paste0("QR", (dplyr::row_number() + 10000))) %>%
  dplyr::mutate(Quantity = ifelse(Region %in% c("EMEA", "NA"), round(Quantity*1.5), Quantity),
                Price = ifelse(Region %in% c("EMEA", "NA"), round(Price*1.3, 1), Price)) %>%
  dplyr::mutate(Quantity = dplyr::case_when(
    lubridate::month(Date) %in% c(10, 11, 12) ~ round(Quantity*(lubridate::month(Date)^2/80)),
    lubridate::month(Date) %in% c(1, 6, 7, 8) ~ round(Quantity*0.7),
    TRUE ~ Quantity
  )
  ) %>%
  # Quantity for 3 Products is decreasing and others increasing
  dplyr::mutate(Quantity = dplyr::case_when(
    Product %in% c("Clothing", "Home", "Tools") ~ round(Quantity*(1 - (lubridate::year(Date)-2018)/sample(c(10, 12, 14, 16), size = 1))),
    Product %in% c("Baby", "Electronics", "Food & Beverage") ~ round(Quantity*(1 + (lubridate::year(Date)-2018)/sample(c(7, 8, 9, 10, 12), size = 1))),
    TRUE ~ Quantity
  )
  ) %>%
  dplyr::mutate(Sales = ifelse(Promotion == 1, Price*0.8*Quantity, Price*Quantity)) %>%
  dplyr::relocate(`Order ID`, .before = 1) %>%
  dplyr::sample_frac()

# Save Data
sales %>%
  readr::write_csv("data-raw/sales.csv")

usethis::use_data(sales, overwrite = TRUE, internal = FALSE)
