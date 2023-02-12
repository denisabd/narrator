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
product <- sample(c("Baby", "Electronics", "Food and Beverage", "Clothing", "Home", "Tools"),
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
  dplyr::mutate(Sales = ifelse(Promotion == 1, Price*0.8*Quantity, Price*Quantity)) %>%
  dplyr::relocate(`Order ID`, .before = 1)

# Save Data
sales %>%
  readr::write_csv("data-raw/sales.csv")

usethis::use_data(sales, overwrite = TRUE, internal = FALSE)
