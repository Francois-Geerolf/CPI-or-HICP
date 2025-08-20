library(tidyverse)

table1_idbanks <- c("001759970", # Monthly CPI
                    "001759971", # Monthly HICP
                    "001764363", # Annual CPI
                    "001762489"  # Annual HICP
)

table1 <- paste(table1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  filter(TIME_PERIOD %in% c("1999-06", "2021-06", "2024-06", "2021", "2022", "2023")) |>
  transmute(TIME_PERIOD,
            OBS_VALUE = as.numeric(OBS_VALUE), 
            `CPI or HICP ?` = case_when(grepl("harmonisé", TITLE_FR) ~ "HICP inflation",
                                        TRUE ~ "CPI inflation")) |>
  spread(TIME_PERIOD, OBS_VALUE) |>
  transmute(`CPI or HICP ?`, 
            infl2022 = `2022`/`2021` - 1,
            infl2023 = `2023`/`2022` - 1,
            g3y = `2024-06`/`2021-06` - 1,
            g25y = `2024-06`/`1999-06` - 1) |>
  gt::gt() |>
  gt::fmt_percent(
    columns = 2:5,
    decimals = 1,
    force_sign = TRUE
  ) |>
  gt::cols_align(
    align = "center",
    columns = everything()
  ) |>
  gt::cols_label(
    infl2022 = gt::html("2022<br>Annual"),
    infl2023 = gt::html("2023<br>Annual"),
    g3y = gt::html("June 2021 - June 2024<br>3-year change"),
    g25y = gt::html("June 1999 - June 2024<br>25-year change")
  ) |>
  gt::tab_footnote("Source: Insee, author’s calculations")

table1  |>
  gt::gtsave(filename = "table1.png")

table1  |>
  gt::gtsave(filename = "table1.pdf")

system("pdfcrop table1.pdf table1.pdf")


