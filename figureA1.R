library(tidyverse)
library(tibble)

figureA1_idbanks <- c("001762353", "001762354", "001762357", "001762360",
                      "001766350", "001766351", "001766355")

figureA1 <- paste(figureA1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  as_tibble() |>
  transmute(Coicop2016 = stringr::str_extract(TITLE_FR, "(?<=Nomenclature Coicop : ).*"),
            TIME_PERIOD,
            OBS_VALUE = as.numeric(OBS_VALUE),
            `CPI or HICP ?` = case_when(grepl("harmonisé", TITLE_FR) ~ "HICP",
                                        T ~ "CPI"))


coicop_labels <- tribble(
  ~Coicop2016, ~Coicop2016_en,
  "10 - Enseignement", "10 - Education",
  "10.1 - Enseignement préélémentaire et primaire", "10.1 - Pre-primary and primary education",
  "10.2 - Enseignement secondaire", "10.2 - Secondary education",
  "10.4 - Enseignement supérieur", "10.4 - Tertiary education"
)


figureA1 %>%
  left_join(coicop_labels, by = "Coicop2016") |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01-01")))  |>
  filter(date >= as.Date("1996-01-01"),
         date <= as.Date("2024-01-01")) |>
  arrange(date) |>
  ggplot() + ylab("") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE/10000, color =  Coicop2016_en, linetype = `CPI or HICP ?`)) +
  scale_x_date(breaks = as.Date(paste0(seq(1920, 2100, 2), "-01-01")),
               labels = scales::date_format("%Y")) +
  theme(legend.position = c(0.78, 0.78),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = 0.01*seq(0, 300, .1),
                     labels = scales::percent_format(accuracy = .1)) +
  labs(caption = "Source: Insee")

ggsave("figureA1.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figureA1.pdf", width = 1.25*6, height = 1.25*3.375)
