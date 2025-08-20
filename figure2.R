library(tidyverse)

CPI_or_HICP_idbanks <- tribble(~ IDBANK, ~ CPI_or_HICP,
                               "001764363", "CPI",  # Annual CPI
                               "001762489", "HICP"  # Annual HICP
)

figure2_idbanks <- tribble(~ IDBANK, ~ Occupation,
                           "010752321", "Managers including business owners",   # Net wage FTE managers private sector
                           "010752324", "Intermediate professions",             # Net wage FTE intermediate professions private sector
                           "010752330", "Workers"                               # Net wage FTE workers private sector
)

CPI_or_HICP <- CPI_or_HICP_idbanks |>
  mutate(data = map(IDBANK, ~ paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble() |>
                      transmute(TIME_PERIOD,
                                OBS_VALUE = as.numeric(OBS_VALUE)))) |>
  unnest(cols = c(data)) %>%
  select(-IDBANK) %>%
  spread(CPI_or_HICP, OBS_VALUE)

figure2 <- figure2_idbanks |>
  mutate(data = map(IDBANK, ~ paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble() |>
                      transmute(TIME_PERIOD,
                                OBS_VALUE = as.numeric(OBS_VALUE)))) |>
  unnest(cols = c(data)) |>
  select(-IDBANK) |>
  left_join(CPI_or_HICP, by = "TIME_PERIOD") |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01-01"))) |>
  filter(date >= as.Date("1996-01-01"),
         date <= as.Date("2022-01-01")) |>
  group_by(Occupation) %>%
  arrange(date) %>%
  transmute(date,
            Occupation,
            `Real (HICP inflation)` = 100*(OBS_VALUE/OBS_VALUE[1])/(HICP/HICP[1]),
            `Real (CPI inflation)`  = 100*(OBS_VALUE/OBS_VALUE[1])/(CPI/CPI[1])) %>%
  gather(variable, value, -date, -Occupation)



figure2 %>%
  ggplot() + geom_line(aes(x = date, y = value,  color = variable, linetype = Occupation)) +
  theme_minimal() + ylab("") + xlab("") +
  #scale_color_manual(values = c("darkgrey", "#F8766D", "#619CFF")) +
  scale_x_date(breaks = seq(1996, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.3, 0.8),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(0, 200, 1)) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(caption = "Source: Insee, author’s calculations")

ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375)

