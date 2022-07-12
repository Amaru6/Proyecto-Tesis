# libraries --------------
## essential_packages -----------------#
pacman::p_load(tidyverse, lubridate, janitor, gt, gtExtras,
               scales, socviz, skimr, rvest, here)
## wrangling ----------#
pacman::p_load(tidyxl, unpivotr, metaDigitise)
## graphics
pacman::p_load(patchwork, binsreg, ggannotate, ggpattern, ggtext,
               ggbump, gghighlight)
## econometrics -------#
pacman::p_load(modelsummary, marginaleffects, lspline, 
               fixest, sandwich, broom, ggdag, dagitty, fpp3)
## machine_learning ----------#
pacman::p_load(tidymodels, corrr)
## julio_theme ----------------#
## julio_theme ----------------#
source(here("code", "julio_theme", "chapter_0.R"))
theme_set(theme_julio())

# data sets --------------------
## tidy_1 ----------
xlsx_cells(path = here("data", "raw_data", "efotw-2021-master-index-data-for-researchers.xlsx"),
           sheets = "EFW Data 2021 Report") -> all_cells
all_cells %>% 
  filter(!is_blank) %>% 
  select(row, col, data_type, character, numeric, local_format_id) -> all_cells


all_cells %>% 
  filter(col == 2, row >= 6) %>% 
  select(row, col, year = numeric) -> row_header_1
all_cells %>% 
  filter(col == 5, row >= 6) %>% 
  select(row, col, country = character) -> row_header_2

all_cells %>% 
  filter(row == 2) %>% 
  select(row, col, area = character) -> col_header_1
 
all_cells %>% 
  filter(row == 3) %>% 
  select(row, col, sub_area = character) -> col_header_2

all_cells %>% 
  filter(row == 4) %>% 
  select(row, col, sub_sub_area = character) -> col_header_3 

all_cells %>% 
  filter(row == 5, col >= 9) %>% 
  select(row, col, sub_sub_sub_area = character) -> col_header_4

all_cells %>% 
  filter(row >= 6, col >= 6) %>% 
  filter(data_type == "numeric") %>% 
  select(row, col, measurement = numeric) -> data_cells


data_cells %>% 
  enhead(row_header_1, "left") %>% 
  enhead(row_header_2, "left") %>% 
  enhead(col_header_1, "up-left") %>% 
  enhead(col_header_2, "up-left") %>% 
  enhead(col_header_3, "up-left") %>% 
  enhead(col_header_4, "up") -> data_cells_cleaned

data_cells_cleaned %>% 
  select(-c(row, col)) -> data_cells_cleaned

data_cells_cleaned %>% arrange(country) -> data_cells_cleaned
data_cells_cleaned

## tidy_2 -----------------#
xlsx_cells(path = here("data", "raw_data", "efotw-2021-master-index-data-for-researchers.xlsx"),
           sheets = "EFW Data 2021 Report") -> all_cells_2
xlsx_formats(path = here("data", "raw_data", "efotw-2021-master-index-data-for-researchers.xlsx")
             ) -> formats
which(formats$local$font$bold) -> bold

all_cells_2 %>% filter(!is_blank) %>% 
  select(row, col, data_type, character, numeric, local_format_id) -> all_cells_2


all_cells_2 %>% 
  filter(col == 2, row >= 6) %>% 
  select(row, col, year = numeric) -> row_header_1
row_header_1
all_cells_2 %>% 
  filter(col == 5, row >= 6) %>% 
  select(row, col, country = character) -> row_header_2
row_header_2

all_cells_2 %>% 
  filter(col == 6, row >= 6) %>% 
  select(row, col, freedom_index = numeric) -> row_header_3
row_header_3

all_cells_2 %>% 
  filter(col >= 10, row == 5) %>% 
  filter(local_format_id %in% bold) %>% 
  filter(character != "Gender Legal Rights Adjustment") %>% 
  select(row, col, col_header_1 = character) -> col_header_1

all_cells_2 %>% filter(col>= 9, row == 5) %>% 
  filter(character != "data") %>% 
  select(row, col, col_header_2 = character) %>% 
  anti_join(col_header_1, by = c("col_header_2" = "col_header_1")
            ) -> col_header_2

all_cells_2 %>% 
  filter(row >= 6, col >= 6) %>% 
  filter(data_type == "numeric") %>% 
  select(row, col, measurement = numeric) -> data_cells_2 

data_cells_2 %>% 
  enhead(row_header_1, "left") %>%
  enhead(row_header_2, "left") %>% 
  enhead(row_header_3, "left") %>% 
  enhead(col_header_1, "up-right") %>% 
  enhead(col_header_2, "up") %>% 
  select(-c(row, col)) -> data_cells_cleaned_2


data_cells_cleaned_2 %>% nrow
data_cells_cleaned_2 %>% 
  distinct(year, country, col_header_1, col_header_2) %>% distinct %>% nrow

data_cells_cleaned_2 %>% View

data_cells_cleaned_2 %>% View
data_cells_cleaned_2 %>% distinct(col_header_1, col_header_2) %>% nrow()
data_cells_cleaned_2 %>% distinct(col_header_2) %>% nrow



data_cells_cleaned_2 %>% 
  mutate(col_header_1 = str_to_lower(col_header_1)) %>% 
  distinct(col_header_1) -> col_header_1_to_remove
col_header_1_to_remove

data_cells_cleaned_2 %>% 
  pivot_wider(names_from = c(col_header_1, col_header_2),
              values_from = measurement,
              names_sep = "_") %>% clean_names() %>% 
  arrange(country) -> data_cells_cleaned_2
data_cells_cleaned_2

## data_to_join_to_get_averages_or_the_latent_variables_itself -----#
all_cells_2

all_cells_2 %>% 
  filter(col == 2, row >= 6) %>% 
  select(row, col, year = numeric) -> row_header_1
row_header_1
all_cells_2 %>% 
  filter(col == 5, row >= 6) %>% 
  select(row, col, country = character) -> row_header_2
row_header_2

all_cells_2 %>% 
  filter(col == 6, row >= 6) %>% 
  select(row, col, freedom_index = numeric) -> row_header_3
row_header_3


all_cells_2 %>% 
  filter(col >= 19) %>% 
  filter(row == 5) %>% 
  filter(local_format_id %in% bold) %>% 
  filter(character != "Gender Legal Rights Adjustment") %>% 
  select(row, col, pilar = character) -> col_header_1

all_cells_2 %>% 
  filter(col %in% c(21, 31, 39, 55, 74)) %>% 
  filter(data_type == "numeric") %>% 
  select(row, col, val = numeric) -> data_cells_3

data_cells_3 %>% 
  enhead(row_header_1, "left") %>% 
  enhead(row_header_2, "left") %>% 
  enhead(row_header_3, "left") %>% 
  enhead(col_header_1, "up") %>% 
  select(-c(row, col)) -> data_to_join
data_to_join %>% 
  select(-freedom_index) -> data_to_join
data_to_join %>% 
  pivot_wider(names_from = pilar,
              values_from = val) -> data_to_join

## join_them -----------# This is the one!
data_cells_cleaned_2 %>% 
  pivot_wider(names_from = c("col_header_1", "col_header_2"),
              values_from = measurement) %>% 
  left_join(data_to_join, by = c("year", "country")) -> data_cells_cleaned_2

# clean -----#
write_csv(data_cells_cleaned_2,
        here("data", "tidy_data", "countries_freedom.csv"))


## pbi_per_capita ---------
xlsx_cells(path = here("data", "raw_data", "gdp_2015_dollars.xlsx"),
           sheets = "Data") -> all_cells
all_cells %>% 
  filter(!is_blank) %>% 
  select(row, col, data_type, character, numeric, local_format_id
         ) %>% 
  filter(row >= 4) -> all_cells

all_cells %>% filter(col >= 5, row == 4) %>% 
  select(row, col, year = character) -> col_header_1

all_cells %>% 
  filter(col == 1) %>% 
  filter(row >= 5) %>% 
  select(row, col, country = character) -> row_header_1

all_cells %>% 
  filter(col >= 5, row >= 5) %>% 
  select(row, col, measurement = numeric) -> data_cells

data_cells %>% enhead(col_header_1, "up") %>% 
  enhead(row_header_1, "left") -> data_cells

data_cells %>% select(-c(row, col)) -> data_cells
data_cells %>% mutate(year = as.integer(year)) -> data_cells
data_cells %>% rename(pbi_per_capita = measurement) -> data_cells
data_cells %>% View
data_cells %>% distinct(country) %>% View
# write_csv(data_cells, here("data", "tidy_data", "pbi_per_cap_tidy.csv"))  

