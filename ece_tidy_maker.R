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
pacman::p_load(tidymodels, corrr, tidypredict, vip)
# data sets --------------------------
## ece_2019_sec -----------------------------#
xlsx_sheet_names(here("data","raw_data","UMC-0045","IE 2S ECE 15-19.xlsx"))
xlsx_cells(path = here("data","raw_data","UMC-0045","IE 2S ECE 15-19.xlsx"),
           sheets = "data_to_export") %>% 
  filter(between(row, 2, 14524),
         between(col, 1, 151),
         !is_blank) %>% 
  select(row, col, data_type, character, numeric,
         local_format_id) -> all_cells
xlsx_formats(
  path = here("data","raw_data","UMC-0045","IE 2S ECE 15-19.xlsx")
) -> formats
formats$local$font$bold -> bold


## headers ----------------------------------#
all_cells %>% 
  filter(row == 2) %>% 
  select(row, col, year = numeric) -> col_header_1

all_cells %>% 
  filter(row == 3) %>% 
  select(row, col, subject = character) -> col_header_2

all_cells %>% filter(
  row == 4
) %>% select(row, col, col_header_3 = character) -> col_header_3


all_cells %>% 
  filter(col == 1, row >= 5) %>% 
  select(row, col, cod_mod = character) -> row_header_1

all_cells %>% 
  filter(col == 2, row >= 5) %>% 
  select(row, col, anexo = character) -> row_header_2

all_cells %>% 
  filter(col == 3, row >= 6) %>% 
  select(row, col, nombre_ie = character) -> row_header_3

all_cells %>% 
  filter(col == 4, row >= 6) %>% 
  select(row, col, cod_dre = character) -> row_header_4

all_cells %>% 
  filter(col == 5, row >= 6) %>% 
  select(row, col, dre = character) -> row_header_5

all_cells %>% 
  filter(col == 6,row >= 6) %>% 
  select(row, col, cod_ugel = character) -> row_header_6

all_cells %>% 
  filter(col == 7, row >= 6) %>% 
  select(row, col, ugel = character) -> row_header_7

all_cells %>% 
  filter(col == 8, row >= 6) %>% 
  select(row, col, cod_geo = character) -> row_header_8

all_cells %>% 
  filter(col == 9, row >= 6) %>% 
  select(row, col, region = character) -> row_header_9

all_cells %>% 
  filter(col == 10, row >= 6) %>% 
  select(row, col, provincia = character) -> row_header_10

all_cells %>% 
  filter(col == 11, row >= 6) %>% 
  select(row, col, distrito = character) -> row_header_11

all_cells %>% 
  filter(col == 12, row >= 6) %>% 
  select(row, col, centro_poblado = character) -> row_header_12

all_cells %>% 
  filter(col == 13, row >= 6) %>% 
  select(row, col, gestion_2 = character) -> row_header_13

all_cells %>% 
  filter(col == 14, row >= 6) %>% 
  select(row, col, gestion_3 = character) -> row_header_14

all_cells %>% 
  filter(col == 15, row >= 6) %>% 
  select(row, col, area = character) -> row_header_15
## data_cells ----------------------------------#
all_cells %>% 
  filter(col >= 16, row >= 5) %>% 
  filter(data_type == "numeric") %>% 
  select(row, col, numeric) -> data_cells

## enhead! -------------------------------------#
data_cells %>% 
  enhead(col_header_1, "up-left") %>% 
  enhead(col_header_2, "up-left") %>% 
  enhead(col_header_3, "up") %>% 
  enhead(row_header_1, "left") %>% 
  enhead(row_header_2, "left") %>% 
  enhead(row_header_3, "left") %>% 
  enhead(row_header_4, "left") %>% 
  enhead(row_header_5, "left") %>%
  enhead(row_header_6, "left") %>% 
  enhead(row_header_7, "left") %>% 
  enhead(row_header_8, "left") %>% 
  enhead(row_header_9, "left") %>% 
  enhead(row_header_10, "left") %>% 
  enhead(row_header_11, "left") %>% 
  enhead(row_header_12, "left") %>% 
  enhead(row_header_13, "left") %>% 
  enhead(row_header_14, "left") %>% 
  enhead(row_header_15, "left") -> data_ece
data_ece %>% select(-row, - col) -> data_ece
data_ece %>% 
  mutate(subject = subject %>% str_squish()) %>% 
  mutate(subject = subject %>% str_remove(pattern = "\\..+")) -> data_ece 
data_ece %>% 
  mutate(col_header_3 = col_header_3 %>% str_to_lower()) -> data_ece
data_ece %>% 
  mutate(region = region %>% str_squish()) -> data_ece
data_ece %>% glimpse
data_ece %>% 
  mutate(across(.cols = where(is.character),
                .fns = ~.x %>% str_squish %>% 
                  str_to_lower())) -> data_ece
data_ece %>% glimpse()
data_ece %>% glimpse
data_ece %>% rename(measurement = numeric) -> data_ece
data_ece 
data_ece  %>% 
  filter(
    !(nombre_ie %>% 
      str_starts("anexo") # there are anexo == 1 just for these
  )) -> data_ece
data_ece
write_csv(data_ece,here("data", "tidy_data", "ece_tidy.csv"))
