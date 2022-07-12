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

# data sets ---------------
read_csv(here("data", "tidy_data", "pbi_per_cap_tidy.csv")) -> pbi_per_cap_data
read_csv(here("data", "tidy_data",
              "countries_freedom.csv")) -> freedom_data

freedom_data %>% 
  left_join(pbi_per_cap_data, by = c("year", "country")) -> workfile_data

workfile_data %>% mutate(country = str_to_title(country)) -> workfile_data
workfile_data %>% View
workfile_data %>% count(country)

# CSV writing  --------#
# write_csv(workfile_data, here("data", "workfile", "workfile_data.csv")) 

# workfile_data %>% select(year, country, freedom_index, 
#                          pbi_per_capita) -> data_to_analyse
# 


# analysis -------------------
## regressions ------------
## all_countries ----------
### fixed_effects --------#
# data_to_analyse %>% 
#   feols(pbi_per_capita ~ freedom_index |country, data = .,
#         cluster = "country",
#         panel.id = c("year", "country")) -> fixed_reg
# data_to_analyse %>% 
#   group_by(country) %>% 
#   mutate(d_pbi = pbi_per_capita - lag(pbi_per_capita),
#          d_free = freedom_index - lag(freedom_index),
#          d2_free = d_free - lag(d_free)
#          ) %>% ungroup() -> data_to_analyse
# 
# ### first_differences -------#
# data_to_analyse %>% 
#   feols(d_pbi ~ l(d_free, 5) + l(d2_free, 0:4),
#         data = ., cluster = "country", 
#         panel.id = c("year", "country"))  -> first_diff_reg
# 
# data_to_analyse %>% 
#   feols(d_pbi ~ l(d_free, 5) + l(d2_free, 0:4)|year,
#         data = ., cluster = "country",
#         panel.id = c("year", "country")) -> first_diff_reg_2
# 
# 
# data_to_analyse %>% 
#   feols(d_pbi ~ l(d_free, 5) + l(d2_free, 0:4)|year + country,
#         data = ., cluster = "country",
#         panel.id = c("year", "country")) -> first_diff_reg_3 
# 
# 
# 
# ## latinoamerica -------------
# data_to_analyse %>% distinct(country) %>% 
#   filter(country %in% c("Argentina",
#                         "Bolivia",
#                         "Brazil",
#                         "Chile",
#                         "Colombia",
#                         "Ecuador",
#                         "Paraguay",
#                         "Peru",
#                         "Uruguay")) -> countries_to_keep
# 
# data_to_analyse %>% 
#   inner_join(countries_to_keep, by = "country") -> data_to_analyse_latam
# ### fixed_effects ---------#
# 
# data_to_analyse_latam %>% 
#   feols(pbi_per_capita ~ freedom_index|country,
#         data = ., cluster = "country",
#         panel.id = c("year", "country")) -> first_reg_1_latam
# 
# 
# data_to_analyse_latam %>% 
#   feols(log(pbi_per_capita) ~ freedom_index|country,
#         data = ., cluster = "country",
#         panel.id = c("year", "country")) -> first_reg_2_latam
# 
# 
# 
#   
# ## output ---------
# ### latam -----#
# gm <- tribble(~raw, ~clean, ~fmt,
#               "nobs", "N", 0,
#               "r.squared", "R-cuadrado", 2)
# 
# modelsummary(list("Pbi per capita0" = first_reg_1_latam,
#                   "Pbi per capita1" = first_reg_2_latam),
#              output = "gt", fmt = 2, 
#              coef_rename = c("(Intercept)" = "Intercepto"),
#              estimate = "{estimate}{stars}",
#              gof_map = gm
#              ) %>% 
#   regression_theme_julio() -> all_countries_reg
# 
# all_countries_reg %>% 
#   tab_header(title = md("**Tabla 1:** Regresiones para Latinoamerica")) %>% 
#   tab_source_note(source_note = "Fuente: World Bank Data. Elaboracion Propia"
#                   ) %>% 
#   cols_label(
#     "Pbi per capita1" = md("(2)<br>log(Pbi per capita)"),
#     "Pbi per capita0" = md("(1)<br>Pbi per capita")
#   ) %>% 
#   tab_footnote(footnote = "Regresion con efectos fijos",
#                locations = cells_column_labels(
#                  columns = "Pbi per capita0"
#                )) -> tabla_1
# tabla_1
# 
# ### understanding
# data_to_analyse_latam %>%
#   filter(year %in% 2019:2018 ) %>% 
#   ggplot(aes(x = freedom_index, y = pbi_per_capita)) + 
#   geom_point() + facet_wrap(vars(year)) + geom_smooth(method = "lm",
#                                                       se = F,
#                                                       formula = y~x,
#                                                       size = 1.8,
#                                                       colour = "grey50") + 
#   theme(strip.background = element_rect(fill = "white"),
#         axis.title = element_text(size = 18),
#         axis.text = element_text(size = 16)) + 
#   labs(x = "Índice de libertad", y = "Pbi per cápita",
#        caption = "Fuente: Fraser Institute. Elaboración Propia") + 
#   geom_text(aes(label = country), family = "special",
#             size = 5, hjust = -.2, 
#             data = data_to_analyse_latam %>%
#               filter(year %in% 2019:2018 ) %>% 
#               filter(country != "Chile"))  + 
#   geom_text(aes(label = country), family = "special",
#             size = 5, hjust = 1.5, 
#             data = data_to_analyse_latam %>%
#               filter(year %in% 2019:2018 ) %>% 
#               filter(country == "Chile")) -> p3
# 
# df
# df %>% complete(group, item_id)
# 
# df %>% 
#   complete(group, item_id, item_name)
# 
# df
