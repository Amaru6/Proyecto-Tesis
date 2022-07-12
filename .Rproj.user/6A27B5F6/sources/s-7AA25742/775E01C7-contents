# libraries --------------
## essential_packages -----------------#
pacman::p_load(tidyverse, lubridate, janitor, gt, 
               scales, socviz, here, tidytable)
## wrangling ----------#
pacman::p_load(tidyxl, unpivotr)
## graphics
pacman::p_load(patchwork , ggtext, gghighlight)
## econometrics -------#
pacman::p_load(modelsummary, 
               fixest, broom, fpp3)
## julio_theme ----------------#
source(here("code", "julio_theme", "chapter_0.R"))
theme_set(theme_julio())

# data sets ---------------- (sapos)
## workfile_data -----#
read_csv(here("data", "workfile", "workfile_data.csv")) -> data_orig
data <- data_orig
## latam_data -------#
data_orig %>% distinct(country) %>% 
  filter(country %in% c("Argentina",
                        "Bolivia",
                        "Brazil",
                        "Chile",
                        "Colombia",
                        "Ecuador",
                        "Paraguay",
                        "Peru",
                        "Uruguay",
                        "Venezuela, Rb")) -> countries_to_keep
data_orig %>% 
  inner_join(countries_to_keep, by = "country") -> data_to_analyse_latam
data_to_analyse_latam %>% 
  distinct(country)
# analysis -----------
## eda -----------
data_to_analyse_latam %>% View
data_to_analyse_latam %>% glimpse
data_to_analyse_latam %>% 
  clean_names() -> data_to_analyse_latam
## plots --------
### correlation -------
data_to_analyse_latam %>%  filter(country != "Venezuela, Rb") %>% 
  group_by(year) %>% 
  summarise(cor = cor(freedom_index, pbi_per_capita)) %>%
  filter(year > 2000) %>% 
  ggplot(aes(x = year, y = cor)) + geom_bar(stat = "identity",
                                            fill = "grey20") + 
  geom_hline(yintercept = 0, linetype = "solid", 1) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, .3),
                     labels = seq(0, .3, by = .05),
                     breaks = seq(0, .3, by = .05)) + 
  scale_x_continuous(labels = 2001:2019, breaks = 2001:2019) + 
  labs(x = "", y = "", title = "Correlación entre el índice de libertad económica y el pbi per cápita",
       subtitle = "Período: 2001-2019",
       caption = "Fuente: Fraser Institute. Elaboración Propia"
  ) + 
  geom_text(aes(label = round(cor, 2)),
            data = data_to_analyse_latam %>% 
              group_by(year) %>% 
              summarise(cor = cor(freedom_index, pbi_per_capita)) %>%
              filter(year > 2000), vjust = -.5, family = "special",
            size = 5)  + 
  theme(plot.title = element_text(hjust = .0001),
        plot.subtitle = element_text(family = "special")) -> p0
p0
## trend_liberty_index ----------
data_to_analyse_latam %>% 
  ggplot(aes(x = year, y = freedom_index,
             colour = country)) + 
  geom_line(size = 1) + theme(legend.title = element_blank())  + 
  gghighlight(country %in% c("Peru", "Argentina", "Chile", "Venezuela, Rb"),
              label_params = list(vjust = 1, family = "special",
                                  colour = "black"),
              use_direct_label = T) + 
  scale_colour_manual(values = c("black", "black", "black", "black")) + 
  labs(x = "", y = "", title = "Índice de libertad económica en Latinoamérica",
       subtitle = "Periodo: 1970-2019",
       caption = "Fuente: Fraser Institute. Elaboración Propia") -> p1 
p1 # does not work anymore... not sure why
p1 + geom_text(aes(label = round(freedom_index,1)),
               data = pluck(p1, "data") %>% 
                 filter(year == 2018), family = "special",
               vjust = -.5, hjust = 1.5,
               size = 4) + 
  theme(plot.title = element_text(hjust =.0001, size = 18),
        plot.subtitle = element_text(family = "special",
                                     size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) -> p1
p1

## pbi_liberty_index ----------
data_to_analyse_latam %>% 
  select(year, country, freedom_index, pbi_per_capita) -> data_to_analyse_latam_sub_1

data %>% 
  group_by(year) %>% 
  mutate(pbi_bucket = ntile(pbi_per_capita, 3),
         freedom_bucket = ntile(freedom_index,3)) %>% 
  arrange(desc(year), desc(pbi_bucket)) %>% 
  filter(year == 2019) %>% ungroup() %>%
  na.omit() %>% 
  group_by(freedom_bucket, pbi_bucket) %>% 
  summarise(n = n()) %>% 
  mutate(frac = n/sum(n)) %>% 
  mutate(freedom_bucket = as.character(freedom_bucket) %>% 
           str_replace("1", "Libertad baja") %>% 
           str_replace("2", "Libertad media") %>% 
           str_replace("3", "Libertad alta") %>% 
           fct_relevel("Libertad baja", "Libertad media", "Libertad alta"),
         pbi_bucket = as.character(pbi_bucket) %>% 
           str_replace("1", "PBI bajo") %>% 
           str_replace("2", "PBI medio") %>% 
           str_replace("3", "PBI alto") %>% 
           fct_relevel("PBI bajo", "PBI medio", "PBI alto")) %>% 
  ggplot(aes(x = freedom_bucket, y = frac , fill = pbi_bucket)) + 
  geom_bar(stat = "identity", position = "fill", colour = "white") + 
  scale_y_continuous(expand = c(0,0),
                     labels = percent_format(scale = 100)) + 
  theme(
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16, family = "special"),
        legend.title = element_text(size = 16, family = "special"),
        plot.title = element_text(hjust = .001, size = 18),
        plot.subtitle = element_text(hjust = .001, family = "special",
                                     size = 14)) + 
  labs(x = "", y = "Probabilidad condicional",
       fill = "PBI bucket",
       title = "Probabilidad condicional de categoría del PBI per cápita sobre el índice de libertad. Año 2019",
       subtitle = "Nota: Los cálculos se hicieron sobre todos los países",
       caption = "Fuente: World Bank Data. Elaboración Propia") + 
  geom_text(aes(label = percent(round(frac,2))),
            position = position_fill(vjust = .5),
            family = "special",
            size = 6) + 
  scale_fill_grey() -> p3
p3

## table -----------#
data_to_analyse_latam %>% 
  select(year, country, freedom_index) %>% 
  filter(country != "Venezuela, Rb") %>% 
  group_by(year) %>% 
  summarise(freedom_index = mean(freedom_index)) %>% 
  tail() -> tab_gt_1
tab_gt_1 %>% 
  rename("Año" = "year",
         "Índice de libertad" = "freedom_index") -> tab_gt_1

data_to_analyse_latam %>% 
  select(year, country, freedom_index) %>% 
  group_by(year) %>% 
  summarise(freedom_index = mean(freedom_index)) %>% 
  tail() -> tab_gt_2
tab_gt_2 %>% 
  rename("Año" = "year",
         "Índice de libertad." = "freedom_index") -> tab_gt_2

tab_gt_2 %>% 
  gt() %>% 
  fmt_number(columns = 2,
             decimals = 2) %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black", # the line below the col names
    table_body.hlines.color = "white", # the grey row lines 
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    column_labels.border.bottom.width = px(2)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = list(cells_column_labels(everything()),
                     cells_row_groups()) # When using groupname_col
  ) %>% 
  cols_width(
    2 ~ px(190)
  ) %>% 
  tab_footnote(footnote = md("No incluyendo a Venezuela.<br> Aunque, los resultados son robustos a la inclusión de este."),
               locations = cells_column_labels(
                 columns = 2
               )) %>% 
  cols_align(align = "center",
             columns = 1) %>% 
  cols_align(align = "center",
             columns = 2) -> gt_freedom_index_2
gt_freedom_index_2

## five_pillars_of_economic_freedom -------#
data_to_analyse_latam %>% glimpse
data_to_analyse_latam %>% 
  select(country, year, x1_size_of_government, 
         x2_legal_system_property_rights,
         x3_sound_money,
         x4_freedom_to_trade_internationally,
         x5_regulation,
         pbi_per_capita,
         freedom_index,
         pbi_per_capita) -> data_to_analyse_latam_sub_2
data_to_analyse_latam_sub_2 %>% 
  rowwise() %>% 
  mutate(freedom_index_constructed = c_across(3:7) %>% mean(na.rm = T)) %>% 
  ungroup() -> data_to_analyse_latam_sub_2
data_to_analyse_latam_sub_2 %>% 
  pivot_longer(cols = x1_size_of_government:x5_regulation,
              values_to = "val",
              names_to = "metric") %>% 
  relocate(metric, .after = 2) -> data_to_analyse_latam_sub_2_long
data_to_analyse_latam_sub_2_long %>% 
  filter(metric == "x1_size_of_government") %>% View
data_to_analyse_latam_sub_2_long %>% 
  group_by(year, metric) %>% 
  summarise(mean = mean(val, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(metric = metric %>% str_remove(pattern = "x[1-9]_") %>% 
           str_to_title()) -> data_to_analyse_latam_sub_2_long
data_to_analyse_latam_sub_2_long %>% 
  mutate(metric = metric %>% 
           str_replace_all(pattern = "\\_", 
                           replacement = " ")) -> data_to_analyse_latam_sub_2_long
data_to_analyse_latam_sub_2_long

data_to_analyse_latam_sub_2_long %>% 
  ggplot(aes(x = year, y = mean, colour = metric)) + 
  geom_line(size = 1.2) + 
  labs(title = "Media de los componentes del índice de libertad por año en Latinoamérica",  
       x ="", caption = "Fuente: Fraser Institue. Elaboración Propia",
       y = "") + 
  theme(legend.title = element_blank()) + 
  scale_colour_viridis_d() -> p4 # Something happened between 1960 and 1990
p4 + theme(plot.title = element_text(hjust = .001))

### see those years
data_to_analyse_latam_sub_2 %>% 
  filter(between(year, 1960, 1990)) %>% 
  select(country, year, x3_sound_money) %>% group_by(country) %>% 
  summarise(mean = mean(x3_sound_money)) %>% 
  arrange(desc(mean)) # Peru inflation years!

data_to_analyse_latam_sub_2 %>% 
  filter(between(year, 1960, 1990)) %>% 
  select(country, year, x3_sound_money) %>% 
  pivot_wider(names_from = year,
              values_from = x3_sound_money) %>% 
  rowwise() %>% 
  mutate(Media = c_across(2:6) %>% mean) %>% 
  ungroup() %>% 
  gt() %>% 
  fmt_number(columns = 2:7,
             decimals = 2)  %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black", # the line below the col names
    table_body.hlines.color = "white", # the grey row lines 
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    column_labels.border.bottom.width = px(2)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = list(cells_column_labels(everything()),
                     cells_row_groups()) # When using groupname_col
  ) %>% 
  tab_style(
    style = cell_fill(color = "lightgrey"),
    locations = list(
      cells_body(columns = "Media"),
      cells_column_labels(columns = "Media")
    )
  ) %>% 
  tab_source_note(source_note = md("**Fuente:** Fraser Institute. Elaboración Propia")) %>% 
  tab_options(data_row.padding = px(12)) %>% 
  cols_label("country" = "País") %>% 
  tab_header(title = "Califiación en el índice de estabilidad monetaria en países latinoamericanos") %>% 
  cols_move(columns = "1970", after = 1) %>% 
  cols_move(columns = "1975", after = 6) %>% 
  cols_move(columns = "1980", after = 5) %>% 
  cols_move(columns = "1985", after = 4) -> gt_1
gt_1

### last_plot_individual_countries
data_to_analyse_latam %>% 
  ggplot(aes(x = freedom_index, y = pbi_per_capita, colour = country)) + 
  geom_point() + 
  scale_y_log10() + 
  geom_smooth(method = "lm", se = F,
              formula = y~x) + 
  facet_wrap(vars(country)) + 
  theme(strip.background = element_rect(fill = "white",
                                        colour = "white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, family = "special"),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 16)
        ) -> p5_1
p5_1 + 
  labs(x = "Índice de libertad económica",
       y = "PBI per cápita",
       caption = "Fuente: Elaboración Propia") -> p5_1
p5_1

data_to_analyse_latam %>% 
  select(country, year, freedom_index, pbi_per_capita) %>% 
  nest(data = -country) %>% 
  mutate(modelo = map(data, function(data){
    r1 <- data %>% lm(pbi_per_capita ~ freedom_index, data = .)
  }),
  b1 = map_dbl(modelo, ~.x %>% pluck("coefficients",2)),
  n = map_dbl(data, ~.x %>% nrow),
  varianza_x = map_dbl(data, ~.x %>% pluck(., "freedom_index") %>% var)
  ) %>% 
  select(country, data, b1) %>% 
  ggplot(aes(x = fct_reorder(country, b1), y = b1)) + geom_bar(stat = "identity",
                                              fill = "grey20") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 4900)) + 
  labs(x = "", y = "",
       title = "Coeficiente del pbi per capita sobre el índice de libertad económica",
       caption = "Fuente: Elaboración Propia") + 
  theme(plot.title = element_text(hjust = .001),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 16)) + 
  geom_text(aes(label = round(b1,2)), family = "special", vjust = -.5,
            size = 6) -> p5_2
p5_2
## regresion_separada -----------#
data_to_analyse_latam %>% 
  select(country, year, freedom_index, pbi_per_capita) %>% 
  nest(data = -country) %>% 
  mutate(modelo = map(data, function(data){
    r1 <- data %>% lm(pbi_per_capita ~ freedom_index, data = .)
  }),
  b1 = map_dbl(modelo, ~.x %>% pluck("coefficients",2)),
  n = map_dbl(data, ~.x %>% nrow),
  varianza_x = map_dbl(data, ~.x %>% pluck(., "freedom_index") %>% var)
  ) %>% 
  mutate(weight = n/sum(n),
         weight_numerator = weight*varianza_x,
         denominator = sum(weight_numerator),
         final_weight = weight_numerator/denominator
         ) %>% 
  mutate(weight_cofficiente = final_weight*b1) %>% 
  summarise(beta_total = sum(weight_cofficiente))


data_to_analyse_latam %>% 
  select(country, year, freedom_index, pbi_per_capita) %>% 
  nest(data = -country) %>% 
  mutate(modelo = map(data, function(data){
    r1 <- data %>% lm(pbi_per_capita ~ freedom_index, data = .)
  }),
  b1 = map_dbl(modelo, ~.x %>% pluck("coefficients",2)),
  n = map_dbl(data, ~.x %>% nrow),
  varianza_x = map_dbl(data, ~.x %>% pluck(., "freedom_index") %>% var)
  )  %>% 
  mutate(weight = n/sum(n),
         weight_numerator = weight*varianza_x,
         denominator = sum(weight_numerator),
         final_weight = weight_numerator/denominator
  ) %>% 
  mutate(weight_cofficiente = final_weight*b1) %>% 
  select(country, b1, n, varianza_x, final_weight, weight_cofficiente) %>% 
  gt()  %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black", # the line below the col names
    table_body.hlines.color = "white", # the grey row lines 
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    column_labels.border.bottom.width = px(2)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = list(cells_column_labels(everything()),
                     cells_row_groups()) # When using groupname_col
  ) %>% 
  cols_label("country" = "País") %>% 
  tab_source_note(source_note = "Fuente: Elaboración Propia") %>% 
  tab_options(data_row.padding = px(15)) %>% 
  tab_header(title = "Regresión separada por cada país del pbi per cápita sobre el índice de libertad económica",
             subtitle = "Periodo: 1970-2019") %>% 
  opt_align_table_header(align = "left") -> gt_2

## regressions -----
data_to_analyse_latam %>% 
  select(country, year, freedom_index, pbi_per_capita) -> data_to_analyse_reg
setFixest_estimation(panel.id = c("country", "year"))

data_to_analyse_reg %>% 
    feols(pbi_per_capita ~ freedom_index|country, data = .,
          cluster = "country")

data_to_analyse_reg %>% 
  filter(country != "Venezuela, Rb") %>% 
  nest(data = -country) %>% 
  mutate(model = map(data, function(data){
   r1 <-  data %>% 
      lm(pbi_per_capita ~ freedom_index, .)
   r1
  }),
  b1 = map_dbl(model, ~.x %>% pluck("coefficients",2)),
  n = map_dbl(data, ~.x %>% nrow),
  varianza_x = map_dbl(data, ~.x %>% pluck("freedom_index") %>% var)
  ) %>% 
  mutate(weight = n/sum(n),
         weight_numerador = weight*varianza_x,
         denominador = sum(weight_numerador),
         final_weight = weight_numerador/denominador
         ) %>% 
  select(country, data, b1, final_weight) %>% 
  mutate(b1_final_weight = final_weight*b1) %>% 
  summarise(b1_final = sum(b1_final_weight))
