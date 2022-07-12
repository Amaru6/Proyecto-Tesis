# theme_Julio -----------------
library(showtext)
font_add_google("Cardo", family = "special")
showtext_auto()


theme_julio <- function(){

  theme_bw() +
    theme(panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),
          axis.text = element_text(family = "special",
                                   size = 14),
          axis.title = element_text(family = "special", size = 14),

          legend.text = element_text(family = "special", size = 14),
          plot.title = element_text(family = "special", size = 18,
                                    hjust = 0.5,
                                    face = "plain"),
          plot.caption = element_text(family = "special", size = 14,
                                      hjust = 0.0111),
          strip.text = element_text(family = "special",
                                    size = 14),
          plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")
          ) 
}
