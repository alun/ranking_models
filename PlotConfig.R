library(ggplot2)
library(ggpubr)
library(extrafont) 

# fonts can be downloaded from here www.fontsquirrel.com/fonts/latin-modern-roman
# follow this article to install https://fulowa.medium.com/latex-font-in-a-ggplot-9120caaa5250

# execute once to add fonts:
# font_import(pattern = "lmroman*") 

loadfonts()

colors <- list(
  alpha = 0.7,
  color_0 = 'black',
  color_1 = 'green',
  color_2 = 'magenta'
)

theme_set(
  theme_bw() + theme(
    text = element_text(size = 10, family = "LM Roman 10"),
    title = element_text(size = 22),
    legend.text = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )
)

save_plot <- function(plot, file, width = 12, height = 8) {
   ggsave(
    file = file,
    plot = plot,
    width = width,
    height = height
  ) 
  plot
}
