library(ggplot2)

theme_set(
  theme_bw() + theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 22),
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
