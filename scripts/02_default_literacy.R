library(ggplot2)

# Default graph: no additional styling
default_plot <- ggplot(
  literacy,
  aes(x = age, y = literate)
) +
  geom_point()

print(default_plot)

# Save as SVG
ggsave(
  "figures/default.svg",
  plot = default_plot,
  width = 9,
  height = 6
)