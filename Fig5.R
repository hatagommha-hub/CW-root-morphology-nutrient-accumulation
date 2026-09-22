# ============================================================
# 04_figure5_rue.R
# Purpose: Resource use efficiency (Fig. 5)
# ============================================================

library(ggplot2)
library(dplyr)
library(readr)

# Load data
rue_data <- read_csv("data/rue_data.csv")

# Plot
p <- ggplot(rue_data, aes(x = Species, y = RUE, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.7) +
  geom_errorbar(aes(ymin = RUE - SE, ymax = RUE + SE),
                position = position_dodge(0.9), width = 0.25,
                color = "black") +
  facet_wrap(~ Parameter, scales = "free_y") +
  labs(x = "Species", y = "RUE (% removal per kg biomass)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(color = "black", size = 11, angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black", size = 11),
    axis.title.x = element_text(color = "black", size = 12, face = "bold"),
    axis.title.y = element_text(color = "black", size = 12, face = "bold"),
    strip.text = element_text(color = "black", size = 11, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = "black"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "none"
  )

ggsave("output/figures/figure5_rue.png", p,
       width = 10, height = 5, dpi = 300)
print(p)