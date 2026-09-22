# ============================================================
# 03_figure4_pathogens.R
# Purpose: Microbial removal efficiency (Fig. 4)
# ============================================================

library(ggplot2)
library(dplyr)
library(readr)

# Load data
pathogen_data <- read_csv("data/pathogen_data.csv")

# Plot
p <- ggplot(pathogen_data, aes(x = Species, y = Removal, fill = HRT)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.7) +
  geom_errorbar(aes(ymin = Removal - 3, ymax = Removal + 3),
                position = position_dodge(0.9), width = 0.25,
                color = "black") +
  facet_wrap(~ Pathogen, scales = "free_y", ncol = 2) +
  labs(x = "Treatment", y = "Removal Efficiency (%)") +
  scale_fill_manual(values = c("3 days" = "#1f77b4",
                                "5 days" = "#ff7f0e",
                                "7 days" = "#2ca02c")) +
  theme_bw() +
  theme(
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 11, face = "bold"),
    strip.text = element_text(color = "black", size = 10, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = "black"),
    legend.text = element_text(color = "black", size = 10),
    legend.title = element_text(color = "black", size = 10, face = "bold"),
    panel.border = element_rect(color = "black"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("output/figures/figure4_pathogens.png", p,
       width = 10, height = 8, dpi = 300)
print(p)