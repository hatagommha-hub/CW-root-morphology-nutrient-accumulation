# ============================================================
# 02_figure3_bod_tds.R
# Purpose: BOD5 and TDS removal (Fig. 3)
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Load data
water <- read_csv("data/water_quality_data.csv")

# Filter BOD5 and TDS
bod_tds <- water %>%
  filter(Parameter %in% c("BOD5", "TDS")) %>%
  pivot_wider(names_from = HRT, values_from = Concentration) %>%
  mutate(
    Removal_3 = (Day0 - Day3) / Day0 * 100,
    Removal_5 = (Day0 - Day5) / Day0 * 100,
    Removal_7 = (Day0 - Day7) / Day0 * 100
  ) %>%
  pivot_longer(
    cols = starts_with("Removal"),
    names_to = "HRT",
    values_to = "Removal"
  ) %>%
  mutate(
    HRT = factor(HRT,
                 levels = c("Removal_3", "Removal_5", "Removal_7"),
                 labels = c("3 days", "5 days", "7 days"))
  )

# Plot
p <- ggplot(bod_tds, aes(x = Species, y = Removal, fill = HRT)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.7) +
  geom_errorbar(aes(ymin = Removal - 5, ymax = Removal + 5),
                position = position_dodge(0.9), width = 0.25,
                color = "black") +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 2) +
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

ggsave("output/figures/figure3_bod_tds.png", p,
       width = 10, height = 6, dpi = 300)
print(p)