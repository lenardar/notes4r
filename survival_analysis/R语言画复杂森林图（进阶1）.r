# 1. set work directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 2. import library
library(tidyverse)
library(janitor)
library(ggh4x)

# 3. Clean data
data <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter")) %>%
    as_tibble() %>%
    clean_names()
data <- data %>%
    mutate(main_group = ifelse(str_starts(subgroup, " "), NA, subgroup), .before = 1) %>%
    fill(main_group, .direction = "down") %>%
    drop_na() %>%
    mutate(
        subgroup = str_trim(subgroup),
        treatment = as.character(treatment), placebo = as.character(placebo),
        se = (log(hi) - log(est)) / 1.96,
        hr_95_percent_ci = ifelse(is.na(se), "", sprintf("%.2f (%.2f~%.2f)", est, low, hi))
    )
unique(data$main_group) %>% dput()
# c("All Patients", "Sex", "Age", "Body-mass index", "Race", "Baseline Statin Treatment", "Intensity of statin treatment")

# 4. position definition
mean_est <- data %>%
    drop_na() %>%
    pull(est) %>%
    mean()
head_y_position <- 15
subgroup_x_position <- -8
treatment_x_position <- -5
placebo_x_position <- -2.5
hazard_x_position <- 6.5

# 5. Plot
data %>%
    ggplot(aes(y = fct_rev(weave_factors(subgroup, main_group)), x = est, xmin = low, xmax = hi)) +
    geom_hline(yintercept = seq(1, head_y_position - 1, 2), linewidth = 7, color = "#eff4f2") +
    geom_point(aes(size = est), shape = 18, show.legend = FALSE) +
    geom_errorbar(width = 0.3, linewidth = 0.3, show.legend = FALSE) +
    # Subgroup
    annotate("text", label = "Subgroup", x = subgroup_x_position, y = head_y_position, size = 4, hjust = 0, fontface = "bold") +
    geom_text(aes(label = subgroup), x = subgroup_x_position, hjust = 0, size = 3, color = "black") +
    # Treatment
    annotate("text", label = "Treatment", x = treatment_x_position, y = head_y_position, size = 4, hjust = 0, fontface = "bold") +
    geom_text(aes(label = treatment), x = treatment_x_position, hjust = 0, size = 3, color = "black") +
    # Placebo
    annotate("text", label = "Placebo", x = placebo_x_position, y = head_y_position, size = 4, hjust = 0, fontface = "bold") +
    geom_text(aes(label = placebo), x = placebo_x_position, hjust = 0, size = 3, color = "black") +
    # Hazard Ratio and Mean Estimation
    annotate("text", label = "Hazard Ratio", x = mean_est, y = head_y_position, size = 4, fontface = "bold") +
    geom_segment(x = mean_est, xend = mean_est, y = 0.5, yend = head_y_position - 0.5, linetype = "dashed", linewidth = 0.3) +
    # Hazard Ratio (95% CI)
    annotate("text", label = "Hazard Ratio (95% CI)", x = hazard_x_position, y = head_y_position, size = 4, fontface = "bold") +
    geom_text(aes(label = hr_95_percent_ci), x = hazard_x_position) +
    scale_x_continuous(
        limits = c(subgroup_x_position, hazard_x_position + 2), breaks = seq(0, 4, 1),
        expand = expansion(mult = 0.02),
        guide = guide_axis_truncated(trunc_lower = 0, trunc_upper = 4)
    ) +
    scale_size_continuous(range = c(2, 4)) +
    theme(
        axis.text = element_text(color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.3),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        ggh4x.axis.nestline.y = element_line(linewidth = 1, color = "grey80", lineend = "round"),
        ggh4x.axis.nesttext.y = element_text(hjust = 1, margin = margin(r = 5), size = 10, color = "black"),
        aspect.ratio = 3 / 5,
    ) +
    guides(
        y = guide_axis_nested(extend = 0.7)
    ) +
    coord_cartesian(clip = "off")

ggsave("forest_plot1.pdf", width = 8, height = 4.5)
