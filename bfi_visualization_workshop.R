# DATA VISUALIZATION WORKSHOP — 90-minute practical
# Dataset: psych::bfi | Packages: psych, ggplot2, dplyr, tidyr
# Run sections in order. Lines marked CHALLENGE invite participants to edit code.
# Install packages ONCE (uncomment if needed):
# install.packages(c("psych", "ggplot2", "dplyr", "tidyr"))

# ================================================================
# 0–10 MIN | 1. Load and inspect the data
# ================================================================
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)

data("bfi", package = "psych")
bfi_raw <- as_tibble(bfi)
glimpse(bfi_raw)
summary(select(bfi_raw, age, gender, N1, A1))
# BFI: 2,800 records, 25 Big Five items (1–6), age, gender, education.
# Original gender coding: 1 = male, 2 = female.

# ================================================================
# 10–20 MIN | 2. Reverse-code and calculate scale scores
# ================================================================
# For six-point items, reverse coding is 7 - response.
# We retain the original items and create new reverse-scored columns.
bfi_scored <- bfi_raw %>%
  mutate(
    A1_R = 7 - A1,
    C4_R = 7 - C4,
    C5_R = 7 - C5,
    E1_R = 7 - E1,
    E2_R = 7 - E2,
    O2_R = 7 - O2,
    O5_R = 7 - O5,
    Gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female"))
  )

# Scale scores require at least three of five items to be answered.
# This is a transparent teaching rule, not a claim about an official
# scoring requirement; follow your own instrument's scoring manual.
mean_if_3 <- function(x) {
  answered <- rowSums(!is.na(x))
  score <- rowMeans(x, na.rm = TRUE)
  score[answered < 3] <- NA_real_
  score
}

bfi_scored <- bfi_scored %>%
  mutate(
    Agreeableness = mean_if_3(pick(A1_R, A2, A3, A4, A5)),
    Conscientiousness = mean_if_3(pick(C1, C2, C3, C4_R, C5_R)),
    Extraversion = mean_if_3(pick(E1_R, E2_R, E3, E4, E5)),
    Neuroticism = mean_if_3(pick(N1, N2, N3, N4, N5)),
    Openness = mean_if_3(pick(O1, O2_R, O3, O4, O5_R))
  )

summary(select(bfi_scored, Agreeableness, Conscientiousness,
               Extraversion, Neuroticism, Openness))
# CHALLENGE: Why is A1 reversed, but N1 is not?

# ================================================================
# 20–30 MIN | 3. Distribution of scale scores
# ================================================================
p_hist <- ggplot(bfi_scored, aes(x = Neuroticism)) +
  geom_histogram(binwidth = 0.25, boundary = 1,
                 fill = "#4477AA", color = "white") +
  labs(x = "Neuroticism (mean item score)", y = "Number of participants") +
  theme_classic(base_size = 12)
print(p_hist)

p_density <- ggplot(bfi_scored, aes(x = Neuroticism)) +
  geom_density(fill = "#4477AA", alpha = 0.3, na.rm = TRUE) +
  labs(x = "Neuroticism (mean item score)", y = "Density") +
  theme_classic(base_size = 12)
print(p_density)
# CHALLENGE: Try binwidth = 0.1 and 0.5. What changes visually?

# ================================================================
# 30–40 MIN | 4. Compare groups and reveal observations
# ================================================================
p_box <- ggplot(bfi_scored, aes(x = Gender, y = Neuroticism)) +
  geom_boxplot(fill = "#A6CEE3", width = 0.5, na.rm = TRUE) +
  labs(x = "Gender", y = "Neuroticism (mean item score)") +
  theme_classic(base_size = 12)
print(p_box)

set.seed(42)  # reproducible placement of jittered points
p_jitter <- ggplot(bfi_scored, aes(x = Gender, y = Neuroticism)) +
  geom_boxplot(fill = "#A6CEE3", width = 0.5,
               outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(width = 0.16, height = 0, alpha = 0.08,
              size = 0.9, na.rm = TRUE) +
  labs(x = "Gender", y = "Neuroticism (mean item score)") +
  theme_classic(base_size = 12)
print(p_jitter)
# CHALLENGE: Replace geom_boxplot(...) with geom_violin(...).

# ================================================================
# 40–50 MIN | 5. Age and Neuroticism
# ================================================================
p_scatter <- ggplot(bfi_scored, aes(x = age, y = Neuroticism)) +
  geom_point(alpha = 0.15, size = 1.2, na.rm = TRUE) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#4477AA", na.rm = TRUE) +
  labs(x = "Age (years)", y = "Neuroticism (mean item score)") +
  theme_classic(base_size = 12)
print(p_scatter)

p_scatter_groups <- ggplot(
  filter(bfi_scored, !is.na(Gender)),
  aes(x = age, y = Neuroticism, color = Gender)
) +
  geom_point(alpha = 0.10, size = 1.1, na.rm = TRUE) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              na.rm = TRUE) +
  scale_color_manual(values = c("Male" = "#4477AA",
                                "Female" = "#CC6677")) +
  labs(x = "Age (years)", y = "Neuroticism (mean item score)",
       color = "Gender") +
  theme_classic(base_size = 12)
print(p_scatter_groups)
# CHALLENGE: What happens when color = Gender is moved outside aes()?
# (Try it, then undo the change.)

# ================================================================
# 50–60 MIN | 6. Mean and error bars: SD, SE, 95% CI
# ================================================================
bfi_summary <- bfi_scored %>%
  filter(!is.na(Gender), !is.na(Neuroticism)) %>%
  group_by(Gender) %>%
  summarise(
    n = n(),
    Mean = mean(Neuroticism),
    SD = sd(Neuroticism),
    SE = SD / sqrt(n),
    CI_halfwidth = qt(0.975, df = n - 1) * SE,
    .groups = "drop"
  )
print(bfi_summary)

# Make three panels using a common y-axis and error-bar construction.
error_data <- bfi_summary %>%
  select(Gender, Mean, SD, SE, CI_halfwidth) %>%
  pivot_longer(cols = c(SD, SE, CI_halfwidth),
               names_to = "Error_type", values_to = "Halfwidth") %>%
  mutate(Error_type = factor(
    Error_type,
    levels = c("SD", "SE", "CI_halfwidth"),
    labels = c("Mean ± 1 SD", "Mean ± 1 SE", "Mean and 95% CI")
  ))

p_errors <- ggplot(error_data, aes(x = Gender, y = Mean)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = Mean - Halfwidth, ymax = Mean + Halfwidth),
                width = 0.12) +
  facet_wrap(~ Error_type, nrow = 1) +
  coord_cartesian(ylim = c(1, 6)) +
  labs(x = "Gender", y = "Mean neuroticism score") +
  theme_classic(base_size = 12)
print(p_errors)
# DISCUSS: SD = spread of individuals; SE/CI = precision of group means.
# Overlap/nonoverlap of two group CIs is NOT itself a formal test of the difference.

# ================================================================
# 60–85 MIN | 7. Work with your own data (or optional BFI challenge)
# ================================================================
# Before workshop: participants bring an analysis-ready CSV file.
# Uncomment and update the path as needed:
# my_data <- read.csv(file.choose())
# str(my_data)
# names(my_data)
# summary(my_data)

# REPLACE variable names below with those in your own dataset:
# p_my_plot <- ggplot(my_data, aes(x = predictor, y = outcome)) +
#   geom_point(alpha = 0.4) +
#   geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
#   labs(x = "Predictor (units)", y = "Outcome (units)") +
#   theme_classic(base_size = 12)
# print(p_my_plot)

# OPTIONAL BFI CHALLENGE: Compare distributions of all five traits.
bfi_long <- bfi_scored %>%
  pivot_longer(
    cols = c(Agreeableness, Conscientiousness, Extraversion,
             Neuroticism, Openness),
    names_to = "Trait", values_to = "Score"
  )

p_facets <- ggplot(bfi_long, aes(x = Score)) +
  geom_density(fill = "#4477AA", alpha = 0.35, na.rm = TRUE) +
  facet_wrap(~ Trait, ncol = 3) +
  labs(x = "Mean item score", y = "Density") +
  theme_classic(base_size = 12)
print(p_facets)
# CHALLENGE: Add fill = Gender inside aes(), using bfi_long, and
# position legends / transparency for a readable group comparison.

# ================================================================
# 85–90 MIN | 8. Export and reflect
# ================================================================
ggsave("BFI_age_neuroticism.png", plot = p_scatter_groups,
       width = 6, height = 4, units = "in", dpi = 300)
ggsave("BFI_age_neuroticism.pdf", plot = p_scatter_groups,
       width = 6, height = 4, units = "in")
# CHECK: Can a reader understand your axes, units, groups, and uncertainty?
# Journal figures may have specific format/resolution requirements.
