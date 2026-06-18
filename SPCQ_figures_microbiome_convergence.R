# =========================================================
# SPC-Q FIGURE GENERATION SCRIPT
# ---------------------------------------------------------
# This script reproduces the figures presented in the
# manuscript:
#
#   "Distinct behavioural pathways converge on similar
#    microbiome states"
#
# The script assumes that the SPC-Q model has already been
# run and that the required objects (or output files) are
# available. No simulations are performed here.
#
# Author: William Ross Hunter
# =========================================================

# -------------------------
# Housekeeping
# -------------------------
rm(list = ls())

# -------------------------
# Required packages
# -------------------------
library(dplyr)
library(ggplot2)
library(ggtern)
library(patchwork)
library(proxy)
library(grid)

# =========================================================
# LOAD DATA
# =========================================================
# Assumes the model script has been run and saved outputs
# are available. If preferred, replace with read.csv() or
# readRDS() calls.

# Agent-level summaries (mean across Monte Carlo runs)
# Expected columns: agent, S, P, C, entropy, repertoire_size,
# dominance, micro_entropy, micro_richness
agent_summary <- read.csv("Microbiome_alpha_beta_summary_MC.csv")

# Full Monte Carlo outputs (optional, for degeneracy analysis)
# If stored separately, update the path accordingly
# all_runs <- read.csv("outputs/all_runs.csv")
# all_results <- read.csv("outputs/all_results.csv")

# =========================================================
# FIGURE 1 — EMERGENT BEHAVIOUR
# =========================================================

# PANEL A — Conceptual schematic
p_concept <- ggplot() +
  annotate("rect", xmin = 0.05, xmax = 0.95,
           ymin = 0.75, ymax = 0.9,
           fill = "grey90", color = "black") +
  annotate("text", x = 0.5, y = 0.825,
           label = "Sensory (S) | Predictability (P) | Context (C)",
           size = 3.8, fontface = "bold") +
  annotate("rect", xmin = 0.2, xmax = 0.8,
           ymin = 0.55, ymax = 0.68,
           fill = "grey95", color = "black") +
  annotate("text", x = 0.5, y = 0.615,
           label = "State modulation\n(stress, sensory load)",
           size = 3.6) +
  annotate("segment", x = 0.5, xend = 0.5,
           y = 0.75, yend = 0.68,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("rect", xmin = 0.2, xmax = 0.8,
           ymin = 0.35, ymax = 0.48,
           fill = "grey90", color = "black") +
  annotate("text", x = 0.5, y = 0.415,
           label = "Probabilistic food choice\n(softmax + learning)",
           size = 3.6) +
  annotate("segment", x = 0.5, xend = 0.5,
           y = 0.55, yend = 0.48,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("rect", xmin = 0.3, xmax = 0.7,
           ymin = 0.12, ymax = 0.25,
           fill = "grey80", color = "black") +
  annotate("text", x = 0.5, y = 0.185,
           label = "Emergent diet\n(entropy, repertoire)",
           size = 3.6, fontface = "bold") +
  annotate("segment", x = 0.5, xend = 0.5,
           y = 0.35, yend = 0.25,
           arrow = arrow(length = unit(0.2, "cm"))) +
  theme_void() +
  labs(title = "Model schematic")

# PANEL B — Latent sensory space
p_latent <- ggplot(agent_summary,
                   aes(x = S, y = P, color = entropy)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "Latent sensory space",
    x = "Sensory drive (S)",
    y = "Predictability preference (P)",
    color = "Entropy"
  )

# PANEL C — Emergent dietary behaviour
p_behaviour <- ggplot(agent_summary,
                      aes(x = repertoire_size, y = entropy)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "Emergent dietary behaviour",
    x = "Repertoire size",
    y = "Dietary entropy"
  )

# Combine Figure 1 panels
figure1 <- (p_concept | p_latent) / p_behaviour

# Save Figure 1
ggsave("Figure1_EmergentBehaviour_MC.png",
       plot = figure1,
       width = 10,
       height = 7,
       dpi = 300)

# =========================================================
# FIGURE 2 — SPC → BEHAVIOURAL STRATEGIES
# =========================================================

# Derive SPC proportions
driver_space <- agent_summary %>%
  mutate(
    S_pos = abs(S),
    P_pos = abs(P),
    C_pos = C
  ) %>%
  rowwise() %>%
  mutate(
    total = S_pos + P_pos + C_pos,
    S_prop = S_pos / total,
    P_prop = P_pos / total,
    C_prop = C_pos / total
  ) %>%
  ungroup()

# Strategy space
strategy_space <- driver_space %>%
  mutate(
    entropy_norm = (entropy - min(entropy)) /
      (max(entropy) - min(entropy)),
    rep_norm = (repertoire_size - min(repertoire_size)) /
      (max(repertoire_size) - min(repertoire_size)),
    Explorer    = entropy_norm,
    Specialist  = 1 - entropy_norm,
    Opportunist = rep_norm
  ) %>%
  rowwise() %>%
  mutate(
    total = Explorer + Specialist + Opportunist,
    Explorer    = Explorer / total,
    Specialist  = Specialist / total,
    Opportunist = Opportunist / total
  ) %>%
  ungroup() %>%
  mutate(
    strategy_class = case_when(
      entropy_norm < 0.33 ~ "Specialist",
      entropy_norm > 0.66 ~ "Explorer",
      TRUE ~ "Opportunist"
    )
  )

# Panel A — SPC ternary
p_drivers <- ggtern(strategy_space,
                    aes(S_prop, P_prop, C_prop, color = entropy)) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Latent SPC space", color = "Entropy")

# Panel B — Strategy ternary
p_strategy <- ggtern(strategy_space,
                     aes(Specialist, Opportunist, Explorer,
                         color = dominance)) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Emergent strategies", color = "Dominance")

# Panel C — Trait → strategy map
p_trait_map <- ggplot(strategy_space,
                      aes(S, P, color = strategy_class)) +
  geom_point(size = 3) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Explorer" = "#E64B35",
    "Opportunist" = "#4DBBD5",
    "Specialist" = "#00A087"
  )) +
  labs(
    title = "Latent traits map to behavioural strategies",
    x = "Sensory drive (S)",
    y = "Predictability preference (P)",
    color = "Strategy"
  )

# Panel D — Diet → microbiome
p_micro <- ggplot(strategy_space,
                  aes(entropy, dominance, color = strategy_class)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Explorer" = "#E64B35",
    "Opportunist" = "#4DBBD5",
    "Specialist" = "#00A087"
  )) +
  labs(
    title = "Dietary diversity predicts microbiome structure",
    x = "Dietary entropy",
    y = "Microbiome dominance",
    color = "Strategy"
  )

# Combine Figure 2 panels
figure2 <- (p_drivers | p_strategy) / (p_trait_map | p_micro)

# Save Figure 2
ggsave("Figure2_SPC_Strategies_MC.png",
       plot = figure2,
       width = 10,
       height = 8,
       dpi = 300)

# =========================================================
# FIGURE 3 — DEGENERACY ANALYSIS
# =========================================================

library(proxy)

# -------------------------
# CHECK DATA EXISTS
# -------------------------
# Requires: all_runs (Monte Carlo outputs)
# Expected columns:
# S, P, C, entropy, repertoire_size, dominance, micro_entropy

# -------------------------
# 1. PREP DATA
# -------------------------

latent_mat    <- as.matrix(all_runs[, c("S", "P", "C")])
behaviour_mat <- as.matrix(all_runs[, c("entropy", "repertoire_size")])
micro_mat     <- as.matrix(all_runs[, c("dominance", "micro_entropy")])

# -------------------------
# 2. DISTANCE MATRICES
# -------------------------

latent_dist    <- as.matrix(dist(latent_mat))
behaviour_dist <- as.matrix(dist(behaviour_mat))
micro_dist     <- as.matrix(dist(micro_mat))

dist_df <- data.frame(
  latent      = as.vector(latent_dist),
  behaviour   = as.vector(behaviour_dist),
  microbiome  = as.vector(micro_dist)
) %>%
  filter(latent > 0)

# -------------------------
# 3. DEFINE DEGENERACY
# -------------------------

degenerate_pairs <- dist_df %>%
  mutate(
    degenerate = (latent > quantile(latent, 0.7)) &
                 (behaviour < quantile(behaviour, 0.3))
  )

# -------------------------
# 4. SAMPLE FOR VISUAL CLARITY
# -------------------------

set.seed(123)

plot_df <- degenerate_pairs %>%
  sample_n(min(15000, nrow(degenerate_pairs)))

# -------------------------
# 5. PANELS
# -------------------------

# A — Latent → Behaviour
p_lat_behav <- ggplot(plot_df, aes(latent, behaviour)) +
  geom_point(data = subset(plot_df, !degenerate),
             color = "grey80", alpha = 0.03, size = 0.6) +
  geom_point(data = subset(plot_df, degenerate),
             color = "red", alpha = 0.7, size = 0.8) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Latent vs behavioural distance",
    x = "Latent distance (S, P, C)",
    y = "Behavioural distance"
  )

# B — Behaviour → Microbiome
plot_df <- plot_df %>%
  mutate(behaviour_jitter = behaviour + rnorm(n(), 0, 0.3))

p_behav_micro <- ggplot(plot_df,
                        aes(behaviour_jitter, microbiome)) +
  geom_point(data = subset(plot_df, !degenerate),
             color = "grey80", alpha = 0.03, size = 0.6) +
  geom_point(data = subset(plot_df, degenerate),
             color = "red", alpha = 0.7, size = 0.8) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Behaviour vs microbiome distance",
    x = "Behavioural distance",
    y = "Microbiome distance"
  )

# C — Latent → Microbiome
p_lat_micro <- ggplot(plot_df, aes(latent, microbiome)) +
  geom_point(data = subset(plot_df, !degenerate),
             color = "grey80", alpha = 0.03, size = 0.6) +
  geom_point(data = subset(plot_df, degenerate),
             color = "red", alpha = 0.7, size = 0.8) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Latent vs microbiome distance",
    x = "Latent distance",
    y = "Microbiome distance"
  )

# D — Degenerate regime
p_example <- ggplot(plot_df %>% filter(degenerate),
                    aes(latent, behaviour)) +
  geom_point(color = "red", alpha = 0.25, size = 0.7) +
  geom_density_2d(color = "black", linewidth = 0.5) +
  theme_minimal() +
  labs(
    title = "Degenerate regime structure",
    x = "Latent distance",
    y = "Behavioural distance"
  )

# -------------------------
# 6. COMBINE
# -------------------------

figure3 <- (p_lat_behav | p_behav_micro) /
           (p_lat_micro | p_example)

ggsave("Figure3_Degeneracy.png",
       plot = figure3,
       width = 10,
       height = 8,
       dpi = 300)

# =========================================================
# FIGURE 4 — ROBUSTNESS / SENSITIVITY ANALYSIS
# =========================================================

library(tidyr)
library(purrr)

# -------------------------
# Parameter grid
# -------------------------

grid <- tibble::tribble(
  ~param, ~value, ~label,
  "alpha", 0.02, "low",
  "alpha", 0.1,  "medium",
  "alpha", 0.4,  "high",
  "beta",  1,    "low",
  "beta",  5,    "medium",
  "beta",  15,   "high",
  "decay", 0.5,  "low",
  "decay", 0.9,  "medium",
  "decay", 0.98, "high"
)

n_sims_per_setting <- 30

# -------------------------
# Grid runner
# -------------------------

run_grid_point <- function(param, value, label) {

  alpha <- if (param == "alpha") value else 0.1
  beta  <- if (param == "beta")  value else 5
  decay <- if (param == "decay") value else 0.9

  mc <- lapply(1:n_sims_per_setting, function(s) {
    run_model_param(seed = s, alpha = alpha, beta = beta, decay = decay)
  })

  combined <- bind_rows(mc) %>%
    group_by(agent) %>%
    summarise(
      S = mean(S), P = mean(P), C = mean(C),
      entropy = mean(entropy), repertoire_size = mean(repertoire_size),
      dominance = mean(dominance),
      .groups = "drop"
    )

  latent_dist <- as.matrix(dist(combined[, c("S", "P", "C")]))
  behav_dist  <- as.matrix(dist(scale(combined[, c("entropy", "repertoire_size")])))

  idx <- upper.tri(latent_dist)

  tibble(
    param = param,
    label = label,
    latent_distance = latent_dist[idx],
    behavioural_distance = behav_dist[idx]
  )
}

robustness_pairs <- grid %>%
  pmap_dfr(run_grid_point)

# -------------------------
# Correlations
# -------------------------

cor_labels <- robustness_pairs %>%
  group_by(param, label) %>%
  summarise(
    rho = cor(latent_distance, behavioural_distance, method = "spearman"),
    .groups = "drop"
  ) %>%
  mutate(label_text = paste0("rho = ", sprintf("%.2f", rho)))

# -------------------------
# Plot
# -------------------------

robustness_pairs$label <- factor(robustness_pairs$label,
                                 levels = c("low", "medium", "high"))

param_labels <- c(
  alpha = "Learning rate (alpha)",
  beta  = "Choice stochasticity (beta)",
  decay = "Microbiome persistence (lambda)"
)

p_robust <- ggplot(robustness_pairs,
                   aes(latent_distance, behavioural_distance)) +
  geom_density_2d(colour = "grey40") +
  geom_point(alpha = 0.12, size = 0.7, colour = "grey30") +
  geom_smooth(method = "loess", se = FALSE, colour = "black") +
  geom_text(
    data = cor_labels,
    aes(label = label_text),
    x = Inf, y = Inf,
    hjust = 1.1, vjust = 1.3,
    size = 3
  ) +
  facet_grid(label ~ param,
             labeller = labeller(param = param_labels)) +
  theme_minimal() +
  labs(
    title = "Degeneracy persists across parameter space",
    x = "Latent distance (S, P, C)",
    y = "Behavioural distance"
  )

ggsave("Figure4_Robustness.png",
       p_robust,
       width = 9,
       height = 7,
       dpi = 300)

# =========================================================
# END OF SCRIPT
# =========================================================
