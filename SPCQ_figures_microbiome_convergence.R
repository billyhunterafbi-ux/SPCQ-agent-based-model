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
# END OF SCRIPT
# =========================================================
