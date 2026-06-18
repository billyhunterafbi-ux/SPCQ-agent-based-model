# =========================================================
# SPC-Q AGENT-BASED MODEL WITH MONTE CARLO SIMULATIONS
# ---------------------------------------------------------
# This script implements the Sensory–Predictability–Context
# (SPC-Q) model described in the accompanying manuscript.
#
# The model simulates food choice as a stochastic,
# state-dependent decision process with reinforcement
# learning, and links emergent dietary behaviour to
# microbiome structure. Multiple Monte Carlo runs are
# performed to characterise variability and degeneracy.
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
library(dplyr)      # data manipulation
library(tidyr)      # tidy data tools
library(ggplot2)    # plotting
library(proxy)      # distance calculations
library(ggtern)     # ternary plots
library(knitr)      # summary tables

# =========================================================
# GLOBAL SETTINGS
# =========================================================

n_sims <- 30   # number of Monte Carlo simulations

# =========================================================
# UTILITY FUNCTIONS
# =========================================================

# Softmax choice rule
# Converts utilities into choice probabilities
softmax <- function(u, beta = 5) {
  u_scaled <- beta * u
  u_scaled <- u_scaled - max(u_scaled)  # numerical stability
  exp(u_scaled) / sum(exp(u_scaled))
}

# =========================================================
# CORE MODEL FUNCTION
# =========================================================

# Runs a single simulation given a random seed
run_model <- function(seed) {
  
  set.seed(seed)
  
  # -------------------------
  # FOOD ENVIRONMENT
  # -------------------------
  n_foods <- 100
  foods <- data.frame(
    food_id        = 1:n_foods,
    intensity      = runif(n_foods, 0, 1),
    novelty        = runif(n_foods, 0, 1),
    predictability = runif(n_foods, 0, 1)
  )
  
  # -------------------------
  # MICROBIOME STRUCTURE
  # -------------------------
  n_taxa   <- 120
  n_groups <- 4
  
  # Assign taxa to functional groups
  taxa_groups <- sample(1:n_groups, n_taxa, replace = TRUE)
  
  # Diet–microbiome mapping matrix
  food_microbe_profile <- matrix(0, nrow = n_foods, ncol = n_taxa)
  
  for (i in 1:n_foods) {
    active_groups <- sample(1:n_groups, 2)
    
    for (g in active_groups) {
      group_idx <- which(taxa_groups == g)
      active_taxa <- sample(group_idx,
                            size = max(1, round(0.2 * length(group_idx))))
      food_microbe_profile[i, active_taxa] <-
        runif(length(active_taxa), 0.8, 1)
    }
  }
  
  # Enforce sparsity
  food_microbe_profile[food_microbe_profile < 0.85] <- 0
  
  # -------------------------
  # AGENT POPULATION
  # -------------------------
  n_agents <- 60
  agents <- data.frame(
    agent_id = 1:n_agents,
    S = rnorm(n_agents),   # sensory drive
    P = rnorm(n_agents),   # predictability preference
    C = runif(n_agents)    # context sensitivity
  )
  
  # -------------------------
  # ENVIRONMENTAL DYNAMICS
  # -------------------------
  T <- 200
  env <- data.frame(
    t              = 1:T,
    stress         = runif(T),
    sensory_load   = runif(T),
    predictability = runif(T)
  )
  
  # -------------------------
  # STORAGE OBJECTS
  # -------------------------
  results <- data.frame()            # food choice trajectories
  microbiome_results <- data.frame() # final microbiome summaries
  
  alpha <- 0.1  # learning rate
  decay <- 0.9  # microbiome persistence
  
  # =========================================================
  # AGENT LOOP
  # =========================================================
  
  for (a in 1:n_agents) {
    
    S <- agents$S[a]
    P <- agents$P[a]
    C <- agents$C[a]
    
    Q <- rep(0, n_foods)   # learned food values
    microbiome <- rep(0, n_taxa)
    
    for (t in 1:T) {
      
      E <- env[t, ]
      
      # State-dependent modulation
      S_t <- S + C * (0.5 * E$stress - 0.3 * E$sensory_load)
      P_t <- P + C * (0.4 * E$predictability - 0.3 * E$stress)
      
      # Utility calculation
      U <- with(foods,
                S_t * (intensity + novelty) -
                  abs(S_t) * intensity +
                  P_t * predictability -
                  E$stress)
      
      U <- U + Q
      
      # Probabilistic choice
      probs  <- softmax(U, beta = 5)
      choice <- sample(1:n_foods, 1, prob = probs)
      
      # Reinforcement learning update
      reward <-
        S_t * foods$novelty[choice] +
        P_t * foods$predictability[choice] -
        E$stress
      
      Q[choice] <- Q[choice] + alpha * (reward - Q[choice])
      
      # Microbiome update
      microbiome <- decay * microbiome + food_microbe_profile[choice, ]
      microbiome <- microbiome / sum(microbiome)
      
      # Store trajectory
      results <- rbind(results, data.frame(
        agent  = a,
        time   = t,
        choice = choice
      ))
    }
    
    # -------------------------
    # MICROBIOME SUMMARY METRICS
    # -------------------------
    
    microbiome_prop <- microbiome / sum(microbiome)
    counts <- as.numeric(rmultinom(1, 1000, microbiome_prop))
    observed_prop <- counts / sum(counts)
    
    p <- observed_prop[observed_prop > 0]
    
    microbiome_results <- rbind(
      microbiome_results,
      data.frame(
        agent          = a,
        micro_entropy  = -sum(p * log(p)),
        micro_richness = sum(counts > 0),
        dominance      = max(observed_prop)
      )
    )
  }
  
  # -------------------------
  # DIETARY METRICS
  # -------------------------
  
  diet_profile <- results %>%
    group_by(agent, choice) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(agent) %>%
    mutate(p = n / sum(n)) %>%
    summarise(
      repertoire_size = n_distinct(choice),
      entropy         = -sum(p * log(p))
    )
  
  agent_summary <- agents %>%
    rename(agent = agent_id) %>%
    left_join(diet_profile, by = "agent") %>%
    left_join(microbiome_results, by = "agent")
  
  agent_summary$sim <- seed
  
  return(list(
    agent_summary = agent_summary,
    results       = results
  ))
}

# =========================================================
# MONTE CARLO EXECUTION
# =========================================================

mc_output <- lapply(1:n_sims, run_model)

# Combine agent-level summaries
all_runs <- bind_rows(lapply(mc_output, function(x) x$agent_summary))

# Combine time-series data
all_results <- bind_rows(
  lapply(seq_along(mc_output), function(i) {
    df <- mc_output[[i]]$results
    df$sim <- i
    df
  })
)

# =========================================================
# AGGREGATED SUMMARY
# =========================================================

agent_summary <- all_runs %>%
  group_by(agent) %>%
  summarise(
    S               = mean(S),
    P               = mean(P),
    C               = mean(C),
    entropy         = mean(entropy),
    repertoire_size = mean(repertoire_size),
    dominance       = mean(dominance),
    micro_entropy   = mean(micro_entropy),
    micro_richness  = mean(micro_richness)
  )

# =========================================================
# COMPOSITIONAL TRANSFORM (SPC SPACE)
# =========================================================

agent_summary <- agent_summary %>%
  mutate(
    S_pos = abs(S),
    P_pos = abs(P),
    C_pos = C
  ) %>%
  rowwise() %>%
  mutate(
    total  = S_pos + P_pos + C_pos,
    S_prop = S_pos / total,
    P_prop = P_pos / total,
    C_prop = C_pos / total
  ) %>%
  ungroup()

# =========================================================
# BASIC PLOT (EXAMPLE)
# =========================================================

p1 <- ggplot(agent_summary, aes(entropy, dominance)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    title = "Diet diversity vs microbiome dominance",
    x = "Dietary entropy",
    y = "Dominance"
  )

print(p1)

# =========================================================
# STRATEGY SPACE
# =========================================================

strategy_space <- agent_summary %>%
  mutate(
    entropy_norm = (entropy - min(entropy)) /
      (max(entropy) - min(entropy))
  ) %>%
  mutate(
    Specialist  = 1 - entropy_norm,
    Explorer    = entropy_norm,
    Opportunist = 1 - abs(entropy_norm - 0.5) * 2
  ) %>%
  rowwise() %>%
  mutate(
    total = Specialist + Explorer + Opportunist,
    Specialist  = Specialist  / total,
    Explorer    = Explorer    / total,
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

# =========================================================
# STRATEGY PLOT
# =========================================================

p2 <- ggplot(strategy_space,
             aes(entropy, dominance, color = strategy_class)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal()

print(p2)

# =========================================================
# SUMMARY TABLE
# =========================================================

summary_df <- strategy_space %>%
  group_by(strategy_class) %>%
  summarise(
    mean_dom = mean(dominance),
    beta_dom = sd(dominance),
    mean_div = mean(micro_richness),
    beta_div = sd(micro_richness)
  )

kable(summary_df, digits = 3)

write.csv(
  summary_df,
  "Microbiome_alpha_beta_summary_MC.csv",
  row.names = FALSE
)
