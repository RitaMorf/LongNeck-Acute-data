# Post-hoc power calculation with unequal groups via simulation
set.seed(123)  # Reproduzierbarkeit

# Gruppenbezeichnungen und Größen
group_sizes <- c(EP = 64, AP = 28, DP = 24, FA = 7)
n_total <- sum(group_sizes)
groups <- rep(names(group_sizes), times = group_sizes)

# Effektgröße festlegen (f = 0.29 entspricht eta² ≈ 0.08)
# Wir simulieren Gruppenmittelwerte mit einem realistischen Unterschied
means <- c(0.2, 0.5, 0.8, 1.1)  # mittlerer Effekt über Gruppen hinweg
sd <- 1  # Standardabweichung innerhalb der Gruppen

# Simulationsfunktion für eine ANOVA
simulate_anova <- function() {
  y <- unlist(mapply(function(n, m) rnorm(n, mean = m, sd = sd), group_sizes, means))
  data <- data.frame(y = y, group = factor(groups))
  result <- summary(aov(y ~ group, data = data))
  p_value <- result[[1]][["Pr(>F)"]][1]
  return(p_value < 0.05)
}

# 5000 Durchläufe simulieren
n_sim <- 5000
significant_results <- replicate(n_sim, simulate_anova())
power_estimate <- mean(significant_results)

cat("Post-hoc Power Estimate:", round(power_estimate, 3), "\n")

