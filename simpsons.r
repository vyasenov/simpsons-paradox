# Load required libraries
library(ggplot2)
library(patchwork)
library(dplyr)

# Set random seed for reproducibility
set.seed(4904)

# Define data dimensions
n_a <- 200
n_b <- 20

# Create the dataset
data <- data.frame(
  Hospital = rep(c("Hospital A", "Hospital B"), each = n_a + n_b),
  Treatment = c(
    rep("Treatment 1", n_a), rep("Treatment 2", n_b),  # Hospital A
    rep("Treatment 1", n_b), rep("Treatment 2", n_a)   # Hospital B
  ),
  Improvement = c(
    rnorm(n_a, mean = 20, sd = 2), rnorm(n_b, mean = 20, sd = 2),  # Hospital A
    rnorm(n_b, mean = 10, sd = 2), rnorm(n_a, mean = 10, sd = 2)   # Hospital B
  ),
  Aggregated = "Overall"
)

# Calculate mean improvement for each treatment and hospital
mean_improvement <- data %>%
  group_by(Hospital, Treatment) %>%
  summarize(Mean_Improvement = mean(Improvement), .groups = "drop")

# Calculate overall mean improvement for each treatment
overall_mean <- data %>%
  group_by(Treatment) %>%
  summarize(Mean_Improvement = mean(Improvement), .groups = "drop")

# Create the disaggregated plot
plot1 <- ggplot(data, aes(x = Hospital, y = Improvement, color = Treatment)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
  geom_hline(data = mean_improvement, aes(yintercept = Mean_Improvement, color = Treatment), linetype = "solid") +
  labs(
    title = "By Hospital",
    x = NULL,
    y = "Health Improvement",
    color = "Treatment"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Create the aggregated plot
plot2 <- ggplot(data, aes(x = Aggregated, y = Improvement, color = Treatment)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
  geom_hline(data = overall_mean, aes(yintercept = Mean_Improvement, color = Treatment), linetype = "solid") +
  labs(
    title = "Overall",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

# Combine the plots with a common legend and title
final_plot <- (plot1 + plot2) +
  plot_annotation(
    title = "Simpson's Paradox: Treatment Effectiveness Within Hospitals and Overall",
    theme = theme(plot.title = element_text(hjust = 0.5))
  ) &
  theme(legend.position = "bottom")

# Display the combined plot
print(final_plot)


# Save the plot as a PNG file
ggsave(
  filename = "simpsons_paradox_plot.png",  # File name
  plot = final_plot,                      # The plot object
  width = 10,                             # Width in inches
  height = 6,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)
