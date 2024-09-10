# Define parameters
power <- 0.8
alpha <- 0.05
n_ratio <- 1
mde <- -0.07  # Specify the Minimum Detectable Effect (MDE)

# Function to calculate sample size
calculate_sample_size <- function(mde, power, alpha, n_ratio) {
  # Load pwr package
  library(pwr)
  
  # Calculate sample size
  sample_size <- pwr.t.test(d = mde, sig.level = alpha, power = power, alternative = "two.sided")$n
  adjusted_sample_size <- sample_size * n_ratio
  return(adjusted_sample_size)
}

# Calculate sample size
sample_size <- calculate_sample_size(mde, power, alpha, n_ratio)

# Print sample size
cat("Required sample size:", sample_size, "\n")
install.packages("gt")
library(gt)

# Define parameters
parameters <- data.frame(
  Parameter = c("Power", "Alpha", "N Ratio", "MDE"),
  Value = c(power, alpha, n_ratio, mde)
)

# Calculate sample size
sample_size <- calculate_sample_size(mde, power, alpha, n_ratio)

# Combine parameters and sample size into a single data frame
results <- rbind(parameters, data.frame(Parameter = "Sample Size", Value = sample_size))

# Create a gt table
table <- gt(results) %>%
  tab_header(title = "Sample Size Calculation Results")

# Print the table
print(table)