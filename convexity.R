library(ggplot2)

# Function to calculate convexity for regular bonds
calc_convexity_regular_bond <- function(C, F, i, N, f) {
  i <- i / 2  # Adjust for semiannual
  
  D1 <- (C * ((1 + i)^N * (1 + i - f) - (1 + i)) + i * (N - f) * (F * i - C)) / (C * i * ((1 + i)^N - 1) + F * i^2)
  D2 <- (C * (1 + i)^N * ((1 + i - f)^2 + 1 + i) - (1 + i) * (2 * (1 + i + N * i - f) + i) + i^2 * (N - f)^2 * (F * i - C)) / 
    (C * i^2 * ((1 + i)^N - 1) + F * i^3)
  
  convexity <- (D1 + D2) / (1 + i)^2
  
  return(convexity)
}

# Parameters
C <- 6 # Semiannual coupon payment
F <- 100 # Face value
N <- 20 # Total number of periods
f <- 0.5 # Fraction of period since last coupon

# Generating a sequence of yields to maturity from 1% to 10%
yields <- seq(0.01, 0.10, by = 0.001)
convexities <- sapply(yields, function(i) calc_convexity_regular_bond(C, F, i, N, f))

# Creating a data frame for plotting
data <- data.frame(Yield = yields, Convexity = convexities)

# Plotting
ggplot(data, aes(x = Yield, y = Convexity)) +
  geom_line(color = "blue") +
  ggtitle("Convexity vs. Yield to Maturity for a Regular Bond") +
  xlab("Yield to Maturity") +
  ylab("Convexity") +
  theme_minimal()
