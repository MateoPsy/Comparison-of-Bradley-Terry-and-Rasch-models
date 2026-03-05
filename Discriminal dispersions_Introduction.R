### Discriminal process graph for two stimuli in Thurstone scaling

# Parameters -------------------------------------------------------------
mu_A <- 0        # discriminal mean for stimulus A
mu_B <- 1        # discriminal mean for stimulus B
sigma_A <- 0.8   # discriminal dispersion (SD) for A
sigma_B <- 0.8   # discriminal dispersion (SD) for B

# Range for plotting
x <- seq(mu_A - 4, mu_B + 4, length.out = 1000)

# Normal density functions
dens_A <- dnorm(x, mean = mu_A, sd = sigma_A)
dens_B <- dnorm(x, mean = mu_B, sd = sigma_B)

# Plot -------------------------------------------------------------
plot(x, dens_A,
     type = "l", lwd = 3, col = "blue",
     xlab = "Underlying psychological continuum",
     ylab = "Density",
     main = "Discriminal Processes for Two Stimuli (Thurstone)")
lines(x, dens_B, lwd = 3, col = "red")

# Add legend
legend("topright",
       legend = c("Stimulus A", "Stimulus B"),
       col = c("blue", "red"),
       lwd = 3)



### Annotated Discriminal Process Graph for Two Stimuli (Thurstone)

# Parameters -------------------------------------------------------------
mu_A <- 0        # discriminal mean (scale value) for A
mu_B <- 1.5        # discriminal mean (scale value) for B
sigma_A <- 0.7   # discriminal dispersion for A
sigma_B <- 0.9   # discriminal dispersion for B

# Range for plotting
x <- seq(mu_A - 4, mu_B + 4, length.out = 2000)

# Densities
dens_A <- dnorm(x, mean = mu_A, sd = sigma_A)
dens_B <- dnorm(x, mean = mu_B, sd = sigma_B)

# ------------------------------------------------------------------------
# Base plot
# ------------------------------------------------------------------------
plot(x, dens_A,
     type = "l", lwd = 3, col = "blue",
     xlab = "Underlying psychological continuum",
     ylab = "Density",
     main = "Thurstone Discriminal Processes with Means and Dispersions",
     ylim = c(0, max(dens_A, dens_B) * 1.2))

lines(x, dens_B, lwd = 3, col = "red")

# ------------------------------------------------------------------------
# Add means (scale values)
# ------------------------------------------------------------------------

segments(mu_A, 0, mu_A, max(dens_A), lty = 2, lwd = 2, col = "blue")
segments(mu_B, 0, mu_B, max(dens_B), lty = 2, lwd = 2, col = "red")


text(mu_A, max(dens_A)*1.05, "Mean of statement A", col = "blue", cex = 0.9)
text(mu_B, max(dens_B)*1.05, "Mean of statement B", col = "red", cex = 0.9)

# ------------------------------------------------------------------------
# Add standard deviation lines (±1 SD) for each discriminal distribution
# ------------------------------------------------------------------------
segments(mu_A - sigma_A, 0, mu_A - sigma_A, dnorm(mu_A - sigma_A, mu_A, sigma_A),
         lwd = 1.5, col = "blue", lty = 3)
segments(mu_B + sigma_B, 0, mu_B + sigma_B, dnorm(mu_B + sigma_B, mu_B, sigma_B),
         lwd = 1.5, col = "red", lty = 3)

text(mu_A - sigma_A, -0.006, "-1 SD", col = "blue", cex = 0.8)
text(mu_B + sigma_B, -0.006, "+1 SD", col = "red", cex = 0.8)

# ------------------------------------------------------------------------
# Overlap shading (probability region underlying the Law of Comparative Judgment)
# ------------------------------------------------------------------------
overlap_y <- pmin(dens_A, dens_B)  # minimum of the two densities
polygon(c(x, rev(x)),
        c(overlap_y, rep(0, length(overlap_y))),
        col = rgb(0.5, 0.5, 0.5, 0.4),  # grey with 40% opacity
        border = NA)


text(mean(c(mu_A, mu_B)), max(dens_A, dens_B)*0.04,
     "Overlapping area =\nprobability of misordering\n(B judged < A or vice versa)",
     cex = 0.9)

# ------------------------------------------------------------------------
# Legend
# ------------------------------------------------------------------------
legend("topright",
       legend = c("Discriminal process A", "Discriminal process B"),
       col = c("blue", "red"),
       lwd = 3,
       bg = "white")





### Annotated Discriminal Process Graph for Two Stimuli (Thurstone)
### Mean lines overlapping other distributions made semi-transparent

# Parameters -------------------------------------------------------------
mu_A <- 0        # discriminal mean (scale value) for A
mu_B <- 1.5      # discriminal mean (scale value) for B
sigma_A <- 0.7   # discriminal dispersion for A
sigma_B <- 0.9   # discriminal dispersion for B

# Range for plotting
x <- seq(mu_A - 4, mu_B + 4, length.out = 2000)

# Densities
dens_A <- dnorm(x, mean = mu_A, sd = sigma_A)
dens_B <- dnorm(x, mean = mu_B, sd = sigma_B)


setwd("Pairwise comparisons_study")

png("Thurstone_discriminal_processes_fainter_means.png",
    width = 1800, height = 1200, res = 175)
# ------------------------------------------------------------------------
# Base plot
# ------------------------------------------------------------------------
plot(x, dens_A,
     type = "l", lwd = 3, col = "blue",
     xlab = "Underlying psychological continuum",
     ylab = "",             # remove y-axis label
     main = "Thurstone Discriminal Processes",
     ylim = c(0, max(dens_A, dens_B) * 1.2),
     yaxt = "n")            # suppress y-axis ticks and labels

lines(x, dens_B, lwd = 3, col = "red")


# ------------------------------------------------------------------------
# Mean lines (up to peak of distribution)
# ------------------------------------------------------------------------
# Use semi-transparent colors for mean lines that overlap the other distribution
segments(mu_A, 0, mu_A, max(dens_A),
         lty = 2, lwd = 2, col = rgb(0, 0, 1, 0.5))  # blue, 50% transparent
segments(mu_B, 0, mu_B, max(dens_B),
         lty = 2, lwd = 2, col = rgb(1, 0, 0, 0.5))  # red, 50% transparent

# ------------------------------------------------------------------------
# ±1 SD lines
# ------------------------------------------------------------------------
segments(mu_A - sigma_A, 0, mu_A - sigma_A, dnorm(mu_A - sigma_A, mu_A, sigma_A),
         lwd = 1.5, col = "blue", lty = 3)
segments(mu_B + sigma_B, 0, mu_B + sigma_B, dnorm(mu_B + sigma_B, mu_B, sigma_B),
         lwd = 1.5, col = "red", lty = 3)

# ------------------------------------------------------------------------
# Grey transparent overlapping area
# ------------------------------------------------------------------------
overlap_y <- pmin(dens_A, dens_B)
polygon(c(x, rev(x)),
        c(overlap_y, rep(0, length(overlap_y))),
        col = rgb(0.5, 0.5, 0.5, 0.4),  # grey 40% opacity
        border = NA)

text(mean(c(mu_A, mu_B)), max(dens_A, dens_B)*0.04,
     "Overlapping area =\nprobability of misordering\n(B judged < A or vice versa)",
     cex = 0.9)

# ------------------------------------------------------------------------
# Legend
# ------------------------------------------------------------------------
legend("topright",
       legend = c("Discriminal distribution A", "Discriminal distribution B"),
       col = c("blue", "red"),
       lwd = 3,
       bg = "white")

dev.off()

