library(dplyr)

# Ideal number of species
Stot = 101

# Generate peak dates for every species, with more peaks mid-year
set.seed(14)
peaks = round(rnorm(Stot, mean = 183, sd = 80))
peaks = peaks[order(peaks)]
# Eliminate peak dates that are too early or late to be meaningful
peaks = peaks[peaks > 5 & peaks < 360]

# Realized number of species with reasonable peak dates
S = length(peaks)

# Assume all species are observed some fixed number of times
nRecsPerSpecies = 100


# Generate data frame of Julian day occurrences by drawing from
# normal distribution around each species' peak date
df = data.frame(jd = 1:365)
for (i in 1:S) {
  
  occs = data.frame(jd = round(rnorm(nRecsPerSpecies, peaks[i], 10))) %>% 
    count(jd)
  
  df = left_join(df, occs, by = 'jd') %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    rename(!!(paste('sp',i, sep = '')) := n)

}

# Sum of all observations on a given Julian day
df$totobs = rowSums(df[,2:ncol(df)])

pdf('figs/correcting_by_tot_obs.pdf', height = )
par(mfrow = c(2, 1), mar = c(4, 4, 1, 1), mgp = c(3, 1, 0))
plot(df$jd, df$totobs, xlab = "Julian day", ylab = "No. observations", type = 'l', las = 1)
points(df$jd[df$sp1 > 0], df$sp1[df$sp1 > 0], type = 'l', col = 'red', lwd = 3)
points(df$jd[df[, round(1+S/2)] > 0], df[, round(1+S/2)][df[, round(1+S/2)] > 0], type = 'l', col = 'purple', lwd = 3)
points(df$jd[df[, S + 1] > 0], df[, S + 1][df[, S + 1] > 0], type = 'l', col = 'blue', lwd = 3)
abline(v = peaks[c(1, round(S/2), S)], col = c('red', 'purple', 'blue'), lty = 'dashed')
text(200, 70, "total observations", adj = 0)
text(8, 20, "early sp", col = 'red')
text(350, 22, "late sp", col = 'blue')
text(180, 20, "mid sp", col = 'purple')

plot(df$jd[df$sp1 > 0], 100*(df$sp1/df$totobs)[df$sp1 > 0], xlab = "Julian day", ylab = "% of total observations", 
     type = 'l', col = 'red', xlim = c(1, 365), ylim = c(0, 100), lwd = 3, las = 1)
points(df$jd[df[, round(1+S/2)] > 0], 100*(df[, round(1+S/2)]/df$totobs)[df[, round(1+S/2)] > 0], type = 'l', col = 'purple', lwd = 3)
points(df$jd[df[, S + 1] > 0], 100*(df[, S + 1]/df$totobs)[df[, S + 1] > 0], type = 'l', col = 'blue', lwd = 3)
abline(v = peaks[c(1, round(S/2), S)], col = c('red', 'purple', 'blue'), lty = 'dashed')
