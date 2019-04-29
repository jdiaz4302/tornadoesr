


# Import data
tor_df <- read.csv('../../data/raw/tor_data_with_ACS.csv')


# Separating the rows with missing data for analysis
na_df <- tor_df[rowSums(is.na(tor_df)) > 0, ]


# Plot distribution of raw data, log data, missing data, and retained data
par(mfrow = c(2, 2))
hist(tor_df$DAMAGE_PROPERTY, breaks = 50, col = '#3E4A89FF', main = paste0('Raw Property Damages\n(n = ', nrow(tor_df), ')'), xlab = 'U.S. Dollars')
hist(log(tor_df$DAMAGE_PROPERTY + 1, base = 10),
     breaks = seq(from = min(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)),
                  to = max(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)),
                  length.out = 50), col = '#1F9E89FF',
     xlim = c(min(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)), max(log(tor_df$DAMAGE_PROPERTY + 1, base = 10))),
     main = paste0('Log10-transformed\nProperty Damages\n(n = ', nrow(tor_df), ')'), xlab = 'Log10-transformed U.S. Dollars')
hist(log(na_df$DAMAGE_PROPERTY + 1, base = 10), breaks = 50, col = '#CF4446FF',
     xlim = c(min(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)), max(log(tor_df$DAMAGE_PROPERTY + 1, base = 10))),
     main = paste0('Log10-transformed\nOmitted Property Damages\n(n = ', nrow(na_df), ')'), xlab = 'Log10-transformed U.S. Dollars')
hist(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10), col = '#FDE725FF',
     xlim = c(min(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)), max(log(tor_df$DAMAGE_PROPERTY + 1, base = 10))),
     breaks = seq(from = min(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)),
                  to = max(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)),
                  length.out = 50),
     main = paste0('Log10-transformed\nRetained Property Damages\n(n = ', nrow(na.omit(tor_df)), ')'), xlab = 'Log10-transformed U.S. Dollars')
rug(log(na_df$DAMAGE_PROPERTY + 1, base = 10), col = '#CF4446FF', lwd = 2)


# Exploring the zero-inflated poisson distribution (ZIP)
# Plotting the log data, the ZIP distribution, the QQ plot for ZIP, and QQ for normal
par(mfrow = c(2, 2))
hist(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10), col = '#FDE725FF',
     xlim = c(min(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)), max(log(tor_df$DAMAGE_PROPERTY + 1, base = 10))),
     breaks = seq(from = min(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)),
                  to = max(log(tor_df$DAMAGE_PROPERTY + 1, base = 10)),
                  length.out = 50),
     main = paste0('Log10-transformed\nRetained Property Damages\n(n = ', nrow(na.omit(tor_df)), ')'), xlab = 'Log10-transformed U.S. Dollars')
hist(main = 'Simulated from a\nZero-Inflated Poisson\n(rate = 14, p = 0.3, scaled by 1/3)', rzipois(23000, 14, 0.3) / 3, breaks = 50,
     col = '#FDE725FF', xlab = 'x')
qqplot(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10), rzipois(23000, 14, 0.3) / 3,
       xlab = 'Log10-transformed U.S. Dollars', ylab = 'Simulated from Zero-Inflated Poisson',
       col = '#FDE725FF',
       xlim = c(min(rzipois(23000, 14, 0.3) / 3), max(rzipois(23000, 14, 0.3) / 3)),
       cex = 0.5, main = 'QQ with Zero-Inflated Poisson\n(rate = 14, p = 0.3, scaled by 1/3)')
abline(a = 0, b = 1, col = 'black')
qqplot(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10),
       rnorm(23000,
             mean(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10)),
             sd(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10))),
       xlab = 'Log10-transformed U.S. Dollars', ylab = 'Simulated from Normal',
       col = '#CF4446FF', xlim = c(min(rnorm(23000,
             mean(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10)),
             sd(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10)))),
                                  max(rnorm(23000,
             mean(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10)),
             sd(log(na.omit(tor_df)$DAMAGE_PROPERTY + 1, base = 10))))),
       cex = 0.5, , main = 'QQ with Normal\n(mu = sample mean, sd = sample sd)')
abline(a = 0, b = 1, col = 'black')


