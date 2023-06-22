# -------------------------------------------------------------------------- ###
# Soru 1a ----
# -------------------------------------------------------------------------- ###
https://github.com/hilaldogancay/Final21080436.git
# -------------------------------------------------------------------------- ###
# Soru 2a ----
# -------------------------------------------------------------------------- ###
library(dplyr)

titanic %>%
  group_by(sex) %>%
  summarize(mean_fare = mean(fare))

# -------------------------------------------------------------------------- ###
# Soru 2b ----
# -------------------------------------------------------------------------- ###
library(ggplot2)

ggplot(data = na.omit(titanic), aes(x = sex, y = age)) +
  geom_boxplot() +
  xlab("Cinsiyet") +
  ylab("Yaş") +
  ggtitle("Cinsiyete Göre Yaşların Kutu Grafiği")

# -------------------------------------------------------------------------- ###
# Soru 2c ----
# -------------------------------------------------------------------------- ###
ggplot(data = titanic, aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightblue", color = "black") +
  geom_density(alpha = 0.5, fill = "lightgreen") +
  xlab("Yaş") +
  ylab("Yoğunluk") +
  ggtitle("Yaş Dağılımı ve Yoğunluk")

# -------------------------------------------------------------------------- ###
# Soru 3a ----
# -------------------------------------------------------------------------- ###
'10, 13'

# -------------------------------------------------------------------------- ###
# Soru 3b ----
# -------------------------------------------------------------------------- ###
dat3 <- merge(dat1, dat2, all = TRUE)

# -------------------------------------------------------------------------- ###
# Soru 3c ----
# -------------------------------------------------------------------------- ###
library(ggplot2)

# Verilerin tanımlanması
a <- c(-0.5, 0.0, 0.5, 1.0)
b <- c(17.5, 20, 22.5, 25, 27.5)

# Verileri içeren bir veri çerçevesi oluşturma
df <- data.frame(a = a, b = b)

# Saçılım grafiğini çizme
ggplot(data = df, aes(a = a, b = b)) +
  geom_point() +
  xlab("A Eksen Etiketi") +
  ylab("B Eksen Etiketi") +
  ggtitle("Saçılım Grafiği")

# -------------------------------------------------------------------------- ###
# Soru 3d ----
# -------------------------------------------------------------------------- ###
mylist <- list(1:3, c(3:5, NA))
myresult <- map(mylist, ~ mean(.x, na.rm = TRUE)) %>% unlist()

myresult
# [1] 2 4

# -------------------------------------------------------------------------- ###
# Soru 3e ----
# -------------------------------------------------------------------------- ###

µ <- 0  # µ (ortalama)
σ <- 1  # σ (standart sapma)

set.seed(42)
X <- rnorm(25, mean = µ, sd = σ)

Xbar <- mean(X)  
S <- sd(X)  

Z <- 5 * (Xbar - µ) / S  

P_Z_leq_1 <- pnorm(1, mean = 0, sd = 1)  
P_Z_leq_1

# -------------------------------------------------------------------------- ###
# Soru 3f ----
# -------------------------------------------------------------------------- ###
simulate_dice_roll <- function(n) {
  result <- data.frame(Die1 = integer(n), Die2 = integer(n)) 
  
  for (i in 1:n) {
    die1 <- sample(1:6, 1, replace = TRUE)  
    die2 <- sample(1:6, 1, replace = TRUE)  
    
    result[i, "Die1"] <- die1  
    result[i, "Die2"] <- die2
  }
  
  return(result)  
}

# -------------------------------------------------------------------------- ###
# Soru 3g ----
# -------------------------------------------------------------------------- ###
survived_age <- titanic$age[titanic$survived == 1]
not_survived_age <- titanic$age[titanic$survived == 0]
result <- t.test(survived_age, not_survived_age, var.equal = TRUE)
cat("Test İstatistiği:", result$statistic, "\n")
cat("P-değer:", result$p.value, "\n")

# -------------------------------------------------------------------------- ###
# Soru 4a ----
# -------------------------------------------------------------------------- ###
library(tidyr)
dat <- tibble::tribble(
  ~country, ~'2018', ~'2019', ~'2020',
  "_Ingiltere", 8000, 8100, 8500,
  "Almanya", 10000, 11000, 10200
)
names(dat) <- c("country", "2018", "2019", "2020")
dat2 <- gather(dat, year, gdp, `2018`:`2020`)
names(dat2) <- c("country", "year", "gdp")

# -------------------------------------------------------------------------- ###
# Soru 5a ----
# -------------------------------------------------------------------------- ###
library(ggplot2)

dat <- tibble::tribble(
  ~price, ~cut, ~depth, ~color,
  326, "Ideal", 61.5, "E",
  326, "Premium", 59.8, "E",
  327, "Good", 56.9, "E",
  334, "Premium", 62.4, "I",
  335, "Good", 63.3, "J")
  
ggplot(dat, aes(x = depth, y = price, color = color)) +
    geom_point()