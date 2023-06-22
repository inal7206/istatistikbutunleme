# -------------------------------------------------------------------------- ###
# Soru 1a ---- https://github.com/inal7206/istatistikbutunleme.git
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2a ----
library(dplyr)

titanic %>%
  group_by(sex) %>%
  summarize(mean_fare = mean(fare))

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2b ----  
titanic %>% 
na.omit() %>% 
  ggplot(aes(x = sex, y = age)) +
  geom_boxplot() +
  labs(x = "Cinsiyet",
       y = "yaş") 
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 2c ---- 
titanic%>%
ggplot(aes(x = age)) +
  geom_histogram(aes(y = ..density..),
                 colour = "black", 
                 fill = "white") +
  geom_density(alpha = 0.01, 
               fill = "#ff0000") 
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3a ----  
10 ve 13
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3b ---- 
library(dplyr)

dat3 <- inner_join(dat1, dat2)
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3c ----  titanic %>%
ggplot(aes(x = a, y = b)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous("a") +
  scale_y_continuous("b")
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3d ---- 
2.000000 NA
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3e ---- 
zarlar <- function() {
zar1 <- sample(1:6, 1, replace = TRUE)  
zar2 <- sample(1:6, 1, replace = TRUE)

cat("Zar 1 sonucu:", zar1, "\n")
cat("Zar 2 sonucu:", zar2, "\n")

return(list(zar1 = zar1, zar2 = zar2))

zarSonuclari <- zarlar()

zarSonuclari$zar1
zarSonuclari$zar2
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3f ----
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 3g ----
library(dplyr)
library(stats)

mean_survived <- titanic %>%
  filter(survived == 1) %>%
  summarize(mean_age = mean(age, na.rm = TRUE))

mean_not_survived <- titanic %>%
  filter(survived == 0) %>%
  summarize(mean_age = mean(age, na.rm = TRUE))

t.test(mean_survived$mean_age, mean_not_survived$mean_age, var.equal = TRUE)

# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 4a ----
library(tidyr)

dat2 <- tibble::tribble(
  ~country,    ~year, ~gdp,
  "_Ingiltere", "2018", 8000,
  "_Ingiltere", "2019", 8100,
  "_Ingiltere", "2020", 8500,
  "Almanya",    "2018", 10000,
  "Almanya",    "2019", 11000,
  "Almanya",    "2020", 10200
)

dat2 <- pivot_wider(dat2, names_from = year, values_from = gdp)

colnames(dat2) <- gsub("`", "", colnames(dat2))

print(dat2)

  
# -------------------------------------------------------------------------- ###


# -------------------------------------------------------------------------- ###
# Soru 5a ----  
ggplot(dat, aes(x = price)) +
geom_histogram(fill = "white", color = "black", bins = 10) +
  labs(title = "Price Histogram", x = "Price", y = "Frequency")

ggplot(dat, aes(x = cut)) +
  geom_histogram(fill = "white", color = "black", bins = 5) +
  labs(title = "Cut Histogram", x = "Cut", y = "Frequency")

ggplot(dat, aes(x = depth)) +
  geom_histogram(fill = "white", color = "black", bins = 10) +
  labs(title = "Depth Histogram", x = "Depth", y = "Frequency")

ggplot(dat, aes(x = color)) +
  geom_histogram(fill = "white", color = "black", bins = 5) +
  labs(title = "Color Histogram", x = "Color", y = "Frequency")
# -------------------------------------------------------------------------- ###