library(distr)
library(tidyr)
library(dplyr)
library(MASS)
library(fitdistrplus)

plik28 <- read.csv("C:/Users/Tomek/Desktop/studia/3 rok/2_semestr/RyzykoKredytowe/cw4/plik28.csv", sep=";")

plik28 <- plik28 |>
  mutate(strata = as.numeric(strata))

# BussDistr ---------------------------------------------------------------

rok_BussDistr <- plik28 |>
  filter( linia == "Buss_Distr") |>
  group_by(rok) |>
  summarize(liczba_zdarzen = n())

straty_BussDistr <- plik28 |>
  filter(linia == "Buss_Distr") |>
  group_by(rok) |>
  summarize(straty = sum(strata, na.rm = TRUE))

mean_BussDistr = mean(rok_BussDistr$liczba_zdarzen)
sd_BussDistr = sd(rok_BussDistr$liczba_zdarzen)
#�EX >𝐷2𝑋 rozkład dwumianowy-
#𝐸𝑋 =𝐷2𝑋 rozkład Poissona-
#𝐸𝑋 <𝐷2𝑋 rozkład ujemny dwumianowy

BussDistr_fit <- fitdist(rok_BussDistr$liczba_zdarzen, "nbinom") # rozkład ujemny dwumianowy
gofstat(BussDistr_fit)
plot(fitdist(rok_BussDistr$liczba_zdarzen, "nbinom"))


#BussDistr1 <- fitdist(straty_BussDistr$straty, "weibull")
BussDistr2 <- fitdist(straty_BussDistr$straty, method="mme","lnorm")
BussDistr3 <- fitdist(straty_BussDistr$straty, method="mme","exp")
BussDistr4 <- fitdist(straty_BussDistr$straty, method="mme","gamma")
names_test = c("lnorm", "exp", "gamma")

Result_BussDist<-gofstat(list(BussDistr2, BussDistr3, BussDistr4), fitnames=names_test)
Result_BussDist$chisqpvalue
Result_BussDist$adtest
Result_BussDist$kstest

plot(fitdist(straty_BussDistr$straty, "gamma"))
plot(fitdist(straty_BussDistr$straty, "lnorm"))
#Lnorm

set.seed(123)

#najpierw losujesz liczbę zdarzeń (z nbinom)
#potem losujesz tyle strat z rozkładu lnorm
#sumujesz straty → to Twój 1 scenariusz strat rocznych

# Parametry dopasowanego rozkładu nbinom
nbinom_params <- BussDistr_fit$estimate 
size_param <- nbinom_params["size"]
mu_param <- nbinom_params["mu"]

# Parametry dopasowanego rozkładu log-normalnego
lnorm_params <- BussDistr2$estimate
meanlog <- lnorm_params["meanlog"]
sdlog <- lnorm_params["sdlog"]

n_sim <- 20000 #liczba symulacji

# Symulacje
symulacje_BussDist <- replicate(n_sim, {
  liczba_zdarzen <- rnbinom(1, size = size_param, mu = mu_param)
  if (liczba_zdarzen == 0) return(0)
  sum(rlnorm(liczba_zdarzen, meanlog = meanlog, sdlog = sdlog))
})

# Obliczenie OpVaR (99.9%) i OpES
OpVaR_999_BussDist <- quantile(symulacje_BussDist, 0.999)
OpES_999_BussDist <- mean(symulacje_BussDist[symulacje_BussDist > OpVaR_999_BussDist])

#histogramu rozkładu symulowanych strat:
hist_BussDist <-hist(symulacje_BussDist, breaks = 100, col = "lightblue", main = "Symulowany rozkład strat rocznych", xlab = "Suma strat")
abline(v = OpVaR_999_BussDist, col = "red", lwd = 2, lty = 2)


cat("OpVaR (99.9%) =", OpVaR_999, "\n")
cat("OpES  (99.9%) =", OpES_999, "\n")



# Com_Ban -----------------------------------------------------------------


rok_Com_Ban <- plik28 |>
  filter( linia == "Com_Ban") |>
  group_by(rok) |>
  summarize(liczba_zdarzen = n())

straty_Com_Ban <- plik28 |>
  filter(linia == "Com_Ban") |>
  group_by(rok) |>
  summarize(straty = sum(strata, na.rm = TRUE))

mean_Com_Ban = mean(rok_Com_Ban$liczba_zdarzen)
sd_Com_Ban = sd(rok_Com_Ban$liczba_zdarzen)

size_value <- 100


Com_Ban_fit <- fitdist(rok_Com_Ban$liczba_zdarzen, "binom",fix.arg = list(size = size_value))
gofstat(Com_Ban_fit)
plot(Com_Ban_fit)


#Com_Ban <- fitdist(straty_Com_Ban$straty,"weibull")
Com_Ban2 <- fitdist(straty_Com_Ban$straty, method="mme","lnorm")
Com_Ban3 <- fitdist(straty_Com_Ban$straty, method="mme","exp")
Com_Ban4 <- fitdist(straty_Com_Ban$straty, method="mme","gamma")
names_test = c("lnorm", "exp", "gamma")

Result_Com_Ban<-gofstat(list(Com_Ban2, Com_Ban3, Com_Ban4), fitnames=names_test)
Result_Com_Ban$chisqpvalue
Result_Com_Ban$adtest
Result_Com_Ban$kstest

plot(fitdist(straty_Com_Ban$straty, "gamma"))
plot(fitdist(straty_Com_Ban$straty, "lnorm"))

#gamma

set.seed(123)


# Parametry dopasowanego rozkładu binom
binom_params <- Com_Ban_fit$estimate 
prob_param <- binom_params["prob"]

# Parametry dopasowanego rozkładu log-normalnego
gamma_params <- Com_Ban4$estimate
rate <- gamma_params["rate"]
shape <- gamma_params["shape"]

n_sim <- 20000 #liczba symulacji
size_param_binom <- 100

symulacje_Com_Ban <- replicate(n_sim, {
  liczba_zdarzen <- rbinom(1, size = size_param_binom, prob = prob_param)
  if (liczba_zdarzen == 0) return(0)
  sum(rexp(liczba_zdarzen, rate = rate))
})

# Obliczenie OpVaR (99.9%) i OpES
OpVaR_999_Com_Ban <- quantile(symulacje_Com_Ban, 0.999)
OpES_999_Com_Ban <- mean(symulacje_Com_Ban[symulacje_Com_Ban > OpVaR_999_Com_Ban])

#histogramu rozkładu symulowanych strat:
hist_Com_Ban <-hist(symulacje_Com_Ban, breaks = 100, col = "lightblue",
     main = "Symulowany rozkład strat rocznych (Com_Ban)",
     xlab = "Suma strat")
abline(v = OpVaR_999_Com_Ban, col = "red", lwd = 2, lty = 2)



cat("OpVaR (99.9%) = ", OpVaR_999_Com_Ban, "\n")
cat("OpES  (99.9%) = ", OpES_999_Com_Ban, "\n")


# Damage ------------------------------------------------------------------


rok_Damage <- plik28 |>
  filter( linia == "Damage") |>
  group_by(rok) |>
  summarize(liczba_zdarzen = n())

straty_Damage <- plik28 |>
  filter(linia == "Damage") |>
  group_by(rok) |>
  summarize(straty = sum(strata, na.rm = TRUE))


mean_Damage = mean(rok_Damage$liczba_zdarzen)
sd_Damage = sd(rok_Damage$liczba_zdarzen)


Damage_fit <- fitdist(rok_Damage$liczba_zdarzen, "binom",fix.arg = list(size = size_value))
gofstat(Damage_fit)
plot(Damage_fit)

#Com_Ban <- fitdist(straty_Damage$straty,"weibull")
Damage2 <- fitdist(straty_Damage$straty, method="mme","lnorm")
Damage3 <- fitdist(straty_Damage$straty, method="mme","exp")
Damage4 <- fitdist(straty_Damage$straty, method="mme","gamma")
#names_test = c("weibull","lnorm", "exp", "gamma")
names_test = c("lnorm", "exp", "gamma")

Result_Damage<-gofstat(list(Damage2, Damage3, Damage4), fitnames=names_test)
Result_Damage$chisqpvalue
Result_Damage$adtest
Result_Damage$kstest

plot(fitdist(straty_Damage$straty, "gamma"))
plot(fitdist(straty_Damage$straty, "exp"))
#exp

set.seed(123)

# Parametry dopasowanego rozkładu binom
binom_params2 <- Damage_fit$estimate 
prob_param2 <- binom_params2["prob"]

# Parametry dopasowanego rozkładu log-normalnego
exp_params2 <- Damage3$estimate
rate2 <- exp_params2["rate"]

n_sim <- 20000 #liczba symulacji
size_param_binom2 <- 100

symulacje_Damage <- replicate(n_sim, {
  liczba_zdarzen <- rbinom(1, size = size_param_binom2, prob = prob_param2)
  if (liczba_zdarzen == 0) return(0)
  sum(rexp(liczba_zdarzen, rate = rate2))
})

# Obliczenie OpVaR (99.9%) i OpES
OpVaR_999_Damage <- quantile(symulacje_Damage, 0.999)
OpES_999_Damage <- mean(symulacje_Damage[symulacje_Damage > OpVaR_999_Damage])

#histogramu rozkładu symulowanych strat:
hist_Damage <- hist(symulacje_Damage, breaks = 100, col = "lightblue",
     main = "Symulowany rozkład strat rocznych (Damage)",
     xlab = "Suma strat")
abline(v = OpVaR_999_Damage, col = "red", lwd = 2, lty = 2)

cat("OpVaR (99.9%) = ", OpVaR_999_Damage, "\n")
cat("OpES  (99.9%) = ", OpES_999_Damage, "\n")



# Empl_Pract --------------------------------------------------------------


rok_Empl_Pract <- plik28 |>
  filter( linia == "Empl_Pract") |>
  group_by(rok) |>
  summarize(liczba_zdarzen = n())

straty_Empl_Pract <- plik28 |>
  filter(linia == "Empl_Pract") |>
  group_by(rok) |>
  summarize(straty = sum(strata, na.rm = TRUE))

mean_Empl_Pract = mean(rok_Empl_Pract$liczba_zdarzen)
sd_Empl_Pract = sd(rok_Empl_Pract$liczba_zdarzen)



Empl_Pract_fit <- fitdist(rok_Empl_Pract$liczba_zdarzen, "nbinom")
gofstat(Empl_Pract_fit)
plot(Empl_Pract_fit)

Empl_Pract <- fitdist(straty_Empl_Pract$straty,"weibull")
Empl_Pract2 <- fitdist(straty_Empl_Pract$straty, method="mme","lnorm")
Empl_Pract3 <- fitdist(straty_Empl_Pract$straty, method="mme","exp")
Empl_Pract4 <- fitdist(straty_Empl_Pract$straty, method="mme","gamma")
names_test = c("weibull","lnorm", "exp", "gamma")
#names_test = c("lnorm", "exp", "gamma")

Result_Empl_Pract<-gofstat(list(Empl_Pract,Empl_Pract2, Empl_Pract3, Empl_Pract4), fitnames=names_test)
Result_Empl_Pract$chisqpvalue
Result_Empl_Pract$adtest
Result_Damage$kstest

plot(fitdist(straty_Empl_Pract$straty, "lnorm"))
plot(fitdist(straty_Empl_Pract$straty, "gamma"))

#lnorm/gamma

set.seed(123)

# Parametry dopasowanego rozkładu binom
nbinom_params3 <- Empl_Pract_fit$estimate 
size_param3 <- nbinom_params3["size"]
mu_param3 <- nbinom_params3["mu"]

# Parametry dopasowanego rozkładu gamma
gamma_params3 <- Empl_Pract4$estimate
shape_param3 <- gamma_params3["shape"]
rate_param3 <- gamma_params3["rate"]

n_sim <- 20000 #liczba symulacji

symulacje_Empl_Pract <- replicate(n_sim, {
  liczba_zdarzen <- rnbinom(1, size = size_param3, mu = mu_param3)
  if (liczba_zdarzen == 0) return(0)
  sum(rgamma(liczba_zdarzen, shape = shape_param3, rate = rate_param3))
})

# Obliczenie OpVaR (99.9%) i OpES
OpVaR_999_Empl_Pract <- quantile(symulacje_Empl_Pract, 0.999)
OpES_999_Empl_Pract <- mean(symulacje_Empl_Pract[symulacje_Empl_Pract > OpVaR_999_Empl_Pract])

hist_Empl_Pract <- hist(symulacje_Empl_Pract, breaks = 100, col = "lightblue",
     main = "Symulowany rozkład strat rocznych (Empl_Pract)",
     xlab = "Suma strat")
abline(v = OpVaR_999_Empl_Pract, col = "red", lwd = 2, lty = 2)

# Wyniki
cat("OpVaR (99.9%) = ", OpVaR_999_Empl_Pract, "\n")
cat("OpES  (99.9%) = ", OpES_999_Empl_Pract, "\n")



# Execut_Del --------------------------------------------------------------

rok_Execut_Del <- plik28 |>
  filter( linia == "Execut_Del") |>
  group_by(rok) |>
  summarize(liczba_zdarzen = n())

straty_Execut_Del <- plik28 |>
  filter(linia == "Execut_Del") |>
  group_by(rok) |>
  summarize(straty = sum(strata, na.rm = TRUE))

mean_Execut_Del = mean(rok_Execut_Del$liczba_zdarzen)
sd_Execut_Del = sd(rok_Execut_Del$liczba_zdarzen)


Execut_Del_fit <- fitdist(rok_Execut_Del$liczba_zdarzen, "binom",fix.arg = list(size = size_value))
gofstat(Execut_Del_fit)
plot(Execut_Del_fit)


Execut_Del1 <- fitdist(straty_Execut_Del$straty, "weibull")
Execut_Del2 <- fitdist(straty_Execut_Del$straty, method="mme","lnorm")
Execut_Del3 <- fitdist(straty_Execut_Del$straty, method="mme","exp")
Execut_Del4 <- fitdist(straty_Execut_Del$straty, method="mme","gamma")
names_test = c("Weibull","lnorm", "exp", "gamma")

Result_Execut_Del<-gofstat(list(Execut_Del1, Execut_Del2,Execut_Del3,Execut_Del4), fitnames=names_test)
Result_Execut_Del$chisqpvalue
Result_Execut_Del$adtest
Result_Execut_Del$kstest

plot(fitdist(straty_Execut_Del$straty, "weibull"))
plot(fitdist(straty_Execut_Del$straty, "gamma"))

#weibull

set.seed(123)

# Parametry binom
binom_params_exec <- Execut_Del_fit$estimate
prob_param_exec <- binom_params_exec["prob"]

# Parametry Weibulla
weibull_params_exec <- Execut_Del1$estimate
shape_exec <- weibull_params_exec["shape"]
scale_exec <- weibull_params_exec["scale"]


n_sim <- 20000
size_param_binom <- 100

symulacje_Execut_Del <- replicate(n_sim, {
  liczba_zdarzen <- rbinom(1, size = size_param_binom, prob = prob_param_exec)
  if (liczba_zdarzen == 0) return(0)
  sum(rweibull(liczba_zdarzen, shape = shape_exec, scale = scale_exec))
})

OpVaR_999_Execut_Del <- quantile(symulacje_Execut_Del, 0.999)
OpES_999_Execut_Del <- mean(symulacje_Execut_Del[symulacje_Execut_Del > OpVaR_999_Execut_Del])

hist_Execut_Del <- hist(symulacje_Execut_Del, breaks = 100, col = "lightblue",
     main = "Symulowany rozkład strat (Execut_Del)", xlab = "Suma strat")
abline(v = OpVaR_999_Execut_Del, col = "red", lwd = 2, lty = 2)

cat("OpVaR (99.9%) = ", OpVaR_999_Execut_Del, "\n")
cat("OpES  (99.9%) = ", OpES_999_Execut_Del, "\n")




# External_Fr -------------------------------------------------------------


rok_External_Fr <- plik28 |>
  filter( linia == "External_Fr") |>
  group_by(rok) |>
  summarize(liczba_zdarzen = n())


straty_External_Fr <- plik28 |>
  filter(linia == "External_Fr") |>
  group_by(rok) |>
  summarize(straty = sum(strata, na.rm = TRUE))

mean_External_Fr = mean(rok_External_Fr$liczba_zdarzen)
sd_External_Fr = sd(rok_External_Fr$liczba_zdarzen)

External_Fr_fit <- fitdist(rok_External_Fr$liczba_zdarzen, "binom",fix.arg = list(size = size_value))
gofstat(External_Fr_fit)
plot(External_Fr_fit)


External_Fr1 <- fitdist(straty_External_Fr$straty, "weibull")
External_Fr2 <- fitdist(straty_External_Fr$straty, method="mme","lnorm")
External_Fr3 <- fitdist(straty_External_Fr$straty, method="mme","exp")
External_Fr4 <- fitdist(straty_External_Fr$straty, method="mme","gamma")
names_test = c("Weibull","lnorm", "exp", "gamma")

Result_External_Fr<-gofstat(list(External_Fr1, External_Fr2,External_Fr3,External_Fr4), fitnames=names_test)
Result_External_Fr$chisqpvalue
Result_External_Fr$adtest
Result_External_Fr$kstest

plot(fitdist(straty_External_Fr$straty, "lnorm"))
plot(fitdist(straty_External_Fr$straty, "gamma"))
#Lnorm

set.seed(123)

# Parametry binom
binom_params_ext <- External_Fr_fit$estimate
prob_param_ext <- binom_params_ext["prob"]

# Parametry log-normal
lnorm_params_ext <- External_Fr2$estimate
meanlog_ext <- lnorm_params_ext["meanlog"]
sdlog_ext <- lnorm_params_ext["sdlog"]

symulacje_External_Fr <- replicate(n_sim, {
  liczba_zdarzen <- rbinom(1, size = size_param_binom, prob = prob_param_ext)
  if (liczba_zdarzen == 0) return(0)
  sum(rlnorm(liczba_zdarzen, meanlog = meanlog_ext, sdlog = sdlog_ext))
})

OpVaR_999_External_Fr <- quantile(symulacje_External_Fr, 0.999)
OpES_999_External_Fr <- mean(symulacje_External_Fr[symulacje_External_Fr > OpVaR_999_External_Fr])

hist_External_Fr <- hist(symulacje_External_Fr, breaks = 100, col = "lightblue",
     main = "Symulowany rozkład strat (External_Fr)", xlab = "Suma strat")
abline(v = OpVaR_999_External_Fr, col = "red", lwd = 2, lty = 2)

cat("OpVaR (99.9%) = ", OpVaR_999_External_Fr, "\n")
cat("OpES  (99.9%) = ", OpES_999_External_Fr, "\n")


# Internal_Fr -------------------------------------------------------------


rok_Internal_Fr <- plik28 |>
  filter( linia == "Internal_Fr") |>
  group_by(rok) |>
  summarize(liczba_zdarzen = n())

straty_Internal_Fr <- plik28 |>
  filter(linia == "Internal_Fr") |>
  group_by(rok) |>
  summarize(straty = sum(strata, na.rm = TRUE))

mean_Internal_Fr = mean(rok_Internal_Fr$liczba_zdarzen)
sd_Internal_Fr = sd(rok_Internal_Fr$liczba_zdarzen)

Internal_Fr_fit <- fitdist(rok_Internal_Fr$liczba_zdarzen, "nbinom")
gofstat(Internal_Fr_fit)
plot(Internal_Fr_fit)

#Internal_Fr1 <- fitdist(straty_Internal_Fr$straty, "weibull")
Internal_Fr2 <- fitdist(straty_Internal_Fr$straty, method="mme","lnorm")
Internal_Fr3 <- fitdist(straty_Internal_Fr$straty, method="mme","exp")
Internal_Fr4 <- fitdist(straty_Internal_Fr$straty, method="mme","gamma")
names_test = c("lnorm", "exp", "gamma")

Result_Internal_Fr<-gofstat(list(Internal_Fr2, Internal_Fr3,Internal_Fr4), fitnames=names_test)
Result_Internal_Fr$chisqpvalue
Result_Internal_Fr$adtest
Result_Internal_Fr$kstest


plot(fitdist(straty_Internal_Fr$straty, "lnorm"))
plot(fitdist(straty_Internal_Fr$straty, "gamma"))
#Lnorm


set.seed(123)

# Parametry nbinom
nbinom_params_int <- Internal_Fr_fit$estimate
size_param_int <- nbinom_params_int["size"]
mu_param_int <- nbinom_params_int["mu"]

# Parametry log-normal
lnorm_params_int <- Internal_Fr2$estimate
meanlog_int <- lnorm_params_int["meanlog"]
sdlog_int <- lnorm_params_int["sdlog"]

symulacje_Internal_Fr <- replicate(n_sim, {
  liczba_zdarzen <- rnbinom(1, size = size_param_int, mu = mu_param_int)
  if (liczba_zdarzen == 0) return(0)
  sum(rlnorm(liczba_zdarzen, meanlog = meanlog_int, sdlog = sdlog_int))
})

OpVaR_999_Internal_Fr <- quantile(symulacje_Internal_Fr, 0.999)
OpES_999_Internal_Fr <- mean(symulacje_Internal_Fr[symulacje_Internal_Fr > OpVaR_999_Internal_Fr])

hist_Internal_Fr <- hist(symulacje_Internal_Fr, breaks = 100, col = "lightblue",
     main = "Symulowany rozkład strat (Internal_Fr)", xlab = "Suma strat")
abline(v = OpVaR_999_Internal_Fr, col = "red", lwd = 2, lty = 2)

cat("OpVaR (99.9%) = ", OpVaR_999_Internal_Fr, "\n")
cat("OpES  (99.9%) = ", OpES_999_Internal_Fr, "\n")


# tabelka -----------------------------------------------------------------

## tabelka podsumowująca:
symulacje_df <- data.frame(
  Scenariusz = 1:n_sim,
  BussDist = symulacje_BussDist,
  Com_Ban = symulacje_Com_Ban,
  Damage = symulacje_Damage,
  Empl_Pract = symulacje_Empl_Pract,
  Execut_Del = symulacje_Execut_Del,
  External_Fr = symulacje_External_Fr,
  Internal_Fr = symulacje_Internal_Fr
)

# Dodanie kolumny z sumą strat dla każdego scenariusza
symulacje_df$Suma <- rowSums(symulacje_df[, -1])

# Podgląd
head(symulacje_df)


wyniki <- data.frame(
  Linia = c("Buss_Distr", "Com_Ban", "Damage", "Empl_Pract", "Execut_Del", "External_Fr", "Internal_Fr"),
  OpVaR_999 = c(OpVaR_999, OpVaR_999_Com_Ban, OpVaR_999_Damage, OpVaR_999_Empl_Pract, OpVaR_999_Execut_Del, NA),  # NA jeśli ostatnie niedokończone
  OpES_999 = c(OpES_999, OpES_999_Com_Ban, OpES_999_Damage, OpES_999_Empl_Pract, OpES_999_Execut_Del, NA)
)
print(wyniki)


library(tibble)
library(dplyr)

# Tabela ze wskaźnikami
tabela_oprisk <- tibble(
  Linia_Biznesowa = c("Business Disruption", "Commercial Banking", "Damage to Assets", 
                      "Employment Practices", "Execution & Delivery", 
                      "External Fraud", "Internal Fraud"),
  OpVaR_999 = c(OpVaR_999_BussDist, OpVaR_999_Com_Ban, OpVaR_999_Damage,
                OpVaR_999_Empl_Pract, OpVaR_999_Execut_Del,
                OpVaR_999_External_Fr, OpVaR_999_Internal_Fr),
  OpES_999 = c(OpES_999_BussDist, OpES_999_Com_Ban, OpES_999_Damage,
               OpES_999_Empl_Pract, OpES_999_Execut_Del,
               OpES_999_External_Fr, OpES_999_Internal_Fr)
) %>%
  mutate(across(OpVaR_999:OpES_999, round, 2))

# Wyświetlenie tabeli
library(knitr)

kable(tabela_oprisk, caption = "Tabela 1: Wskaźniki OpVaR i OpES dla poszczególnych linii biznesowych", 
      align = "lrr", format = "markdown")


library(gridExtra)

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
plot(hist_BussDist)
plot(hist_Com_Ban)
plot(hist_Damage)
plot(hist_Empl_Pract)
plot(hist_Execut_Del)
plot(hist_External_Fr)
plot(hist_Internal_Fr)



