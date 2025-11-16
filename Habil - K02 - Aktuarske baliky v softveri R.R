# Doplnkový súbor k habilitačnej práci s názvom: 
# Pravdepodobnostné modelovanie v poisťovníctve
# Autor: Gábor Szűcs
# Pracovisko: KAMŠ FMFI UK v Bratislave
# 
# Verzia: 2025-09-25
# Kódovanie súboru: UTF-8
# 
# Kapitola 2 - Aktuárske balíky v softvéri R
# =================================================


# The Software R for Statistical Computing
# R Core Team (2025). R: A Language and Environment for Statistical  Computing. R Foundation for Statistical Computing, Vienna, Austria, <https://www.R-project.org/>.

# Adresa webového sídla:
# https://www.r-project.org/

# Odkaz pre stiahnutie voľne dostupného softvéru R:
# https://cloud.r-project.org/bin/windows/base/

# Odkaz pre stiahnutie nadstavbového softvéru RStudio: 
# https://posit.co/download/rstudio-desktop/
# =================================================


# inštalácia vybraných balíkov (je potrebné pritom mať funkčné pripojenie k internetu); 
# tento príkaz stačí spustiť v R-konzole iba pri prvom použití tohto súboru: 
install.packages(c("actuar", "ChainLadder", "lifecontingencies", "demography", "MASS", "fitdistrplus", "VGAM", "forecast", "ggplot2", "moments", "insuranceData"));


# načítanie balíkov, otvorenie Pomocníku (Help) a zobrazenie podkladových publikácii
library(actuar)
help(package="actuar")
citation("actuar")

library(ChainLadder)
help(package="ChainLadder")
citation("ChainLadder")

library(lifecontingencies)
help(package="lifecontingencies")
citation("lifecontingencies")

library(demography)
help(package="demography")
citation("demography")

library(MASS)
help(package="MASS")
citation("MASS")

library(fitdistrplus)
help(package="fitdistrplus")
citation("fitdistrplus")

library(VGAM)
help(package="VGAM")
citation("VGAM")

library(forecast)
help(package="forecast")
citation("forecast")

library(insuranceData)
help(package="insuranceData")
citation("insuranceData")
# =================================================



# Príklad 2.1
# gama rozdelenie pravdepodobnosti v softvéri R

# grafické znázornenie kriviek hustoty a distribučnej funkcie gama rozdelenia pre rôzne voľby jeho parametrov
# 
par(mfrow=c(3, 2), mar=c(5,6,4,1)+.1, mgp=c(3, 1.5, 0))
curve(dgamma(x,2,1/50), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(f)[italic(X)], (italic(x)))), main=expression(paste("(a)  ",italic(X)," ~ ",Gama(2, 1/50))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="forestgreen")

curve(pgamma(x,2,1/50), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(F)[italic(X)], (italic(x)))), main=expression(paste("(b)  ",italic(X)," ~ ",Gama(2, 1/50))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="forestgreen")

curve(dgamma(x,8,1/50), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(f)[italic(X)], (italic(x)))), main=expression(paste("(c)  ",italic(X)," ~ ",Gama(8, 1/50))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="navy")

curve(pgamma(x,8,1/50), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(F)[italic(X)], (italic(x)))), main=expression(paste("(d)  ",italic(X)," ~ ",Gama(8, 1/50))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="navy")

curve(dgamma(x,2,1/200), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(f)[italic(X)], (italic(x)))), main=expression(paste("(e)  ",italic(X)," ~ ",Gama(2, 1/200))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="darkorange")

curve(pgamma(x,2,1/200), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(F)[italic(X)], (italic(x)))), main=expression(paste("(f)  ",italic(X)," ~ ",Gama(2, 1/200))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="darkorange")
# -----


# grafické znázornenie kriviek hustoty a kvantilovej funkcie gama rozdelenia pre rôzne voľby jeho parametrov
# 
par(mfrow=c(3, 2), mar=c(5,6,4,1)+.1, mgp=c(3, 1.5, 0))
curve(dgamma(x,2,1/50), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(f)[italic(X)], (italic(x)))), main=expression(paste("(a)  ",italic(X)," ~ ",Gama(2, 1/50))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="forestgreen")

curve(qgamma(x,2,1/50), from=0, to=1, xlab=expression(italic(x)), ylab=expression(paste(italic(F)[italic(X)]^{-1}, (italic(x)))), main=expression(paste("(b)  ",italic(X)," ~ ",Gama(2, 1/50))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="forestgreen")

curve(dgamma(x,8,1/50), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(f)[italic(X)], (italic(x)))), main=expression(paste("(c)  ",italic(X)," ~ ",Gama(8, 1/50))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="navy")

curve(qgamma(x,8,1/50), from=0, to=1, xlab=expression(italic(x)), ylab=expression(paste(italic(F)[italic(X)]^{-1}, (italic(x)))), main=expression(paste("(d)  ",italic(X)," ~ ",Gama(8, 1/50))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="navy")

curve(dgamma(x,2,1/200), from=0, to=1000, xlab=expression(italic(x)), ylab=expression(paste(italic(f)[italic(X)], (italic(x)))), main=expression(paste("(e)  ",italic(X)," ~ ",Gama(2, 1/200))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="darkorange")

curve(qgamma(x,2,1/200), from=0, to=1, xlab=expression(italic(x)), ylab=expression(paste(italic(F)[italic(X)]^{-1}, (italic(x)))), main=expression(paste("(f)  ",italic(X)," ~ ",Gama(2, 1/200))), lwd=3, cex.lab=2, cex.main=2, cex.axis=2, col="darkorange")
# -----


# simulačné pokusy
# X ~ Gama(8, 1/50)
# 
k <- 8                   # shape parameter gama rozdelenia
lambda <- 1/50           # rate parameter gama rozdelenia


# SIM 1.
n1 <- 100                # počet simulovaných hodnôt

set.seed(20200815)       # zafixovanie začiatočného nastavenia pseudo-náhodného generátora
X.sim1 <- rgamma(n=n1, shape=k, rate=lambda)

# grafické znázornenie histogramu simulovaných hodnôt a teoretickej hustoty gama rozdelenia: X ~ Gama(8, 1/50)
# 
par(mfrow=c(1, 1), mar=c(5,6,4,1)+.1, mgp=c(3, 1.5, 0))
hist(X.sim1, freq=FALSE, xlab=expression(italic(x)), ylab="relat. simul. početnosti a hodnoty funkcie hustoty", main=expression(paste("Histogram simulovaných hodnôt a hustota n. p. ",italic(X)," ~ ",Gama(8, 1/50))), density=10, col="maroon")
curve(dgamma(x,8,1/50), from=0, to=1000, lwd=3, cex.lab=1, cex.main=1, cex.axis=2, col="navy", add=TRUE)
# ----------

# porovnanie simulovaných a teoretických momentov
mean(X.sim1)
# = 397,1506             # simulovaná stredná hodnota

k/lambda
# = 400                  # teoretická stredná hodnota gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

var(X.sim1)
# = 13 455,68            # simulovaná disperzia

k/lambda^2
# = 20 000               # teoretická disperzia gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

require(moments)
skewness(X.sim1)
# = 0,2301727            # simulovaný koeficient šikmosti

2/sqrt(k)
# = 0,7071068            # teoretická hodnota koeficientu šikmosti gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

require(moments)
kurtosis(X.sim1) - 3
# = -0,2899697           # simulovaný koeficient excesu

6/k
# =  0,75                # teoretická hodnota koeficientu excesu gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

# porovnanie zvolených simulovaných a teoretických kvantilov (percentilov)
quantile(X.sim1, 0.90)   
# = 555,2539             # simulovaný 90-percentný kvantil (90. percentil)

qgamma(p=0.90, shape=k, rate=lambda)
# = 588,5457             # teoretická hodnota 90-percentného kvantilu (90-teho percentilu)
# ----------

quantile(X.sim1, 0.95)   
# = 588,1692             # simulovaný 95-percentný kvantil (95. percentil)

qgamma(p=0.95, shape=k, rate=lambda)
# = 657,4057             # teoretická hodnota 95-percentného kvantilu (95-teho percentilu)
# ----------

quantile(X.sim1, 0.99)   
# = 646,0004             # simulovaný 99-percentný kvantil (99. percentil)

qgamma(p=0.99, shape=k, rate=lambda)
# = 799,9982             # teoretická hodnota 99-percentného kvantilu (99-teho percentilu)
# -------------------------------------------------


# SIM 2.
n2 <- 2000               # počet simulovaných hodnôt

set.seed(20200815)       # zafixovanie začiatočného nastavenia pseudo-náhodného generátora
X.sim2 <- rgamma(n=n2, shape=k, rate=lambda)

# grafické znázornenie histogramu simulovaných hodnôt a teoretickej hustoty gama rozdelenia: X ~ Gama(8, 1/50)
# 
par(mfrow=c(1, 1), mar=c(5,6,4,1)+.1, mgp=c(3, 1.5, 0))
hist(X.sim2, freq=FALSE, xlab=expression(italic(x)), ylab="relat. simul. početnosti a hodnoty funkcie hustoty", main=expression(paste("Histogram simulovaných hodnôt a hustota n. p. ",italic(X)," ~ ",Gama(8, 1/50))), density=10, col="maroon")
curve(dgamma(x,8,1/50), from=0, to=1000, lwd=3, cex.lab=1, cex.main=1, cex.axis=2, col="navy", add=TRUE)
# ----------

# porovnanie simulovaných a teoretických momentov
mean(X.sim2)
# = 398,8997             # simulovaná stredná hodnota

k/lambda
# = 400                  # teoretická stredná hodnota gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

var(X.sim2)
# = 20 492,03            # simulovaná disperzia

k/lambda^2
# = 20 000               # teoretická disperzia gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

require(moments)
skewness(X.sim2)
# = 0,6768643            # simulovaný koeficient šikmosti

2/sqrt(k)
# = 0,7071068            # teoretická hodnota koeficientu šikmosti gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

require(moments)
kurtosis(X.sim2) - 3
# = 0,6960683            # simulovaný koeficient excesu

6/k
# = 0,75                 # teoretická hodnota koeficientu excesu gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

# porovnanie zvolených simulovaných a teoretických kvantilov (percentilov)
quantile(X.sim2, 0.90)   
# = 591,2824             # simulovaný 90-percentný kvantil (90. percentil)

qgamma(p=0.90, shape=k, rate=lambda)
# = 588,5457             # teoretická hodnota 90-percentného kvantilu (90-teho percentilu)
# ----------

quantile(X.sim2, 0.95)   
# = 653,2163             # simulovaný 95-percentný kvantil (95. percentil)

qgamma(p=0.95, shape=k, rate=lambda)
# = 657,4057             # teoretická hodnota 95-percentného kvantilu (95-teho percentilu)
# ----------

quantile(X.sim2, 0.99)   
# = 803,2470             # simulovaný 99-percentný kvantil (99. percentil)

qgamma(p=0.99, shape=k, rate=lambda)
# = 799,9982             # teoretická hodnota 99-percentného kvantilu (99-teho percentilu)
# -------------------------------------------------


# SIM 3.
n3 <- 300000             # počet simulovaných hodnôt

set.seed(20200815)       # zafixovanie začiatočného nastavenia pseudo-náhodného generátora
X.sim3 <- rgamma(n=n3, shape=k, rate=lambda)

# grafické znázornenie histogramu simulovaných hodnôt a teoretickej hustoty gama rozdelenia: X ~ Gama(8, 1/50)
# 
par(mfrow=c(1, 1), mar=c(5,6,4,1)+.1, mgp=c(3, 1.5, 0))
hist(X.sim3, freq=FALSE, breaks=50, xlab=expression(italic(x)), ylab="relat. simul. početnosti a hodnoty funkcie hustoty", main=expression(paste("Histogram simulovaných hodnôt a hustota n. p. ",italic(X)," ~ ",Gama(8, 1/50))), density=10, col="maroon")
curve(dgamma(x,8,1/50), from=0, to=1000, lwd=3, cex.lab=1, cex.main=1, cex.axis=2, col="navy", add=TRUE)
# ----------

# porovnanie simulovaných a teoretických momentov
mean(X.sim3)
# = 399,6528             # simulovaná stredná hodnota

k/lambda
# = 400                  # teoretická stredná hodnota gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

var(X.sim3)
# = 19 936,53            # simulovaná disperzia

k/lambda^2
# = 20 000               # teoretická disperzia gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

require(moments)
skewness(X.sim3)
# = 0,7036397            # simulovaný koeficient šikmosti

2/sqrt(k)
# = 0,7071068            # teoretická hodnota koeficientu šikmosti gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

require(moments)
kurtosis(X.sim3) - 3
# = 0,7317812            # simulovaný koeficient excesu

6/k
# = 0,75                 # teoretická hodnota koeficientu excesu gama rozdelenia: X ~ Gama(8, 1/50)
# ----------

# porovnanie zvolených simulovaných a teoretických kvantilov (percentilov)
quantile(X.sim3, 0.90)   
# = 588,1186             # simulovaný 90-percentný kvantil (90. percentil)

qgamma(p=0.90, shape=k, rate=lambda)
# = 588,5457             # teoretická hodnota 90-percentného kvantilu (90-teho percentilu)
# ----------

quantile(X.sim3, 0.95)   
# = 657,0749             # simulovaný 95-percentný kvantil (95. percentil)

qgamma(p=0.95, shape=k, rate=lambda)
# = 657,4057             # teoretická hodnota 95-percentného kvantilu (95-teho percentilu)
# ----------

quantile(X.sim3, 0.99)   
# = 799,2912             # simulovaný 99-percentný kvantil (99. percentil)

qgamma(p=0.99, shape=k, rate=lambda)
# = 799,9982             # teoretická hodnota 99-percentného kvantilu (99-teho percentilu)
# ================================================= 


