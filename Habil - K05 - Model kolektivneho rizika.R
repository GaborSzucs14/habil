# Doplnkový súbor k habilitačnej práci s názvom: 
# Pravdepodobnostné modelovanie v poisťovníctve
# Autor: Gábor Szűcs
# Pracovisko: KAMŠ FMFI UK v Bratislave
# 
# Verzia: 2025-09-25
# Kódovanie súboru: UTF-8
# 
# Kapitola 5 - Model kolektívneho rizika
# ==================================================


# Inštalácia potrebných doplnkových balíkov v softvéri R:
# (tento príkaz stačí spustiť iba pri prvom spustení tohto skriptu)
install.packages(c("actuar", "VGAM"))
# =================================================


# Úloha 5.1

# načítanie knižnice VGAM
library(VGAM)

# Paretovo rozdelenie pravdepodobnosti typu I
# X ~ Pa(shape=1,35; rate=0,008)
k <- 1.35
lambda <- 0.008

# teoretický výpočet strednej hodnoty náhodnej premennej X:
EX <- k * (1/lambda) / (k - 1)
EX
# očakávaná výška škody = 482,1429 eura

# ilustratívny simulačný výpočet strednej hodnoty náhodnej premennej X:
X.sim <- NULL
X.sim <- VGAM::rpareto(1000000, scale=(1/lambda), shape=k)
mean(X.sim)


# výpočet pravdepodobnosti toho, že výška jednej škody presiahne 1000 eur:
1 - VGAM::ppareto(1000, scale=(1/lambda), shape=k)
# 0,06037102

VGAM::ppareto(1000, scale=(1/lambda), shape=k, lower.tail=FALSE)
# 0,06037102
# 
# simulačný výpočet pravdepodobnosti toho, že výška jednej škody presiahne 1000 eur:
length(X.sim[X.sim>1000]) / length(X.sim)

# Odpoveď.
# Očakávaná výška škody je 482,14 eura. Pravdepodobnosť toho, že výška jednej škody presiahne 1000 eur, je 0,06037102.
# =================================================



# Úloha 5.2

# Weibullovo rozdelenie pravdepodobnosti 
# X ~ Wei(shape=4,08; scale=220)
k <- 4.08
theta <- 220

# G = peňažná suma vyjadrená v eurách, pre ktorú platí, že výšky poistných nárokov ju nepresiahnu s pravdepodobnosťou 99,5 %
# G = ? 

# G = 99,5-percentný kvantil náhodnej premennej X, ktorá sa riadi podľa Weibullovho rozdelenia pravdepodobnosti;
# výpočet 99,5-percentného kvantilu náhodnej premennej X
G <- qweibull(0.995, shape=k, scale=theta) 
G 
# 331,0606 eura

# výpočet strednej hodnoty náhodnej premennej X, ktorá sa riadi podľa Weibullovho rozdelenia pravdepodobnosti:
EX <- theta* gamma(1+1/k)
EX
# 199,63 eura

# výpočet smerodajnej odchýlky náhodnej premennej X, ktorá sa riadi podľa Weibullovho rozdelenia pravdepodobnosti:
sigmaX <- sqrt( theta^2 * (gamma(1+2/k) - (gamma(1+1/k)^2)))
sigmaX
# 55,01 eura
# ----------

# ilustratívny simulačný výpočet 99,5-percentného kvantilu, strednej hodnoty a smerodajnej odchýlky náhodnej premennej X:
X.sim <- NULL
X.sim <- rweibull(10000000, shape=k, scale=theta) 
  quantile(X.sim, 0.995)
  mean(X.sim)
  sd(X.sim)

# Odpoveď.
# Peňažná suma vyjadrená v eurách, pre ktorú platí, že výšky poistných nárokov ju nepresiahnu s pravdepodobnosťou 99,5 %, v tomto prípade má výšku 331,06 eura. Stredná výška poistných nárokov je 199,63 eura, kým smerodajná odchýlka výšok poistných nárokov je 55,01 eura.
# =================================================



# Úloha 5.3

# načítanie knižnice actuar
library(actuar)

# Burrovo rozdelenie pravdepodobnosti typu XII
# X ~ Burr(shape1=5,51; shape2=3.92; scale=520)
k1 <- 5.51
k2 <- 3.92
theta <- 520

# výpočet očakávanej hodnoty výšky poistných nárokov:
actuar::mburr(order=1, shape1=k1, shape2=k2, scale=theta)
# 314,0324 eura

# výpočet smerodajnej odchýlky výšky poistných nárokov pomocou vzťahu:
# sigma.X = SQRT( E(X^2) - (E(X))^2 )
sqrt(actuar::mburr(order=2, shape1=k1, shape2=k2, scale=theta) - actuar::mburr(order=1, shape1=k1, shape2=k2, scale=theta)^2)
# 97,54797 eura

# ilustratívny simulačný výpočet strednej hodnoty a smerodajnej odchýlky náhodnej premennej X:
X.sim <- NULL
X.sim <- rburr(10000000, shape1=k1, shape2=k2, scale=theta) 
  mean(X.sim)
  sd(X.sim)

# Odpoveď.
# Stredná výška poistných nárokov je 314,03 eura, kým smerodajná odchýlka výšok poistných nárokov je 97,55 eura.
# =================================================



# Úloha 5.4

# negatívne binomické rozdelenie pravdepodobnosti
# N ~ NegBin(size=95; prob=0,11)
r <- 95
p <- 0.11

# výpočet očakávaného počtu poistných udalostí nahlásených za jeden mesiac:
EN <- r*(1-p) / p
EN
# E(N) = 768,64

# výpočet pravdepodobnosti toho, že v najbližšom mesiaci bude musieť niečo plniť aj zaisťovňa: 
1 - pnbinom(1000, size=r, prob=p)
# 0,004934087

# výpočet pravdepodobnosti toho, že v najbližšom mesiaci počet nahlásených poistných udalostí presiahne hodnotu 1000:
pnbinom(1000, size=r, prob=p, lower.tail=FALSE)

# ilustratívny simulačný výpočet pravdepodobnosti toho, že v najbližšom mesiaci počet nahlásených poistných udalostí presiahne hodnotu 1000:
N.sim <- rnbinom(10000000, size=r, prob=p)
length(N.sim[N.sim>1000]) / length(N.sim)

# Odpoveď.
# Pravdepodobnosť toho, že v najbližšom mesiaci bude musieť niečo plniť aj zaisťovňa, je 0,4934 %.
# =================================================



# Úloha 5.5

# vlastnosti, charakteristiky a momenty náhodnej premennej S v modeli kolektívneho rizika
# N ~ Po(nu=280)
# X ~ Lomax(shape=4,8; scale=420)

# Poznámka. 
# Tradičné R-funkcie pre Lomaxovo rozdelenie pravdepodobnosti sú implementované napríklad v R-balíku "VGAM". 
# -----

# inštalácia a načítanie potrebných balíkov
install.packages(c("VGAM", "e1071", "actuar"))
require(VGAM); 

options(digits=10, scipen=999)

# zadané parametre Lomaxovho rozdelenia pravdepodobnosti
sh <- 4.8              # shape parameter
sc <- 420              # scale parameter

# teoretické momenty náhodnej premennej X, ktorá sa riadi podľa Lomaxovho rozdelenia pravdepodobnosti
(EX <- sc / (sh-1))
(DX <- (sc^2 *sh) / ((sh-1)^2 *(sh-2)))

      # E(X) =   110,5263
      # D(X) = 20941,83
	  
# Stredná (očakávaná) výška jedného poistného nároku je zhruba 110,53 peňažných jednotiek. 

# simulačné "overenie" súladu teoretických momentov a VGAM-implementácie Lomaxovho rozdelenia pravdepodobnosti
set.seed(20220913)
XX <- VGAM::rlomax(n=100000, shape3.q=sh, scale=sc)
mean(XX)   # =   110,7013
var(XX)    # = 21002,70
# -----

# teoretické momenty náhodnej premennej N, ktorá sa riadi podľa Poissonovho rozdelenia pravdepodobnosti

nu <- 280              # zadaný parameter Poissonovho rozdelenia
(EN <- nu)
(DN <- nu)

# Stredný (očakávaný) počet poistných nárokov nahlásených za jeden kalendárny mesiac je 280. 
# -----

# (a)
# teoretické momenty náhodnej premennej S počítame pomocou Waldových identít
# 
(ES <- EN * EX) 
# E(S) = 30 947,37

(DS <- EN*DX + DN*(EX^2))
# D(S) = 9 284 210,53

# teoretická smerodajná odchýlka náhodnej premennej S
(sigma_S <- sqrt(DS))
# sigma_S = 3047 

# Celková stredná výška poistných nárokov (nahlásených za jeden kalendárny mesiac) je zhruba 30 947 peňažných jednotiek. 
# Disperzia celkovej výšky poistných nárokov (nahlásených za jeden kalendárny mesiac) je približne 9 284 211 peňažných jednotiek na druhú. 
# Smerodajná odchýlka celkovej výšky poistných nárokov (nahlásených za jeden kalendárny mesiac) je približne 3047 peňažných jednotiek. 
# -------------------------------------------------


# (b)
# normálna aproximácia rozdelenia pravdepodobnosti celkovej výšky poistných nárokov

# Náhodná premenná S v skutočnosti má zložené rozdelenie pravdepodobnosti, ktoré vzniká zložením Poissonovho a Lomaxovho rozdelenia pravdepodobnosti. Toto netriviálne rozdelenie sa snažíme aproximovať pomocou Gaussovho normálneho rozdelenia, a to podľa (za predpokladu platnosti) istej verzie centrálne limitnej vety. 

# momenty náhodnej premennej S sme vypočítali v bode (a); pomocou nich môžeme nastaviť parametre normálneho rozdelenia 
(mu <- ES)
(sigma <- sigma_S)

# grafické znázornenie
# kumulatívna distribučná funkcia pre celkovú výšku poistných nárokov - normálna aproximácia
curve(pnorm(x, mean=mu, sd=sigma), from=(mu-5*sigma), to=(mu+5*sigma), main="Normálna aproximácia CDF náhodnej premennej S", xlab=expression(italic(s)), ylab=expression(italic(F[S](s))), col="blue", lwd=2)

# funkcia hustoty rozdelenia pravdepodobnosti celkovej výšky poistných nárokov - normálna aproximácia
curve(dnorm(x, mean=mu, sd=sigma), from=(mu-5*sigma), to=(mu+5*sigma), main="Normálna aproximácia funkcie hustoty náhodnej premennej S", xlab=expression(italic(s)), ylab=expression(italic(f[S](s))), col="cyan3", lwd=2)
# -----

# výpočet kvantilov aproximovaného rozdelenia pravdepodobnosti náhodnej premennej S

# 90-percentný kvantil normálneho rozdelenia
qnorm(0.90, mean=mu, sd=sigma)
# q(0,90) = 34 852,26 peňažných jednotiek
# 
# S 90-percentnou pravdepodobnosťou platí, že celková výška poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) by nemala nepresiahnuť hodnotu 34 852,26 peňažných jednotiek.
# -----

# 95-percentný kvantil normálneho rozdelenia
qnorm(0.95, mean=mu, sd=sigma)
# q(0,95) = 35 959,24 peňažných jednotiek
# 
# S 95-percentnou pravdepodobnosťou platí, že celková výška poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) by nemala nepresiahnuť hodnotu 35 959,24 peňažných jednotiek.
# -----

# 99-percentný kvantil normálneho rozdelenia
qnorm(0.99, mean=mu, sd=sigma)
# q(0,99) = 38 035,75 peňažných jednotiek
# 
# S 99-percentnou pravdepodobnosťou platí, že celková výška poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) by nemala nepresiahnuť hodnotu 38 035,75 peňažných jednotiek.
# -----

# 99,5-percentný kvantil normálneho rozdelenia
qnorm(0.995, mean=mu, sd=sigma)
# q(0,995) = 38 795,92 peňažných jednotiek
# 
# S 99,5-percentnou pravdepodobnosťou platí, že celková výška poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) by nemala nepresiahnuť hodnotu 38 795,92 peňažných jednotiek.
# -----

# +++
# normálna aproximácia rozdelenia celkovej výšky poistných nárokov pomocou balíku 'actuar' a funkcie 'aggregateDist'
require(actuar)

Fs.norm <- aggregateDist("normal", moments=c(mu, sigma^2))

# grafické znázornenie
# kumulatívna distribučná funkcia pre celkovú výšku poistných nárokov - normálna aproximácia
curve(Fs.norm, from=(mu-5*sigma), to=(mu+5*sigma), main="Normálna aproximácia CDF n. p. S (actuar::aggregateDist)", xlab=expression(italic(s)), ylab=expression(italic(F[S](s))), col="deepskyblue4", lwd=2, lty="longdash")

# funkcia hustoty rozdelenia pravdepodobnosti celkovej výšky poistných nárokov - normálna aproximácia
s <- seq(from=(mu-5*sigma), to=(mu+5*sigma), by=1)
Fs.s <- Fs.norm(s)
plot(seq(from=(mu-5*sigma), to=(mu+5*sigma-1), by=1), diff(Fs.s), main="Normálna aproximácia funkcie hustoty náhodnej premennej S (actuar::aggregateDist)", xlab=expression(italic(s)), ylab=expression(italic(f[S](s))), col="dodgerblue3", type="p", pch=16, cex=0.1)

# výpočet kvantilov rozdelenia celkovej výšky poistných nárokov - normálna aproximácia
quantile(Fs.norm, probs=0.900)           # q(0,900) = 34 852,26 peňažných jednotiek
quantile(Fs.norm, probs=0.950)           # q(0,950) = 35 959,24 peňažných jednotiek
quantile(Fs.norm, probs=0.990)           # q(0,990) = 38 035,75 peňažných jednotiek
quantile(Fs.norm, probs=0.995)           # q(0,995) = 38 795,92 peňažných jednotiek
# -------------------------------------------------


# (c)
# simulačné štúdie pre celkovú výšku poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) 

# Monte Carlo simulácie I.
# počet simulácií
M <- 100000

# pomocná premenná pre ukladanie simulovaných hodnôt celkovej výšky poistných nárokov
S.sim.I <- rep(0, times=M)

# úvodné nastavenie náhodného generátora (zafixovanie náhodnosti)
set.seed(20220916)

for(t in 1:M)
{
  N.sim <- rpois(n=1, lambda=nu)
  X.sim <- VGAM::rlomax(n=N.sim, shape3.q=sh, scale=sc)
  S.sim.I[t] <- sum(X.sim)
}

# simulovaná kumulatívna distribučná funkcia pre celkovú výšku poistných nárokov
Fs.I <- ecdf(S.sim.I)

# grafické znázornenie
# simulovaná kumulatívna distribučná funkcia pre celkovú výšku poistných nárokov - Monte Carlo simulácie I.
curve(Fs.I(x), from=min(S.sim.I), to=max(S.sim.I), main="Simulovaná kumulatívna distribučná funkcia celkovej výšky poistných nárokov", col="blue", lwd=2)

# simulovaný histogram celkovej výšky poistných nárokov - Monte Carlo simulácie I.
hist(S.sim.I, col="firebrick3", breaks=50, angle=45, density=25, border="black", main="Simulovaný histogram celkovej výšky poistných nárokov", xlab="celková výška poistných nárokov", ylab="početnosti")

# simulovaná funkcia hustoty rozdelenia pravdepodobnosti celkovej výšky poistných nárokov - Monte Carlo simulácie I.
# spoločné zobrazenie v normálnou aproximáciou funkcie hustoty 
curve(dnorm(x, mean=mu, sd=sigma), from=(mu-5*sigma), to=(mu+5*sigma), main="Porovnanie simulovanej hustoty a aproximovanej normálnej hustoty", xlab=expression(italic(s)), ylab=expression(italic(f[S](s))), col="cyan3", lwd=2)
lines(density(S.sim.I), type="l", col="firebrick3", lwd=2, lty="longdash")
legend("topleft", c("simulovaná funkcia hustoty", "normálna aproximácia hustoty"), col=c("firebrick3", "cyan3"), lty=c("longdash", "solid"), lwd=c(2,2), bty="n")
# -----

# základné charakteristiky simulovaného vektora celkovej výšky poistných nárokov
summary(S.sim.I)
#       Min.     1st Qu.     Median       Mean     3rd Qu.       Max. 
#  20 054,13  28 849,45   30 848,35   30 953,08  32 937.19  59 364,33

# ďalšie charakteristiky (momenty) simulovaného vektora celkovej výšky poistných nárokov
require(e1071)
# 
mean(S.sim.I)            # výberový priemer         =    30 953,08 peňažných jednotiek
var(S.sim.I)             # výberová disperzia       = 9 277 271,47 (peňažných jednotiek)^2
sd(S.sim.I)              # smerodajná odchýlka      =      3045,86 peňažných jednotiek
skewness(S.sim.I)        # koeficient šikmosti      =         0,23
kurtosis(S.sim.I)        # koeficient špicatosti    =         0,18

# výpočet kvantilov simulovaného rozdelenia celkovej výšky poistných nárokov
quantile(Fs.I, probs=0.900)              # q(0,90)  = 34 907,58 peňažných jednotiek
quantile(Fs.I, probs=0.950)              # q(0,95)  = 36 119,83 peňažných jednotiek
quantile(Fs.I, probs=0.990)              # q(0,99)  = 38 535,57 peňažných jednotiek
quantile(Fs.I, probs=0.995)              # q(0,995) = 39 497,63 peňažných jednotiek
# -----

# Záver k Monte Carlo simuláciám I.
# Teoretická a simulovaná stredná hodnota náhodnej premennej S sú k sebe veľmi blízko (30 947,37 vs. 30 953,08 peňažných jednotiek). Rovnaké konštatovanie platí aj pre teoretickú a simulovanú smerodajnú odchýlku náhodnej premennej S (3047 vs. 3045,86 peňažných jednotiek).
# 
# Pri porovnaní koeficientov šikmosti a koeficientov špicatosti sme už ale zistili isté odchýlky. Normálne rozdelená náhodná premenná totiž má nulový koeficient šikmosti nulový koeficient špicatosti, kým pri Monte Carlo simuláciách I. sme dostali koeficient šikmosti na úrovni 0,23 a koeficient špicatosti rovný hodnote 0,18. Pri simuláciách sme použili až 100000 simulačných behov, takže sa dá predpokladať, že nejde len o efekt náhody. Z uvedených číselných hodnôt vyplýva, že simulované zložené rozdelenie (Poisson+Lomax) náhodnej premennej S je mierne pozitívne zošikmené a má aj trochu vyššiu mieru špicatosti, ako normálne rozdelenie. 
# 
# Aj pri porovnaní grafov hustoty si môžete všimnúť, že medzi normálnou hustotou a simulovanou hustotou zloženého rozdelenia Poisson+Lomax sú nejaké odlišnosti. Tieto odchýlky sa prejavili aj pri komparácii horných kvantilov, ktoré v prípade Monte Carlo simulácií nadobudli vyššie hodnoty. Pri 99,5-percentom kvantile je odchýlka vyše 700 peňažných jednotiek (38 795,92 peňažných jednotiek pri normálnej aproximácii vs. 39 497,63 peňažných jednotiek pri simulovanom zloženom rozdelení Poisson+Lomax). Táto rozdielnosť mohla vzniknúť kvôli tomu, že Lomaxovo rozdelenie pravdepodobnosti má ťažký pravý chvost, kým normálne rozdelenie má ľahké chvosty. Inými slovami: náhodná premenná, ktorá sa riadi podľa Lomaxovho rozdelenia pravdepodobnosti, s väčšou pravdepodobnosťou môže nadobúdať vysoké (až extrémne) hodnoty, ako normálne rozdelená náhodná premenná. Dodávame, že maximum simulovaného súboru je až 59 364,33 peňažných jednotiek, čo pri simulovanej strednej hodnote 30 953,08 peňažných jednotiek a simulovanej smerodajnej odchýlke 3045,86 peňažných jednotiek môžeme považovať za odľahlé pozorovanie. 
# 
# Pri normálnej aproximácii rozdelenia pravdepodobnosti celkovej výšky poistných nárokov treba postupovať obozretne, a to najmä v tých prípadoch, keď sa výšky poistných nárokov riadia podľa rozdelenia s ťažkým pravým chvostom. Nevhodná alebo neprimeraná aplikácia normálneho rozdelenia môže viesť k podceneniu rizikovosti skúmaného poistného kmeňa, napríklad v tom zmysle, ako sa to prejavilo v našej simulačnej štúdii pri 99,5-percentom kvantile. Ak by poisťovňa použila normálnu aproximáciu, pravdepodobne by podhodnotila výšku kapitálu, ktorá by s 99,5-percentnou pravdepodobnosťou pokryla celkovú výšku poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci). 
# -----

# +++
# simulačné štúdie pre celkovú výšku poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) pomocou balíku 'actuar' 

# Monte Carlo simulácie II.

require(actuar)
require(VGAM)

# definovanie modelu pre počet poistných nárokov nahlásených za jeden mesiac (claim frequency model)
model.freq <- expression(data=rpois(nu))

# definovanie modelu pre výšky poistných nárokov (claim severity model)
model.sev <- expression(data=VGAM::rlomax(shape3.q=sh, scale=sc))

# odhadovanie kumulatívnej distribučnej funkcie pre celkovú výšku poistných nárokov pomocou balíku 'actuar' a funkcie 'aggregateDist'
# [bez fixovania náhodného generátora]
set.seed(Sys.time())
Fs.II <- aggregateDist("simulation", nb.simul=100000, model.freq=model.freq, model.sev=model.sev)

# grafické znázornenie
# simulovaná kumulatívna distribučná funkcia pre celkovú výšku poistných nárokov - Monte Carlo simulácie II.
curve(Fs.II(x), from=min(knots(Fs.II)), to=max(knots(Fs.II)), main="Simulovaná kumulatívna distribučná funkcia celkovej výšky poistných nárokov", col="steelblue3", lwd=2)

# základné charakteristiky simulovaného vektora celkovej výšky poistných nárokov
summary(knots(Fs.II))

# ďalšie charakteristiky (momenty) simulovaného vektora celkovej výšky poistných nárokov
require(e1071)
# 
mean(knots(Fs.II))     # výberový priemer         
var(knots(Fs.II))      # výberová disperzia       
sd(knots(Fs.II))       # smerodajná odchýlka      
skewness(knots(Fs.II)) # koeficient šikmosti      
kurtosis(knots(Fs.II)) # koeficient špicatosti    

# výpočet horných kvantilov simulovaných výšok poistných nárokov pomocou funkcie 'quantile'
quantile(Fs.II, probs=0.900)   
quantile(Fs.II, probs=0.950)   
quantile(Fs.II, probs=0.990)   
quantile(Fs.II, probs=0.995)   
# -----

# +++
# simulácia zloženého rozdelenia pre celkovú výšku poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) pomocou balíku 'actuar' a funkcie 'rcompound'
# [bez fixovania náhodného generátora]

# Monte Carlo simulácie III.

require(actuar)
require(VGAM)
# 
set.seed(Sys.time())
S.sim.III <- actuar::rcompound(100000, model.freq=rpois(nu), model.sev=VGAM::rlomax(shape3.q=sh, scale=sc))

head(S.sim.III)

# simulovaná kumulatívna distribučná funkcia pre celkovú výšku poistných nárokov
Fs.III <- ecdf(S.sim.III)

# grafické znázornenie
curve(Fs.III(x), from=min(S.sim.III), to=max(S.sim.III), main="Simulovaná kumulatívna distribučná funkcia celkovej výšky poistných nárokov", col="slateblue3", lwd=2)

hist(S.sim.III, col="skyblue3", breaks=50, angle=45, density=25, border="black", main="Histogram simulovanej celkovej výšky poistných nárokov", xlab="celková výška poistných nárokov", ylab="početnosti")

# charakteristiky simulovaných celkových výšok poistných nárokov
require(e1071)
# 
mean(S.sim.III)        # výberový priemer
var(S.sim.III)         # výberová disperzia
sd(S.sim.III)          # smerodajná odchýlka
skewness(S.sim.III)    # koeficient šikmosti
kurtosis(S.sim.III)    # koeficient špicatosti

# ďalšie charakteristiky simulovaných celkových výšok poistných nárokov
summary(S.sim.III)

# výpočet horných kvantilov simulovaných výšok poistných nárokov pomocou funkcie 'quantile'
quantile(Fs.III, probs=0.900)   
quantile(Fs.III, probs=0.950)   
quantile(Fs.III, probs=0.990)   
quantile(Fs.III, probs=0.995)  
# =================================================



# Úloha 5.6

# načítanie dát:
dataN <- as.vector(read.table("http://www.iam.fmph.uniba.sk/ospm/Szucs/data/cp_vtp_data_poist_domacnosti_N.txt", header=FALSE)$V1)

dataX <- as.vector(read.table("http://www.iam.fmph.uniba.sk/ospm/Szucs/data/cp_vtp_data_poist_domacnosti_X.txt", header=FALSE)$V1)

# dataN <- as.vector(read.table("D:\\data\\cp_vtp_data_poist_domacnosti_N.txt", header=FALSE)$V1)
# dataX <- as.vector(read.table("D:\\data\\cp_vtp_data_poist_domacnosti_X.txt", header=FALSE)$V1)

dataN; 
# počet pozorovaní v prípade počtu poistných nárokov
nN <- length(dataN);    nN         # nN =  300

dataX; 
# počet pozorovaní v prípade výšky poistných nárokov
nX <- length(dataX);    nX         # nX = 1446

sum(dataN[(nN-5*12 + 1):nN])       #    = 1446
# -----

# inštalácia potrebných knižníc 
# (nižšie uvedený príkaz stačí spustiť len pri prvom použití tohto skriptu)
install.packages(c("fitdistrplus", "goftest", "e1071", "actuar", "VGAM")); 
# -----


# Grafická analýza údajov a počítanie empirických opisných charakteristík

# empirická kumulatívna distribučná funkcia pre počet poistných nárokov
Fn <- ecdf(dataN)
curve(Fn(x), from=0, to=max(dataN), main="Empirická kumulatívna distribučná funkcia výšky poistných nárokov", col="forestgreen", lwd=2)

hist(dataN, col="green", breaks=50, angle=135, density=20, border="black", main="Histogram počtu poistných nárokov", xlab="počet poistných nárokov", ylab="početnosti")

# charakteristiky dátového súboru (v prípade počtu poistných nárokov)
# 
mean(dataN)            # výberový priemer      = 24,93
var(dataN)             # výberová disperzia    = 23,42987
sd(dataN)              # smerodajná odchýlka   =  4,84044

# ďalšie charakteristiky dátového súboru
summary(dataN)
#    Min.  1st Qu.   Median     Mean   3rd Qu.    Max. 
#   12,00    22,00    25,00    24,93    28,00    39,00 
# -------

# empirická kumulatívna distribučná funkcia pre výšky poistných nárokov
Fn <- ecdf(dataX)
curve(Fn(x), from=0, to=max(dataX), main="Empirická kumulatívna distribučná funkcia výšky poistných nárokov", col="red", lwd=2)

hist(dataX, col="red", breaks=50, angle=45, density=25, border="black", main="Histogram výšky poistných nárokov", xlab="výška poistného nároku", ylab="početnosti")

# charakteristiky dátového súboru (v prípade výšky poistných nárokov)
require(e1071)
# 
mean(dataX)            # výberový priemer      =    251,1543 eura
var(dataX)             # výberová disperzia    = 16 933,4500 eura^2
sd(dataX)              # smerodajná odchýlka   =    130,1286 eura
skewness(dataX)        # koeficient šikmosti   =      0,7059  (> 0)
kurtosis(dataX)        # koeficient špicatosti =      0,5161  (> 0)

# ďalšie charakteristiky dátového súboru
summary(dataX)
#    Min.  1st Qu.   Median     Mean   3rd Qu.     Max. 
#   14,87   153,31   234,20   251,15   333,28    789,77 
# ---------------------------------


# (A)
# Monte Carlo simulácia pre celkovú výšku poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) 
# jednoduchá replikácia hodnôt (simple resampling)

# počet simulácií
M <- 10000

# pomocná premenná pre ukladanie simulovaných hodnôt celkovej výšky poistných nárokov
S.sim <- rep(0, times=M)

for(t in 1:M)
{
  N.sim <- sample(dataN, size=1)
  X.sim <- sample(dataX, size=N.sim, replace=TRUE)
  S.sim[t] <- sum(X.sim)
}

# simulovaná kumulatívna distribučná funkcia pre celkovú výšku poistných nárokov
Fn <- ecdf(S.sim)
curve(Fn(x), from=min(S.sim), to=max(S.sim), main="Simulovaná kumulatívna distribučná funkcia celkovej výšky poistných nárokov", col="blue", lwd=2)

hist(S.sim, col="blue", breaks=50, angle=45, density=25, border="black", main="Histogram celkovej výšky poistných nárokov", xlab="celková výška poistných nárokov", ylab="početnosti")

# charakteristiky simulovaných celkových výšok poistných nárokov
require(e1071)
# 
mean(S.sim)            # výberový priemer
var(S.sim)             # výberová disperzia
sd(S.sim)              # smerodajná odchýlka
skewness(S.sim)        # koeficient šikmosti
kurtosis(S.sim)        # koeficient špicatosti

# ďalšie charakteristiky simulovaných celkových výšok poistných nárokov
summary(S.sim)

# výpočet 99-percentného kvantilu simulovaných výšok poistných nárokov
quantile(S.sim, probs=0.99)
# ---------------------------------


# (B)
# Monte Carlo simulácia pre celkovú výšku poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) 
# pomocou odhadnutých rozdelení pravdepodobnosti 

# počas procesu tvorby aktuárskeho modelu je možné zistiť, že počet poistných nárokov v jednom mesiaci sa riadi podľa Poissonovho rozdelenia v parametrom lambda=25
# (ďalšie podrobnosti o tvorbe aktuárskych modelov sú prezentované v 6. kapitole habilitačnej práce) 

require(fitdistrplus)
fit.pois <- fitdist(dataN, distr="pois", method="mle", discrete=TRUE)

summary(fit.pois)
# 
# Fitting of the distribution ' pois ' by maximum likelihood 
# Parameters : 
#        estimate Std. Error
# lambda    24.93  0.2882707
# Loglikelihood:  -899.4634   AIC:  1800.927   BIC:  1804.631

plot(fit.pois)
gofstat(fit.pois, discrete=TRUE)
# 
# Chi-squared statistic:  8.516071 
# Degree of freedom of the Chi-squared distribution:  9 
# Chi-squared p-value:  0.4830875 
# -------------------------------

# analogicky, počas procesu tvorby aktuárskeho modelu pre individuálne výšky poistných nárokov je možné zistiť, že výšky poistných nárokov sa riadia podľa Rayleigh-ho rozdelenia v parametrom scale=200
# (ďalšie podrobnosti o tvorbe aktuárskych modelov sú prezentované v 6. kapitole habilitačnej práce) 

require(fitdistrplus)
require(VGAM)
fit.rayleigh <- fitdist(dataX, distr="rayleigh", method="mle", start=list(scale=100), discrete=FALSE)

summary(fit.rayleigh)
#
# Fitting of the distribution ' rayleigh ' by maximum likelihood 
# Parameters : 
#       estimate Std. Error
# scale 200.0006   2.629774
# Loglikelihood:  -9006.439   AIC:  18014.88   BIC:  18020.15 

plot(fit.rayleigh)

# testy dobrej zhody
gofstat(fit.rayleigh)
# Goodness-of-fit statistics
#                              1-mle-rayleigh
# Kolmogorov-Smirnov statistic     0.01429897
# Cramer-von Mises statistic       0.04121836
# Anderson-Darling statistic       0.43841947
# 
# Goodness-of-fit criteria
#                                1-mle-rayleigh
# Akaike's Information Criterion       18014.88
# Bayesian Information Criterion       18020.15

# Kolmorogovov-Smirnovov test dobrej zhody
ks.test(dataX, "prayleigh", scale=200)
# 
#         One-sample Kolmogorov-Smirnov test
# 
# data:  dataX
# D = 0.014301, p-value = 0.9289
# alternative hypothesis: two-sided
# ---------------------------------

# odhadovanie kumulatívnej distribučnej funkcie pre celkovú výšku poistných nárokov pomocou balíku 'actuar' a funkcie 'aggregateDist'
require(actuar)
require(VGAM)

# definovanie modelu pre počet poistných nárokov nahlásených za jeden mesiac (claim frequency model)
model.freq <- expression(data=rpois(25))

# definovanie modelu pre počet poistných nárokov nahlásených za jeden mesiac (claim frequency model)
model.sev <- expression(data=VGAM::rrayleigh(200))

# konštruovanie simulovanej kumulatívnej distribučnej funkcie celkovej výšky poistných nárokov
Fs <- aggregateDist("simulation", nb.simul=10000, model.freq=model.freq, model.sev=model.sev)

# grafické znázornenie
plot(Fs)

# výpočet 99-percentného kvantilu simulovaných výšok poistných nárokov pomocou funkcie 'quantile')
quantile(Fs, probs=0.99)
# ----------------------

# +++
# simulácia zloženého rozdelenia pre celkovú výšku poistných nárokov (nahlásených napríklad v nasledujúcom mesiaci) pomocou balíku 'actuar' a funkcie 'rcompound'
require(actuar)
require(VGAM)
# 
S.sim.c <- rcompound(10000, model.freq=rpois(25), model.sev=VGAM::rrayleigh(200))

head(S.sim.c) 

# simulovaná kumulatívna distribučná funkcia pre celkovú výšku poistných nárokov
Fn <- ecdf(S.sim.c)
curve(Fn(x), from=min(S.sim.c), to=max(S.sim.c), main="Empirická kumulatívna distribučná funkcia celkovej výšky poistných nárokov", col="blue", lwd=2)

# simulovaný histogram celkových výšok poistných nárokov
hist(S.sim.c, col="blue", breaks=50, angle=45, density=25, border="black", main="Histogram celkovej výšky poistných nárokov", xlab="celková výška poistných nárokov", ylab="početnosti")

# charakteristiky simulovaných celkových výšok poistných nárokov
require(e1071)
# 
mean(S.sim.c)          # výberový priemer
var(S.sim.c)           # výberová disperzia
sd(S.sim.c)            # smerodajná odchýlka
skewness(S.sim.c)      # koeficient šikmosti
kurtosis(S.sim.c)      # koeficient špicatosti

# ďalšie charakteristiky simulovaných celkových výšok poistných nárokov
summary(S.sim.c)

# výpočet 99-percentného kvantilu simulovaných výšok poistných nárokov
quantile(S.sim.c, probs=0.99)
# =================================================


