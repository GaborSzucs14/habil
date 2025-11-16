# Doplnkový súbor k habilitačnej práci s názvom: 
# Pravdepodobnostné modelovanie v poisťovníctve
# Autor: Gábor Szűcs
# Pracovisko: KAMŠ FMFI UK v Bratislave
# 
# Verzia: 2025-09-25
# Kódovanie súboru: UTF-8
# 
# Kapitola 8 - Spoluúčasť a zaistenie v neživotnom poistení
# =================================================


# Príklad 8.1
# havarijné poistenie, gama rozdelenie, rôzne formy spoluúčasti a franšízy

# parametre zo zadania úlohy
k <- 12                # parameter tvaru gama rozdelenia (shape parameter)
beta <- 1/50           # (inverzný) parameter škály gama rozdelenia (rate parameter)

# grafické znázornenie hustoty gama rozdelenia
curve(dgamma(x, shape=k, scale=(1/beta)), from=, to=1200, lwd=3, col="red", lty="solid", xlab="x", ylab=expression(f(x)))
# -----


# (a)
# spoluúčasť 5 %, bez minimálnej sumy spoluúčasti

h <- 0.05              # podiel spoluúčasti
D <- 0                 # minimálna suma spoluúčasti (vyjadrená v eurách)

# na netto princíp sa v tomto prípade môžeme pozerať ako na poistný princíp rovnakej užitočnosti (z hľadiska poistníka) s lineárnou úžitkovou funkciou
# 
# E(poistník sa poistí) = E(poistník sa nepoistí)
# 
# E(A - P - h*X) = E(A - X)          &   A označuje kapitál poistníka
# 
# A - P - h*E(X) = A - E(X)
# 
# P = (1-h) * E(X)

            # stredná hodnota gama rozdelenia: 
            # E(X) = k/beta

# „spravodlivá” výška netto poistného
P_a <- (1-h) *k/beta
P_a
            # P_a = 570 eur
# -----------------------------------
            

# simulačná aproximácia poistného pomocou očakávanej hodnoty časti X_Aj škody, ktorú má platiť poistník 
# (Monte Carlo simulácie k bodu (a))

N<-1000000             # počet simulácií
X<-rgamma(N, shape=k, scale=(1/beta))
X_A<-rep(0, times=N)

X_A <- (1-h) * X

P_a_sim <- mean(X_A)
P_a_sim
            # ukážka simulovaného výsledku: 
            # P_a_sim = 569,9909 eura

# Odpoveď k bodu (a).
# „Spravodlivá” výška netto poistného by mala byť 570 eur.
# ---------------------------------------------------


# (b)
# spoluúčasť 5% , minimálna suma spoluúčasti 166 eur

h <- 0.05              # podiel spoluúčasti
D <- 166               # minimálna suma spoluúčasti (vyjadrená v eurách)

# po nastatí škodovej udalosti môžu nastať nasledujúce 3 situácie:
# 
    # 1. ak výška X_Aj škody je menej ako 166 eur, tak poistník ju hradí sám a v plnej výške,
    # 
    # 2. ak výška X_Aj škody je viac ako 166 eur a zároveň menej ako 166/0,05 = 3320 eur, tak poistník hradí fixnú (minimálnu) sumu 166 eur, zvyšnú časť X_Aj škody hradí poisťovňa; (ak výška X_Aj škody pochádza z intervalu <166 eur; 3320 eur>, tak pri 5 percentnej spoluúčasti dostaneme menšiu sumu spoluúčasti ako 166 eur; podľa zadania úlohy ale poistník musí na plnení podieľať minimálne sumou 166 eur, takže v tomto prípade paušálne platí 166 eur),
    # 
    # 3. ak výška X_Aj škody je viac ako 166/0,05 = 3320 eur, tak poistník platí 5 % X_Aj škody; zvyšnú časť X_Aj škody hradí poisťovňa; ak výška škody presiahne sumu 3320 eur, tak výška 5 percentnej spoluúčasti poistníka je vyššia ako 166 eur - v tom prípade teda platí práve 5 % zo vzniknutej X_Aj škody.

# Pre priemernú výšku škodových výdavkov poistníka (E(X_A)) môžeme písať:
#
# E(X_A) = E[X | X ∈ <0;166)]  +  E[166 | X ∈ <166;3320)]  +  E[0,05*X | X ∈ <3320; ∞)]
# 
# E(X_A) = INT_0^166 x f(x)dx  +  INT_166^3320 166* f(x)dx   +  INT_3320^∞ 0,05*x f(x)dx
#
# [odvodenie je prezentované v PDF-súbore habilitačnej práce]
# 
EX_A <- k/beta * pgamma(D, shape=(k+1), rate=beta) + D* (pgamma(D/h, shape=k, rate=beta) - pgamma(D, shape=k, rate=beta)) +  h* k/beta * (1 - pgamma(D/h, shape=(k+1), rate=beta))

EX_A
            # E(X_A) = 165,9971 eura

# Pre „spravodlivú” výšku netto poistného P_b potom platí:
# P_b = E(X) - E(X_A)
P_b <- k/beta - EX_A
P_b
            # P_b = 434,0029 eura
# -----------------------------------


# simulačná aproximácia poistného pomocou očakávanej hodnoty časti X_Aj škody, ktorú má platiť poistník 
# (Monte Carlo simulácie k bodu (b))

N<-1000000             # počet simulácií
X<-rgamma(N, shape=k, scale=(1/beta))
X_A<-rep(0, times=N)

for(i in 1:N) {
    if(X[i]<D) {X_A[i] <- X[i]}
    else{
    if(X[i]>=D && X[i]<(D/h)) {X_A[i] <- D}
    else {X_A[i] <- h*X[i]}
    }
}

P_b_sim <- k/beta - mean(X_A)      # P_b = E(X) - E(X_A)
P_b_sim
            # ukážky simulovaných výsledkov: 
            # P_b_sim = 434,0032 eura
            # P_b_sim = 434,0035 eura

# Odpoveď k bodu (b).
# „Spravodlivá” výška netto poistného by mala byť približne 434,00 eura.
# ---------------------------------------------------


# (c)
# spoluúčasť 0 %, minimálna suma spoluúčasti 299 eur

h <- 0                 # podiel spoluúčasti
D <- 299               # minimálna suma spoluúčasti (vyjadrená v eurách)

# po nastatí škodovej udalosti môžu nastať nasledujúce 2 situácie:
# 
    # 1. ak výška X_Aj škody je menej ako 299 eur, tak poistník ju hradí sám a v plnej výške,
    # 
    # 2. ak výška X_Aj škody je viac ako 299 eur, tak poistník platí len fixnú minimálnu sumu spoluúčasti (299 eur) a na platení zvyšnej sumy sa proporcionálne nepodieľa; zvyšnú sumu platí poisťovňa. 

# Pre priemernú výšku škodových výdavkov poistníka (E(X_A)) môžeme písať:
#
# E(X_A) = E[X | X ∈ <0;299)]  +  E[299 | X ∈ <299; ∞)]
# 
# E(X_A) = INT_0^299 x f(x)dx  +  INT_299^∞ 299* f(x)dx
# 
EX_A <- k/beta * pgamma(D, shape=(k+1), rate=beta) + D * (1 - pgamma(D, shape=k, rate=beta))

EX_A
            # E(X_A) = 298,2888 eura

# Pre „spravodlivú” výšku netto poistného P_c potom platí:
# P_c = E(X) - E(X_A)
P_c <- k/beta - EX_A
P_c
            # P_c = 301,7112 eura
# -----------------------------------


# simulačná aproximácia poistného pomocou očakávanej hodnoty časti X_Aj škody, ktorú má platiť poistník 
# (Monte Carlo simulácie k bodu (c))

N<-1000000             # počet simulácií
X<-rgamma(N, shape=k, scale=(1/beta))
X_A<-rep(0, times=N)

for(i in 1:N) {
    if(X[i]<D) {X_A[i] <- X[i]}
    else {X_A[i] <- D}
}

P_c_sim <- k/beta - mean(X_A)            # P_c = E(X) - E(X_A)
P_c_sim
            # ukážky simulovaných výsledkov: 
            # P_c_sim = 301,7092 eura
            # P_c_sim = 301,7075 eura

# Odpoveď k bodu (c).
# „Spravodlivá” výška netto poistného by mala byť približne 301,71 eura.
# ---------------------------------------------------


# (d)
# spoluúčasť 5 %, minimálna suma spoluúčasti 266 eur

h <- 0.05              # podiel spoluúčasti
D <- 266               # minimálna suma spoluúčasti (vyjadrená v eurách)

EX_A <- k/beta * pgamma(D, shape=(k+1), rate=beta) + D* (pgamma(D/h, shape=k, rate=beta) - pgamma(D, shape=k, rate=beta)) +  h* k/beta * (1 - pgamma(D/h, shape=(k+1), rate=beta))

EX_A
            # E(X_A) = 265,7368 eura

# Pre „spravodlivú” výšku netto poistného P_d potom platí:
# P_d = E(X) - E(X_A)
P_d <- k/beta - EX_A
P_d
            # P_d = 334,2632 eura
# -----------------------------------


# simulačná aproximácia poistného pomocou očakávanej hodnoty časti X_Aj škody, ktorú má platiť poistník 
# (Monte Carlo simulácie k bodu (d))

N<-1000000             # počet simulácií
X<-rgamma(N, shape=k, scale=(1/beta))
X_A<-rep(0, times=N)

for(i in 1:N) {
    if(X[i]<D) {X_A[i] <- X[i]}
    else {
    if(X[i]>=D && X[i]<(D/h)) {X_A[i] <- D}
    else {X_A[i] <- h*X[i]}
    }
}

P_d_sim <- k/beta - mean(X_A)      # P_d = E(X) - E(X_A)
P_d_sim
            # ukážky simulovaných výsledkov: 
            # P_d_sim = 334,2610 eura
            # P_d_sim = 334,2667 eura

# Odpoveď k bodu (d).
# „Spravodlivá” výška netto poistného by mala byť približne 334,26 eura.
# ---------------------------------------------------


# (e)
# spoluúčasť 20 %, minimálna suma spoluúčasti 450 eur

h <- 0.20              # podiel spoluúčasti
D <- 450               # minimálna suma spoluúčasti (vyjadrená v eurách)

EX_A <- k/beta * pgamma(D, shape=(k+1), rate=beta) + D* (pgamma(D/h, shape=k, rate=beta) - pgamma(D, shape=k, rate=beta)) +  h* k/beta * (1 - pgamma(D/h, shape=(k+1), rate=beta))

EX_A
            # E(X_A) = 435,8897 eura

# Pre „spravodlivú” výšku netto poistného P_e potom platí:
# P_e = E(X) - E(X_A)
P_e <- k/beta - EX_A
P_e
            # P_e = 164,1103 eura
# -----------------------------------


# simulačná aproximácia poistného pomocou očakávanej hodnoty časti X_Aj škody, ktorú má platiť poistník 
# (Monte Carlo simulácie k bodu (e))

N<-1000000             # počet simulácií
X<-rgamma(N, shape=k, scale=(1/beta))
X_A<-rep(0, times=N)

for(i in 1:N) {
    if(X[i]<D) {X_A[i] <- X[i]}
    else {
    if(X[i]>=D && X[i]<(D/h)) {X_A[i] <- D}
    else {X_A[i] <- h*X[i]}
    }
}

P_e_sim <- k/beta - mean(X_A)      # P_e = E(X) - E(X_A)
P_e_sim
            # ukážky simulovaných výsledkov: 
            # P_e_sim = 164,1250 eura
            # P_e_sim = 164,0507 eura

# Odpoveď k bodu (e).
# „Spravodlivá” výška netto poistného by mala byť približne 164,11 eura.
# ---------------------------------------------------


# (f)
# franšíza 299 eur

G <- 299               # franšíza (vyjadrená v eurách)

EX_A <- k/beta * pgamma(G, shape=(k+1), rate=beta)

EX_A
            # E(X_A) = 5,162662 eura

# Pre „spravodlivú” výšku netto poistného P_f potom platí:
# P_f = E(X) - E(X_A)
P_f <- k/beta - EX_A
P_f
            # P_f = 594,8373 eura
# -----------------------------------


# simulačná aproximácia poistného pomocou očakávanej hodnoty časti X_Aj škody, ktorú má platiť poistník 
# (Monte Carlo simulácie k bodu (f))

N<-1000000             # počet simulácií
X<-rgamma(N, shape=k, scale=(1/beta))
X_A<-rep(0, times=N)

for(i in 1:N) {
    if(X[i]<G) {X_A[i] <- X[i]}
    else { X_A[i] <- 0}
}

P_f_sim <- k/beta - mean(X_A)      # P_f = E(X) - E(X_A)
P_f_sim
            # ukážky simulovaných výsledkov: 
            # P_f_sim = 594,8685 eura
            # P_f_sim = 594,7852 eura

# Odpoveď k bodu (f).
# „Spravodlivá” výška netto poistného by mala byť približne 594,84 eura.
# ---------------------------------------------------


# (g)
# franšíza 450 eur

G <- 450               # franšíza (vyjadrená v eurách)

EX_A <- k/beta * pgamma(G, shape=(k+1), rate=beta)

EX_A
            # E(X_A) = 74,53594 eura

# Pre „spravodlivú” výšku netto poistného P_g potom platí:
# P_g = E(X) - E(X_A)
P_g <- k/beta - EX_A
P_g
            # P_g = 525,4641 eura
# -----------------------------------


# simulačná aproximácia poistného pomocou očakávanej hodnoty časti X_Aj škody, ktorú má platiť poistník 
# (Monte Carlo simulácie k bodu (g))

N<-1000000             # počet simulácií
X<-rgamma(N, shape=k, scale=(1/beta))
X_A<-rep(0, times=N)

for(i in 1:N) {
    if(X[i]<G) {X_A[i] <- X[i]}
    else { X_A[i] <- 0}
}

P_g_sim <- k/beta - mean(X_A)      # P_g = E(X) - E(X_A)
P_g_sim
            # ukážky simulovaných výsledkov: 
            # P_g_sim = 525,5256 eura
            # P_g_sim = 525,5639 eura

# Odpoveď k bodu (g).
# „Spravodlivá” výška netto poistného by mala byť približne 525,46 eura.
# =================================================



# Príklad 8.2
# príklad o rôznych formách spoluúčasti

# Riešenie tohto príkladu je na domáce cvičenie.
# =================================================



# Príklad 8.3
# príklad na kvótové zaistenie

# Podrobné riešenie tohto príkladu je možné nájsť v PDF-súbore habilitačnej práce a v excelovom súbore [Habil - K08 - Zaistenie - Priklad 8-3.xlsx].
# =================================================



# Príklad 8.4
# príklad na excedentné (surplus) zaistenie

# Podrobné riešenie tohto príkladu je možné nájsť v PDF-súbore habilitačnej práce a v excelovom súbore [Habil - K08 - Zaistenie - Priklad 8-4.xlsx].
# =================================================



# Príklad 8.5
# príklad na zaistné reťazce kvóta+surplus resp. surplus+kvóta

# Podrobné riešenie tohto príkladu je možné nájsť v PDF-súbore habilitačnej práce a v excelovom súbore [Habil - K08 - Zaistenie - Priklad 8-5.xlsx].
# =================================================



# Príklad 8.6
# príklad na zaistenie škodového nadmerku

# Riešenie tohto príkladu je možné nájsť v PDF-súbore habilitačnej práce a v excelovom súbore [Habil - K08 - Zaistenie - Priklad 8-6.xlsx].
# =================================================



# Príklad 8.7
# príklad na zaistenie nadmerku škodovosti

# Riešenie tohto príkladu je možné nájsť v excelovom súbore [Habil - K08 - Zaistenie - Priklad 8-7.xlsx].
# =================================================



# Úloha 8.1 
# úloha o spoluúčasti a zaistení

# výška poistných nárokov
# X ~ Exp(lambda=1/400)
lambda <- 1/400        # rate-parameter exponenciálneho rozdelenia
D <- 100               # hranica spoluúčasti
Z <- 1000              # hranica zaistenia

# grafické znázornenie hustoty výšky poistných nárokov
curve(dexp(x, rate=lambda), from=0, to=1200, lwd=2, col="red", xlab="x", ylab=expression(f[X](x)), main="Hustota exponenciálneho rozdelenia s rate-parametrom 1/400")
# -----


# (a) P(X<100)
# teoreticky
PA <- pexp(D,rate=lambda)
PA
# 0,2211992
# V 22,12 % prípadov vzniknutú škodu hradí poistník sám, v plnej výške.

                
# simulačné štúdium
N <- 1000000
X <- rexp(N,rate=lambda)
g <- 0

for(i in 1:N) {
    if(X[i]<100) g<-g+1
}

g/N
# ukážka simulovaného výsledku: 
# 0,221274
# ----------------------


# (b) P(X>1000)
# teoreticky
PB <- 1-pexp(Z,rate=lambda)
PB
# 0,082085
# V 8,21 % prípadov musí zasahovať aj zaisťovňa.

                
# simulačné štúdium
N <- 1000000
X <- rexp(N,rate=lambda)
g <- 0

for(i in 1:N) {
    if(X[i]>1000) g<-g+1
}

g/N
# ukážka simulovaného výsledku: 
# 0,082314
# ----------------------


# (c) Koľko hradí poistník v priemere?
# teoreticky
400 - 400*exp(-1/4)
# 88,47969
# Po vzniku škodovej udalosti poistník v priemere musí zaplatiť 88,48 eura.
                
# simulačné štúdium
N <- 1000000
X <- rexp(N,rate=lambda)
E <- 0

for(i in 1:N) {
    if(X[i]<100) {E<-E+X[i]}
    else {E<-E+100}
}

E/N
# ukážka simulovaného výsledku: 
# 88,49149
# ----------------------


# (d) Koľko hradí poisťovňa v priemere?
# teoreticky
400*exp(-1/4) - 400*exp(-10/4)
# 278,6863
# Po nahlásení škody poisťovňa v priemere platí sumu 278,69 eura.

                
# simulačné štúdium
N <- 1000000
X <- rexp(N,rate=lambda)
E <- 0

for(i in 1:N) {
    if(X[i]<100) {E<-E+0}
    if(X[i] < 1000 && X[i]>100)  { E<-E+X[i]-100 }
    if(X[i] > 1000) {E<-E+900}
}

E/N
# ukážka simulovaného výsledku: 
# 278,9537
# ----------------------


# (e) Koľko hradí zaisťovňa v priemere?
# teoreticky
400*exp(-10/4)
# 32,834
# Zaisťovňa v priemere hradí 32,83 eura.

        
# simulačné štúdium
N <- 3000000
X <- rexp(N,rate=lambda)
E <- 0

for(i in 1:N) {
    if(X[i]<1000) {E<-E+0}
    else {E<-E+X[i]-1000}
}

E/N
# ukážka simulovaného výsledku: 
# 32,80284
# =================================================



# Úloha 8.2
# úloha o zaistení typu WXL/R

# výšky poistných nárokov
# Z ~ Gama(shape=k=7, scale=80)
# 
# Z ~ Gama(shape=k=7, rate=lambda=1/80)

k <- 7                 # shape parameter gama rozdelenia
lambda <- 1/80         # scale parameter gama rozdelenia

pgamma(  50, shape=k, scale=1/lambda);
pgamma( 500, shape=k, scale=1/lambda);
pgamma(1500, shape=k, scale=1/lambda);

# Grafické znázornenie distribučnej funkcie gama rozdelenia
curve(dgamma(x,shape=k,scale=1/lambda), n=10000, from=0.001, to=2000, main="Hustota gama rozdelenia", xlab="z", ylab="f(z)", lwd=3, lty="longdash")
legend("topright",c("shape=7","rate=1/80"), bty="n", cex=1)

D  <- 100              # hranica spoluúčasti
Z1 <- 500              # dolná hranica zaistenia
Z2 <- 1500             # horná hranica zaistenia

options(digits=5)
# Tabuľka hodnôt distribučnej funkcie, do TeX-u
pgamma(seq(from=0, 900, by=100), shape=k, scale=1/lambda)

pgamma(seq(from=1000, 1900, by=100), shape=k, scale=1/lambda)

options(digits=5)
# Tabuľka hodnôt distribučnej funkcie, do TeX-u
pgamma(seq(from=100, 900, by=100), shape=k+1, scale=1/lambda)

pgamma(seq(from=1000, 1900, by=100), shape=(k+1), scale=1/lambda)
# ----------------------


# (a) P(Z>500)
# teoreticky
PB <- 1-pgamma(Z1, shape=k, scale=1/lambda)
PB
# 0,56622

# Odpoveď.
# V 56,622 % prípadov musí plniť aj zaisťovňa.

                
# simulačné štúdium
N <- 1000000
X <- rgamma(N, shape=k, scale=1/lambda)
g <- 0

for(i in 1:N) {
    if(X[i]>500) g<-g+1
}

g/N
# ukážka simulovaného výsledku: 
# 0,56585
# ----------------------


# (b) Koľko hradí poisťovňa v priemere?
# teoreticky
# 0* Pr(Z<100) + INT_100^500 {(z-100) f(z) dz} + INT_500^1500 {400 f(z) dz} + INT_1500^INF {(z-100-1000) f(z) dz}
# 
#   INT_100^500 {z f(z) dz} - 100* INT_100^500 {f(z) dz} 
# + 400* INT_500^1500 { f(z) dz} 
# + INT_1500^INF {z f(z) dz} - 1100 * INT_1500^INF {f(z) dz}
# 
#   INT_100^500 {z f(z) dz} - 100* Pr(Z € <100; 500>) 
# + 400* Pr(Z € <500; 1500>) 
# + INT_1500^INF {z f(z) dz} - 1100 * Pr(Z € <1500; INF>) 
# 
#   INT_100^500 {z f(z) dz} - 100* (F_G_k(500) - F_G_k(100))
# + 400* (F_G_k(1500) - F_G(_k500))
# + INT_1500^INF {z f(z) dz} - 1100 * (F_G_k(INF) - F_G_k(1500))
# 
#   INT_100^500 {z * (lambda^k / gamma(k)) * z^(k-1) * exp(-z*lambda) dz} - 100* (F_G_k(500) - F_G_k(100))
# + 400* (F_G_k(1500) - F_G_k(500))
# + INT_1500^INF {z * (lambda^k / gamma(k)) * z^(k-1) * exp(-z*lambda) dz} - 1100 * (F_G_k(INF) - F_G(_k1500))
# 
### Vo vnútri tých zostávajúci dvoch integrálov "umelo vyrobíme" hustotu nového gama rozdelenia,
### ktoré bude mať shape parameter rovný (k+1) a rate parameter rovný 'lambda'.
### Násobiaci člen (z^1) spojíme s mocninným členom (z^(k-1)), ktorý vystupuje v gama hustote.
### 
### Navyše je potrebné pridať nový konštantný člen, ktorý bude prislúchať k novej gama hustote.
### Výraz v integráloch vynásobíme takouto jednotkou:
### 1 = (lambda^(k+1) /gamma(k+1)) * (gamma(k+1)/ lambda^(k+1)).
#
#   INT_100^500 {(lambda^k / gamma(k)) * z^((k+1)-1) * exp(-z*lambda) dz} - 100* (F_G_k(500) - F_G_k(100))
# + 400* (F_G_k(1500) - F_G_k(500))
# + INT_1500^INF {(lambda^k / gamma(k)) * z^((k+1)-1) * exp(-z*lambda) dz} - 1100 * (F_G_k(INF) - F_G_k(1500))
# 
#   INT_100^500 {(lambda^k / gamma(k)) * (lambda^(k+1) / gamma(k+1)) * (gamma(k+1)/lambda^(k+1)) * z^((k+1)-1) * exp(-z*lambda) dz} - 100* (F_G_k(500) - F_G_k(100))
# + 400* (F_G_k(1500) - F_G_k(500))
# + INT_1500^INF {(lambda^k / gamma(k)) * (lambda^(k+1) / gamma(k+1)) * (gamma(k+1)/lambda^(k+1)) * z^((k+1)-1) * exp(-z*lambda) dz} - 1100 * (F_G_k(INF) - F_G_k(1500))
# 
#   (lambda^k / gamma(k))*(gamma(k+1)/lambda^(k+1)) INT_100^500 { (lambda^(k+1)/gamma(k+1))* z^((k+1)-1)* exp(-z*lambda) dz} - 100* (F_G_k(500) - F_G_k(100))
# + 400* (F_G_k(1500) - F_G_k(500))
# + (lambda^k / gamma(k))*(gamma(k+1)/lambda^(k+1)) INT_1500^INF { (lambda^(k+1) / gamma(k+1))* z^((k+1)-1)* exp(-z*lambda) dz} - 1100 * (F_G_k(INF) - F_G_k(1500))
# 
#   (lambda^k / gamma(k))*(gamma(k+1)/lambda^(k+1))* (F_G_(k+1)(500) - F_G_(k+1)(100)) - 100* (F_G_k(500) - F_G_k(100))
# + 400* (F_G_k(1500) - F_G_k(500))
# + (lambda^k / gamma(k))*(gamma(k+1)/lambda^(k+1))* (F_G_(k+1)(INF) - F_G_(k+1)(1500)) - 1100 * (F_G_k(INF) - F_G_k(1500))
# 
#   (lambda^k / lambda^(k+1))*(gamma(k+1)/gamma(k)) * (F8(500)-F8(100)) - 100* (F7(500)-F7(100)) 
# + 400* (F7(1500)-F7(500)) 
# + (lambda^k / lambda^(k+1))*(gamma(k+1)/gamma(k)) * (1-F8(1500)) - 1100 * (1-F7(1500))
# 

(k / lambda) * (pgamma(500, shape=(k+1), scale=1/lambda) - pgamma(100, shape=(k+1), scale=1/lambda)) - 
100*(pgamma(500, shape=k, scale=1/lambda) - pgamma(100, shape=k, scale=1/lambda)) + 
400*(pgamma(1500, shape=k, scale=1/lambda) - pgamma(500, shape=k, scale=1/lambda)) + 
(k / lambda) *(1 - pgamma(1500, shape=(k+1), scale=1/lambda)) -
1100*(1- pgamma(1500, shape=k, scale=1/lambda))
# 
# 346,195004430762

# Odpoveď.
# Po nahlásení škody poisťovňa v priemere platí sumu 346,195 eura.

                
# simulačné štúdium
N <- 1000000
X < -rgamma(N, shape=k, scale=1/lambda)
E <- 0

for(i in 1:N) {
    if(X[i] < 100) {E<-E+0}
    if(X[i] < 500 && X[i]>100)  { E<-E+X[i]-100 }
    if(X[i] < 1500 && X[i]>500) { E<-E+400 }
    if(X[i] > 1500) {E<-E+X[i]-100-1000}
}

E/N
# ukážka simulovaného výsledku: 
# 346,315170657691
# ----------------------


# (c) Koľko hradí zaisťovňa v priemere?
# teoreticky

# 0* Pr(Z<500) + INT_500^1500 {(z-500) f(z) dz} + INT_1500^INF {1000 f(z) dz}
# 
#   INT_500^1500 {z f(z) dz} - 500* INT_500^1500 {f(z) dz} 
# + INT_1500^INF {1000 f(z) dz} 
# 
#   (lambda^k / lambda^(k+1))*(gamma(k+1)/gamma(k)) * (F8(1500)-F8(500)) - 500* (F7(1500)-F7(500)) 
# - 1000 * (1-F7(1500))
# 

(k / lambda) * (pgamma(1500, shape=(k+1), scale=1/lambda) - pgamma(500, shape=(k+1), scale=1/lambda)) - 
500*(pgamma(1500, shape=k, scale=1/lambda) - pgamma(500, shape=k, scale=1/lambda)) + 
1000*(1- pgamma(1500, shape=k, scale=1/lambda))
# 
# 113,809532201545

# Odpoveď.
# Zaisťovňa v priemere hradí 113,81 eura.

        
# simulačné štúdium
N <- 3000000
X <- rgamma(N, shape=k, scale=1/lambda)
E <- 0

for(i in 1:N) {
    if(X[i] < 500) {E<-E+0}
    if(X[i] < 1500 && X[i] > 500) { E<-E+X[i]-500 }
    if(X[i] > 1500) {E<-E+1000}

}

E/N
# ukážky simulovaných výsledkov: 
# 113,880887573027
# 113,839643602832
# =================================================


