# Doplnkový súbor k habilitačnej práci s názvom: 
# Pravdepodobnostné modelovanie v poisťovníctve
# Autor: Gábor Szűcs
# Pracovisko: KAMŠ FMFI UK v Bratislave
# 
# Verzia: 2025-09-25
# Kódovanie súboru: UTF-8
# 
# Kapitola 7 - Bonusové-malusové schémy v neživotnom poistení
# =================================================


# Bonusové-malusové schémy (BMS), No-Claim Discount systémy (NCD systems), 

# Článok o bonusových-malusových systémoch na Slovensku v ďalších krajinách V4:
# 
# Skřivánková, V., Gajdošová, B.: Analýza bonus-malus systémov v neživotnom poistení. In: Forum Statisticum Slovacum, ročník XII, č. 2/2016, s. 53-66, 2016. Dostupné na adrese: <http://ssds.sk/casopis/archiv/2016/fss0216.pdf#page=55>.
# =================================================


# Ďalšie práce o bonusových-malusových systémoch: 
# 
# Cingelová, M.: Použitie bonus-malus systému v neživotnom poistení. Bakalárska práca, Fakulta matematiky, fyziky a informatiky, Univerzita Komenského v Bratislave, Bratislava, 2014.
# 
# Ivančíková, M.: Techniky navrhovania bonusových-malusových schém v poistení motorových vozidiel. Diplomová práca, Fakulta matematiky, fyziky a informatiky, Univerzita Komenského v Bratislave, Bratislava, 2024.
# =================================================



# Úloha 7.1
# BMS
# Podrobné riešenie tejto úlohy je možné nájsť aj v PDF-súbore habilitačnej práce.

options(digits=10)

# matica prechodu
p0<-pnbinom(0,4,0.92); p0;   # p0 = 0,71639296
p<-pnbinom(0,4,0.92)
q<-1-p0; q;                  #  q = 0,28360704

P<-matrix(c(q,p,0,0,0, q,0,p,0,0, q,0,0,p,0, 0,q,0,0,p, 0,0,q,0,p), ncol=5, nrow=5, byrow=TRUE)

# ANALYTICKÉ RIEŠENIE

pi3 <- (q * p0^3) / (1 - 2*q*p0^2)
pi3
# pi3 = 0,1470919835

pi0 <- (1-2*p0^2 + p0^3) / p0^3 * pi3
pi0
# pi0 = 0,1365150565

pi1 <- (1-p0^2) / p0^2 * pi3
pi1
# pi1 = 0,1395147475

pi2 <- 1/p0 * pi3 
pi2
# pi2 = 0,2053230443

pi4 <- p0/q * pi3
pi4
# pi4 = 0,3715551682
# -------------------

# počet poistníkov na jednotlivých bonusových stupňoch:
round(20000 * c(pi0, pi1, pi2, pi3, pi4), 0)
# 
# 2730 2790 4106 2942 7431

# (b)
# celkové poistné
PP <- 780000

# bonusové stupne:
bs <- c(1,0.9,0.8,0.7,0.5)

# výška základného ročného poistného
P. <- PP / (bs %*% round(20000 * c(pi0, pi1, pi2, pi3, pi4), 0))
P.            # P = 54,5427846 eura
# ------------------------


# (a)
# ITERATÍVNE RIEŠENIE
# iniciálny vektor
d<-c(1, 0, 0, 0, 0)

# hľadanie stacionárneho rozdelenia iteratívnym spôsobom
# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(5*N)),ncol=5,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D
# stac. rozd. = 
# (0,1365151; 0,1395147; 0,2053230; 0,1470920; 0,3715552)
# --------------

# začiatočný počet: 20000
# na začiatku všetci sú na úrovni 0
# teda vektor poistníkov na jednotlivých úrovniach je:
d<-c(20000,0,0,0,0)

# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(5*N)),ncol=5,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D
# 2730,301;  2790,295;  4106,461; 2941,840; 7431,103;

# stabilizovaný počet poistníkov:
stab <- c(2730, 2790, 4107, 2942,7431)


# (b)
# celkové poistné
PP <- 780000

# bonusové stupne:
bs <- c(1,0.9,0.8,0.7,0.5)

# výška základného poistného
P <-PP / sum(bs*stab)
P           # P = 54,53973 eura
#----------------


# Stacionárne rozdelenie:

ST <- c(0.1365151, 0.1395147, 0.2053230, 0.1470920, 0.3715552)

# Generovanie zo stacionárneho rozdelenia
M<-200000
x<-runif(M,0,1)
g<-rep(0, times=5)

for(j in 1:M) {
    if(x[j]<sum(ST[1:1])) g[1]<-g[1]+1
    if(x[j]>sum(ST[1:1]) && x[j]<sum(ST[1:2])) g[2]<-g[2]+1
    if(x[j]>sum(ST[1:2]) && x[j]<sum(ST[1:3])) g[3]<-g[3]+1
    if(x[j]>sum(ST[1:3]) && x[j]<sum(ST[1:4])) g[4]<-g[4]+1
    if(x[j]>sum(ST[1:4]) && x[j]<sum(ST[1:5])) g[5]<-g[5]+1
}

# generovane stacionárne rozdelenie:
g/M
# 0,136220;   0,140060;   0,207205;   0,147040;   0,369475;

# teoretické stacionárne rozdelenie
ST
# 0,1365151;  0,1395147;  0,2053230;  0,1470920;  0,3715552;

# generované stacionárne rozdelenie sa zhoduje s teoretickým
# --------------------------------------------

# simulácia vývoja; jeho pohyb na bonusových stupňoch

ROK<-25
LEVEL<-0
res<-rep(0, times=25)
stu<-rep(0, times=25)

for(i in 1:ROK) {
    stu[i]<-LEVEL
    y<-rnbinom(1,4,0.92)
    if(y<0.9) { LEVEL<-min(LEVEL+1,4)}
    else { 
            if(LEVEL==1) {LEVEL<-0  }
            else {LEVEL<-max(LEVEL-2,0)}
        }
    res[i]<-y    
}

stu
res

plot(seq(1,ROK,by=1),-stu, type="s", main="Ukážka pohybu poistenca na bonusových stupňoch", lwd=4, col="blue", xlab="roky", ylab="bonus level")
# =================================================



# Úloha 7.2
# NCD systém, bonusová schéma
# Podrobné riešenie tejto úlohy je možné nájsť aj v PDF-súbore habilitačnej práce. 

# ANALYTICKÉ RIEŠENIE
options(digits=10)

# pravdepodobnosť neohlásenia / ohlásenia škodovej udalosti u dobrých vodičov
p<-0.9; q<-1-p;

# stacionárne rozdelenie
PI <- c(q^2/(1-p*q), p*q/(1-p*q), p^2/(1-p*q))
PI
# PI = 0.01098901099 0.09890109890 0.89010989011

c(1/91, 9/91, 81/91)
# 0.01098901099 0.09890109890 0.89010989011

# počet poistníkov (dobrých vodičov) na bonusových stupňoch v ustálenom stave systému:
5000*PI
# 55  494.5 4450.5
# -----------------------


# pravdepodobnosť neohlásenia / ohlásenia škodovej udalosti u priemerných vodičov
r<-0.8; s<-1-r;

# stacionárne rozdelenie
TAU <- c(s^2/(1-r*s), r*s/(1-r*s), r^2/(1-r*s))
TAU
# TAU = 0.04761904762 0.19047619048 0.76190476190

c(1/21, 4/21, 16/21)
# 0.04761904762 0.19047619048 0.76190476190

# počet poistníkov (priemerných vodičov) na bonusových stupňoch v ustálenom stave systému:
5000*TAU
# 238  952.5 3809.5
# -----------------------


# ITERAČNÉ RIEŠENIE
# matica prechodu v prípade dobrých vodičov
P1<-matrix(c(0.1,0.9,0,0.1,0,0.9,0,0.1,0.9),ncol=3,nrow=3,byrow=TRUE)

# začiatočný počet: 5000
# na začiatku všetci sú na úrovni 0
# teda vektor poistníkov na jednotlivých úrovniach je:
d<-c(5000,0,0)

# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(3*N)),ncol=3,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P1
}

D
#----------------

# matica prechodu v prípade priemerných vodičov
P2<-matrix(c(0.2,0.8,0,0.2,0,0.8,0,0.2,0.8),ncol=3,nrow=3,byrow=TRUE)

# začiatočný počet: 5000
# na začiatku všetci sú na úrovni 0
# teda vektor poistníkov na jednotlivých úrovniach je:
h<-c(5000,0,0)

# simulujeme budúci vývoj v bonusovom systéme:
N<-50
H<-matrix(rep(0, times=(3*N)),ncol=3,nrow=50,byrow=TRUE)

for(i in 1:50) {
    H[i,]<-h
    h<-h%*%P2
}

H
#----------------

# Stacionárne rozdelenia:

S1<-c(1/91,9/91,81/91)
S2<-c(1/21,4/21,16/21)
#-------------------------

# Dobrý vodič, ktorý vstúpi do systému na úrovni 0,
# simulácia vývoja; jeho pohyb na bonusových stupňoch

ROK<-25
LEVEL<-0
res<-rep(0, times=25)
stu<-rep(0, times=25)

for(i in 1:ROK) {
    stu[i]<-LEVEL
    y<-runif(1,0,1)
    if(y<0.1) { LEVEL<-max(LEVEL-1,0) }
    else { LEVEL<-min(LEVEL+1,2) }
    res[i]<-y    
}

stu
res

plot(seq(1,ROK,by=1), stu, type="s", main="Ukážka pohybu dobrého vodiča na bonusových stupňoch", lwd=4, col="green", xlab="roky", ylab="bonus level")
# --------------------------------------------


# Priemerný vodič, ktorý vstúpi do systému na úrovni 0,
# simulácia vývoja; jeho pohyb na bonusových stupňoch

ROK<-25
LEVEL<-0
res<-rep(0, times=25)
stu<-rep(0, times=25)

for(i in 1:ROK) {
    stu[i]<-LEVEL
    y<-runif(1,0,1)
    if(y<0.2) { LEVEL<-max(LEVEL-1,0) }
    else { LEVEL<-min(LEVEL+1,2) }
    res[i]<-y    
}

stu
res

windows(); plot(seq(1,ROK,by=1), stu, type="s", main="Ukážka pohybu priemerného vodiča na bonusových stupňoch", lwd=4, col="blue", xlab="roky", ylab="bonus level")
# --------------------------------------------

# Ročné poistné
# (55 + 494.5 * 0.80 + 4450.5* 0.60 + 238 + 952.5* 0.80 + 3810.5*0.60)*P = 250000
# 6407.2*P = 250000
P <- 250000 / (55 + 494.5 * 0.80 + 4450.5* 0.60 + 238 + 952.5* 0.80 + 3810.5*0.60)
P 
# P = 39,0186 eura
# =================================================



# Úloha 7.3
# NCD systém
# bonus-malus schéma: 4 úrovne, theta, rho
# skúsení vodiči, mladí vodiči

# Riešenie tejto úlohy je možné nájsť v PDF-súbore habilitačnej práce.
# =================================================



# Úloha 7.4

# NCD systém

# SKUPINA I.
lambdaI <- 0.5

# matica prechodu
p0 <- dpois(0, lambdaI); p0        # 0,6065307
p1 <- dpois(1, lambdaI); p1        # 0,3032653
q <- 1-p0-p1; q                    # 0,09020401

P <- matrix(c(p1+q,p0,0,0, p1+q,0,p0,0, q,p1,0,p0, q,0,p1,p0), ncol=4,nrow=4,byrow=TRUE)

# iniciálny vektor
d<-c(1/4,1/4,1/4,1/4)

# hľadanie stacionárneho rozdelenia iteratívnym spôsobom
# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(4*N)),ncol=4,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D
# stac. rozd. = 0,2169760; 0,2010473; 0,2289900; 0,3529867;

# Poznámka. 
# Úlohu je možné riešiť aj ručným počítaním tak, ako sa hľadalo stacionárne rozdelenie pri riešení Úlohy 7.1, 7.2 a 7.3 (pozri aj PDF-súbor habilitačnej práce). 
# --------------

# začiatočný počet: 30000*0.15 = 4500
# na začiatku všetci sú na úrovni 0
# teda vektor poistníkov na jednotlivých úrovniach je:
d<-c(0,30000*0.15,0,0)

# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(4*N)), ncol=4,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D
# 976,3919;  904,7129; 1030,4550; 1588,440;
# -----------------------------------------


# SKUPINA II.
lambdaII <- 0.2

# matica prechodu
p0 <- dpois(0, lambdaII); p0       # 0,8187308
p1 <- dpois(1, lambdaII); p1       # 0,1637462
q <- 1-p0-p1; q                    # 0,0175231

P <- matrix(c(p1+q,p0,0,0, p1+q,0,p0,0, q,p1,0,p0, q,0,p1,p0), ncol=4,nrow=4,byrow=TRUE)

# iniciálny vektor
d<-c(1/4,1/4,1/4,1/4)

# hľadanie stacionárneho rozdelenia iteratívnym spôsobom
# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(4*N)), ncol=4,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D
# stac. rozd. = 0,03129448; 0,05280757; 0,1660241; 0,7498738;

# Poznámka. 
# Úlohu je možné riešiť aj ručným počítaním tak, ako sa hľadalo stacionárne rozdelenie pri riešení Úlohy 7.1, 7.2 a 7.3 (pozri aj PDF-súbor habilitačnej práce). 
# --------------

# začiatočný počet: 30000*0.20 = 6000
# na začiatku všetci sú na úrovni 0
# teda vektor poistníkov na jednotlivých úrovniach je:
d<-c(0,30000*0.20,0,0)

# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(4*N)),ncol=4,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D
# 187,7669;  316,8454;  996,1448; 4499,243;
# -----------------------------------------


# SKUPINA III.
lambdaIII <- 0.1

# matica prechodu
p0 <- dpois(0, lambdaIII); p0      # 0,9048374
p1 <- dpois(1, lambdaIII); p1      # 0,09048374
q <- 1-p0-p1; q                    # 0,00467884

P <- matrix(c(p1+q,p0,0,0, p1+q,0,p0,0, q,p1,0,p0, q,0,p1,p0), ncol=4, nrow=4, byrow=TRUE)

# iniciálny vektor
d<-c(1/4,1/4,1/4,1/4)

# Hľadanie stacionárneho rozdelenia iteratívnym spôsobom
# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(4*N)),ncol=4,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D
# stac. rozd. = 0,006574862; 0,01437942; 0,09316852; 0,8858772;

# Poznámka. 
# Úlohu je možné riešiť aj ručným počítaním tak, ako sa hľadalo stacionárne rozdelenie pri riešení Úlohy 7.1, 7.2 a 7.3 (pozri aj PDF-súbor habilitačnej práce). 
# --------------

# začiatočný počet: 30000*(1-0.15-0.20) = 19500
# na začiatku všetci sú na úrovni 0
# teda vektor poistníkov na jednotlivých úrovniach je:
d<-c(0,30000*(1-0.15-0.20),0,0)

# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(4*N)), ncol=4, nrow=50, byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D
# 128,2098;   280,3986;  1816,786; 17274,61;
# ------------------------------------------


# výpočet ročnej výšky poistného
#
#   (976.5*(1+0.10) + 904.5*1 + 1030.5*(1-0.25) + 1588.5*(1-0.45))*P +
# + (188*(1+0.10) + 317*1 + 996*(1-0.25) + 4499*(1-0.45))*P +
# + (128*(1+0.10) + 280.5*1 + 1817*(1-0.25) + 17274.5*(1-0.45))*P =
# = 2200000

# (976.5*(1+0.10) + 904.5*1 + 1030.5*(1-0.25) + 1588.5*(1-0.45) + 188*(1+0.10) + 317*1 + 996*(1-0.25) + 4499*(1-0.45) + 128*(1+0.10) + 280.5*1 + 1817*(1-0.25) + 17274.5*(1-0.45)) * P = 2200000

# 18655.47*P = 2200000 eur

P <- 2200000 / (976.5*(1+0.10) + 904.5*1 + 1030.5*(1-0.25) + 1588.5*(1-0.45) + 188*(1+0.10) + 317*1 + 996*(1-0.25) + 4499*(1-0.45) + 128*(1+0.10) + 280.5*1 + 1817*(1-0.25) + 17274.5*(1-0.45))
P 
# P = 117,9278 eura
# =================================================



# Úloha 7.5

# NCD systém
# počet nárokov sa riadi podľa rozdelenia pravdepodobnosti Po(0,28)

# matica prechodu
p0 <- dpois(0, 0.28);    p0;       # p0 = 0,7557837
p1 <- dpois(1, 0.28);    p1;       # p1 = 0,2116194
q <- 1-p0-p1;            q;        # q  = 0,03259681

P <- matrix(c(p1+q,p0,0,0, p1+q,0,p0,0, q,p1,0,p0, q,0,p1,p0),ncol=4,nrow=4,byrow=TRUE)
P

# (p1+q, p0,  0,  0)
# (p1+q,  0, p0,  0)
# (q,    p1,  0, p0)
# (q,     0, p1, p0)

# iniciálny vektor
d<-c(1/4,1/4,1/4,1/4)

# Hľadanie stacionárneho rozdelenia iteratívnym spôsobom
# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(4*N)),ncol=4,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D

stac.rozd <- D[N,];        stac.rozd    
# stac.rozd = 0,06648523; 0,09365326; 0,20510783; 0,63475367;

# Poznámka. 
# Úlohu je možné riešiť aj ručným počítaním tak, ako sa hľadalo stacionárne rozdelenie pri riešení Úlohy 7.1, 7.2 a 7.3 (pozri aj PDF-súbor habilitačnej práce). 
# --------------

# začiatočný počet: 12739
# na začiatku všetci sú na úrovni 0
# teda vektor poistníkov na jednotlivých úrovniach je:
d<-c(0,12739,0,0)

# simulujeme budúci vývoj v bonusovom systéme:
N<-50
D<-matrix(rep(0, times=(4*N)),ncol=4,nrow=50,byrow=TRUE)

for(i in 1:50) {
    D[i,]<-d
    d<-d%*%P
}
D

# Rozdelenie poistníkov na začiatku tohto kalendárneho roka
# 846,9554;    1193,0489;     2612,869;    8086,127;
poistenci0 <- c(847, 1193, 2613, 8086)


# Počet poistníkov na konci roka (bez odchodov, bez uzavretia nových zmlúv)
poistenci01 <- t(poistenci0) %*% P 
poistenci01
# 846,9544; 1193,11; 2612,805; 8086,13;
# 847; 1193; 2613; 8086;


# Počet poistníkov na konci roka (s odchodmi 40 %, 10 %, 2 %, 2 %; počet nových zmlúv = 520)
# 
poistenci1 <- c(847-0.40*847, 1193-0.10*1193+520, 2613-0.02*2613, 8086-0.02*8086)
poistenci1
# 508,20; 1593,70; 2560,74; 7924,28;
# 508; 1594; 2561; 7924;
    
sum(poistenci1)        # 12 587 poistníkov
# =================================================


