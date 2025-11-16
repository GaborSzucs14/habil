# Doplnkový súbor k habilitačnej práci s názvom: 
# Pravdepodobnostné modelovanie v poisťovníctve
# Autor: Gábor Szűcs
# Pracovisko: KAMŠ FMFI UK v Bratislave
# 
# Verzia: 2025-09-25
# Kódovanie súboru: UTF-8
# 
# Kapitola 6 - Tvorba aktuárskych modelov v neživotnom poistení
# =================================================


# Úloha 6.1

# výšky nahlásených škôd na leteckých dopravných prostriedkoch (uvedené v tisícoch eur) 

# 0a.
# Získanie, spracovanie a načítanie dát:
dataX <- as.vector(read.table("http://www.iam.fmph.uniba.sk/ospm/Szucs/data/cp_vtp_data_letecke_skody.txt", header=FALSE)$V1)

# dataX <- as.vector(read.table("D:\\data\\cp_vtp_data_letecke_skody.txt", header=FALSE)$V1)

dataX; length(dataX)        # počet pozorovaní = 610

# Kontrola dátového súboru:
# - dátový súbor má správnu štruktúru, neobsahuje žiadne nesprávne (nevhodné) údaje,
# - nie je potrebné upraviť dátový súbor, dáta môžeme priamo použiť.
# -----

# inštalácia potrebných knižníc 
# (nižšie uvedený príkaz stačí spustiť len pri prvom použití tohto skriptu)
install.packages(c("fitdistrplus", "goftest", "e1071", "actuar")); 
# -----

# Grafická analýza údajov a počítanie empirických opisných charakteristík
# 
# empirická distribučná funkcia
Fn <- ecdf(dataX)
curve(Fn(x), from=0, 800, main="Empirická distribučná funkcia zostavená na základe dát", col="Indianred", lwd=2)

# charakteristiky dátového súboru
require(e1071)
# 
mean(dataX)            # výberový priemer      =    160,292700 tisíc eur
var(dataX)             # výberová disperzia    = 12 926,150000 tisíc eur^2
sd(dataX)              # smerodajná odchýlka   =    113,693200 tisíc eur
skewness(dataX)        # koeficient šikmosti   =      1,275175 (> 0)
kurtosis(dataX)        # koeficient špicatosti =      1,776852 (> 0)

# ďalšie charakteristiky dátového súboru
summary(dataX)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   3.531  71.907 132.665 160.293 213.561 637.661

hist(dataX)
# ---------------------------------


# 0b.
# Využitie aktuárskych znalostí a skúseností

# Keďže chceme modelovať výšky škodových udalostí, pri modelovaní je odporúčané používať spojité rozdelenia pravdepodobnosti, ktoré sú definované na množine nezáporných reálnych čísel. Výberový koeficient šikmosti (výberový koeficient asymetrie) počítaný na základe historických dát je kladný (1,275175), preto je rozumné aplikovať pozitívne (pravostranne) zošikmené rozdelenia pravdepodobnosti (napr. exponenciálne, gama, Paretovo, Weibullovo rozdelenie, a pod.). 
# 
# https://statisticsbyjim.com/basics/skewed-distribution/

# Zo základných opisných štatistík vidíme, že aritmetický priemer výšok škôd je približne 160 tisíc eur, kým najväčšia škodová udalosť má výšku zhruba 637 tisíc eur. To znamená, že v dátovom súbore sa nenachádza žiadna extrémne vysoká škoda (žiadne odľahlé pozorovanie, žiaden "outlier"), takže pri modelovaní je rozumnejšie použiť rozdelenia s ľahkým chvostom.

# Odhadovanie rozdelenia výšky škôd spravíme v prostredí softvéru R pomocou balíku "fitdistrplus".

# načítanie knižnice
library(fitdistrplus);
# https://cran.r-project.org/web/packages/fitdistrplus/fitdistrplus.pdf
# https://www.jstatsoft.org/article/view/v064i04/v64i04.pdf
# https://cran.r-project.org/web/packages/fitdistrplus/vignettes/fitdistrplus_vignette.html

# podrobný popis funkcie fitdist()
# help(fitdist)

# fitdist(data, distr, method = c("mle", "mme", "qme", "mge"), 
#         start=NULL, fix.arg=NULL, discrete, keepdata=TRUE, ...)
# 
# možné metódy odhadovania parametrov:
# method="mle": maximum likelihood estimation
# method="mme": moment matching estimation
# method="qme": quantile matching estimation
# method="mge": maximum goodness-of-fit estimation

# Pri riešení tohto príkladu používame metódu maximálnej vierohodnosti (method="mle").
# ---------------------------------


# AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
# 1.
# Voľba konkrétneho modelu (A)
# 
# Na úvod zvolíme jeden z najjednoduchších spojitých rozdelení pravdepodobnosti (definovaných na množine nezáporných reálnych čísel): exponenciálne rozdelenie. Toto rozdelenie pravdepodobnosti je pravostranne (pozitívne) zošikmené a má ľahký pravý chvost, no je parametrizovaný len pomocou jediného parametra.
# ---------------------------------

# 2.
# Kalibrácia modelu (A)
# 
# (A) výška škody ~ exponenciálne rozdelenie

fit.exp <- fitdist(dataX, distr="exp", method="mle", discrete=FALSE)

    # ďalšie možnosti odhadovania parametrov zvoleného rozdelenia
    # fit.exp <- fitdist(dataX, distr="exp", method="mme", discrete=FALSE)
    # fit.exp <- fitdist(dataX, distr="exp", method="mge", discrete=FALSE, gof="KS")

summary(fit.exp)
#
#         estimate   Std. Error
# rate 0.006238586 0.0002458969
# Loglikelihood:  -3706.971   AIC:  7415.942   BIC:  7420.356

# Odhadovanie parametra exponenciálneho rozdelenia prebehlo v poriadku, pomocou metódy maximálnej vierohodnosti sme dostali odhad rate-parametra 0,006238586. To znamená, že stredná hodnota odhadnutého exponenciálneho rozdelenia je 160,2927 tisíc eur, čo veľmi presne "triafa" do stredu dátového súboru (výberový aritmetický priemer dát je 160,293 tisíc eur). 
# Naozaj je tento odhadnutý model taký dobrý, vhodný a presný? Zistíme to v ďalšom bode, pri validácii modelu.
# ---------------------------------

# 3.
# Validácia modelu (A)
# 
# grafické posúdenie presnosti odhadnutého modelu
plot(fit.exp)

# výsledky testov dobrej zhody
gofstat(fit.exp)
# Goodness-of-fit statistics
#                               1-mle-exp
# Kolmogorov-Smirnov statistic  0.1378253
# Cramer-von Mises statistic    4.1853390
# Anderson-Darling statistic   24.8710823
# 
# Goodness-of-fit criteria
#                                1-mle-exp
# Aikake's Information Criterion  7415.942
# Bayesian Information Criterion  7420.356 
# -----

# Kolmogorovov-Smirnovov test dobrej zhody
ks.test(dataX, "pexp", rate=fit.exp$estimate)
#
# D = 0.13783, p-value = 1.723e-10
# alternative hypothesis: two-sided

# Cramérov-von Misesov test dobrej zhody
require(goftest)
cvm.test(dataX, "pexp", rate=fit.exp$estimate)
# 
# data:  dataX
# omega2 = 4.1853, p-value = 1.657e-10

# Andersonov-Darlingov test dobrej zhody
require(goftest)
ad.test(dataX, "pexp", rate=fit.exp$estimate)
# 
# data:  dataX
# An = 24.871, p-value = 9.836e-07

# Z grafov a výstupov testov dobrej zhody vidíme, že odhadnutý model založený na exponenciálnom rozdelení s rate-parametrom 0,006238586 nepresne opisuje podkladové dáta. Pri každom teste dobrej zhody sme dostali veľmi nízku p-hodnotu, čo znamená, že nulovú hypotézu s veľkou istotou môžeme zamietnuť, teda medzi empirickým rozdelením a odhadnutým exponenciálnym rozdelením sú štatisticky významné odchýlky.
# ---------------------------------

# 4. 
# Overenie vhodnosti modelu (A)
# 
# Pri validácii modelu sme zistili, že exponenciálne rozdelenie s rate-parametrom 0,006238586 nie je vhodné použiť pri charakterizácii výšok škodových udalostí súvisiacich so škodami na leteckých dopravných prostriedkoch. S veľkou istotou môžeme vyhlásiť, že výšky týchto škôd sa neriadia podľa exponenciálneho rozdelenia. Je potrebné, aby sme sa vrátili k 1. kroku a zvolili iný model, t. j. ďalej hľadali vhodné rozdelenie pravdepodobnosti.
# ---------------------------------


# BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
# 1.
# Voľba konkrétneho modelu (B)
# 
# V tomto kole zvolíme normálne rozdelenie. Už pred vykonaním ďalších krokov môžeme očakávať, že normálne rozdelenie nebude najvhodnejšie, pretože je definované na množine všetkých reálnych čísel, kým výšky škodových udalostí pochádzajú z oboru kladných čísel. Ďalej je známe, že normálne rozdelenie je symetrické (t. j. je charakterizované nulovým koeficientom šikmosti), kým pri prvotnej analýze dátového súboru sme zistili, že výberový koeficient šikmosti (výberový koeficient asymetrie) výšok škôd je kladný (1,275175) a výrazne odlišný od nuly. Napriek tomu skúsime odhadnúť parametre normálneho rozdelenia a vykonáme aj validáciu modelu založeného na normálnom rozdelení.
# ---------------------------------

# 2.
# Kalibrácia modelu (B)
# 
# (B) výška škody ~ normálne rozdelenie

fit.norm <- fitdist(dataX, distr="norm", method="mle", discrete=FALSE)

summary(fit.norm)
#
#      estimate   Std. Error
# mean 160.2927   4.599526
# sd   113.6000   3.252356
# Loglikelihood:  -3752.489   AIC:  7508.979   BIC:  7517.806 
# ---------------------------------

# 3.
# Validácia modelu (B)
# 
# grafické posúdenie presnosti odhadnutého modelu
plot(fit.norm)

# výsledky testov dobrej zhody
gofstat(fit.norm)
# 
#                              1-mle-norm
# Kolmogorov-Smirnov statistic  0.1009363
# Cramer-von Mises statistic    2.4133140
# Anderson-Darling statistic   15.0276714
# 
# Goodness-of-fit criteria
#                                1-mle-norm
# Akaike's Information Criterion   7508.979
# Bayesian Information Criterion   7517.806
# -----

# Kolmogorovov-Smirnovov test dobrej zhody
ks.test(dataX, "pnorm", mean=fit.norm$estimate[1], sd=fit.norm$estimate[2])
#
# D = 0.10094, p-value = 7.998e-06
# alternative hypothesis: two-side

# Cramérov-von Misesov test dobrej zhody
require(goftest)
cvm.test(dataX, "pnorm", mean=fit.norm$estimate[1], sd=fit.norm$estimate[2])
# 
# data:  dataX
# omega2 = 2.4133, p-value = 1.466e-06

# Andersonov-Darlingov test dobrej zhody
require(goftest)
ad.test(dataX, "pnorm", mean=fit.norm$estimate[1], sd=fit.norm$estimate[2])
# 
# data:  dataX
# An = 15.028, p-value = 9.836e-07

# Z grafov a výstupov testov dobrej zhody vidíme, že odhadnutý model založený na normálnom rozdelení s parametrami mu=160,2927 a sigma=113,6 nepresne opisuje podkladové dáta. Pri každom teste dobrej zhody sme dostali veľmi nízku p-hodnotu, čo znamená, že nulovú hypotézu s veľkou istotou môžeme zamietnuť. Medzi empirickým rozdelením a odhadnutým normálnym rozdelením sú štatisticky významné odchýlky. Naplnili sa teda naše očakávania o nevhodnosti normálneho modelu. 
# --------------------------------

# 4. 
# Overenie vhodnosti modelu (B)
# 
# Pri validácii modelu sme zistili, že Gaussovo normálne rozdelenie s parametrami mu=160,2927 a sigma=113,6 nie je vhodné použiť pri charakterizácii výšok škodových udalostí súvisiacich so škodami na leteckých dopravných prostriedkoch. S veľkou istotou môžeme vyhlásiť, že výšky týchto škôd sa neriadia podľa normálneho rozdelenia. Je potrebné, aby sme sa vrátili k 1. kroku a zvolili iný model, t. j. ďalej hľadali vhodné rozdelenie pravdepodobnosti.
# ---------------------------------


# CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
# 1.
# Voľba konkrétneho modelu (C)
# 
# V ďalšom kole si vyskúšame troj-parametrové Burrovo rozdelenie. Toto rozdelenie pravdepodobnosti je pravostranne zošikmené, no má ťažký pravý chvost, aj keď pri vhodnej voľbe parametrov je možné ho použiť aj pre modelovanie malých škôd.
# https://cran.r-project.org/web/packages/actuar/actuar.pdf#page=16
# ---------------------------------

# 2.
# Kalibrácia modelu (C)
# 
# (C) výška škody ~ Burrovo rozdelenie

# načítanie knižnice 'actuar'
require(actuar);
# https://cran.r-project.org/web/packages/actuar/actuar.pdf
# https://www.actuaries.org/astin/colloquia/orlando/papers/goulet.pdf

# popis Burrovho rozdelenia
# help(pburr)

fit.burr <- fitdist(dataX, distr="burr", method="mle", start=list(shape1=2, shape2=3, rate=0.1), discrete=FALSE)

summary(fit.burr)
#        estimate   Std. Error
# shape1 5.220614820 3.138841e-01
# shape2 1.653913928 4.940364e-02
# rate   0.002265688 7.040725e-05
# Loglikelihood:  -3638.394   AIC:  7282.788   BIC:  7296.029 
# ---------------------------------

# 3.
# Validácia modelu (C)
# 
# grafické posúdenie presnosti odhadnutého modelu
plot(fit.burr)

# výsledky testov dobrej zhody
gofstat(fit.burr)
#                              1-mle-burr
# Kolmogorov-Smirnov statistic 0.03165206
# Cramer-von Mises statistic   0.05625388
# Anderson-Darling statistic   0.43387026
# 
# Goodness-of-fit criteria
#                                1-mle-burr
# Aikake's Information Criterion   7282.788
# Bayesian Information Criterion   7296.029
# -----

# Kolmogorovov-Smirnovov test dobrej zhody
ks.test(dataX, "pburr", shape1=fit.burr$estimate[1], shape2=fit.burr$estimate[2], rate=fit.burr$estimate[3])
# 
# D = 0.031652, p-value = 0.5741
# alternative hypothesis: two-sided

# Cramérov-von Misesov test dobrej zhody
require(goftest)
cvm.test(dataX, "pburr", shape1=fit.burr$estimate[1], shape2=fit.burr$estimate[2], rate=fit.burr$estimate[3])
# 
# data:  dataX
# omega2 = 0.056254, p-value = 0.8377

# Andersonov-Darlingov test dobrej zhody
require(goftest)
ad.test(dataX, "pburr", shape1=fit.burr$estimate[1], shape2=fit.burr$estimate[2], rate=fit.burr$estimate[3])
# 
# data:  dataX
# An = 0.43387, p-value = 0.8145

# Z grafov a výstupov testov dobrej zhody vidíme, že odhadnutý model založený na Burrovom rozdelení (s parametrami shape1 = 5,2206; shape2 = 1,6539; rate = 0,002265688) celkom dobre opisuje podkladové dáta. Pri každom teste dobrej zhody sme dostali vysokú p-hodnotu, čo znamená, že nulovú hypotézu s veľkou istotou nezamietame. To znamená, že medzi empirickým rozdelením a odhadnutým Burrovým rozdelením pravdepodobne nie sú štatisticky významné odchýlky.  
# ---------------------------------

# 4. 
# Overenie vhodnosti modelu (C)
# 
# Pri validácii modelu sme zistili, že Burrovo rozdelenie (s parametrami shape1 = 5,2206; shape2 = 1,6539; rate = 0,002265688) by mohlo byť celkom vhodné pre charakterizáciu výšok škodových udalostí súvisiacich so škodami na leteckých dopravných prostriedkoch. Tu by sme už teoreticky mohli skončiť a vyhlásiť Burrov model za finálny. Ešte raz sa ale vrátime k 1. kroku a vyskúšame si ešte jedno rozdelenie pravdepodobnosti.
# ---------------------------------


# DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
# 1.
# Voľba konkrétneho modelu (D)
# 
# V tomto kole si vyskúšame gama rozdelenie. Toto rozdelenie pravdepodobnosti je pravostranne zošikmené, má ľahký pravý chvost a vzniklo zovšeobecnením exponenciálneho rozdelenia. Je všeobecne známe, že gama rozdelenie je zvyčajne vhodné pre modelovanie malých škôd.
# ---------------------------------

# 2.
# Kalibrácia modelu (D)
# 
# (D) výška škody ~ Gama rozdelenie

fit.gamma <- fitdist(dataX, distr="gamma", method="mle", discrete=FALSE)

summary(fit.gamma)
#
#         estimate   Std. Error
# shape 2.00405513 0.1050289020
# rate  0.01250343 0.0007392339
# Loglikelihood:  -3636.069   AIC:  7276.139   BIC:  7284.966 
# ---------------------------------

# 3.
# Validácia modelu (D)
# 
# grafické posúdenie presnosti odhadnutého modelu
plot(fit.gamma)

# výsledky testov dobrej zhody
gofstat(fit.gamma)
# 
# Goodness-of-fit statistics
#                              1-mle-gamma
# Kolmogorov-Smirnov statistic  0.02929601
# Cramer-von Mises statistic    0.03853891
# Anderson-Darling statistic    0.27024053
# 
# Goodness-of-fit criteria
#                                1-mle-gamma
# Akaike's Information Criterion    7276.139
# Bayesian Information Criterion    7284.966
# -----

# Kolmogorovov-Smirnovov test dobrej zhody
ks.test(dataX, "pgamma", shape=fit.gamma$estimate[1], rate=fit.gamma$estimate[2])
#
# D = 0.029296, p-value = 0.6717
# alternative hypothesis: two-sided

# Cramérov-von Misesov test dobrej zhody
require(goftest)
cvm.test(dataX, "pgamma", shape=fit.gamma$estimate[1], rate=fit.gamma$estimate[2])
# 
# data:  dataX
# omega2 = 0.038539, p-value = 0.9407

# Andersonov-Darlingov test dobrej zhody
require(goftest)
ad.test(dataX, "pgamma", shape=fit.gamma$estimate[1], rate=fit.gamma$estimate[2])
# 
# data:  dataX
# An = 0.27024, p-value = 0.9587

# Z grafov a výstupov testov dobrej zhody vidíme, že odhadnutý model založený na gama rozdelení (s parametrami shape = 2,00405513; rate = 0,01250343) veľmi dobre opisuje podkladové dáta. Pri každom teste dobrej zhody sme dostali veľmi vysokú p-hodnotu, čo znamená, že nulovú hypotézu s veľkou istotou nezamietame. To znamená, že medzi empirickým rozdelením a odhadnutým gama rozdelením pravdepodobne nie sú štatisticky významné odchýlky.  
# ---------------------------------

# 4. 
# Overenie vhodnosti modelu (D)
# 
# Pri validácii modelu sme zistili, že gama rozdelenie (s parametrami shape = 2,00405513; rate = 0,01250343) je veľmi vhodné pre charakterizáciu výšok škodových udalostí súvisiacich so škodami na leteckých dopravných prostriedkoch. Za finálny model teda vyhlásime model založený na gama rozdelení (s vyššie uvedenými parametrami). 
# =================================


# Záver tvorby aktuárskeho modelu (pri Úlohe 6.1)
# 
# Počas tvorby aktuárskeho modelu sme zistili, že výška škody by mohla mať gama rozdelenie s parametrami shape = 2,00405513; rate = 0,01250343.

# Tiež sa ukázalo, že výšky škôd by mohli mať napríklad aj Burrovo rozdelenie. Je vhodnejšie ale zvoliť gama rozdelenie, pretože pri každom teste dobrej zhody gama-model vykazoval lepšiu zhodu ako Burrov model (viď nižšie). Zároveň pri gama rozdelení sme lepšiu zhodu dosiahli len pomocou dvoch parametrov, kým pri Burrovom rozdelení sa používala trojica parametrov. Podľa zásady jednoduchosti by sme teda mali používať ten model, ktorý má menší počet parametrov (ak majú porovnávané modely podobnú kvalitu v zmysle testov dobrej zhody).


# Hodnoty Kolmogorovovej-Smirnovovej D-štatistiky a p-hodnoty K-S testov:
# 
# KS.D(exp)  = 0,137830      (p-hodnota = 1,723e-10)
# KS.D(norm) = 0,100940      (p-hodnota = 7,998e-06)
# KS.D(Burr) = 0,031652      (p-hodnota = 0,5741)
# KS.D(gama) = 0,028336      (p-hodnota = 0,7115)
# ---------------------------------------------------

# Hodnoty Cramérovej-von Misesovej W2-štatistiky a p-hodnoty C-vM testov:
# 
# CvM.W2(exp)  = 4,185300    (p-hodnota = 1,657e-10)
# CvM.W2(norm) = 2,413300    (p-hodnota = 1,466e-06)
# CvM.W2(Burr) = 0,056254    (p-hodnota = 0,8377)
# CvM.W2(gama) = 0,038539    (p-hodnota = 0,9407)
# ---------------------------------------------------

# Hodnoty Andersonovej-Darlingovej A2-štatistiky a p-hodnoty A-D testov:
# 
# AD.A2(exp)  = 24,87100     (p-hodnota = 9,836e-07)
# AD.A2(norm) = 15,02800     (p-hodnota = 9,836e-07)
# AD.A2(Burr) =  0,43387     (p-hodnota = 0,8145)
# AD.A2(gama) =  0,27024     (p-hodnota = 0,9587)
# ---------------------------------------------------

# Hodnoty Akaikeho informačného kritéria (AIC) a umiestnenie modelu podľa AIC-kritéria:
# 
# AIC(exp)  = 7415,942       (3.)
# AIC(norm) = 7508,979       (4.)
# AIC(Burr) = 7282,788       (2.)
# AIC(gama) = 7276,139       (1.)
# ---------------------------------------------------

# Hodnoty Bayesovho informačného kritéria (BIC) a umiestnenie modelu podľa BIC-kritéria:
# 
# BIC(exp)  = 7420,356       (3.)
# BIC(norm) = 7508,979       (4.)
# BIC(Burr) = 7296,029       (2.)
# BIC(gama) = 7284,966       (1.)
# =================================================



# Úloha 6.2

# Modelovanie počtu poistných nárokov pri cestovnom poistení pomocou diskrétnych rozdelení pravdepodobnosti

# Riešenie tejto úlohy je na domáce cvičenie.
# =================================================



# Úloha 6.3

# Modelovanie výšky poistných nárokov pri havarijnom poistení motorových vozidiel

# Riešenie tejto úlohy je na domáce cvičenie.
# =================================================



# Úloha 6.4

# Modelovanie výšok škodových udalostí pri cestovnom poistení

# Riešenie tejto úlohy je na domáce cvičenie.
# =================================================


