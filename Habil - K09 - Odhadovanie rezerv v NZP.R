# Doplnkový súbor k habilitačnej práci s názvom: 
# Pravdepodobnostné modelovanie v poisťovníctve
# Autor: Gábor Szűcs
# Pracovisko: KAMŠ FMFI UK v Bratislave
# 
# Verzia: 2025-09-25
# Kódovanie súboru: UTF-8
# 
# Kapitola 9 - Odhadovanie výšky rezerv v neživotnom poistení
# =================================================


# Balík ChainLadder v softvéri R
# https://cran.r-project.org/web/packages/ChainLadder/ChainLadder.pdf
# https://cran.r-project.org/web/packages/ChainLadder/vignettes/ChainLadder.html
# https://github.com/mages/ChainLadder

install.packages("ChainLadder")
library(ChainLadder)

# dáta RAA (Reinsurance Association of America)
# dáta pochádzajú z obdobia 1981-1990
RAA

# konštrukcia modelu Chain-Ladder
CL <- chainladder(RAA)
CL

# odhad zložiek dolného trojuholníka
predict(CL)
# =================================================



# Úloha 9.1 

# Podrobné riešenie tejto úlohy je uvedené v excelovom súbore [Habil - K09 - Rezervy v NZP - Uloha 9-1.xlsx].
# -----

# Riešenie úlohy v prostredí softvéru R

# inštalácia a načítanie balíku "ChainLadder"
install.packages("ChainLadder")
library(ChainLadder)

# načítanie dát, vytvorenie nekumulatívneho vývojového trojuholníka
# (peňažné hodnoty sú uvedené v miliónoch eur)
# 
nekumul.trojuholnik <- matrix(c(
   9, 12, 10, 11,  8,  0,
  13, 15,  8, 10, 14, NA, 
  14, 15, 15, 16, NA, NA,
  16,  8, 18, NA, NA, NA,
  12, 14, NA, NA, NA, NA,
  11, NA, NA, NA, NA, NA), byrow=TRUE, ncol=6)

rownames(nekumul.trojuholnik) <- c("2010", "2011", "2012", "2013", "2014", "2015")
colnames(nekumul.trojuholnik) <- c("0", "1", "2", "3", "4", "5")

nekumul.trojuholnik 

# zostrojenie kumulatívneho vývojového trojuholníka
kumul.trojuholnik  <- incr2cum(nekumul.trojuholnik)
kumul.trojuholnik
# ---------------


# (A)
# Mackov Chain-Ladder Model
# 
# odhadnutie hodnôt dolného kumulatívneho vývojového trojuholníka
# odhadnutie celkovej výšky IBNR rezervy
M.A <- MackChainLadder(Triangle = kumul.trojuholnik)
plot(M.A)

# doplnený dolný kumulatívny vývojový trojuholník s odhadnutými hodnotami
M.A$FullTriangle

# konvertovanie kumulatívnej schémy na nekumulatívnu
cum2incr(M.A$FullTriangle)
# -----

# celkový výstup Mackovho Chain-Ladder modelu
M.A            # je možné použiť aj príkaz summary(M.A)

# VÝSTUP:
#      Latest Dev.To.Date Ultimate IBNR Mack.S.E CV(IBNR)
# 2010     50         1.0       50    0     0.00      NaN
# 2011     60         1.0       60    0     3.31      Inf
# 2012     60         0.8       75   15     6.65    0.444
# 2013     42         0.6       70   28     6.89    0.246
# 2014     26         0.4       65   39    11.29    0.290
# 2015     11         0.2       55   44    14.78    0.336
# 
#           Totals
# Latest:   249.00
# Dev:        0.66
# Ultimate: 375.00
# IBNR:     126.00
# Mack.S.E   27.13
# CV(IBNR):   0.22

# V prvej časti výstupu (okrem iného) sú zobrazené inverzné koeficienty reťazovo-rebríkovej metódy (v stĺpci Dev.To.Date) a celkové (kumulatívne) výšky poistných plnení podľa roku vzniku (v stĺpci Ultimate).

# V dolnej časti výstupu vidíme výšku doteraz vyplatených poistných plnení (Latest = 249 milión eur) a celkovú odhadnutú výšku poistných plnení potrebných na krytie poistných udalostí vzniknutých medzi rokmi 2010 a 2015 (Ultimate = 375 milión eur). Najdôležitejším údajom z výstupu je odhadnutá výška rezervy, ktorú by poisťovňa mala držať na konci roku 2015, aby bola schopná pokryť poistné plnenia za škody vzniknuté medzi rokmi 2010 a 2015 (IBNR = 126 miliónov eur). Táto suma sa zhoduje s výsledkom, ktorú sme vypočítali v excelovom súbore [Habil - K09 - Rezervy v NZP - Uloha 9-1.xlsx] na hárku 'Habil-K09-Uloha_9.1-A'.
# -------------------------------------------------


# (B)
# Mackov Chain-Ladder Model s infláciou 

# Balík "ChainLadder" ponúka možnosť upraviť vývojový trojuholník mierou inflácie, no vo funkcii 'inflateTriangle(Triangle, rate)' je možné aplikovať len jedinú mieru inflácie (spoločnú pre všetky sledované roky). 
# Spomeňme si na to, že v našej úlohe sme pre každý rok aplikovali inú mieru inflácie (pozri zadanie úlohy a riešenie v excelovom súbore [Habil - K09 - Rezervy v NZP - Uloha 9-1.xlsx] na hárku 'Habil-K09-Uloha_9.1-B'). Keby sme vo funkcii 'inflateTriangle(Triangle, rate)' použili akúsi priemernú mieru inflácie, zrejme by sme dostali iný výsledok ako v excelovom súbore. 

# Druhou možnosťou by bolo "ručne" upraviť náš vývojový trojuholník o infláciu (pomocou krátkeho programu v softvéri R). Tento postup by bol veľmi podobný, ako postup uvedený na excelovom hárku 'Habil-K09-Uloha_9.1-B'. Keby sme už mali k dispozícii vývojový trojuholník upravený o infláciu, stačilo by zavolať funkciu 'MackChainLadder' a dostali by sme veľmi podobné riešenie ako v excelovom súbore.
# -------------------------------------------------


# (C)
# Clarkova Cape Cod metóda

M.C <- ClarkCapeCod(Triangle = kumul.trojuholnik, Premium = rep(62.5, times=6), adol=FALSE, cumulative = TRUE, maxage=6)
M.C

# V balíku "ChainLadder" je implementovaná špeciálna verzia Cape Cod metódy, ktorá sa nazýva Clarkova Cape Cod metóda. Hlavný rozdiel medzi touto metódou a "našou" verziou Cape Cod metódy (prezentovanou na excelovom hárku 'Habil-K09-Uloha_9.1-C') spočíva v odhadovaní škodového pomeru. Kvôli týmto odlišnostiam Clarkova Cape Cod metóda poskytuje iný odhad pre celkovú výšku rezerv.
# -------------------------------------------------


# (D), (E)
# V balíku "ChainLadder" (zatiaľ) nie je implementovaná Bornhütterova-Fergusonova metóda resp. separačná metóda, s ktorými sme sa zaoberali na excelovom hárku 'Habil-K09-Uloha_9.1-D' resp. na hárku 'Habil-K09-Uloha_9.1-E'.

# Dopĺňame ale, že knižnica "ChainLadder" poskytuje niekoľko ďalších metód a postupov používaných na odhadovanie rezerv v trojuholníkových schémach, napr. známu Munich Chain-Ladder metódu:
library(ChainLadder)
help(MunichChainLadder)

# Ďalšie podrobnosti je možné nájsť v pomocníku (User Manual) knižnice:
# https://cran.r-project.org/web/packages/ChainLadder/ChainLadder.pdf
# https://cran.r-project.org/web/packages/ChainLadder/vignettes/ChainLadder.html
# https://github.com/mages/ChainLadder
# =================================================


