# Doplnkový súbor k habilitačnej práci s názvom: 
# Pravdepodobnostné modelovanie v poisťovníctve
# Autor: Gábor Szűcs
# Pracovisko: KAMŠ FMFI UK v Bratislave
# 
# Verzia: 2025-09-25
# Kódovanie súboru: UTF-8
# 
# Kapitola 3 - Poistné princípy a ich vlastnosti
# =================================================


# Príklad o nevhodnosti používania poistného princípu variancie a poistného princípu smerodajnej odchýlky v prípade, keď výška poistných nárokov sa riadi podľa rozdelenia s ťažkým pravým chvostom

install.packages("VGAM")
library(VGAM)

set.seed(530)
x <- rlomax(n=10000, scale = 300, shape3.q=2.2)
mean(x)
sd(x)
max(x)
hist(x, breaks=250)

x.sorted <- sort(x, decreasing=TRUE)
x.sorted[1:10]
# =================================================



# Príklad o exponenciálnom poistnom princípe - ilustrácia averzie poistníka voči riziku pri rôznych voľbách koeficientu alfa

# P = H(S) = 1/alfa * ln E(e^{alfa*S})

# parameter alfa>0 je daný parameter exponenciálneho poistného princípu a nazýva sa koeficient odporu voči riziku (risk aversion coefficient)

# simulovanie náhodných výšok poistných nárokov z gama rozdelenia
# predpokladajme, že výšky poistných nárokov sú vyjadrené v eurách
set.seed(530)
x <- rgamma(n=10000, shape=320, scale=2)

hist(x)

# odhadnutá stredná hodnota
mean(x)            # = 639,62 eura

# teoretická stredná hodnota gama rozdelenia = shape*scale
320*2            # = 640 eur
# -----


# 1.
alfa <- 0.5
1/alfa * log(mean(exp(alfa*x)))                    # P = 781,6212 eura
# Poisťovňa má vysokú averziu voči riziku, čo znamená, že veľmi nerada akceptuje rizikové poistenia.
# Poisťovňa si pýta veľmi vysoké poistné, aby pravdepodobnosť toho, že utrpí stratu, znížila na veľmi nízku hladinu.
# 
# Teoreticky pri týchto nastaveniach platí, že pravdepodobnosť toho, že poisťovňa pri jednej poistnej zmluve utrpí stratu, je:
1 - pgamma(781.6212, shape=320, scale=2)           # = 0,000100607
# -----


# 2.
alfa <- 0.05
1/alfa * log(mean(exp(alfa*x)))                    # P = 673,6902 eura
# Poisťovňa v porovnaní s predchádzajúcim prípadom má nižší odpor voči riziku.
# Poisťovňa si pýta prijateľné poistné, aby možnosť poistiť sa bola lákavá pre klientov, a zároveň aby pravdepodobnosť toho, že utrpí stratu, bola na prijateľnej úrovni.
# 
# Teoreticky pri týchto nastaveniach platí, že pravdepodobnosť toho, že poisťovňa pri jednej poistnej zmluve utrpí stratu, je:
1 - pgamma(673.6902, shape=320, scale=2)           # = 0,1725361

# Ak od všetkých 10000 poistníkov vyberie poistné vo výške 673,69 eura, tak bude mať celkové príjmy vo výške
10000 * 673.69                                     # = 6 736 900  eur
# 
# Na druhej strane, ak výšky škôd budú presne v takej výške, ako sme ich simulovali (z gama rozdelenia), tak poisťovňa bude mať celkové výdavky vo výške
sum(x)                                             # = 6 396 208 eur
# 
# Z toho vidíme, že pri týchto konkrétnych nastaveniach (v tejto situácii) by poisťovňa neutrpela stratu, ale dosiahla by zisk.
# -----


# 3.
alfa <- 0.001
1/alfa * log(mean(exp(alfa*x)))                    # P = 640,2536 eura
# Poisťovňa v tomto prípade má veľmi nízku averziu voči riziku. To znamená, že rada ide do rizika, nízkym ročným poistným (aj na úkor nižšej ziskovosti) chce prilákať čo najviac klientov. Inými slovami môžeme povedať, že poisťovňa má vysoký rizikový apetít.
# 
# Teoreticky pri týchto nastaveniach platí, že pravdepodobnosť toho, že poisťovňa pri jednej poistnej zmluve utrpí stratu, je:
1 - pgamma(640.2536, shape=320, scale=2)           # = 0,4897395

# Ak by poisťovňa od všetkých 10000 poistníkov vybrala poistné vo výške 640,25 eura, tak by mala celkové príjmy vo výške
10000 * 640.25                                     # = 6 402 500 eur
# 
# Na druhej strane, ak výšky škôd budú presne v takej výške, ako sme ich simulovali (z gama rozdelenia), tak poisťovňa bude mať celkové výdavky vo výške
sum(x)                                             # = 6 396 208 eur
# 
# Z toho vidíme, že ani v tomto prípade by poisťovňa neutrpela stratu, ale dosiahla by mierny zisk.
# =================================================



# Úloha 3.1

# Dôkazy ohľadom vlastností poistného princípu očakávanej hodnoty - (B) Expected Value Premium Principle

# Riešenie tejto úlohy je prezentované v PDF-súbore habilitačnej práce.
# =================================================



# Úloha 3.2

# Dôkazy o vlastnostiach poistného princípu smerodajnej odchýlky - (D) Standard Deviation Premium Principle 
# 
# poistný princíp smerodajnej odchýlky nie je ani aditívny, ani superaditívny
# poistný princíp smerodajnej odchýlky ale spĺňa vlastnosť subaditivity

# Riešenie tejto úlohy je prezentované v PDF-súbore habilitačnej práce.
# =================================================



# Úloha 3.3

# Dôkaz o tom, že exponenciálny poistný princíp - (E) Exponential Premium Principle - je aditívny iba v prípade nezávislých rizík  

# Riešenie tejto úlohy je prezentované v PDF-súbore habilitačnej práce.
# =================================================



# Úloha 3.4

# Poistný princíp rovnakej užitočnosti s exponenciálnou úžitkovou funkciou 
# a jeho súvis s exponenciálnym poistným princípom

# Riešenie tejto úlohy je prezentované v PDF-súbore habilitačnej práce.
# =================================================



# Úloha 3.5

# Vybrané vlastnosti Essherovho poistného princípu

# Riešenie tejto úlohy je na domáce cvičenie. 
# =================================================



# Úloha 3.6

# Netto princíp a exponenciálny poistný princíp
# Riešenie tejto úlohy je uvedené aj v PDF-súbore habilitačnej práce.

# (a) 
# Aká by bola výška ročného poistného, keby poisťovňa používala princíp netto poistného?

# výška škody ~ Gama rozdelenie(shape=2, rate=1/80)
# S ~ Gama rozdelenie(shape=2, rate=1/80)

# Stredná hodnota gama rozdelenia = shape / rate
# 2 / (1/80) = 160 tisíc eur

# Odpoveď.
# Keby poisťovňa používala princíp netto poistného, tak výška ročného poistného by bola 160 000 eur.
# ------------------------------


# (b)
# Exponenciálny poistný princíp

# P = 1/alfa * ln E(exp(alfa*S))
alfa <- 0.007

# TEORETICKY (odvodené v PDF-súbore)
1/alfa*log((1/80)^2 / (alfa-1/80)^2)
# 234,5659

# Odpoveď.
# Výška ročného poistného je 234 566 eur.
# -----------------------------------

# simulačné štúdium (Monte Carlo simulácie) 
# pre výpočet ročného poistného z exponenciálneho poistného princípu
# 
M <- 1000000
S <- rgamma(M, shape=2, rate=1/80)
P <- 1/alfa*log(mean(exp(alfa*S)))
P            

# ilustratívny výsledok jednej konkrétnej simulačnej štúdie: 
# P = 234,548 tisíc eura
# =================================================



# Úloha 3.7

# Rôzne princípy výpočtu poistného, pričom výška škody sa riadi podľa Paretovho rozdelenia typu I

# Riešenie tejto úlohy je na domáce cvičenie. 
# =================================================


