# Doplnkový súbor k habilitačnej práci s názvom: 
# Pravdepodobnostné modelovanie v poisťovníctve
# Autor: Gábor Szűcs
# Pracovisko: KAMŠ FMFI UK v Bratislave
# 
# Verzia: 2025-09-25
# Kódovanie súboru: UTF-8
# 
# Kapitola 4 - Model individuálneho rizika
# ==================================================


# Príklad 4.1

# Model degenerovanej náhodnej premennej v modeli individuálneho rizika,
# skonštruovať graf (nekumulatívneho) rozdelenia pravdepodobnosti, graf kumulatívnej distribučnej funkcie náhodnej premennej Z_i, 
# odvodiť prvý moment (strednú hodnotu), druhý počiatočný moment a druhý centrálny moment (disperziu) náhodnej premennej Z_i.

# Riešenie tohto príkladu je možné nájsť v PDF-súbore habilitačnej práce.
# =================================================



# Úloha 4.1

# Formulovanie príkladov na model degenerovanej náhodnej premennej v modeli individuálneho rizika.

# Riešenie tejto úlohy je na domáce cvičenie. 
# =================================================



# Úloha 4.2

# Mini-portfólio, 2 poistné zmluvy,
# životné poistenie pre prípad úmrtia, tri príčiny smrti (úraz, vážna choroba, iná príčina),
# úloha: vypočítať očakávanú hodnotu celkovej výšky poistných plnení.

# Možné príčiny smrti, ktoré môžu viesť ku vzniku poistnej udalosti:
# j = 1:  smrť následkom úrazu,
# j = 2:  smrť následkom vážnej choroby,
# j = 3:  úmrtie kvôli inej príčine.

# výšky poistných plnení pri Zmluve 1 za predpokladu, že nastala poistná udalosť pri Zmluve 1
y11 <- 50000
y12 <- 20000
y13 <- 10000

# rozdelenie pravdepodobnosti náhodnej premennej  Y1 | N1=1
pi11 <- 0.30
pi12 <- 0.05
pi13 <- 0.65
# ----------

# výšky poistných plnení pri Zmluve 2 za predpokladu, že nastala poistná udalosť pri Zmluve 2
y21 <- 18000
y22 <- 15000
y23 <- 12000

# rozdelenie pravdepodobnosti náhodnej premennej  Y2 | N2=1
pi21 <- 0.15
pi22 <- 0.25
pi23 <- 0.60
# ----------

# pravdepodobnosť vzniku poistnej udalosti pri Zmluve 1
p1 <- 0.01

# pravdepodobnosť vzniku poistnej udalosti pri Zmluve 2
p2 <- 0.02
# ----------


# Výpočet očakávanej výšky poistného plnenia pri Zmluve 1:
# E(Z1) = ?

# E(Z1) = E( E(Y1 | N1) )
#
# E(Z1) = E(Y1 | N1 = 0) * Pr(N1=0)  + E(Y1 | N1 = 1) * Pr(N1=1)
# 
# E(Z1) =  0 * (1-p1) + E(Y1 | N1 = 1) * p1

EZ1 <- 0 * (1-p1) + (y11*pi11 + y12*pi12 + y13*pi13) * p1
EZ1
			# E(Z1) = 225 eur
# ---------------------------


# Výpočet očakávanej výšky poistného plnenia pri Zmluve 2:
# E(Z2) = ?

# E(Z2) = E( E(Y2 | N2) )
#
# E(Z2) = E(Y2 | N2 = 0) * Pr(N2=0)  + E(Y2 | N2 = 1) * Pr(N2=1)
# 
# E(Z2) = 0 * (1-p2) + E(Y2 | N1 = 2) * p2

EZ2 <- 0 * (1-p2) + (y21*pi21 + y22*pi22 + y23*pi23) * p2
EZ2
			# E(Z2) = 273 eur
# ---------------------------


# Výpočet očakávanej hodnoty celkovej výšky poistných plnení:
# E( S^ind ) = ?

# E( S^ind ) = E( Z1 + Z2 ) = E(Z1) + E(Z2)

ESind <- EZ1 + EZ2
ESind	
			# E( S^{ind} ) = 498 eur
			
# Odpoveď.
# Očakávaná výška poistného plnenia pri Zmluve 1 je 225 eur. Očakávaná výška poistného plnenia pri Zmluve 1 je 273 eur. Očakávaná hodnota celkovej výšky poistných plnení je 498 eur.
# =================================================



# Úloha 4.3

# Portfólio zmlúv o poistenie majetku, 4 typy rizík, 5 homogénnejších skupín 
# súvisiaca tabuľka je prezentovaná v PDF-súbore habilitačnej práce,
# úloha: vypočítať očakávanú hodnotu celkovej výšky poistných plnení.

# Riešenie tejto úlohy je na domáce cvičenie. 
# =================================================



# Úloha 4.4

# Mini-portfólio, 2 poistné zmluvy,
# pokračovanie Úlohy 1.2,
# životné poistenie pre prípad úmrtia, tri príčiny smrti (úraz, vážna choroba, iná príčina),
# úloha: vypočítať disperziu celkovej výšky poistných plnení.

# Výpočet disperzie výšky poistného plnenia pri Zmluve 1:		
# D(Z1) = ?

# D(Z1) = E( D(Y1 | N1) ) + D( E(Y1 | N1) )
#
# D(Z1) = E( D(Y1 | N1) ) + E((E(Y1 | N1))^{2}) - ( E((E(Y1 | N1))) )^{2}
#

# D(Z1) = D(Y1 | N1 = 0) * Pr(N1=0)  + D(Y1 | N1 = 1) * Pr(N1=1) +
#       + (E(Y1 | N1 = 0))^2 * Pr(N1=0)  + (E(Y1 | N1 = 1))^2 * Pr(N1=1) -
#       - ( E(Y1 | N1 = 0) * Pr(N1=0)  + E(Y1 | N1 = 1) * Pr(N1=1) )^2

# Poznámka.
# Hodnotu posledného výrazu  E((E(Y1 | N1))) sme už vypočítali vyššie (pri Úlohe 1.2):
# E(Z1) = E( E(Y1 | N1) ) = 225 eur.

	# Vedľajší výpočet:
	# E(Y1 | N1=1) = y11*pi11 + y12*pi12 + y13*pi13
	EY1 <- y11*pi11 + y12*pi12 + y13*pi13
	EY1						# E(Y1 | N1=1) = 22 500 eur 

# Pokračovanie výpočtu disperzie:
# 
# D(Z1) = 0 * (1-p1)  + ( (y11-EY1)^2 *pi11 + (y12-EY1)^2 *pi12 + (y13-EY1)^2 *pi13 ) * p1 +
#       + 0 * (1-p1)  + (E(Y1 | N1=1))^2 * p1 -
#       - (E(Z1))^2

DZ1 <- ( (y11-EY1)^2 *pi11 + (y12-EY1)^2 *pi12 + (y13-EY1)^2 *pi13 ) * p1 +
       (EY1)^2 * p1  - 
	   (EZ1)^2

DZ1
	# D(Z1) = 8 299 375 eur^2	   
	   

	# +++
	# prípadná úprava predchádzajúceho vzťahu do iného tvaru:
	# 	   
	# ( (y11-EY1)^2 *pi11 + (y12-EY1)^2 *pi12 + (y13-EY1)^2 *pi13 ) * p1 + 
	#  + p1*(EY1)^2 - (p1 * (EY1))^2	   
	# 
	# ( (y11-EY1)^2 *pi11 + (y12-EY1)^2 *pi12 + (y13-EY1)^2 *pi13 ) * p1 + 
	#  + p1*(EY1)^2 - p1^2 * (EY1)^2	   
	# 
	# p1 * ( (y11-EY1)^2 *pi11 + (y12-EY1)^2 *pi12 + (y13-EY1)^2 *pi13 ) + 
	#  + (EY1)^2 * p1*(1-p1)  
	# 
	# D(Z1) = p1 ×  D(Y1 | N1 = 1)  +  (E(Y1 | N1=1))^2 × D(N1)
	# -----

	# výpočet disperzie výšky poistného plnenia pri Zmluve 1 pomocou vzťahu (4.6), 
	# ktorý je prezentovaný vo 4. kapitole habilitačnej práce
	# 
	# D(Z_i) = p_i × (sigma_i)^2  +  (nu_i)^2 × D(N_i)
	# 
	# D(Z_i) = p_i × (sigma_i)^2  +  (nu_i)^2 × p_i × (1-p_i)

	p1 * ( (y11-EY1)^2 *pi11 + (y12-EY1)^2 *pi12 + (y13-EY1)^2 *pi13 ) + (EY1)^2 * p1*(1-p1)
	# 
	# D(Z1) = 8 299 375 eur^2
	# -----
			

# simulačný výpočet strednej hodnoty a disperzie výšky poistného plnenia pri Zmluve 1:
# 
M <- 10000000			
N.sim <- sample(x=c(0,1), size=M, prob=c(1-p1, p1), replace=TRUE)
Y.sim <- sample(x=c(y11,y12,y13), size=M, prob=c(pi11, pi12, pi13), replace=TRUE)
Z.sim <- N.sim * Y.sim

# (výsledky jednej konkrétnej simulačnej štúdie)
mean(Z.sim)			# =       225,30 eura
var(Z.sim)			# = 8 313 000 eur^2
# ---------------------------


# Výpočet disperzie výšky poistného plnenia pri Zmluve 2:		
# D(Z2) = ?

# D(Z2) = E( D(Y2 | N2) ) + D( E(Y2 | N2) )
#
# D(Z2) = E( D(Y2 | N2) ) + E((E(Y2 | N2))^{2}) - ( E((E(Y2 | N2))) )^{2}
#

# D(Z2) = D(Y2 | N2 = 0) * Pr(N2=0)  + D(Y2 | N2 = 1) * Pr(N2=1) +
#       + (E(Y2 | N2 = 0))^2 * Pr(N2=0)  + (E(Y2 | N2 = 1))^2 * Pr(N2=1) -
#       - ( E(Y2 | N2 = 0) * Pr(N2=0)  + E(Y2 | N2 = 1) * Pr(N2=1) )^2

# Poznámka.
# Hodnotu posledného výrazu  E((E(Y2 | N2))) sme už vypočítali vyššie (pri Úlohe 1.2):
# E(Z2) = E( E(Y2 | N2) ) = 273 eur.

	# Vedľajší výpočet:
	# E(Y2 | N2=1) = y21*pi21 + y22*pi22 + y23*pi23
	EY2 <- y21*pi21 + y22*pi22 + y23*pi23
	EY2						# E(Y2 | N2=1) = 13 650 eur 

# Pokračovanie výpočtu disperzie:
# 
# D(Z2) = 0 * (1-p2)  + ( (y21-EY2)^2 *pi21 + (y22-EY2)^2 *pi22 + (y23-EY2)^2 *pi23 ) * p2 +
#       + 0 * (1-p2)  + (E(Y2 | N2=1))^2 * p2 -
#       - (E(Z2))^2

DZ2 <- ( (y21-EY2)^2 *pi21 + (y22-EY2)^2 *pi22 + (y23-EY2)^2 *pi23 ) * p2 +
       (EY2)^2 * p2  - 
	   (EZ2)^2

DZ2
	# D(Z2) = 3 750 471 eur^2	   
	   

	# +++
	# prípadná úprava predchádzajúceho vzťahu do iného tvaru:
	# 	   
	# ( (y21-EY2)^2 *pi21 + (y22-EY2)^2 *pi22 + (y23-EY2)^2 *pi23 ) * p2 + 
	#  + p2*(EY2)^2 - (p2 * (EY2))^2	   
	# 
	# ( (y21-EY2)^2 *pi21 + (y22-EY2)^2 *pi22 + (y23-EY2)^2 *pi23 ) * p2 + 
	#  + p2*(EY2)^2 - p2^2 * (EY2)^2	   
	# 
	# p2 * ( (y21-EY2)^2 *pi21 + (y22-EY2)^2 *pi22 + (y23-EY2)^2 *pi23 ) + 
	#  + (EY2)^2 * p2*(1-p2)  
	# 
	# D(Z2) = p2 ×  D(Y2 | N2 = 1)  +  (E(Y2 | N2=1))^2 × D(N2)
	# -----
	
	# výpočet disperzie výšky poistného plnenia pri Zmluve 1 pomocou vzťahu (4.6), 
	# ktorý je prezentovaný vo 4. kapitole habilitačnej práce
	# 
	# D(Z_i) = p_i × (sigma_i)^2  +  (nu_i)^2 × D(N_i)
	# 
	# D(Z_i) = p_i × (sigma_i)^2  +  (nu_i)^2 × p_i × (1-p_i)
	
	p2 * ( (y21-EY2)^2 *pi21 + (y22-EY2)^2 *pi22 + (y23-EY2)^2 *pi23 ) + (EY2)^2 * p2*(1-p2)
	# 
	# D(Z2) = 3 750 471 eur^2
	# -----
			

# simulačný výpočet strednej hodnoty a disperzie výšky poistného plnenia pri Zmluve 2:
# 
M <- 10000000			
N.sim <- sample(x=c(0,1), size=M, prob=c(1-p2, p2), replace=TRUE)
Y.sim <- sample(x=c(y21,y22,y23), size=M, prob=c(pi21, pi22, pi23), replace=TRUE)
Z.sim <- N.sim * Y.sim

# (výsledky jednej konkrétnej simulačnej štúdie)
mean(Z.sim)			# =       272,6103 eura
var(Z.sim)			# = 3 747 164 eur^2
# ---------------------------


# Výpočet disperzie celkovej výšky poistného plnenia v rámci sledovaného mini-portfólia:

# D(S^{ind}) = D(Z1 + Z2)
#
# Využívame, že náhodné premenné Z1 a Z2 sú nezávislé:
# 
# D(S^{ind}) = D(Z1) + D(Z2)

DSind <- DZ1 + DZ2
DSind
					# D(S^{ind}) = 12 049 846 eur^2

# Odpoveď.
# Disperzia výšky poistného plnenia pri Zmluve 1 je 8 299 375 eur^2. Disperzia výšky poistného plnenia pri Zmluve 2 je 3 750 471 eur^2. Disperzia celkovej výšky poistného plnenia v rámci sledovaného mini-portfólia je 12 049 846 eur^2.
# =================================================



# Úloha 4.5

# Portfólio zmlúv o poistenie majetku, 4 typy rizík, 5 homogénnejších skupín 
# súvisiaca tabuľka je uvedená v PDF-súbore habilitačnej práce,
# pokračovanie Úlohy 1.3,
# úloha: vypočítať disperziu celkovej výšky poistných plnení.

# Riešenie tejto úlohy je na domáce cvičenie.
# =================================================



# Úloha 4.6

# Životné poistenie pre prípad úmrtia, dve príčiny smrti (úraz, iná príčina),
# poistná udalosť nastala, 
# úloha: vypočítať pravdepodobnosť, že vyplatené poistné plnenie bude mať výšku 500 000 p. j.

# Riešenie tejto úlohy je na domáce cvičenie. 
# Riešenie tejto úlohy je možné nájsť v knihe Horáková, G., Páleš, M., Slaninka, F.: Teória rizika v poistení.
# =================================================



# Úloha 4.7

# Havarijné poistenie
# 
# úloha: vypočítať priemernú výšku škody z tohto rizika a smerodajnú odchýlku náhodnej premennej, ktorá opisuje túto škodu,
# vypočítať pravdepodobnosť, že škoda z tohto rizika nebude vyššia ako 800 p. j.

# Riešenie tejto úlohy je možné nájsť v knihe Horáková, G., Páleš, M., Slaninka, F.: Teória rizika v poistení.

# pomôcka k druhej časti bodu (a):
# 
# E(Y^2 | N=1)
E_Y2_N1 <- (0.0018*1000^3/3 - 0.0018*1000^4/4000) + 0.1 * 1000^2
E_Y2_N1
# 
# E(Y^2 | N=1) = 250 000

# E(Y | N=1) = nu_i
E_Y_N1 <- 400

# D(Y | N=1) = E(Y^2 | N=1) - E^2(Y | N=1)
#              250 000 - 400^2
D_Y_N1 <- E_Y2_N1 - (E_Y_N1)^2
D_Y_N1 
# 
# D(Y | N=1) = 90 000 = (sigma_i)^2

# výpočet disperzie podľa vzťahu (1.6): smerodajnej odchýlky:
# D(Z_i) =  p_i × (sigma_i)^2 + p_i ×(1 - p_i)× (nu_i)^{2}
# D(Z_i) =  0.1 * 90000 + 0.1*(1-0.1)*400^2 
# 
DZi <- 0.1 * D_Y_N1  + 0.1*(1-0.1)*(E_Y_N1)^2 		
DZi 
# D(Z_i) = 23 400

# výpočet smerodajnej odchýlky ako odmocniny z disperzie:
sigma_i <- sqrt(DZi)
sigma_i
# 152,9706 p. j.
# =================================================


