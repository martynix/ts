setwd('C:/Users/marty/Desktop/GITHUB/ts')

#Wprowadzenie
#Analizowane szeregi czasowe dotyczą wielkości populacji ludzi w wieku powyżej 55 roku życia (włącznie) oraz cen warzyw w Polsce. 
#Analiza pierwszego zagadnienia ma na celu obserwację zmian zachodzących w społeczeństwie na całym świecie. Przewidywanie takich procesów jak starzenie się (lub odmładzanie) społeczeństwa wpływa np. na decyzje polityczne w sprawach polityki prorodzinnej czy prodemograficznej.

#Analiza cen warzyw w Polsce pozwoli zobrazować, jak na przestrzeni roku zmieniają się ceny warzyw. W zależności od miesiąca, wahania cen są mocno zauważalne; w trakcie zbiorów są najtańsze, a pożniej drożeją, co można powiązać np. z kosztami magazynowania i przechowywania w odpowiednich warunkach.
#Indeks harmonizowany cen towarów i usług konsumpcyjnych (Harmonized Index of Consumer Prices, HICP) jest miarą inflacji lub zmiany ogólnego poziomu cen. Wskaźnik ten jest stosowany w Unii Europejskiej do porównywania cen konsumpcyjnych między krajami członkowskimi.
#indeks 2015=100 oznacza, że wszystkie wartości indeksu są wyrażane jako odchylenia od poziomu cen z 2015 roku. Na przykład, jeśli indeks wynosi 105, oznacza to, że ceny wzrosły o 5% w porównaniu do roku referencyjnego.
#Oba zbiory danych pochodzą ze strony https://fred.stlouisfed.org/. (fred.stlouisfed.org - baza danych Rezerwy Federalnej) 
#Ten dotyczący populacji obejmuje okres od stycznia 1948 do maja 2023, a drugi, dotyczący cen warzyw, od stycznia 1996 do czerwca 2023.


## Wczytanie danych
pop <- read.csv('pop.csv')
veg <- read.csv('veg.csv')

colnames(pop) <- c('data','liczba')
colnames(veg) <- c('data', 'cena')

pop$data <- as.Date(pop$data)
pop$liczba <- as.numeric(pop$liczba)

veg$data <- as.Date(veg$data)
veg$cena <- as.numeric(veg$cena)

pop <- ts(pop$liczba, start=1948, frequency = 12)
pop

veg <- ts(veg$cena, start=1996, frequency = 12)
veg

is.ts(pop)
is.ts(veg)

## Omówienie głównych cech analizowanych szeregów na podstawie poznanych typów wykresów

## Wykresy punktowe
plot(pop)
plot(veg)


## Wykresy sezonowe
monthplot(pop)
monthplot(veg)

#Powyższa funkcja pochodzi z pakietu stats i przedstawia podzbiory danych dla kazdego z okresów - dla miesięcznytch szeregów, dla każdego miesiąca rysowane są odrębne szeregi czasowe zawierające wartości zaobserwowane w kolejnych latach. Ponadto wyznacznana jest średnia wartość zaznaczona poziomą linią.

library(forecast)

seasonplot(pop, 
           col = rainbow(75), 
           year.labels = TRUE, 
           pch = 19)

seasonplot(veg, 
           col = rainbow(27), 
           year.labels = TRUE, 
           pch = 19)

#Funkcja seasonplot z pakietu forecast pozwala na zaprezentowanie danych w rozbiciu na kolejne okresy jednostkowe. Forma taka pomaga analizować i zauważać zarówno sezonowość okresów jak i odstępstwom od wzorca sezonowości w poszczególnych okresach (np. jednostkowe, nietypowe zachowanie w konkretnym roku).

## Wykresy pudełkowe
boxplot(pop ~ cycle(pop))
boxplot(veg ~ cycle(veg))

#Wykres typu boxplot dostarcza nam syntetycznych informacji na temat zmienności szeregów dla poszczególnych okresów. Na wykresie mamy mediany, kwartyle(brzegi pudełek), wartości minimalne i maksymalne(wąsy) oraz wartości odstające (kropki)

## Wykresy rozrzutu dla wartości opóźnionych
lag.plot(pop, lags=12, do.lines=FALSE)
lag.plot(veg, lags=12, do.lines=FALSE)

#Wykres rozrzutu jest podstawowym narzędziem graficznym wykorzystywanym do badania zależności dwóch zmiennych w klasycznej analizie danych. Na wykresach rozrzutu dla wartości opóźnionych przedstawiamy zależność wartości szeregu od wartości przesuniętych o opóźnienie lag - rysujemy pary (X(t), X(t-lag)) dla kolejnych chwil t. Jeżeli punkty na wykresie przedstawiają jednorodną chmurę punktów, bez jakiejkolwiek struktury, świadczy to o braku istotnej zależności czasowej pomiędzy wartościami szeregu przesuniętymi o konkretne opóźnienie.

## Wykresy autokorelacji ACF
acf(pop)
acf(veg)

## Wykresy cząstkowej korelacji PACF
pacf(pop)
pacf(veg)

## Dekompozycja

#Dekompozycja addytywna szeregu z trendem
dpop <- decompose(pop, type = "additive")
plot(dpop)

#Dekompozycja addytywna szeregu z sezonowością
dveg <- decompose(veg, type = "additive")
plot(dveg)

#Dekompozycja multiplikatywna szeregu z trendem
dpop1 <- decompose(pop, type = "multiplicative")
plot(dpop1)

#Dekompozycja multiplikatywna szeregu z sezonowością
dveg1 <- decompose(veg, type = "multiplicative")
plot(dveg1)

#Dekompozycja za pomocą ruchomej średniej szeregu z trendem
ts1ma1 <- filter(pop, sides=2, filter=rep(1/3,3))
ts1ma2 <- filter(pop, sides=2, filter=rep(1/7,7))
ts1ma3 <- filter(pop, sides=2, filter=rep(1/11,11))
plot(pop, col="black", lty=2)
lines(ts1ma1, col="red", lty=2)
lines(ts1ma2, col="blue", lty=2)
lines(ts1ma3, col="green", lty=2)

#Dekompozycja za pomocą ruchomej średniej szeregu z sezonowością
ts1ma11 <- filter(veg, sides=2, filter=rep(1/11,11))
ts1ma22 <- filter(veg, sides=2, filter=rep(1/20,20))
ts1ma33 <- filter(veg, sides=2, filter=rep(1/25,25))

plot(veg, col="black", lty=2)
lines(ts1ma11, col="red", lty=2)
lines(ts1ma22, col="blue", lty=2)
lines(ts1ma33, col="green", lty=2)

#Dekompozycja wielomianowa szeregu z trendem
pop_poly1 <- tslm(pop~trend)
pop_poly2 <- tslm(pop ~ trend + season)

plot(pop)
lines(fitted(pop_poly1), col = "blue", lty = 1)
lines(fitted(pop_poly2), col = "red", lty = 2)

#Dekompozycja wielomianowa szeregu z sezonowością
veg_poly1 <- tslm(veg~trend)
veg_poly2 <- tslm(veg ~ trend + season)

plot(veg)
lines(fitted(veg_poly1), col = "blue", lty = 1)
lines(fitted(veg_poly2), col = "red", lty = 2)

## Eliminacja trendu i sezonowości
#Na szeregach powstałych po dekompozycji addytywnej
dpop.trend <- dpop$trend
dpop.sezonowosc <- dpop$seasonal
dpop.indeksy <- dpop$figure
dpop.reszty <- dpop$random
barplot(dpop.indeksy, names.arg = month.abb, main="Indeksy sezonowe")
tsdisplay(dpop.reszty, main="reszty losowe")


dveg.trend <- dveg$trend
dveg.sezonowosc <- dveg$seasonal
dveg.indeksy <- dveg$figure
dveg.reszty <- dveg$random
barplot(dveg.indeksy, names.arg = month.abb, main="Indeksy sezonowe")
tsdisplay(dveg.reszty, main="reszty losowe")

#Odsezonowanie szeregu veg
dveg1 <- decompose(veg, type = "multiplicative")
dveg2 <- seasadj(dveg1)
plot(veg)
lines(dveg2, col="orange", lty=1)


## Uczynienie szeregów stacjonarnymi
#Różnicowanie na szeregach pierwotnych z pomocą funkcji diff

#Przed
tsdisplay(pop)
#Po
popdiff <- diff(pop)
tsdisplay(popdiff)
popdiff1 <- diff(popdiff, lag=12)  
tsdisplay(popdiff1)

#Przed
tsdisplay(veg)
#Po
vegdiff <- diff(veg)
tsdisplay(vegdiff)
vegdiff1 <- diff(vegdiff, lag=12)
tsdisplay(vegdiff1)

#test Kwiatkowskiego-Phillipsa-Schmidta (KPSS) w celu sprawdzenia stacjonarności szeregu czasowego
#Hipoteza zerowa (H0): Szereg czasowy jest stacjonarny.
#Hipoteza alternatywna (HA): Szereg czasowy nie jest stacjonarny.

library(tseries)
kpss.test(popdiff1)

#P-wartość dla testu KPSS wynosi 0.1, co jest większe od poziomu istotności 0.05. To oznacza, że nie ma wystarczających dowodów, aby odrzucić hipotezę zerową (H0) o stacjonarności poziomu szeregu czasowego.

kpss.test(vegdiff1)
#P-wartość dla testu KPSS wynosi 0.1, co jest większe od poziomu istotności 0.05. To oznacza, że nie ma wystarczających dowodów, aby odrzucić hipotezę zerową (H0) o stacjonarności poziomu szeregu czasowego.

#Na podstawie powyższych wyników można wnioskować, że nie ma istotnych dowodóhttp://127.0.0.1:10779/graphics/plot_zoom_png?width=2548&height=1335w na niestacjonarność poziomu szeregów czasowych popdiff1 i vegdiff1.

#Symulacja szumu białego
SB2 <- rnorm(n=50)
SB2 <- as.ts(SB2)
plot(SB2, main='Szum biaĹ‚y (n=50)')
Acf(SB2, lag.max=48)
Pacf(SB2, lag.max=48)
tsdisplay(SB2)

tsdisplay(popdiff1)

# szereg nie jest realizacją szumu białego ponieważ wartości korelacji wystają poza przedziały ufności

tsdisplay(SB2)

tsdisplay(vegdiff1)
# szereg nie jest realizacją szumu białego ponieważ wartości korelacji są duże i wystają poza przedziały ufności
#Teoretyczna funkcja ACF wynosi 0 dla h>0 (1 dla h=0)
#Reguła identyfikacyjna WN: Szereg możemy uznać za realizację białego szumu jeżeli:
  #i) co najmniej 95% autokorelacji próbkowych (ACF(h), h=1,2,.., hmax) znajduje się w przedziale ufności 
  #ii) nie ma autokorelacji „istotnie ” wychodzących poza przedział ufności 

#Dla szeregu popdiff1 warto brać pod uwagę modele AR(p) rzędu 36, a także 24 i 12, oraz modele MA(q) rzędu 36, 24, 12
#Dla szeregu vegdiff1 warto brać pod uwagę modele AR(p) rzędu 36, a także 24 i 12, oraz modele MA(q) rzędu 35, 23, 12


## Metody estymacji
#Dopasowanie modelu AR dla szeregu popdiff1
# 1.metoda Yule-Walkera 
popdiff1.yw <- ar(popdiff1, aic=FALSE, order.max=36, method=c("yule-walker"))
popdiff1.yw

# 2.metoda największej wiarygodności (MLE-Maksimum Likelihood Estimation)
popdiff1.mle <- ar(popdiff1, aic=FALSE, order.max=36, method=c("mle"))
popdiff1.mle

# 3.Automatyczny dobór: (aic=TRUE)
popdiff1.aic <- ar(popdiff1, aic=TRUE)
popdiff1.aic


#Dopasowanie modelu AR dla szeregu vegdiff1
# 1.metoda Yule-Walkera 
vegdiff1.yw <- ar(vegdiff1, aic=FALSE, order.max=36, method=c("yule-walker"))
vegdiff1.yw

# 2.metoda największej wiarygodności (MLE-Maksimum Likelihood Estimation)
vegdiff1.mle <- ar(vegdiff1, aic=FALSE, order.max=36, method=c("mle"))
vegdiff1.mle

# 3.Automatyczny dobór: (aic=TRUE)
vegdiff1.aic <- ar(vegdiff1, aic=TRUE)
vegdiff1.aic

#Automatyczny dobór jest zbliżony do metody Yule-Walkera. Automatyczny dobór w obu przypadkach dobrał modele rzędu niższego niż ten dobrany przed dwie pozostałe metody.

## Wyznaczenie współczynników dla modelu MA(q)
popdiff1_arima36 <- arima(popdiff1, order=c(0,0,36))
summary(popdiff1_arima36)

popdiff1_arima24 <- arima(popdiff1, order=c(0,0,24))
summary(popdiff1_arima24)

popdiff1_arima12 <- arima(popdiff1, order=c(0,0,12))
summary(popdiff1_arima12)

vegdiff1_arima35 <- arima(vegdiff1, order=c(0,0,35))
summary(vegdiff1_arima35)

vegdiff1_arima23 <- arima(vegdiff1, order=c(0,0,23))
summaru(vegdiff1_arima23)

vegdiff1_arima12 <- arima(vegdiff1, order=c(0,0,12))
summary(vegdiff1_arima12)


## Wyznaczenie optymalnych modeli z wykorzystaniem funkcji auto.arima()
#Dla szeregu popdiff1
popdiff1_auto_arimaAIC <- auto.arima(popdiff1,ic="aic")
popdiff1_auto_arimaAIC

popdiff1_auto_arimaAICC <- auto.arima(popdiff1,ic="aicc")
popdiff1_auto_arimaAICC

popdiff1_auto_arimaBIC <- auto.arima(popdiff1,ic="bic")
popdiff1_auto_arimaBIC
#Najlepszym modelem jest ARIMA(0,0,1)(0,0,1) wyznaczona z identycznymi współczynnikami przez dwa pierwsza kryteria dobroci dopasowania - aic i aicc.

#Dla szeregu vegdiff1
vegdiff1_auto_arimaAIC <- auto.arima(vegdiff1,ic="aic")
vegdiff1_auto_arimaAIC

vegdiff1_auto_arimaAICC <- auto.arima(vegdiff1,ic="aicc")
vegdiff1_auto_arimaAICC

vegdiff1_auto_arimaBIC <- auto.arima(vegdiff1,ic="bic")
vegdiff1_auto_arimaBIC
#Najlepszym modelem jest ARIMA(2,0,2)(1,0,1) wyznaczona z identycznymi współczynnikami przez dwa pierwsza kryteria dobroci dopasowania - aic i aicc.

## Prognozowanie z wykorzystaniem metod naiwnych
pop.meanf <- meanf(pop, h = 30)
plot(pop.meanf, main="Prognoza na podstawie średniej (szereg z trendem)")

popdiff1.meanf <- meanf(popdiff1, h=30)
plot(popdiff1.meanf, main="Prognoza na podstawie średniej (szereg stacjonarny)")

pop.naive <- naive(pop, h=24)
plot(pop.naive, main="Metoda naiwna")

pop.snaive <- snaive(pop, h=24)
plot(pop.snaive, main="Metoda naiwna sezonowa")


veg.meanf <- meanf(veg, h = 24)
plot(veg.meanf, main="Prognoza na podstawie średniej (szereg z trendem i sezonowością)")

vegdiff1.meanf <- meanf(vegdiff1, h=24)
plot(vegdiff1.meanf, main="Prognoza na podstawie średniej (szereg stacjonarny)")

veg.naive <- naive(veg, h=24)
plot(veg.naive, main="Metoda naiwna")

veg.snaive <- snaive(veg, h=24)
plot(veg.snaive, main="Metoda naiwna sezonowa")


## Prognozowanie z wykorzystaniem innych metod 
hw.pop <- hw(pop)
plot(hw.pop, main="Prognozowanie za pomocą metody Holt-Wintersa")

hw.veg <- hw(veg)
plot(hw.veg, main="Prognozowanie za pomocą metody Holt-Wintersa")

arima.pop <- forecast(pop, h=100)
plot(arima.pop, main="Prognozowanie za pomocą modelu ARIMA")

arima.veg <- forecast(veg, h=100)
plot(arima.veg, main="Prognozowanie za pomocą modelu ARIMA")



