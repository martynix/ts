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
dpop <- decompose(pop, type = "multiplicative")
plot(dpop)

#Dekompozycja multiplikatywna szeregu z sezonowością
dveg <- decompose(veg, type = "multiplicative")
plot(dveg)

#Dekompozycja za pomocą ruchomej średniej szeregu z trendem
ts1ma1 <- filter(pop, sides=2, filter=rep(1/3,3))
ts1ma2 <- filter(pop, sides=2, filter=rep(1/7,7))
ts1ma3 <- filter(pop, sides=2, filter=rep(1/11,11))
plot(pop, col="black", lty=2)
lines(ts1ma1, col="red", lty=2)
lines(ts1ma2, col="blue", lty=2)
lines(ts1ma3, col="green", lty=2)

#Dekompozycja za pomocą ruchomej średniej szeregu z sezonowością
ts1ma11 <- filter(veg, sides=2, filter=rep(1/3,3))
ts1ma22 <- filter(veg, sides=2, filter=rep(1/7,7))
ts1ma33 <- filter(veg, sides=2, filter=rep(1/11,11))
ts1ma44 <- filter(veg, sides=2, filter=rep(1/20,20))

plot(veg, col="black", lty=2)
lines(ts1ma11, col="red", lty=2)
lines(ts1ma22, col="blue", lty=2)
lines(ts1ma33, col="green", lty=2)
lines(ts1ma44, col="purple", lty=2)

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
