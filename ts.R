setwd('C:/Users/marty/Desktop/GITHUB/ts')

## Wczytanie danych
pop <- read.csv('pop.csv')
veg <- read.csv('veg.csv')

colnames(pop) <- c('data','liczba')
colnames(veg) <- c('data', 'cena')

str(pop)
str(veg)

