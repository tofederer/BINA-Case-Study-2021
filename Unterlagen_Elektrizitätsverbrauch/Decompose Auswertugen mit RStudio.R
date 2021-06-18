# Packages installieren, falls nicht vorhanden
if(!"tseries" %in% rownames(installed.packages())) install.packages("tseries")
if(!"forecast" %in% rownames(installed.packages())) install.packages("forecast")
if(!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2")
if(!"ggfortify" %in% rownames(installed.packages())) install.packages("ggfortify")

# Packages laden
library("tseries")
library("forecast")
library("ggplot2")
library("ggfortify")

#Set WORKING Directory
#setwd(choose.dir())
#getwd()

#-----------------------------------------------------------------------
# Absolute Werte auf 15min Basis
data15 <- read.csv(file = "SwissGrid Daten 15min Basis breinigt3.csv",header = TRUE,sep = ";")

#data15 <-data15[,-3]
# Daten prüfen
sum(is.na(data15))
str(data15)
head(data15)

# Spalte Zeitstempel mappen
data15$ï..Zeitstempel <- strptime(data15$ï..Zeitstempel,"%d.%m.%Y %H:%M")

# Zur Kontrolle
class(data15)
summary(data15)

# in Zeitreie umwandeln
data15 <- ts(data15$Summe.endverbrauchte.Energie, frequency = 35040)

# Zur Kontrolle
frequency(data15)
summary(data15)

data15B <- decompose(data15, type = c("additive"))
#data15B <- decompose(data15, type = c("multiplicative"))

# Visualsierung
plot(data15B)

#---------------------------------------------------------------------
# Absolute Werte auf Studen Basis
dataH <- read.csv(file = "SwissGrid Daten aggregiert auf Stunden.csv",header = TRUE,sep = ";")

# Daten prüfen
sum(is.na(dataH))
str(dataH)
head(dataH)

# Spalte Zeitstempel mappen
dataH$ï..Stunde.von.Zeitstempel <- strptime(dataH$ï..Stunde.von.Zeitstempel,"%d.%m.%Y %H:%M")

dataH$Summe.endverbrauchte.Energie = dataH$Summe.endverbrauchte.Energie/1000000.000000
# Zur Kontrolle
class(dataH)
summary(dataH)

# in Zeitreie umwandeln
dataH <- ts(dataH$Summe.endverbrauchte.Energie, frequency = 8760)
#dataH <- ts(dataH$Summe.produzierte.Energie, frequency = 8760)
# Zur Kontrolle
frequency(dataH)
summary(dataH)


dataHB <- decompose(dataH, type = c("additive"))
#dataHB <- decompose(dataH, type = c("multiplicative"))

# Visualsierung
plot(dataHB)

par(mfrow=c(3,1))
plot(dataHB$trend, type="l", xlab="Jahr", ylab="GWh", main= "Trend")				# the TREND
plot(dataHB$seasonal, type="l",xlab="Jahr", ylab="GWh", main= "Saisonality")		# the SAISONALITY
plot(dataHB$random, type="l",xlab="Jahr", ylab="GWh", main= "Noise")

plot(dataHB$seasonal[1:8760], type="l",xlab="Jahr", ylab="GWh", main= "Saisonality")		# the SAISONALITY

#-----------------------------------------------------------------------------
# Absolute Werte auf Tages Basis
dataT <- read.csv(file = "SwissGrid Daten aggregiert auf Tage ohne 2021.csv",header = TRUE,sep = ";")

#data15 <-data15[,-3]
# Daten prüfen
sum(is.na(dataT))
str(dataT)
head(dataT)

# Spalte Zeitstempel mappen
dataT$ï..Tag.von.Zeitstempel <- strptime(dataT$ï..Tag.von.Zeitstempel,"%d.%m.%Y")
dataT$Summe.endverbrauchte.Energie = dataT$Summe.endverbrauchte.Energie/1000000.000000
# Zur Kontrolle
class(dataT)
summary(dataT)

# in Zeitreie umwandeln
dataT <- ts(dataT$Summe.endverbrauchte.Energie, frequency = 365)

# Zur Kontrolle
frequency(dataT)
summary(dataT)


dataTB <- decompose(dataT, type = c("additive"))
#dataTB <- decompose(dataT, type = c("multiplicative"))

# Visualsierung
plot(dataTB)

# --- e.g. SEASONAL figures


par(mfrow=c(3,1))

plot(dataTB$trend, type="l", xlab="Jahr", ylab="GWh", main= "Trend")				# the TREND
plot(dataTB$seasonal, type="l",xlab="Jahr", ylab="GWh", main= "Saisonality")		# the SAISONALITY
plot(dataTB$random, type="l",xlab="Jahr", ylab="GWh", main= "Noise")

plot(dataTB$seasonal[1:365], type="l",xlab="Tage", ylab="GWh", main= "Saisonality")	
plot(dataTB$random[1:365], type="l",xlab="Jahr", ylab="GWh", main= "Noise")

#-----------------------------------------------------------------------------
# Differenz auf Tages Basis
dataTD <- read.csv(file = "Differenz in Tage.csv",header = TRUE,sep = ";")

#data15 <-data15[,-3]
# Daten prüfen
sum(is.na(dataTD))
str(dataTD)
head(dataTD)

# Spalte Zeitstempel mappen
dataTD$ï..Tag <- strptime(dataTD$ï..Tag,"%d.%m.%Y")
dataTD$Differenz = dataTD$Differenz/1000000.000000
# Zur Kontrolle
class(dataTD)
summary(dataTD)

# in Zeitreie umwandeln
dataTD <- ts(dataTD$Differenz, frequency = 365)

# Zur Kontrolle
frequency(dataTD)
summary(dataTD)


dataTDB <- decompose(dataTD, type = c("additive"))
#dataTB <- decompose(dataT, type = c("multiplicative"))

# Visualsierung
plot(dataTDB)

# --- e.g. SEASONAL figures

par(mfrow=c(3,1))

plot(dataTDB$trend, type="l", xlab="Jahr", ylab="GWh", main= "Trend")				# the TREND
plot(dataTDB$seasonal, type="l",xlab="Jahr", ylab="GWh", main= "Saisonality")		# the SAISONALITY
plot(dataTDB$random, type="l",xlab="Jahr", ylab="GWh", main= "Noise")

plot(dataTDB$seasonal[1:365], type="l",xlab="Tage", ylab="GWh", main= "Saisonality")	