#PROGETTO ARCHITETTURA DATI 2019/2020
#setwd("/Users/fabiobeltramelli/Desktop/2019_architetturedati")
#setwd("/Users/Davide Finati/Desktop/2019_architetturedati")

library(readr)

dataset <- read.csv("Dataset.csv", stringsAsFactors = T, sep = ';', header = TRUE)

#COMPLETEZZA (# DI NULL) PER OGNI COLONNA
totalNA = 0
for (i in c(1:ncol(dataset))){
  totalNA = totalNA + sum(dataset[i] == "")
  print(paste(colnames(dataset[i]), "| Number of NA: ", sum(dataset[i] == ""), "| % of NA: ", sum(dataset[i] == "")/nrow(dataset)*100))
}

#COMPLETEZZA (# DI NULL) TUTTO DATASET
print(paste("TABLE | Number of NA: ", totalNA, "| % of NA: ", round((totalNA/(nrow(dataset)*ncol(dataset))*100), 2)))


#GRAFICI COMPARAZIONE
apple <- dataset[dataset$Symbol == "aapl", ]

apple$OpenPrice = as.character(apple$OpenPrice)
hist(parse_number(apple$OpenPrice), main = "Distribuzione OpenPrice", xlab = "OpenPrice")
plot(c(1:nrow(apple)), parse_number(apple$OpenPrice), type = "b", xlab = "N", ylab = "OpenPrice", main = "Grafico OpenPrice")

apple$ClosePrice = as.character(apple$ClosePrice)
hist(parse_number(apple$ClosePrice), main = "Distribuzione ClosePrice", xlab = "ClosePrice")
plot(c(1:nrow(apple)), parse_number(apple$ClosePrice), type = "b", xlab = "N", ylab = "ClosePrice", main = "Grafico ClosePrice")

apple$ChangePerc = as.character(apple$ChangePerc)
hist(parse_number(apple$ChangePerc), main = "Distribuzione ChangePerc", xlab = "ChangePerc")
plot(c(1:nrow(apple)), parse_number(apple$ChangePerc), type = "b", xlab = "N", ylab = "ChangePerc", main = "Grafico ChangePerc")

hist(parse_number(apple$ChangeInDollars), main = "Distribuzione ChangeInDollars", xlab = "ChangeInDollars")
plot(c(1:nrow(apple)), parse_number(apple$ChangeInDollars), type = "b", xlab = "N", ylab = "ChangeInDollars", main = "Grafico ChangeInDollars")

hist(parse_number(apple$Volume), main = "Distribuzione Volume", xlab = "Volume")
plot(c(1:nrow(apple)), parse_number(apple$Volume), type = "b", xlab = "N", ylab = "Volume", main = "Grafico Volume")

hist(parse_number(apple$HighPrice), main = "Distribuzione HighPrice", xlab = "HighPrice")
plot(c(1:nrow(apple)), parse_number(apple$HighPrice), type = "b", xlab = "N", ylab = "HighPrice", main = "Grafico HighPrice")

hist(parse_number(apple$LowPrice), main = "Distribuzione LowPrice", xlab = "LowPrice")
plot(c(1:nrow(apple)), parse_number(apple$LowPrice), type = "b", xlab = "N", ylab = "LowPrice", main = "Grafico LowPrice")

hist(parse_number(apple$PreviousClose), main = "Distribuzione PreviousClose", xlab = "PreviousClose")
plot(c(1:nrow(apple)), parse_number(apple$PreviousClose), type = "b", xlab = "N", ylab = "PreviousClose", main = "Grafico PreviousClose")

hist(parse_number(apple$YearHigh), main = "Distribuzione YearHigh", xlab = "YearHigh")
plot(c(1:nrow(apple)), parse_number(apple$YearHigh), type = "b", xlab = "N", ylab = "YearHigh", main = "Grafico YearHigh")

hist(parse_number(apple$YearLow), main = "Distribuzione YearLow", xlab = "YearLow")
plot(c(1:nrow(apple)), parse_number(apple$YearLow), type = "b", xlab = "N", ylab = "YearLow", main = "Grafico YearLow")

hist(parse_number(apple$NShares), main = "Distribuzione NShares", xlab = "NShares")
plot(c(1:nrow(apple)), parse_number(apple$NShares), type = "b", xlab = "N", ylab = "NShares", main = "Grafico NShares")

hist(parse_number(apple$PE), main = "Distribuzione PE", xlab = "PE")
plot(c(1:nrow(apple)), parse_number(apple$PE), type = "b", xlab = "N", ylab = "PE", main = "Grafico PE")

hist(parse_number(apple$MarketCap), main = "Distribuzione MarketCap", xlab = "MarketCap")
plot(c(1:nrow(apple)), parse_number(apple$MarketCap), type = "b", xlab = "N", ylab = "MarketCap", main = "Grafico MarketCap")

hist(parse_number(apple$Yield), main = "Distribuzione Yield", xlab = "Yield")
plot(c(1:nrow(apple)), parse_number(apple$Yield), type = "b", xlab = "N", ylab = "Yield", main = "Grafico Yield")

hist(parse_number(apple$DividendYield), main = "Distribuzione DividendYield", xlab = "DividendYield")
plot(c(1:nrow(apple)), parse_number(apple$DividendYield), type = "b", xlab = "N", ylab = "DividendYield", main = "Grafico DividendYield")

hist(parse_number(apple$EPS), main = "Distribuzione EPS", xlab = "EPS")
plot(c(1:nrow(apple)), parse_number(apple$EPS), type = "b", xlab = "N", ylab = "EPS", main = "Grafico EPS")

groundtruth <- read.csv("GroundTruth.csv", sep = ';', header = TRUE)

groundtruth$MarketCap = as.character(groundtruth$MarketCap)
dataset$MarketCap = as.character(dataset$MarketCap)
#PRECISIONE COLONNA OPEN
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.null(parse_number(groundtruth[i,]$MarketCap))==F & is.null(parse_number(dataset[j,]$MarketCap))==F){
        if(parse_number(groundtruth[i,]$MarketCap) == parse_number(dataset[j,]$MarketCap)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}

print(paste("PRECISION MarketCap: ", round((totalSame/nrow(dataset))*100, 2)))

