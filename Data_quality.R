#PROGETTO ARCHITETTURA DATI 2019/2020
#setwd("/Users/fabiobeltramelli/Desktop/2019_architetturedati")
#setwd("/Users/Davide Finati/Desktop/2019_architetturedati")

library(readr)

dataset <- read.csv("Dataset.csv", stringsAsFactors = F, sep = ';', header = TRUE)

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

groundtruth <- read.csv("GroundTruth.csv", sep = ';', stringsAsFactors = F, header = TRUE)

#PRECISIONE COLONNA OpenPrice
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(parse_number(groundtruth[i,]$OpenPrice))==F & is.na(parse_number(dataset[j,]$OpenPrice))==F){
        if(parse_number(groundtruth[i,]$OpenPrice) == parse_number(dataset[j,]$OpenPrice)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION OpenPrice: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA ClosePrice
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(parse_number(groundtruth[i,]$ClosePrice))==F & is.na(parse_number(dataset[j,]$ClosePrice))==F){
        if(parse_number(groundtruth[i,]$ClosePrice) == parse_number(dataset[j,]$ClosePrice)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION ClosePrice: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA ChangePerc
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.null(parse_number(groundtruth[i,]$ChangePerc))==F & is.null(parse_number(dataset[j,]$ChangePerc))==F){
        if(parse_number(groundtruth[i,]$ChangePerc) == parse_number(dataset[j,]$ChangePerc)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION ChangePerc: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA ChangeInDollars
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(groundtruth[i,]$ChangeInDollars)==F & is.na(dataset[j,]$ChangeInDollars)==F){
        if(groundtruth[i,]$ChangeInDollars == dataset[j,]$ChangeInDollars){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION ChangeInDollars: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA Volume
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.null(parse_number(groundtruth[i,]$Volume))==F & is.null(parse_number(dataset[j,]$Volume))==F){
        if(parse_number(groundtruth[i,]$Volume) == parse_number(dataset[j,]$Volume)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION Volume: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA HighPrice
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.null(parse_number(groundtruth[i,]$HighPrice))==F & is.null(parse_number(dataset[j,]$HighPrice))==F){
        if(parse_number(groundtruth[i,]$HighPrice) == parse_number(dataset[j,]$HighPrice)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION HighPrice: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA LowPrice
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.null(parse_number(groundtruth[i,]$LowPrice))==F & is.null(parse_number(dataset[j,]$LowPrice))==F){
        if(parse_number(groundtruth[i,]$LowPrice) == parse_number(dataset[j,]$LowPrice)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION LowPrice: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA PreviousClose
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(parse_number(groundtruth[i,]$PreviousClose))==F & is.na(parse_number(dataset[j,]$PreviousClose))==F){
        if(parse_number(groundtruth[i,]$PreviousClose) == parse_number(dataset[j,]$PreviousClose)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION PreviousClose: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA YearHigh
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.null(parse_number(groundtruth[i,]$YearHigh))==F & is.null(parse_number(dataset[j,]$YearHigh))==F){
        if(parse_number(groundtruth[i,]$YearHigh) == parse_number(dataset[j,]$YearHigh)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION YearHigh: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA YearLow
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.null(parse_number(groundtruth[i,]$YearLow))==F & is.null(parse_number(dataset[j,]$YearLow))==F){
        if(parse_number(groundtruth[i,]$YearLow) == parse_number(dataset[j,]$YearLow)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION YearLow: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA NShares
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(parse_number(groundtruth[i,]$NShares))==F & is.na(parse_number(dataset[j,]$NShares))==F){
        if(parse_number(groundtruth[i,]$NShares) == parse_number(dataset[j,]$NShares)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION NShares: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA PE
groundtruth$PE = as.character(groundtruth$PE)
dataset$PE = as.character(dataset$PE)
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(parse_number(groundtruth[i,]$PE))==F & is.na(parse_number(dataset[j,]$PE))==F){
        if(parse_number(groundtruth[i,]$PE) == parse_number(dataset[j,]$PE)){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION PE: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA MarketCap
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
print(paste("PRECISION MarketCap: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA Yield
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(groundtruth[i,]$Yield)==F & is.na(dataset[j,]$Yield)==F){
        if(groundtruth[i,]$Yield == dataset[j,]$Yield){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION Yield: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA DividendYield
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(groundtruth[i,]$DividendYield)==F & is.na(dataset[j,]$DividendYield)==F){
        if(groundtruth[i,]$DividendYield == dataset[j,]$DividendYield){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION DividendYield: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISIONE COLONNA EPS
totalSame = 0
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(dataset)){
    if(as.character(groundtruth[i,]$Symbol) == as.character(dataset[j,]$Symbol)){
      if(is.na(groundtruth[i,]$EPS)==F & is.na(dataset[j,]$EPS)==F){
        if(groundtruth[i,]$EPS == dataset[j,]$EPS){
          totalSame = totalSame + 1
        }
      }  
    }
  }
}
print(paste("PRECISION EPS: ", round((totalSame/nrow(dataset))*100, 2), "%"))


#REDUNDANCY OpenPrice,ClosePrice,HighPrice,LowPrice
totalSame = 0
for(i in c(1:nrow(dataset))){
  if(i < nrow(dataset) && as.character(dataset[i,]$Symbol) == as.character(dataset[i+1,]$Symbol)){
    if(parse_number(dataset[i,]$OpenPrice) == parse_number(dataset[i+1,]$OpenPrice) && 
       parse_number(dataset[i,]$ClosePrice) == parse_number(dataset[i+1,]$ClosePrice) &&
       parse_number(dataset[i,]$HighPrice) == parse_number(dataset[i+1,]$HighPrice) && 
       parse_number(dataset[i,]$LowPrice) == parse_number(dataset[i+1,]$LowPrice)){
      totalSame = totalSame + 1
    }
  }
}
print(paste("REDUNDANCY: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#REDUNDANCY OpenPrice,ClosePrice
totalSame = 0
for(i in c(1:nrow(dataset))){
  if(i < nrow(dataset) && as.character(dataset[i,]$Symbol) == as.character(dataset[i+1,]$Symbol)){
    if(parse_number(dataset[i,]$OpenPrice) == parse_number(dataset[i+1,]$OpenPrice) && 
       parse_number(dataset[i,]$ClosePrice) == parse_number(dataset[i+1,]$ClosePrice)){
      totalSame = totalSame + 1
    }
  }
}
print(paste("REDUNDANCY: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#REDUNDANCY HighPrice,LowPrice
totalSame = 0
for(i in c(1:nrow(dataset))){
  if(i < nrow(dataset) && as.character(dataset[i,]$Symbol) == as.character(dataset[i+1,]$Symbol)){
    if(parse_number(dataset[i,]$HighPrice) == parse_number(dataset[i+1,]$HighPrice) && 
       parse_number(dataset[i,]$LowPrice) == parse_number(dataset[i+1,]$LowPrice)){
      totalSame = totalSame + 1
    }
  }
}
print(paste("REDUNDANCY: ", round((totalSame/nrow(dataset))*100, 2), "%"))

#PRECISION FONTI
totalSame = 0
ds <- dataset[dataset$Source=="msn-money",]
for(i in 1:nrow(groundtruth)){
  for(j in 1:nrow(ds)){
    if(groundtruth[i,]$Symbol == ds[j,]$Symbol){
      if(parse_number(groundtruth[i,]$OpenPrice) == parse_number(ds[j,]$OpenPrice) && 
         parse_number(groundtruth[i,]$ClosePrice) == parse_number(ds[j,]$ClosePrice)&&
         parse_number(groundtruth[i,]$HighPrice) == parse_number(ds[j,]$HighPrice) && 
         parse_number(groundtruth[i,]$LowPrice) == parse_number(ds[j,]$LowPrice)){
        totalSame = totalSame + 1
      }
    }
  }
}
print(paste("PRECISION BLOOMBERG: ", round((totalSame/nrow(ds))*100, 2), "%"))

