#PROGETTO ARCHITETTURA DATI 2019/2020
setwd("/Users/fabiobeltramelli/Desktop/2019_architetturedati")
setwd("/Users/Davide Finati/Desktop/2019_architetturedati")

library(readr)

dataset <- read.csv("Dataset.csv", stringsAsFactors = F, sep = ';', header = TRUE)

myPrint <- function(metric, columnName, totalSame){
  print(paste(metric, " ", columnName, ": ", round((totalSame/nrow(dataset))*100, 2), "%"))
}

#############################################################################################################

#COMPLETEZZA (# DI NULL) PER OGNI COLONNA
totalNA = 0
for (i in c(1:ncol(dataset))){
  totalNA = totalNA + sum(dataset[i] == "")
  print(paste(colnames(dataset[i]), "| Number of NA: ", sum(dataset[i] == ""), "| % of NA: ", sum(dataset[i] == "")/nrow(dataset)*100))
}

#COMPLETEZZA (# DI NULL) TUTTO DATASET
print(paste("TABLE | Number of NA: ", totalNA, "| % of NA: ", round((totalNA/(nrow(dataset)*ncol(dataset))*100), 2)))

#############################################################################################################

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

#############################################################################################################

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
myPrint("Precision", "OpenPrice", totalSame)

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
myPrint("Precision", "ClosePrice", totalSame)

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
myPrint("Precision", "ChangePerc", totalSame)

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
myPrint("Precision", "ChangeInDollars", totalSame)

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
myPrint("Precision", "Volume", totalSame)

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
myPrint("Precision", "HighPrice", totalSame)

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
myPrint("Precision", "LowPrice", totalSame)

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
myPrint("Precision", "PreviousClose", totalSame)

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
myPrint("Precision", "YearHigh", totalSame)

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
myPrint("Precision", "YearLow", totalSame)

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
myPrint("Precision", "NShares", totalSame)

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
myPrint("Precision", "PE", totalSame)

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
myPrint("Precision", "MarketCap", totalSame)

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
myPrint("Precision", "Yield", totalSame)

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
myPrint("Precision", "DividendYield", totalSame)

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
myPrint("Precision", "EPS", totalSame)

#############################################################################################################

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
myPrint("Redundancy", "OpenPrice,ClosePrice,HighPrice,LowPrice", totalSame)

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
myPrint("Redundancy", "OpenPrice,ClosePrice", totalSame)

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
myPrint("Redundancy", "HighPrice,LowPrice", totalSame)

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
myPrint("Precision Bloomberg", "OpenPrice,ClosePrice,HighPrice,LowPrice", totalSame)

#############################################################################################################

#CONSISTENZA (HighPrice > LowPrice)
totalInconsistent = 0
totalWarning = 0
for(i in 1:nrow(dataset)){
  
  isInconsistent = F
  isWarning = F
  
  #Consistency OpenPrice > 0
  if(parse_number(dataset[i,]$OpenPrice) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR OpenPrice < 0"))
    isInconsistent = T
  }
  
  #Consistency ClosePrice > 0
  if(parse_number(dataset[i,]$ClosePrice) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR ClosePrice < 0"))
    isInconsistent = T
  }
  
  #Consistency Volume > 0
  if(parse_number(dataset[i,]$Volume) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR Volume < 0"))
    isInconsistent = T
  }
  
  #Consistency HighPrice > 0
  if(parse_number(dataset[i,]$HighPrice) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR HighPrice < 0"))
    isInconsistent = T
  }
  
  #Consistency LowPrice > 0
  if(parse_number(dataset[i,]$LowPrice) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR LowPrice < 0"))
    isInconsistent = T
  }
  
  #Consistency PreviousClose > 0
  if(dataset[i,]$PreviousClose != "" & parse_number(dataset[i,]$PreviousClose) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR PreviousClose < 0"))
    isInconsistent = T
  }
  
  #Consistency YearHigh > 0
  if(parse_number(dataset[i,]$YearHigh) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR YearHigh < 0"))
    isInconsistent = T
  }
  
  #Consistency YearLow > 0
  if(parse_number(dataset[i,]$YearLow) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR YearLow < 0"))
    isInconsistent = T
  }
  
  #Consistency NShares > 0
  if(dataset[i,]$NShares != "" & parse_number(dataset[i,]$NShares) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR NShares < 0"))
    isInconsistent = T
  }
  
  #DOMANDA ???? 
  #Consistency PE > 0
  if(dataset[i,]$PE != "" & parse_number(dataset[i,]$PE) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR PE < 0"))
    isInconsistent = T
  }
  
  #Consistency MarketCap > 0
  if(dataset[i,]$MarketCap != "" & parse_number(dataset[i,]$MarketCap) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR MarketCap < 0"))
    isInconsistent = T
  }
  
  #DOMANDA ????
  #Consistency EPS > 0
  if(dataset[i,]$EPS != "" && parse_number(dataset[i,]$EPS) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR EPS < 0"))
    isInconsistent = T
  }
  
  #Consistency HighPrice > LowPrice
  if(parse_number(dataset[i,]$HighPrice) < parse_number(dataset[i,]$LowPrice)){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR HighPrice < LowPrice"))
    isInconsistent = T
  }
  
  #Consistency YearHigh > YearLow
  if(parse_number(dataset[i,]$YearHigh) < parse_number(dataset[i,]$YearLow)){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR YearHigh < YearLow"))
    isInconsistent = T
  }
  
  #Consistency PreviousClose = OpenPrice
  if(dataset[i,]$PreviousClose != "" && parse_number(dataset[i,]$OpenPrice) != ""){
    if(parse_number(dataset[i,]$PreviousClose) != parse_number(dataset[i,]$OpenPrice)){
      print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol,"ERROR PreviousClose != OpenPrice"))
      isInconsistent = F
      isWarning = T
    }
  }
  
  #Consistency MarketCap = NShares * HighPrice
  if(dataset[i,]$MarketCap != "" & dataset[i,]$NShares != ""){
    if(parse_number(dataset[i,]$MarketCap) != (parse_number(dataset[i,]$NShares) * parse_number(dataset[i,]$ClosePrice))){
      print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol,"ERROR MarketCap != NShares * ClosePrice"))
      isInconsistent = T
    }
  }
  
  if(isInconsistent)
    totalInconsistent = totalInconsistent + 1
  
  if(isWarning)
    totalWarning = totalWarning + 1
    
}
myPrint("Inconstency", "Row", totalInconsistent)
myPrint("Inconstency", "Row", totalWarning)

