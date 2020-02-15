#PROGETTO ARCHITETTURA DATI 2019/2020
setwd("/Users/fabiobeltramelli/Desktop/2019_architetturedati")
setwd("/Users/Davide Finati/Desktop/2019_architetturedati")

library(readr)

dataset <- read.csv("Dataset.csv", stringsAsFactors = F, sep = ';', header = TRUE)
datasetCleaned <- read.csv("DatasetPostCleaning.csv", stringsAsFactors = F, sep = ',', header = TRUE)

groundtruth <- read.csv("GroundTruth.csv", sep = ';', stringsAsFactors = F, header = TRUE)

myPrint <- function(metric, columnName, totalSame){
  print(paste(metric, " ", columnName, ": ", round((totalSame/nrow(dataset))*100, 2), "%"))
}

myMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#############################################################################################################

#COMPLETEZZA (# DI NULL) PER OGNI COLONNA (DATASET PRECLEANING)
totalNA = 0
for (i in c(1:ncol(dataset))){
  totalNA = totalNA + sum(dataset[i] == "")
  print(paste(colnames(dataset[i]), "| Number of NA: ", sum(dataset[i] == ""), "| % of NA: ", sum(dataset[i] == "")/nrow(dataset)*100))
}
#COMPLETEZZA (# DI NULL) TUTTO DATASET
print(paste("TABLE | Number of NA: ", totalNA, "| % of NA: ", round((totalNA/(nrow(dataset)*ncol(dataset))*100), 2)))


#COMPLETEZZA (# DI NULL) PER OGNI COLONNA (DATASET POSTCLEANING)
totalNA = 0
for (i in c(1:ncol(datasetCleaned))){
  totalNA = totalNA + sum(is.na(datasetCleaned[i]))
  print(paste(colnames(datasetCleaned[i]), "| Number of NA: ", sum(is.na(datasetCleaned[i])), "| % of NA: ", sum(is.na(datasetCleaned[i]))/nrow(datasetCleaned)*100))
}
#COMPLETEZZA (# DI NULL) TUTTO DATASET
print(paste("TABLE | Number of NA: ", totalNA, "| % of NA: ", round((totalNA/(nrow(datasetCleaned)*ncol(datasetCleaned))*100), 2)))


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
ds <- dataset[dataset$Source=="yahoo-finance",]
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

#CONSISTENZA
openPriceInconsistent = 0
closePriceInconsistent = 0
highPriceInconsistent = 0
lowPriceInconsistent = 0
yearHighInconsistent = 0
yearLowInconsistent = 0
volumeInconsistent = 0
previousCloseInconsistent = 0
nSharesInconsistent = 0
peIncosistent = 0
marketCapInconsistent = 0
yieldInconsistent = 0
dividendYieldInconsistent = 0
highPriceLowPriceIncosistent = 0
highPriceOpenPriceIncosistent = 0
highPriceClosePriceIncosistent = 0
lowPriceOpenPriceIncosistent = 0
lowPriceClosePriceIncosistent = 0
yearHighYearLowIncosistent = 0
previousCloseYearHighIncosistent = 0
previousCloseYearLowIncosistent = 0
marketCapNSharesClosePriceIncosistent = 0
previousCloseOpenPriceIncosistent = 0

for(i in 1:nrow(dataset)){
  
  #Consistency OpenPrice > 0
  if(parse_number(dataset[i,]$OpenPrice) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR OpenPrice < 0"))
    openPriceInconsistent = openPriceInconsistent + 1
  }
  
  #Consistency ClosePrice > 0
  if(parse_number(dataset[i,]$ClosePrice) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR ClosePrice < 0"))
    closePriceInconsistent = closePriceInconsistent + 1
  }
  
  #Consistency Volume > 0
  if(parse_number(dataset[i,]$Volume) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR Volume < 0"))
    volumeInconsistent = volumeInconsistent + 1
  }
  
  #Consistency HighPrice > 0
  if(parse_number(dataset[i,]$HighPrice) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR HighPrice < 0"))
    highPriceInconsistent = highPriceInconsistent + 1
  }
  
  #Consistency LowPrice > 0
  if(parse_number(dataset[i,]$LowPrice) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR LowPrice < 0"))
    lowPriceInconsistent = lowPriceInconsistent + 1
  }
  
  #Consistency PreviousClose > 0
  if(dataset[i,]$PreviousClose != "" & parse_number(dataset[i,]$PreviousClose) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR PreviousClose < 0"))
    previousCloseInconsistent = previousCloseInconsistent + 1
  }
  
  #Consistency YearHigh > 0
  if(parse_number(dataset[i,]$YearHigh) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR YearHigh < 0"))
    yearHighInconsistent = yearHighInconsistent + 1
  }
  
  #Consistency YearLow > 0
  if(parse_number(dataset[i,]$YearLow) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR YearLow < 0"))
    yearLowInconsistent = yearLowInconsistent + 1
  }
  
  #Consistency NShares > 0
  if(dataset[i,]$NShares != "" & parse_number(dataset[i,]$NShares) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR NShares < 0"))
    nSharesInconsistent = nSharesInconsistent + 1
  }
  
  #WARNING
  #Consistency PE > 0
  if(dataset[i,]$PE != "" & parse_number(dataset[i,]$PE) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR PE < 0"))
    peIncosistent = peIncosistent + 1
  }
  
  #Consistency MarketCap > 0
  if(dataset[i,]$MarketCap != "" & parse_number(dataset[i,]$MarketCap) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR MarketCap < 0"))
    marketCapInconsistent = marketCapInconsistent + 1
  }
  
  #Consistency Yield > 0
  if(!is.na(dataset[i,]$Yield) && dataset[i,]$Yield != "" & parse_number(dataset[i,]$Yield) < 0){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR Yield < 0"))
    yieldInconsistent = yieldInconsistent + 1
  }
  
  #Consistency HighPrice > LowPrice
  if(parse_number(dataset[i,]$HighPrice) < parse_number(dataset[i,]$LowPrice)){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR HighPrice < LowPrice"))
    highPriceLowPriceIncosistent = highPriceLowPriceIncosistent + 1
  }
  
  #Consistency HighPrice >= OpenPrice
  if(parse_number(dataset[i,]$HighPrice) < parse_number(dataset[i,]$OpenPrice)){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR HighPrice < OpenPrice"))
    highPriceOpenPriceIncosistent = highPriceOpenPriceIncosistent + 1
  }
  
  #Consistency HighPrice >= ClosePrice
  if(parse_number(dataset[i,]$HighPrice) < parse_number(dataset[i,]$ClosePrice)){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR HighPrice < ClosePrice"))
    highPriceClosePriceIncosistent = highPriceClosePriceIncosistent + 1
  }
  
  #Consistency LowPrice <= OpenPrice
  if(parse_number(dataset[i,]$LowPrice) > parse_number(dataset[i,]$OpenPrice)){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR LowPrice < OpenPrice"))
    lowPriceOpenPriceIncosistent = lowPriceOpenPriceIncosistent + 1
  }
  
  #Consistency LowPrice <= ClosePrice
  if(parse_number(dataset[i,]$LowPrice) > parse_number(dataset[i,]$ClosePrice)){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR LowPrice < ClosePrice"))
    lowPriceClosePriceIncosistent = lowPriceClosePriceIncosistent + 1
  }
  
  #Consistency YearHigh > YearLow
  if(parse_number(dataset[i,]$YearHigh) < parse_number(dataset[i,]$YearLow)){
    print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol, "ERROR YearHigh < YearLow"))
    yearHighYearLowIncosistent = yearHighYearLowIncosistent + 1
  }
  
  #WARNING
  #Consistency PreviousClose = OpenPrice
  if(dataset[i,]$PreviousClose != "" && parse_number(dataset[i,]$OpenPrice) != ""){
    if(parse_number(dataset[i,]$PreviousClose) != parse_number(dataset[i,]$OpenPrice)){
      print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol,"WARNING PreviousClose != OpenPrice"))
      previousCloseOpenPriceIncosistent = previousCloseOpenPriceIncosistent + 1
    }
  }
  
  #Consistency PreviousClose <= YearHigh
  if(dataset[i,]$PreviousClose != ""){
    if(parse_number(dataset[i,]$PreviousClose) > parse_number(dataset[i,]$YearHigh)){
      print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol,"ERRORE PreviousClose > YearHigh"))
      previousCloseYearHighIncosistent = previousCloseYearHighIncosistent + 1
    }
  }
  
  #Consistency PreviousClose >= YearLow
  if(dataset[i,]$PreviousClose != ""){
    if(parse_number(dataset[i,]$PreviousClose) < parse_number(dataset[i,]$YearLow)){
      print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol,"ERRORE PreviousClose < YearLow"))
      previousCloseYearLowIncosistent = previousCloseYearLowIncosistent + 1
    }
  }
  
  #Consistency MarketCap = NShares * HighPrice
  if(dataset[i,]$MarketCap != "" & dataset[i,]$NShares != ""){
    if(parse_number(dataset[i,]$MarketCap) != (parse_number(dataset[i,]$NShares) * parse_number(dataset[i,]$ClosePrice))){
      print(paste(i, dataset[i,]$Source, dataset[i,]$Symbol,"ERROR MarketCap != NShares * ClosePrice"))
      marketCapNSharesClosePriceIncosistent = marketCapNSharesClosePriceIncosistent + 1
    }
  }
}

myPrint("Inconstency", "OpenPrice < 0", openPriceInconsistent)
myPrint("Inconstency", "ClosePrice < 0", closePriceInconsistent)
myPrint("Inconstency", "HighPrice < 0", highPriceInconsistent)
myPrint("Inconstency", "LowPrice < 0", lowPriceInconsistent)
myPrint("Inconstency", "YearHigh < 0", yearHighInconsistent)
myPrint("Inconstency", "YearLow < 0", yearLowInconsistent)
myPrint("Inconstency", "Volume < 0", volumeInconsistent)
myPrint("Inconstency", "PreviousClose < 0", previousCloseInconsistent)
myPrint("Inconstency", "NShares < 0", nSharesInconsistent)
myPrint("Inconstency", "MarketCap < 0", marketCapInconsistent)
myPrint("Inconstency", "Yield < 0", yieldInconsistent)

myPrint("Inconstency", "HighPrice < LowPrice", highPriceLowPriceIncosistent)
myPrint("Inconstency", "HighPrice < OpenPrice", highPriceOpenPriceIncosistent)
myPrint("Inconstency", "HighPrice < ClosePrice", highPriceClosePriceIncosistent)

myPrint("Inconstency", "LowPrice > OpenPrice", lowPriceOpenPriceIncosistent)
myPrint("Inconstency", "LowPrice > ClosePrice", lowPriceClosePriceIncosistent)

myPrint("Inconstency", "YearHigh < YearLow", yearHighYearLowIncosistent)
myPrint("Inconstency", "PreviousClose > YearHigh", previousCloseYearHighIncosistent)
myPrint("Inconstency", "PreviousClose < YearLow", previousCloseYearLowIncosistent)

myPrint("Inconstency", "MarketCap != NShares * ClosePrice", marketCapNSharesClosePriceIncosistent)

myPrint("Warning", "PE < 0", peIncosistent)
myPrint("Warning", "PreviousClose != OpenClose", previousCloseOpenPriceIncosistent)

####################################################################################################

#DATA CLEANING NULL

dataset <- read.csv("Dataset.csv", stringsAsFactors = F, sep = ';', header = TRUE)

for(i in 1:nrow(dataset)){

  if(dataset[i,]$PreviousClose == ""){
    other <- dataset[dataset$Symbol == dataset[i,]$Symbol, ]$PreviousClose
    other <- parse_number(other[other != ""])
    dataset[i,]$PreviousClose <- myMode(other)
  }
  
  if(dataset[i,]$NShares == ""){
    other <- dataset[dataset$Symbol == dataset[i,]$Symbol, ]$NShares
    other <- parse_number(other[other != ""])
    dataset[i,]$NShares <- myMode(other)
  }
  
  if(dataset[i,]$Yield == ""){
    other <- dataset[dataset$Symbol == dataset[i,]$Symbol, ]$Yield
    other <- parse_number(other[other != ""])
    dataset[i,]$Yield <- myMode(other)
  }
  
  if(dataset[i,]$DividendYield == ""){
    other <- dataset[dataset$Symbol == dataset[i,]$Symbol, ]$DividendYield
    other <- parse_number(other[other != ""])
    dataset[i,]$DividendYield <- myMode(other)
  }
  
  dataset[i,]$OpenPrice = parse_number(dataset[i,]$OpenPrice)
  dataset[i,]$ClosePrice = parse_number(dataset[i,]$ClosePrice)
  
  dataset[i,]$HighPrice = parse_number(dataset[i,]$HighPrice)
  dataset[i,]$LowPrice = parse_number(dataset[i,]$LowPrice)
  
  if(abs(parse_number(dataset[i,]$ChangeInDollars)) > 10)
    dataset[i,]$ChangeInDollars = parse_number(dataset[i,]$ChangeInDollars) / 1000
  else
    dataset[i,]$ChangeInDollars = parse_number(dataset[i,]$ChangeInDollars)
  
  dataset[i,]$ChangePerc = parse_number(dataset[i,]$ChangePerc)
  
  dataset[i,]$YearHigh = parse_number(dataset[i,]$YearHigh)
  dataset[i,]$YearLow = parse_number(dataset[i,]$YearLow)
  
  #Volume => mil,m = 1000000
  if(grepl("mil",dataset[i,]$Volume, fixed = TRUE) || grepl("m",dataset[i,]$Volume, fixed = TRUE))
    dataset[i,]$Volume = parse_number(dataset[i,]$Volume) * 1000000
  else
    dataset[i,]$Volume = parse_number(dataset[i,]$Volume)
  
  #NShares => mil,m = 1000000
  if(grepl("mil",dataset[i,]$NShares, fixed = TRUE) || grepl("m",dataset[i,]$NShares, fixed = TRUE))
    dataset[i,]$NShares = parse_number(dataset[i,]$NShares) * 1000000
  #NShares => bil,b = 1000000000
  else if(grepl("bil",dataset[i,]$NShares, fixed = TRUE) || grepl("b",dataset[i,]$NShares, fixed = TRUE))
    dataset[i,]$NShares = parse_number(dataset[i,]$NShares) * 1000000000
  else
    dataset[i,]$NShares = parse_number(dataset[i,]$NShares)
  
  dataset[i,]$PreviousClose = parse_number(dataset[i,]$PreviousClose)
  
  #MarketCap => mil,m = 1000000
  if(grepl("mil",dataset[i,]$MarketCap, fixed = TRUE) || grepl("m",dataset[i,]$MarketCap, fixed = TRUE))
    dataset[i,]$MarketCap = parse_number(dataset[i,]$MarketCap) * 1000000
  #MarketCap => bil,b = 1000000000
  else if(grepl("bil",dataset[i,]$MarketCap, fixed = TRUE) || grepl("b",dataset[i,]$MarketCap, fixed = TRUE))
    dataset[i,]$MarketCap = parse_number(dataset[i,]$MarketCap) * 1000000000
  else
    dataset[i,]$MarketCap = parse_number(dataset[i,]$MarketCap) 
  
  if(!is.na(dataset[i,]$Yield))
    dataset[i,]$Yield = parse_number(dataset[i,]$Yield)
  if(!is.na(dataset[i,]$DividendYield))
    dataset[i,]$DividendYield = parse_number(dataset[i,]$DividendYield)
  
  dataset[i,]$PE = parse_number(dataset[i,]$PE)
  
  dataset[i,]$EPS = parse_number(dataset[i,]$EPS)
}

write.csv(dataset, "DatasetPostCleaning.csv")

