############################################################
####   STEP 1. Reading and recoding the data            ####
####                                                    ####
####   Aim: (1) Reading all the files                   ####
####        (2) Tidy the data                           ####
############################################################

#### A. - CMABST
#### 1.1 Reading

# Using list.files to read all the data
library(foreign)
file.list <- list.files(path = "./Data/CMABST", full.names=TRUE)

# Constructing a dataframe
BST.all.data <- data.frame()

for (current.files in file.list){
  
  # Read THE data
  tempData <- read.table(current.files, header=FALSE,  fill = T, sep="")
  # Combine the data in one dataframe
  BST.all.data <- rbind(BST.all.data, tempData)
  
}




## 1.2 Cleaning the data
##
## i Replacing
##
## a. Name 

a = 0
b = 1

BST.all.data[,8]=replace(BST.all.data[,8],BST.all.data[,8]=="",NA)

while (a < 68514){
  a = a + 1
  
  if (is.na(BST.all.data[a,8]) == TRUE){
    
    BST.all.data[a,8] <- BST.all.data[b,8]
    
  }
  else { b = a }
}

## b. Serial Number

BST.all.data <- within(BST.all.data, SerialNum<- BST.all.data[,4])
wanted.data <- BST.all.data[,1] == 66666
summary(BST.all.data$V4[wanted.data])


a = 0
b = 1

BST.all.data$SerialNum=replace(BST.all.data$SerialNum,BST.all.data$SerialNum>54,NA)

while (a < 68514){
  a = a + 1
  
  if (is.na(BST.all.data[a,10]) == TRUE){
    
    BST.all.data[a,10] <- BST.all.data[b,10]
    
  }
  else { b = a }
}



## Save the data.frame into file
## save(BST.all.data, file="ALL_CMABST.Rda")
write.table(BST.all.data, "BST_All_Data.dat", row.names = FALSE)

## 1.2.2  Clean the headers

Head.line <- 1
i <- 0
BST.clean <- BST.all.data

while ( Head.line <= 68514 ){
  
  BST.clean <- BST.clean[- (Head.line - i),]
  Head.line <- Head.line + BST.all.data[Head.line, 3] + 1
  i <- i+1
  
}

write.table(BST.clean, "BST_no_header.dat", row.names = FALSE)


## 1.2.3  Create the new data.frame

Year <- substr(BST.clean[,1], start = 1, stop = 4)
Month <- substr(BST.clean[,1], start = 5, stop = 6)
Date <- substr(BST.clean[,1], start = 7, stop = 8)
Hour <- substr(BST.clean[,1], start = 9, stop = 10)
Name <- as.character(BST.clean[,8])
Intensity <- BST.clean[,2]
Latitude <- BST.clean[,3]
Longitude <- BST.clean[,4]
Pres <- BST.clean[,5]
WND <- BST.clean[,6]
OWD <- BST.clean[,7]
SerialNum <- BST.clean$SerialNum
CMABST <- data.frame(Year, Month, Date, Hour,Name, SerialNum, Intensity, Latitude, Longitude, Pres, WND, OWD)

#### Format cleaning
CMABST$Latitude <- CMABST$Latitude/10
CMABST$Longitude <- CMABST$Longitude/10

summary(CMABST$Latitude)
summary(CMABST$Longitude)

CMABST$SerialNum = paste0(Year, "-",SerialNum,sep = "")

#### B - Landfall

Landfall <- read.csv("./Data/Landfall.csv")
names(Landfall) <- c("Year", "Serial Number", "Chinese ID", "Name", 
                     "Total Times of Landfall", "Order of landfall", "Landfall Province", 
                     "Landfall Intensity Category")
a = 0
b = 1
while (a <= 814){
  a = a + 1
  
  if (is.na(Landfall[a,1]) == TRUE){
    
    Landfall[a,1] <- Landfall[b,1]
    
  }
  else { b = a }
}

Landfall<- Landfall[-815,]

## Rename the level to clean the strange symbols
levels(Landfall$`Landfall Province`)[11] <- "Shanghai:Zhejiang"
levels(Landfall$`Landfall Province`)[15] <- "Zhejiang:Fujian"

