# Scorecard

# Model code

library(haven)
 madison_sample_for_client <- read_sas("G:/Team Drives/Client work/118 118 Money/01. 118 01/10. Credit Policy/Bureau data/RN0024_Madison_Extract/madison_sample_for_client.sas7bdat")
 dat<-madison_sample_for_client
 data_2_6 <- read.csv("~/data_2_6.csv")   ## Tidied CSV with factors classified (0, U, 1, ...  ->  0, 0.5, 1, ....)
 data_2_6_untouched <- read.csv("~/data_2_6_untouched.csv") #Untidy
 rm(madison_sample_for_client)



x<-data.frame(data_2_6_untouched[1:100000,]) # practice set
#x<-data.frame(data_2_6[1:10000,]) # practice set

# -999997 = Not Derivable -99999 = out of bounds
x[x==-999997] <- NA
x[x==-999999] <- NA
x[x==9999999] <- NA

# 
# #Change A and U to 9 and 10
# x[,678:715][x[,678:715]=="A"] <- 9 
# x[,678:715][x[,678:715]=="U"] <- 0
# 
# x$OD<-NULL  #date format - other variables describve same thing better 
# 
# x["testV"] <- NA   #add a column for test varibale
# x["testV6"] <- NA   #add a column for test varibale
# 
# 
# # Find any status greater than 2 - testV=1 if account has gone 2+ over the 2 years
# for (i in 1:length(x[,1])){ #through rows
#   sum=0
#   sum1=0
#   for (j in 678:715) { #through cols
#       if (x[i,j]  > 2 && !is.na(x[i,j])) {
#           sum = sum + 1 } else {sum = sum}
#       
#       if (x[i,j]  > 5 && !is.na(x[i,j])) {
#         sum1 = sum1 + 1 } else {sum1 = sum1}
# 
# 
#       
#       if (sum > 0) {
#         x$testV[i] <- 1
#       } else { x$testV[i] <- 0}
#       
#       if (sum1 > 0) {
#         x$testV6[i] <- 1
#       } else { x$testV6[i] <- 0}
#   }
#   print(i)
# }

# remove performance data, start date, type of account and id number
x$X.1<-NULL
x[, 563:(length(x[1,])-2)]<-NULL
x[, 1:3] <-NULL


x[x=="ND"] <- NA
x[x=="<NA>"] <- NA

# 
# #Columns with only one unique value are removed
# isUnique<-function(x){
#   length(unique(x))!=1   
# }
# x<-x[,sapply(x,isUnique)]
# 

# numerics stored as characters are converted
x$PDB<-as.factor(x$PDB)     # over 25 unique but with "XXX >> stored as factor
x$BTB<-as.factor(x$BTB)
x$HCB<-as.factor(x$HCB)

ConvToNum <- function(x) {
  if (class(x) == "character"  && length(unique(x)) > 25 && !is.na(length(unique(x)))) {
    x[x=="NA"]<-NA
    x[x=="ND"]<-NA
    x<-as.numeric(x)
  } else {x<-x}
}
x<-suppressWarnings(lapply(x,ConvToNum))
x<-data.frame(x)

temp<-lapply(x,function(y){if(class(y) == "character" ) { unique(y)}} )


# Chnage status variables so U comes between 0 and 1 amnd D comes after 6 - stored as numeric
StatusToNumeric11 <-function(x) {
  if (class(x) == "factor"  && length(unique(x)) == 11 && !is.na(length(unique(x)))) {
    x<-as.character(x)
    x[x=="U"] <- "0.5"
    x[x=="D"] <- "7"
    x[x=="?"] <- NA
    x[x=="ND"] <- NA
    x<-as.numeric(x)
  }else {x<-x}
}
x<-suppressWarnings(lapply(x,StatusToNumeric11))
x<-data.frame(x)



x$URB<-as.factor(x$URB) # length=10 unique but with "X" >> stored as factor

StatusToNumeric10 <-function(x) {
  if (class(x) == "factor"  && length(unique(x)) == 10 && !is.na(length(unique(x)))) {
    x<-as.character(x)
    x[x=="U"] <- "0.5"
    x[x=="D"] <- "7"
    x[x=="?"] <- NA
    x[x=="ND"] <- NA
    x<-as.numeric(x)
  }else {x<-x}
}
x<-suppressWarnings(lapply(x,StatusToNumeric10))
x<-data.frame(x)





x$FWB<-as.character(x$FWB)
x$BF<-as.character(x$BF)
x$LDB<-as.character(x$LDB)
x$VRB<-as.character(x$VRB)
x$WRB<-as.character(x$WRB)
StatusToNumeric9 <- function(x) {
  if(class(x) == "factor"  && length(unique(x)) == 9 && !is.na(length(unique(x)))){
    x<-as.character(x)
    x[x=="U"] <- "0.5"
    x[x=="D"] <- "7"
    x[x=="?"] <- NA
    x[x=="ND"] <- NA
    x<-as.numeric(x)
  } else (x<-x)
}
x$FWB<-as.factor(x$FWB)
x$BF<-as.factor(x$BF)
x$LDB<-as.factor(x$LDB)
x$VRB<-as.factor(x$VRB)
x$WRB<-as.factor(x$WRB)

x<-suppressWarnings(lapply(x,StatusToNumeric9))
x<-data.frame(x)



x$FWB<-as.character(x$FWB)
x$BF<-as.character(x$BF)

StatusToNumeric8 <- function(x) {
  if(class(x) == "factor"  && length(unique(x)) == 8 && !is.na(length(unique(x)))){
    x<-as.character(x)
    x[x=="U"] <- "0.5"
    x[x=="D"] <- "7"
    x[x=="?"] <- NA
    x[x=="ND"] <- NA
    x<-as.numeric(x)
  } else (x<-x)
}
x$FWB<-as.factor(x$FWB)
x$BF<-as.factor(x$BF)

x<-suppressWarnings(lapply(x,StatusToNumeric8))
x<-data.frame(x)


# Remaining variables to be treated
x$WAC<-as.character(x$WAC)
x$WAC[x$WAC=="U"] <- "0.5"
x$WAC[x$WAC=="D"] <- "7"
x$WAC[x$WAC=="ND"] <- NA
x$WAC<-as.numeric(x$WAC)

x$GBC<-as.character(x$GBC)
x$GBC[x$GBC=="U"] <- "0.5"
x$GBC[x$GBC=="D"] <- "7"
x$GBC[x$GBC=="ND"] <- NA
x$GBC<-as.numeric(x$GBC)

x$SYB<-as.character(x$SYB)
x$SYB[x$SYB=="U"] <- "0.5"
x$SYB[x$SYB=="D"] <- "7"
x$SYB[x$SYB=="?"] <- NA
x$SYB[x$SYB=="ND"] <- NA
x$SYB<-as.numeric(x$SYB)

x$SDB<-as.character(x$SDB)
x$SDB[x$SDB=="U"] <- "0.5"
x$SDB[x$SDB=="D"] <- "7"
x$SDB[x$SDB=="?"] <- NA
x$SDB[x$SDB=="ND"] <- NA
x$SDB<-as.numeric(x$SDB)

x$NJC<-as.character(x$NJC)
x$NJC[x$NJC=="U"] <- "0.5"
x$NJC[x$NJC=="D"] <- "7"
x$NJC[x$NJC=="?"] <- NA
x$NJC[x$NJC=="ND"] <- NA
x$NJC<-as.numeric(x$NJC)


x$IRB<-as.character(x$IRB)
x$IRB[x$IRB=="U"] <- "0.5"
x$IRB[x$IRB=="D"] <- "7"
x$IRB[x$IRB=="?"] <- NA
x$IRB[x$IRB=="ND"] <- NA
x$IRB<-as.numeric(x$IRB)

x$JBC<-as.character(x$JBC)
x$JBC[x$JBC=="U"] <- "0.5"
x$JBC[x$JBC=="D"] <- "7"
x$JBC[x$JBC=="ND"] <- NA
x$JBC<-as.numeric(x$JBC)

x$SUB<-as.character(x$SUB)
x$SUB[x$SUB=="U"] <- NA
x$SUB<-as.numeric(x$SUB)

x$PDB<-as.character(x$PDB)
x$PDB[x$PDB=="XX"] <- NA
x$PDB<-as.numeric(x$PDB)


x$TRB<-as.character(x$TRB)
x$TRB[x$TRB=="XX"] <- NA
x$TRB<-as.factor(x$TRB)


x$TCB<-as.numeric(as.character(x$TCB))

# 
# isUnique2<-function(x){
#   length(unique(na.omit(x)))!=1   
# }
# x<-x[,sapply(x,isUnique2)]
# 


tidyx <- x









# #  ### MODEL EVALUATION ###
x<-tidyx[1:10000,]
x$testV6<-NULL


# #  # Removal of credit policies #
x<-x[x$IRB < 3,]   # Worst current status on all accounts  **
x$XR<-as.character(x$XR)   # Remove deceased and gone away indicators
x<-x[grepl("N", x$XR),]
x$XR<-as.factor(x$XR)
x<-x[x$QIC < 3,] # No more than 2 seacrhes for short-term loans in past 12 months
x<-x[x$NJC < 2,] #worst current payment status cannot be more than 1  **
x<-x[x$CAB == 0,] #No CIFAS detected
x<-x[x$GQB == 0,] #No IVAs in last 36m detected
x<-x[x$NQB != 1,] #No bankrupcty or scottish sequestration in last 36 months
x<-x[x$PD == 0,] #Currently insolvent removed
x<-x[x$ND > 24 | is.na(x$ND), ] # No CCJ in prev 24 months OR is NA



y<-x[((0.5*length(x[,1]))+1):(length(x[,1])),]
x<-x[1:(0.5*length(x[,1])) ,]


#### START FROM HERE TUES - BUILD MODEL OUT FROM HERE #####
fit <- glm(x$testV ~  SM + HQ  + CYB  + UM + SHC + SVB + SM*UM + SM*HQ - UM*HQ + SHC*HQ + CYB*SM + CYB*SHC  , family="binomial",data=x, na.action=na.exclude) 
fit <- glm(x$testV ~ OOB   , family="binomial",data=x, na.action=na.omit) 

corMat<-data.frame(x$SM,x$HQ,x$CYB,x$UM,x$SHC,x$UUB)
corMat<-cor(corMat,use="pairwise.complete.obs") 

summary(fit)
anova(fit, test="Chisq")


y["predicted"]<-NA
y$predicted<-predict(fit, y, type="response")
y<-y[!is.na(y$predicted),]


cat("\n Gini new: ", Gini(y$predicted, y$testV),"\t\t", "Gini old: ", Giniprev , "\n Difference: ", Gini(y$predicted, y$testV) - Giniprev,"\n\n")
Giniprev<-Gini(y$predicted, y$testV)










