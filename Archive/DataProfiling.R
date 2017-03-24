# Script: Loan Prediction (Analytics Vidhya Practice Dataset)

require("gmodels")
require("ggplot2")
require("Amelia")

############### DATA PROFILING #############

#####Overall Summary#####
attach(LoanTrain)
#summary(LoanTrain)

#####Visualize data by variable#####
CrossTable(Loan_Status,Gender, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE)
CrossTable(Loan_Status,Married, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE)
CrossTable(Loan_Status,Self_Employed, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE)
CrossTable(Loan_Status,Education, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE)
CrossTable(Loan_Status,Dependents, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE)

plot(ApplicantIncome,CoapplicantIncome,pch = as.numeric(Loan_Status))
title(main = "Applicant and Coapplicant Income")
legend(0.8*max(ApplicantIncome),0.9*max(CoapplicantIncome), levels(Loan_Status), pch = 1:length(levels(Loan_Status)))

summary(aov(LoanAmount~ApplicantIncome))
summary(aov(LoanAmount~CoapplicantIncome))

Temp_Credit_History = replace(Credit_History, is.na(Credit_History), -1 )
CrossTable(Loan_Status,Temp_Credit_History, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE, missing.include = TRUE)
rm(Temp_Credit_History)

# Hypothesis - CoapplicantIncome = 0 might have significant meaning
HaveCoapplicantIncome = (CoapplicantIncome == 0)
CrossTable(Loan_Status,HaveCoapplicantIncome, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE)
boxplot(CoapplicantIncome~Loan_Status, ylim = c(0,10000))
title("CoapplicantIncome against Loan Status")
rm(HaveCoapplicantIncome)

#Hypothesis - Absence of Credit_History has similar property to Credit_History = 1
Credit_History_CheckMissing <- LoanTrain[which(Credit_History != 0 | is.na(Credit_History)),]
Credit_History_CheckMissing$Credit_History = replace(Credit_History_CheckMissing$Credit_History,is.na(Credit_History_CheckMissing$Credit_History), -1 )
CrossTable(Credit_History_CheckMissing$Loan_Status,Credit_History_CheckMissing$Credit_History, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE)
rm(Credit_History_CheckMissing)

# Special - Recategorize
CategorizedLoanAmountTerm = cut(replace(Loan_Amount_Term, is.na(Loan_Amount_Term), -1), breaks=c(-10,0,120,180,300,360,480))

qplot(CategorizedLoanAmountTerm,LoanAmount,pch = Loan_Status,main = "Loan Amount and Term Length")+geom_point(position = position_jitter(w = 0.3, h = 0.3))

summary(aov(LoanAmount~Loan_Status))
CrossTable(Loan_Status,CategorizedLoanAmountTerm, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE, missing.include = TRUE)

CrossTable(Loan_Status,Property_Area, chisq = TRUE, prop.r=FALSE, prop.chisq = FALSE, missing.include = TRUE)
rm(CategorizedLoanAmountTerm)
detach(LoanTrain)

############### MISSING VALUE IMPUTATION #############
# Only get important variables
ModeledLoanTrain <- LoanTrain[c("Loan_ID","Married","Education","ApplicantIncome","CoapplicantIncome","Credit_History","Property_Area","Loan_Status")]

# Recode Credit_History
ModeledLoanTrain$Credit_History[is.na(ModeledLoanTrain$Credit_History)] <- -1
factor(ModeledLoanTrain$Credit_History, labels=c("NA","N","Y"))

################# MODEL AND EVALUATION ################

# Split Training and Testing Set
#TrainSet <- ModeledLoanTrain[1:500, ]
#TestSet  <- ModeledLoanTrain[501:nrow(ModeledLoanTrain),]
attach(TrainSet)
LRmodel <- glm(Loan_Status ~ Married+Education+ApplicantIncome+CoapplicantIncome+Credit_History+Property_Area, family=binomial(link='logit'), data=ModeledLoanTrain, subset = 1:500)
summary(LRmodel)

detach(TrainSet)
#####Clean up#####

