# ###### Índice código R
# 0 Recolha e preparação de informação do AmesHousing
# 1 Definir o problema
# 2 Indicar as variáveis (incluir correlações e tratamento da Heating_QC)
# 3 Modelação e apresentação de resultados do AIC backward (tentativa de retirar cada 1 das variáveis, 
# qual acréscimo no Deviance (-2LL) e AIC) 
# 4 Coeficientes e ICs
# 5 Matriz de confusão (cutoff 0.5)
# 6 Interpretação de resultados



# 0 Recolha e preparação de informação do AmesHousing

rm(list = ls())

# Create a Processed Version of the Ames Housing Data
library(AmesHousing)
ames <- make_ames()

nrow(ames)
str(ames)

summary(ames)
summary(ames$Sale_Price)
summary(ames$Year_Built)
summary(ames$Year_Sold)
summary(ames$Garage_Cars)
summary(ames$Heating_QC)


# 1 Definir o problema

# O county de Amex, Iowa, tem um incentivo para a aquisição de casas, atribuindo um bónus fiscal aos 
# compradores de casas de valor igual ou superior a $175.000. Para constituir uma carteira (portfolio) de casas com este
# potencial de venda que consiga captar clientes de elevado  potencial e satisfazê-los com entrega de valor através no benefício fiscal,
# elabora-se modelo de propensão de casa à venda a valor igual ou superior a $175.000.

# Tax incentive for Buyers (Bonus: Binary response variable)
Bonus<-ifelse(ames$Sale_Price>=175000,1,0)
summary(Bonus)


# 2 Indicar as variáveis

# Covariate variables

# Lot Area (Continuous): Lot size in square feet
# Total Bsmt SF (Continuous): Total square feet of basement area
# TotRmsAbvGrd (Discrete): Total rooms above grade (does not include bathrooms)
# Garage Cars (Discrete): Size of garage in car capacity
# HeatingQC (Ordinal): Heating quality and condition
# House years old (aprox.): [Yr Sold (Discrete): Year Sold (YYYY)] - [Year Built (Discrete): Original construction date  (YYYY)]


# Data (Response variable y=Bonus and Covariates)
X<-data.frame(cbind(ifelse(ames$Sale_Price>=175000,1,0),ames$Lot_Area,ames$Total_Bsmt_SF,ames$TotRms_AbvGrd,
                    ames$Garage_Cars,ames$Heating_QC,(ames$Year_Sold-ames$Year_Built+1)))
colnames(X)<-c("Bonus","Lot_Area","Basement_Area","Nr_Rooms","Garage_Cars","Heating_QC","House_years_old")
str(X)
summary(X)

# Categorização da Binary response variable (Bonus) e da covariate categórica/nominal (Heating_QC)
X$Bonus<-as.factor(X$Bonus)
X$Heating_QC<-as.factor(X$Heating_QC)
str(X)
summary(X)

# i. Tratamento e ii. Codificação da variável categórica Heating_QC para a regressão glm (contrast coding)
summary(X$Heating_QC)
#    1    2    3    4    5 
# 1495   92  476    3  864 

# i. Tratamento por dimensão reduzida na categoria "Poor"
summary(ames$Heating_QC)
 # Excellent      Fair      Good      Poor   Typical 
 #      1495        92       476         3       864 
# A ORDEM DESTA VARIÁVEL (SUPOSTAMENTE) ORDINAL É 
# HeatingQC (Ordinal): Heating quality and condition
#     Está (lê-se) --> Devia estar [leia-se] 
# Ex	Excellent (1)--> [1]
# Gd	Good (3)--> [2]
# TA	Average/Typical (5)--> [3]
# Fa	Fair (2)--> [4]
# Po	Poor (4)--> [5]


# AGREGAR: 1º) O "Poor" ao "Fair", por ser exíguo; 2º) O Fair/Poor ao Average/Typical, por exíguos e não significativos na logística
# 1º)
X$Heating_QC_T<-factor(ifelse(X$Heating_QC %in% c(2,4),2,X$Heating_QC))
summary(X$Heating_QC_T)
# NOVA VARIÁVEL CATEGÓRICA TRANSFORMADA:
#    1    2    3    5 
# 1495   95  476  864 
# Heating_QC_T (Categorical): Heating quality and condition
#       Está (lê-se)||Devia estar (leia-se) 
# Ex	  Excellent (1)--> [1]
# Gd	  Good (3)--> [2]
# TA	  Average/Typical (5)--> [3]
# FaPo	Fair/Poor (2;4)--> [4;5]

summary(X)

# 2º)
X$Heating_QC_T<-factor(ifelse(X$Heating_QC %in% c(2,4,5),2,X$Heating_QC_T))
summary(X$Heating_QC_T)
#    1    2    3 
# 1495  959  476 
# Ex	  Excellent (1)
# Gd	  Good (3)
# TAFaPo	Typical/Fair/Poor (5;2;4) é o (2)


# ii. Codificação da variável categórica Heating_QC para a regressão glm (contrast coding)
# DEVIATION CODE (Compares each level to the grand mean) 

contr.sum(3)
contrasts(X$Heating_QC_T) = contr.sum(3)

summary(X)
# Bonus       Lot_Area      Basement_Area     Nr_Rooms       Garage_Cars    Heating_QC House_years_old  Heating_QC_T
# 0:1690    Min.   :  1300   Min.   :   0   Min.   : 2.000   Min.   :0.000   1:1495     Min.   :  0.00   1:1495      
# 1:1240    1st Qu.:  7440   1st Qu.: 793   1st Qu.: 5.000   1st Qu.:1.000   2:  92     1st Qu.:  8.00   2: 959      
#           Median :  9436   Median : 990   Median : 6.000   Median :2.000   3: 476     Median : 35.00   3: 476      
#           Mean   : 10148   Mean   :1051   Mean   : 6.443   Mean   :1.766   4:   3     Mean   : 37.43               
#           3rd Qu.: 11555   3rd Qu.:1302   3rd Qu.: 7.000   3rd Qu.:2.000   5: 864     3rd Qu.: 55.00               
#           Max.   :215245   Max.   :6110   Max.   :15.000   Max.   :5.000              Max.   :137.00               


cor(X[,c(2,3,4,5,7)])
#                    Lot_Area Basement_Area   Nr_Rooms Garage_Cars House_years_old
# Lot_Area         1.00000000     0.2537648  0.2165966   0.1794561     -0.02422657
# Basement_Area    0.25376478     1.0000000  0.2816267   0.4378564     -0.40760251
# Nr_Rooms         0.21659665     0.2816267  1.0000000   0.3554483     -0.11307426
# Garage_Cars      0.17945608     0.4378564  0.3554483   1.0000000     -0.53812060
# House_years_old -0.02422657    -0.4076025 -0.1130743  -0.5381206      1.00000000


# 3 Modelação e apresentação de resultados do AIC backward

# Modelacao Logistic
mylm = glm(X$Bonus==1~X$Lot_Area+X$Basement_Area+X$Nr_Rooms+X$Garage_Cars+X$House_years_old+X$Heating_QC_T,family = binomial)

summary(mylm)

round(summary(mylm)$coefficients,5)

# variable/model selection via AIC (the Akaike Information criterion)

# Análise do efeito backward (modelo completo (<none>) vs retirar 1 variável) no Deviance e AIC  
# ["- X$[var]" faria aumentar o AIC para...(trade-off redução 2k por menos 1 variável vs perda de ajustamento pela sua saída)]

library(MASS)
stepAIC(mylm,direction="backward",trace=10)


# 4 Coeficientes e ICs

coef<-round(summary(mylm)$coefficients,5);coef
# cbind(sum(mylm$coefficients[6],mylm$coefficients[7])*-1,NA,NA,NA)
# Heating hidden coefficient
Heating_QC_T3<-cbind(sum(mylm$coefficients[6],mylm$coefficients[7])*-1,NA,NA,NA);Heating_QC_T3

coefs<-rbind(coef,Heating_QC_T3);coefs
rownames(coefs)<-c(row.names(coefs)[-9],"X$Heating_QC_T3");round(coefs,4)
#                     Estimate Std. Error   z value Pr(>|z|)
# (Intercept)       -7.7341200    0.42045 -18.39477        0
# X$Lot_Area         0.0000800    0.00001   6.63900        0
# X$Basement_Area    0.0018700    0.00018  10.51406        0
# X$Nr_Rooms         0.5583000    0.04506  12.39030        0
# X$Garage_Cars      1.1499500    0.12284   9.36167        0
# X$House_years_old -0.0391600    0.00278 -14.07513        0
# X$Heating_QC_T1    0.6954800    0.08413   8.26679        0
# X$Heating_QC_T2   -0.8060300    0.09873  -8.16418        0
# X$Heating_QC_T3   -0.6563206         NA        NA       NA


# IC's
CIs <- cbind(confint(mylm),confint.default(mylm)); colnames(CIs) <- c("Profile CI","","Wald-type CI",""); CIs


# 5 Matriz de confusao (cutoff 0.5)

# Predict 
# computing all log-odds
log.odds<-predict(mylm, newdata=X)
# computing all odds
odds = exp(log.odds)
# computing all probabilities of being an high-value house
theta.i   = exp(log.odds)/(1+exp(log.odds))
# setting a table with all results
results = cbind(X,log.odds,odds,theta.i)
results = results[order(results[,11]),] # this sorts the data by [prob=theta.i] 
rownames(results) <- c(1:length(X$Bonus))

results$P_Bonus<-ifelse(results$theta.i>=0.5,1,0)
summary(results)

results$P_Bonus<-factor(results$P_Bonus)
# results$Bonus<-factor(results$Bonus)
summary(results)


sum(results$Bonus==0)
sum(results$Bonus==1)
sum(results$P_Bonus==0)
sum(results$P_Bonus==1)

# Accuracy<-sum(results_2$P_Bonus==results_2$Bonus)/length(X$Bonus)
A<-sum(results$P_Bonus==1 & results$Bonus==1) #A
D<-sum(results$P_Bonus==0 & results$Bonus==0) #D
C<-sum(results$P_Bonus==0 & results$Bonus==1) #C
B<-sum(results$P_Bonus==1 & results$Bonus==0) #B

Accuracy<-(A+D)/(A+B+C+D);round(Accuracy,4)
Recall_Sensitivity<-A/(A+C);round(Recall_Sensitivity,4)
Specificity<-D/(B+D);round(Specificity,4)
Precision <- A/(A+B);round(Precision,4)

# Evaluation
Eval<-round(rbind(Accuracy,Recall_Sensitivity,Specificity,Precision),4)
colnames(Eval)<-"Evaluation"
Eval

# Built-in Confusion Matrix
library(caret)
confusionMatrix(results$P_Bonus,results$Bonus,positive = "1")

