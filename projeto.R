####Initial imports####
library(wooldridge)
library(readxl)
library(ggplot2)
library(plm)
library(reshape2)
library(lmtest)
library(car)
library(fastDummies)
library(gap)

setwd("C:/Users/isabe/OneDrive/Desktop/Estatística")
df=read_excel("./Projeto/covid_grades.xlsx")

######Pre-processing######
##Data Exploration##
#check if there are duplicate rows
sum(duplicated(df))

#descriptive statistics
summary(df)

#missing values
colSums(is.na(df))

#outliers
colbp= colnames(df)[!(colnames(df) %in% c("studentID", "school", "gender", "covidpos", "freelunch"))]


for (i in colbp){
  boxplot(df[i])
  title(i)
}


nrow(df) #nº of rows for comparison: 8400
length(df$fathereduc[df$fathereduc>=4]) 
#113, but we think they are important
length(df$mothereduc[df$mothereduc>=4]) 
#113, but we think they are important
#we would be removing a category by removing


#relationship between variables

# heatmap
cormat <- round(cor(df),2)
melted_cormat <- melt(cormat)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "black", high = "white", mid = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 12, hjust = 1))+
  coord_fixed()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
#+geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
# Print the heatmap
print(ggheatmap)



# Feature engineering
df["gradeSL"]= df$readingscoreSL+df$writingscoreSL+df$mathscoreSL
df["gradeSL"]

#separate timeperiod into categories
df <- dummy_cols(df, select_columns = c("timeperiod"), remove_first_dummy=TRUE)

# Remove using subset
df <- df[,!names(df) %in% c("readingscoreSL", "writingscoreSL", "mathscoreSL")]


####Relationships between variables and target####

#Plot for school
boxplot( gradeSL ~ school , data= df)

#Plot for gradelevel
boxplot( gradeSL ~ gradelevel , data= df)

#Plot for gender
boxplot( gradeSL ~ gender , data= df)

#Plot for covidpos
boxplot( gradeSL ~ covidpos , data= df)

#Plot householdincome
ggplot(df, aes(x=householdincome, y= gradeSL)) +
  geom_point() +
  geom_smooth(method='lm',formula = y ~ x , se=FALSE) +
  theme_minimal()

#Plot for freelunch
boxplot( gradeSL ~ freelunch , data= df)

#Plot for numcomputers
boxplot( gradeSL ~ numcomputers , data= df)

#Plot for familysize
boxplot( gradeSL ~ familysize , data= df)

#Plot for fathereduc
boxplot( gradeSL ~ fathereduc , data= df)

#Plot for mothereduc
boxplot( gradeSL ~ mothereduc , data= df)

#Plot readingscore
ggplot(df, aes(x=readingscore, y= gradeSL)) +
  geom_point() +
  geom_smooth(method='lm',formula = y ~ x , se=FALSE) +
  theme_minimal()


#Plot writingscore
ggplot(df, aes(x=writingscore, y= gradeSL)) +
  geom_point() +
  geom_smooth(method='lm',formula = y ~ x , se=FALSE) +
  theme_minimal()


#Plot mathscore
ggplot(df, aes(x=mathscore, y= gradeSL)) +
  geom_point() +
  geom_smooth(method='lm',formula = y ~ x , se=FALSE) +
  theme_minimal()



######PANEL DATA PHASE######
reg_re= plm(gradeSL ~ school + gradelevel + gender + covidpos + householdincome
            + freelunch + numcomputers + familysize + fathereduc + mothereduc 
            + readingscore + writingscore + mathscore + timeperiod_1 +   
            + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5,
                 data = df, index = c("studentID","timeperiod"), model="random")
summary(reg_re)

reg_fe= plm(gradeSL ~ school + gradelevel + gender + covidpos + householdincome
            + freelunch + numcomputers + familysize + fathereduc + mothereduc
            + readingscore + writingscore + mathscore + timeperiod_1 +   
              + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5,
            data = df, index = c("studentID","timeperiod"), model="within")
summary(reg_fe)

#Hausman test
phtest(reg_fe, reg_re)
#p-value = 0.0001279 -> rej H0 for 5%
#RE is inconsistent -> use Fixed Effects


#White special test
bptest(reg_fe, ~ I(fitted(reg_re)) + I(fitted(reg_re)^2))
#summary(lm(resid(reg_re)^2 ~ I(fitted(reg_re)) + I(fitted(reg_re)^2)) )
#p-value<5% -> Rej H0
#There is statistical evidence that there is heteroskedasticity in the model

#Robust estimates of SE's, and good statistical tests 
summary(reg_fe, vcov = vcovHC)


#Teste de RESET - Functional Form Specification
#H0: I(fitted(gradeSL)^2) = I(fitted(gradeSL)^3) = 0 
#H1: I(fitted(gradeSL)^2) != 0 ou I(fitted(gradeSL)^3) != 0
aux_reset <- plm(gradeSL ~ school + gradelevel + gender + covidpos + householdincome
                 + freelunch + numcomputers + familysize + fathereduc + mothereduc
                 + readingscore + writingscore + mathscore + timeperiod_1 +   
                   + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5
                 + I(fitted(reg_fe)^2) + I(fitted(reg_fe)^3),
                 data = df, index = c("studentID","timeperiod"), model="within")


#Sob H0:
# F = ((Rur-Rr)/2)/((1-Rur)/n-k-3)
linearHypothesis(aux_reset, c('I(fitted(reg_fe)^2) ', 'I(fitted(reg_fe)^3)'), vcov=vcovHC)
summary(aux_reset, vcov = vcovHC)
#Conclusion: Since p-value<0.05, we rej H0. 
#So, there is statistical evidence of functional form misspecification



reg_explore= plm(gradeSL ~ school +  gender + covidpos + householdincome
            + freelunch + numcomputers + fathereduc + mothereduc 
            + I(readingscore*writingscore)
            + readingscore + writingscore + mathscore + timeperiod_1 +   
              + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5,
            data = df, index = c("studentID","timeperiod"), model="within")

summary(reg_explore, vcov = vcovHC)


#Estudar a especificação do modelo
#Teste de RESET
#Hipóteses de testes t de cada:
#H0: I(fitted(gradeSL)^2) = I(fitted(gradeSL)^3) = 0 
#H1: I(fitted(gradeSL)^2) != 0 ou I(fitted(gradeSL)^3) != 0
aux_reset <- plm(gradeSL ~ school +  gender + covidpos + householdincome
                 + freelunch + numcomputers + fathereduc + mothereduc 
                 + readingscore +  I(readingscore^2)+ writingscore 
                 +  I(writingscore^2) + mathscore + I(mathscore^2)+ timeperiod_1 +   
                   + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5
                 + I(fitted(reg_fe)^2) + I(fitted(reg_fe)^3),
                 data = df, index = c("studentID","timeperiod"), model="within")


#Sob H0:
# F = ((Rur-Rr)/2)/((1-Rur)/n-k-3)
linearHypothesis(aux_reset, c('I(fitted(reg_fe)^2) ', 'I(fitted(reg_fe)^3)'), vcov=vcovHC)
#summary(aux_reset, vcov=vcovHC)
#the new specification is even worse, as p-value is smaller, 
#the hypothesis of functional form misspecification is more statistically significant


#Final functional form specification
#See summary of best model so far
summary(reg_fe, vcov = vcovHC)
#Since p-value>0.05 in variable "mothereduc", we don't rej H0, 
#so there is no statistical evidence of it being important to explain the grades

#Other (singular) have a p-values<0.05, so there is statistical evidence
#they are important to explain the grades


#F test to see joint significance of timeperiods
linearHypothesis(reg_fe, c('timeperiod_1 ', 'timeperiod_2', 'timeperiod_3', 
                           'timeperiod_4', 'timeperiod_5'), vcov=vcovHC)
#p-value<0.05 -> rej H0. 
#There is statistical evidence that the timeperiod variables are jointly significant
 



####CROSS SECTIONAL - PRESENTIAL####

##PRESENTIAL
df_presential= df[(df["timeperiod_1"]==0 & df["timeperiod_2"]==0 & df["timeperiod_3"]==0 & df["timeperiod_4"]==0 & df["timeperiod_5"]==0),]
df_presential

reg_presential= lm(gradeSL ~ school + gradelevel + gender + covidpos + 
     householdincome + freelunch + numcomputers + familysize + 
     fathereduc + mothereduc + readingscore + writingscore + mathscore, data= df_presential)


#White special
bptest(reg_presential, ~ I(fitted(reg_presential)) + I(fitted(reg_presential)^2))
#p-value>0.05 -> rej H0, so there is statistical evidence of heteroskedasticity in the model

#Robust se
coeftest(reg_presential, vcov=hccm)

#RESET test
reset(reg_presential, vcov=hccm)
#p-value>0.05 -> rej H0, so there is statistical evidence of functional form misspecification

reg_presential1= lm(gradeSL ~ school + gradelevel + gender + covidpos + 
                      householdincome + freelunch+ numcomputers+ familysize
                    + I(mathscore*writingscore) + I(readingscore*writingscore)+ I(readingscore*mathscore)
                    + fathereduc + mothereduc + readingscore + writingscore + mathscore
                    , data= df_presential)

#RESET test
reset(reg_presential1, vcov=hccm)
#Since p-value=1.136e-07, we still reject H0, but the test statistic is less significant
#so, there is an improvement in the model 

summary(reg_presential1, vcov=hccm)
#gradelevel, numcomputers, familysize were not statistically significant.
#But they will only be removed from the model if the same decision is made in the online data 
#(so that the models are comparable)


####CROSS SECTIONAL - ONLINE####
df_online= df[df["timeperiod_3"]==1,]
df_online


reg_online= lm(gradeSL ~ school + gradelevel + gender + covidpos + 
                     householdincome + freelunch + numcomputers + familysize + 
                     fathereduc + mothereduc + readingscore + writingscore + mathscore, data= df_online)


#White special
bptest(reg_online, ~ I(fitted(reg_online)) + I(fitted(reg_online)^2))
#p-value>0.05 -> rej H0, so there is statistical evidence of heteroskedasticity in the model

#Robust se
coeftest(reg_online, vcov=hccm)

#RESET test
reset(reg_online, vcov=hccm)
#p-value>0.05 -> rej H0, so there is statistical evidence of functional form misspecification

#Make same changes as with the presential data
reg_online1= lm(gradeSL ~ school + gradelevel + gender + covidpos + 
                      householdincome + freelunch+ numcomputers+ familysize
                    + I(mathscore*writingscore) + I(readingscore*writingscore)+ I(readingscore*mathscore)
                    + fathereduc + mothereduc + readingscore + writingscore + mathscore
                    , data= df_online)


#RESET test
reset(reg_online1, vcov=hccm)
#Since p-value=1.697e-05, we still reject H0, but the test statistic is less significant
#so, there is an improvement in the model

summary(reg_online1, vcov=hccm)
#gradelevel and familysize are not statistically significant for a 5% level in either of the dataset (online or presential)
#so, we remove them from both models
#numcomputers is statistically significant in this model, so it won't be removed in either of them

####FINAL CROSS-SECTIONAL MODELS####
presential= lm(gradeSL ~ school + gender + covidpos + 
                 householdincome + freelunch+ numcomputers
               + I(mathscore*writingscore) + I(readingscore*writingscore)+ I(readingscore*mathscore)
               + fathereduc + mothereduc + readingscore + writingscore + mathscore
               , data= df_presential)

online= lm(gradeSL ~ school +  gender + covidpos + 
                  householdincome + freelunch+ numcomputers
                + I(mathscore*writingscore) + I(readingscore*writingscore)+ I(readingscore*mathscore)
                + fathereduc + mothereduc + readingscore + writingscore + mathscore
                , data= df_online)

####TESTE CHOW####
#H0: parameters are the same between online and presential time periods
#H1: parameters are different between online and presential
df_chow= df[(df["timeperiod_1"]==0 & df["timeperiod_2"]==0 & df["timeperiod_3"]==0 & df["timeperiod_4"]==0 & df["timeperiod_5"]==0)
            | df["timeperiod_3"]==1,]

chow= lm(gradeSL ~ school +  gender + covidpos + 
             householdincome + freelunch+ numcomputers
           + I(mathscore*writingscore) + I(readingscore*writingscore)+ I(readingscore*mathscore)
           + fathereduc + mothereduc + readingscore + writingscore + mathscore
           , data= df_chow)

SSRonline= sum((online$fitted.values - mean(online$model$gradeSL))^2)
SSRpresential=sum((presential$fitted.values - mean(presential$model$gradeSL))^2)
SSRdfchow=sum((chow$fitted.values - mean(chow$model$gradeSL))^2)

dof=nrow(df_online)+nrow(df_presential)-2*(14+1)
fnum= (((nrow(df_online)-14)*summary(online)$sigma^2)+ ((nrow(df_presential)-14)*summary(presential)$sigma^2))^2
fden= ((nrow(df_online)-14)*(summary(online)$sigma^2)^2)+ ((nrow(df_presential)-14)*(summary(presential)$sigma^2)^2)
f=fnum/fden

Fobsnum= (SSRdfchow - SSRonline-SSRpresential)/(14+1)
Fobsdenom= (SSRonline+SSRpresential)/f

Fobs= Fobsnum/Fobsdenom

prob=(dof/f)*pf(Fobs, 14+1,dof)
2*(1-prob)
#since p-value<0.05, we reject H0. 
#So there is statistical evidence that the parameters are different for different phases

