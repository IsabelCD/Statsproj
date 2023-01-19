####Initial imports####
library(wooldridge)
library(readxl)
library(ggplot2)
library(plm)
library(reshape2)
library(lmtest)
library(car)
library(fastDummies)

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


#Plot householdincome
ggplot(df, aes(x=householdincome, y= gradeSL)) +
  geom_point() +
  geom_smooth(method='lm',formula = y ~ x , se=FALSE) +
  theme_minimal()


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
#p-value = 0.0009178 -> rej H0 for 5%
#RE is inconsistent -> use Fixed Effects


#White special test
bptest(reg_fe, ~ I(fitted(reg_re)) + I(fitted(reg_re)^2))
#summary(lm(resid(reg_re)^2 ~ I(fitted(reg_re)) + I(fitted(reg_re)^2)) )
#p-value<5% -> Rej H0
#There is statistical evidence that there is heteroskedasticity in the model

#Robust estimates of SE's, and good statistical tests 
summary(reg_fe, vcov = vcovHC)


#Estudar a especificação do modelo
#Teste de RESET
#Hipótese:
#H0: I(fitted(gradeSL)^2) = I(fitted(gradeSL)^3) = 0 
#H1: I(fitted(gradeSL)^2) != 0 ou I(fitted(gradeSL)^3) != 0
aux_reset <- plm(gradeSL ~ school + gradelevel + gender + covidpos + householdincome
                 + freelunch + numcomputers + familysize + fathereduc + mothereduc
                 + readingscore + writingscore + mathscore + timeperiod_1 +   
                   + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5
                 + I(fitted(reg_re)^2), # + I(fitted(reg_re)^3)
                 data = df, index = c("studentID","timeperiod"), model="within")
  

#Sob H0:
# F = ((Rur-Rr)/2)/((1-Rur)/n-k-3)
#linearHypothesis(aux_reset, c('I(fitted(reg_re)^2) ', 'I(fitted(reg_re)^3)'), vcov=vcovHC)
summary(aux_reset, vcov = vcovHC)
#Conclusion: Como Fobs pertence RR, rej H0. 
#Logo, EEE de que o modelo está mal especificado


#Teste F de significância dos timeperiods
linearHypothesis(reg_fe, c('timeperiod_1 ', 'timeperiod_2', 'timeperiod_3', 
                           'timeperiod_4', 'timeperiod_5'), vcov=vcovHC)
#p-value<2.2e-16 -> rej H0. 
#EEE que timeperiod vars are conjuntamente significativo


reg_final= plm(gradeSL ~ school +  gender + covidpos + householdincome
            + freelunch + numcomputers + fathereduc + mothereduc 
            + I(fathereduc*mothereduc)+ I(readingscore*writingscore)
            + readingscore + writingscore + mathscore + timeperiod_1 +   
              + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5,
            data = df, index = c("studentID","timeperiod"), model="within")

summary(reg_final, vcov = vcovHC)
#fathereduc and mothereduc interaction is constant through time, so it is removed

#Teste F de significância dos interaction
linearHypothesis(reg_final, c('mothereduc ', 'I(fathereduc*mothereduc)'))
#p-value<2.2e-16 -> rej H0. 
#EEE que interaction vars are conjuntamente significativo



#Estudar a especificação do modelo
#Teste de RESET
#Hipóteses de testes t de cada:
#H0: I(fitted(gradeSL)^2) = I(fitted(gradeSL)^3) = 0 
#H1: I(fitted(gradeSL)^2) != 0 ou I(fitted(gradeSL)^3) != 0
aux_reset <- plm(gradeSL ~ school +  gender + covidpos + householdincome
                + freelunch + numcomputers + fathereduc + mothereduc
                + I(fathereduc*mothereduc) + I(readingscore*writingscore)
                + readingscore + writingscore + mathscore + timeperiod_1 +   
                  + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5
                + I(fitted(reg_re)^2) + I(fitted(reg_re)^3), data=df, model="within")


#Sob H0:
# F = ((Rur-Rr)/2)/((1-Rur)/n-k-3)
linearHypothesis(aux_reset, c('I(fitted(reg_re)^2) ', 'I(fitted(reg_re)^3)'), vcov=vcovHC)
summary(aux_reset, vcov=vcovHC)


#Conclusion: Como Fobs pertence RR, rej H0. 
#Logo, EEE de que o modelo está mal especificado



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
                      householdincome + freelunch + numcomputers + familysize 
                    + I(fathereduc*mothereduc) + I(readingscore*writingscore)
                    + fathereduc + mothereduc + readingscore + writingscore + mathscore
                    , data= df_presential)

#RESET test
reset(reg_presential1, vcov=hccm)




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

reg_online1= lm(gradeSL ~ school + gradelevel + gender + covidpos + 
                      householdincome + freelunch + numcomputers + familysize 
                    + I(fathereduc*mothereduc) + I(readingscore*writingscore)
                    + fathereduc + mothereduc + readingscore + writingscore + mathscore
                    , data= df_online)

#RESET test
reset(reg_online1, vcov=hccm)


####TESTE CHOW####
df_chow= df[(df["timeperiod_1"]==0 & df["timeperiod_2"]==0 & df["timeperiod_3"]==0 & df["timeperiod_4"]==0 & df["timeperiod_5"]==0)
            | df["timeperiod_3"]==1,]

numerador= (SSRdfchow - SSRonline-SSRpresencial)/(k+1)
denominador= (SSRonline+SSRpresencial)/(n-2*(k+1))
FObs= numerador/denominador

pf(Fobs, k+1, n-2*(k+1))