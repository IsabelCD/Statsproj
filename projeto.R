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

######Pre-processamento######
##data exploration##
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

nrow(df)
length(df$fathereduc[df$fathereduc>=4])
length(df$mothereduc[df$mothereduc>=4])


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



######Models######
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
#FE is inconsistent -> use Random Effects


#White special test
bptest(reg_re, ~ I(fitted(reg_re)) + I(fitted(reg_re)^2))
#summary(lm(resid(reg_re)^2 ~ I(fitted(reg_re)) + I(fitted(reg_re)^2)) )
#p-value<5% -> Rej H0
#There is statistical evidence that there is heteroskedasticity in the model

#Robust estimates of SE's, and good statistical tests 
summary(reg_re, vcov = vcovHC)


#Estudar a especificação do modelo
#Teste de RESET
#Hipótese:
#H0: I(fitted(gradeSL)^2) = I(fitted(gradeSL)^3) = 0 
#H1: I(fitted(gradeSL)^2) != 0 ou I(fitted(gradeSL)^3) != 0
aux_reset <- lm(gradeSL ~ school + gradelevel + gender + covidpos + householdincome
                + freelunch + numcomputers + familysize + fathereduc + mothereduc
                + readingscore + writingscore + mathscore + timeperiod_1 +   
                  + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5
                + I(fitted(reg_re)^2) + I(fitted(reg_re)^3), data=df)


#Sob H0:
# F = ((Rur-Rr)/2)/((1-Rur)/n-k-3)
linearHypothesis(aux_reset, c('I(fitted(reg_re)^2) ', 'I(fitted(reg_re)^3)'))
summary(aux_reset)

Fobs=((0.6984-0.67821)/2)/((1-0.6984)/(8400-18-3))
Fobs
#Fobs = 280.4576
qf(0.95,2,8400-18-3)
#Fcrit= 2.996804

#Conclusion: Como Fobs pertence RR, rej H0. 
#Logo, EEE de que o modelo está mal especificado


#Teste F de significância dos timeperiods
linearHypothesis(reg_re, c('timeperiod_1 ', 'timeperiod_2', 'timeperiod_3', 
                           'timeperiod_4', 'timeperiod_5'))
#p-value<2.2e-16 -> rej H0. 
#EEE que timeperiod vars are conjuntamente significativo


reg_final= plm(gradeSL ~ school +  gender + covidpos + householdincome
            + freelunch + numcomputers + fathereduc + mothereduc
            + readingscore + writingscore + mathscore + timeperiod_1 +   
              + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5,
            data = df, index = c("studentID","timeperiod"), model="random")
summary(reg_final, vcov = vcovHC)

#Estudar a especificação do modelo
#Teste de RESET
#Hipóteses de testes t de cada:
#H0: I(fitted(gradeSL)^2) = I(fitted(gradeSL)^3) = 0 
#H1: I(fitted(gradeSL)^2) != 0 ou I(fitted(gradeSL)^3) != 0
aux_reset <- lm(gradeSL ~ school +  gender + covidpos + householdincome
                + freelunch + numcomputers + fathereduc + mothereduc
                + readingscore + writingscore + mathscore + timeperiod_1 +   
                  + timeperiod_2 + timeperiod_3 + timeperiod_4 + timeperiod_5
                + I(fitted(reg_re)^2) + I(fitted(reg_re)^3), data=df)


#Sob H0:
# F = ((Rur-Rr)/2)/((1-Rur)/n-k-3)
linearHypothesis(aux_reset, c('I(fitted(reg_re)^2) ', 'I(fitted(reg_re)^3)'))
summary(aux_reset)

Fobs=((0.6983-0.67806)/2)/((1-0.6983)/(8400-16-3))
Fobs
#Fobs = 281.126
qf(0.95,2,8400-16-3)
#Fcrit= 2.996803

#Conclusion: Como Fobs pertence RR, rej H0. 
#Logo, EEE de que o modelo está mal especificado

