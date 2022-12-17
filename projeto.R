library(wooldridge)
library(readxl)
library(ggplot2)

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
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
  coord_fixed()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
#+geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
# Print the heatmap
print(ggheatmap)

ggplot(df, aes(x=school, y=householdincome)) +
  geom_point() +
  geom_smooth(method='lm',formula = y ~ x , se=FALSE) +
  theme_minimal()


#feature engineering
df["gradeSL"]= df$readingscoreSL+df$writingscoreSL+df$mathscoreSL
df["gradeSL"]


######Models######

