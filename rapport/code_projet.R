library(CASdatasets)
library(car)
data("auscathist")
aus_data = auscathist
sapply(aus_data,function(x) sum(is.na(x)))
aus_data$LastDay = NULL
n = ncol(aus_data)
for(i in 1:n){
  aus_data = subset(aus_data, !is.na(aus_data[,i]))
}
aus_data_states = read.csv2(file.choose(), sep = ",")
aus_data_states$Quarter = factor(aus_data_states$Quarter)
aus_data_states$OriginalCost = as.numeric(aus_data_states$OriginalCost)
aus_data_states$X = NULL
fit = glm(formula = OriginalCost ~ Quarter + State + Type , data = aus_data_states, family = Gamma())
summary(fit)
Anova(fit)
anova(fit, test="Chisq")
fit_gaussian = glm(formula = OriginalCost ~ Quarter + State + Type, data = aus_data_states, family = gaussian())
summary(fit_gaussian)
anova(fit_gaussian, test="Chisq")
frequence = aggregate(aus_data_states$Type, by = aus_data_states["Year"], FUN = length)
colnames(frequence) = c("Year","Frequence per year")
aus_data = merge(aus_data_states, frequence )
fit_poisson = glm(formula = aus_data$`Frequence per year` ~ Quarter + State + Type , data = aus_data, family = poisson )
Anova(fit_poisson,test.statistic = "LR", type = 2)
aus_data$Type <- relevel(aus_data$Type, ref = "Tornado")
aus_data$Quarter <- relevel(aus_data$Quarter, ref = "3")
fit_poisson_2 = glm(formula = aus_data$`Frequence per year` ~ Quarter + Type , data = aus_data, family = poisson )
summary(fit_poisson_2)

