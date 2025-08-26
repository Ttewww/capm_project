library(quantmod)
getSymbols("TTE.PA", src = "yahoo", from = "2016-01-01", to = "2025-01-01", periodicity = "monthly")
getSymbols("^FCHI", src = "yahoo", from = "2016-01-01", to = "2025-01-01", periodicity = "monthly")


adjusted_prices_TTE <- Ad(TTE.PA) #permet d'extraire les prix de clôture 
adjusted_prices_FCHI <- Ad(FCHI)

returns_TTE <- monthlyReturn(adjusted_prices_TTE)
returns_FCHI <- monthlyReturn(adjusted_prices_FCHI)

Mreturns <- data.frame(Date = index(returns_TTE),Rentabilitees_TTE = coredata(returns_TTE), Rentabilitees_FCHI= coredata(returns_FCHI))
colnames(Mreturns) <- c("Date","Rentabilitées TTE","Rentabilitées FCHI")
#print(Mreturns)
library(openxlsx)
fichierexcel <- "/Users/theo.cammisar/Desktop/TTE_FCHI_Returns.xlsx"
write.xlsx(Mreturns, file = fichierexcel, sheetName = "Rentabilitees", rowNames = FALSE)
DATA2 <- read_excel("/Users/theo.cammisar/Desktop/TTE_FCHI_Returns.xlsx")
attach(DATA2)
View(DATA2)
DATA2TTE <- DATA2$"Rentabilitées TTE"
DATA2FCHI <- DATA2$'Rentabilitées FCHI'
RL_TTE_FCHI <- lm(DATA2TTE ~ DATA2FCHI)
summary(RL_TTE_FCHI)
dwtest(RL_TTE_FCHI)
library(ggplot2)
graphic <- ggplot(data = DATA2, aes(x = DATA2$`Rentabilitées FCHI`,y = DATA2$`Rentabilitées TTE`)) + 
  geom_point(color = "red") + 
  geom_smooth(method = "lm", color = "blue") + 
  theme_classic()
print(graphic)
qf(0.95, 1, 107)

#Test d'hétéroscédasticité

Residus <- residuals(RL_TTE_FCHI)
Residus2 <- Residus^2
TEST_BP <- lm(Residus2 ~ DATA2FCHI)
summary(TEST_BP)
qchisq(0.95, 1) #D'après les résultats, on ne peux pas rejetter l'hypothèse nulle

dwtest(RL_TTE_FCHI) #pas d'autocorrélation a priori

