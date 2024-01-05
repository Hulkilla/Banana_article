library(readxl)
library(agricolae)
library(ggplot2)

Ferm_total <- read_excel("Ferm_total.xlsx")
View(Ferm_total)

colnames(Ferm_total)= c("Code1","SSF","SSF_BD","Code2", "Procesos", "Procesos_BD")
Ferm_total$Code1 = as.character(Ferm_total$Code1)
Ferm_total$Code2 = as.character(Ferm_total$Code2)
View(Ferm_total)

### SSF

ggplot(data = Ferm_total) +
  geom_boxplot(aes(x = SSF, y = SSF_BD, colour = Code1)) +
  theme_bw() + theme(legend.position = "none")

model_SSF = aov(SSF_BD ~ SSF, data = Ferm_total)
HSD.test(model_SSF, "SSF", group = TRUE, console = TRUE)

### Procesos

ggplot(data = Ferm_total) +
  geom_boxplot(aes(x = Procesos, y = Procesos_BD, colour = Procesos)) +
  theme_bw() + theme(legend.position = "none")

model_Proc = aov(Procesos_BD ~ Procesos, data = Ferm_total)
HSD.test(model_Proc, "Procesos", group = TRUE, console = TRUE)
