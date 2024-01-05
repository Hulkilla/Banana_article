library(readxl)
library(agricolae)
library(ggplot2)

HE_total <- read_excel("HE_total.xlsx")
View(HE_total)

HE_total$Code = as.character(HE_total$Code)

### Cáscara de plátano - Glucosa

ggplot(data = HE_total) +
  geom_boxplot(aes(x = Code, y = Glucosa_C, colour = Code)) +
  theme_bw() + theme(legend.position = "none")

model_G_C = aov(`Glucosa_C` ~ Code, data = HE_total)
HSD.test(model_G_C, "Code", group = TRUE, console = TRUE)


### Cáscara de plátano - Otros

ggplot(data = HE_total) +
  geom_boxplot(aes(x = Code, y = Otros_C, colour = Code)) +
  theme_bw() + theme(legend.position = "none")

model_C_O = aov(`Otros_C` ~ Code, data = HE_total)
HSD.test(model_C_O, "Code", group = TRUE, console = TRUE)


### Plátano - Glucosa

ggplot(data = HE_total) +
  geom_boxplot(aes(x = Code, y = Glucosa_F, colour = Code)) +
  theme_bw() + theme(legend.position = "none")

model_F_G = aov(`Glucosa_F` ~ Code, data = HE_total)
HSD.test(model_F_G, "Code", group = TRUE, console = TRUE)


### Plátano - Otro

ggplot(data = HE_total) +
  geom_boxplot(aes(x = Code, y = Otros_F, colour = Enzima)) +
  theme_bw() + theme(legend.position = "none")

model_O_F = aov(`Otros_F` ~ Code, data = HE_total)
HSD.test(model_O_F, "Code", group = TRUE, console = TRUE)


### Global

#### Cascara

HE_total$Cascara = HE_total$Glucosa_C + HE_total$Otros_C

ggplot(data = HE_total) +
  geom_boxplot(aes(x = Code, y = Cascara, colour = Code)) +
  theme_bw() + theme(legend.position = "none")

model_G_C = aov(`Cascara` ~ Code, data = HE_total)
HSD.test(model_G_C, "Code", group = TRUE, console = TRUE)

#### Fruto + Cascara

HE_total$Fruto = HE_total$Glucosa_F + HE_total$Otros_F

ggplot(data = HE_total) +
  geom_boxplot(aes(x = Code, y = Fruto, colour = Code)) +
  theme_bw() + theme(legend.position = "none")

model_G_C = aov(`Fruto` ~ Code, data = HE_total)
HSD.test(model_G_C, "Code", group = TRUE, console = TRUE)
