

# Ladda paket
library(readxl)
library(tidyverse)


df <- read_excel("data_insamling_volvo_blocket.xlsx")

# Städar
df_clean <- df %>% 
  select(Försäljningspris, Miltal, Modellår, Hästkrafter) %>%                   # Väljer ut relevanta variabler
  mutate(
    Försäljningspris = as.numeric(Försäljningspris),                            # Säkerställer att priser är siffror
    Miltal = as.numeric(Miltal),                                                # Gör om miltal till numeriskt format
    Modellår = as.numeric(Modellår),                                            # Gör om årsmodell till numeriskt format
    Hästkrafter = as.numeric(Hästkrafter)                                       # Gör om hästkrafter till numeriskt format
  ) %>%
  drop_na() # Rensar bort alla bilar som saknar något av ovanstående värden

# Visualisera 
ggplot(df_clean, aes(x = Miltal, y = Försäljningspris)) +
  geom_point(alpha = 0.4, color = "blue") +                                     # Ritar ut varje bil som en blå punkt
  geom_smooth(method = "lm", color = "red") +                                   # Lägger till en röd trendlinje (regression)
  labs(title = "Samband mellan Miltal och Pris", x = "Miltal", y = "Pris (SEK)")


# Multipel linjär regressionsmodell 
modell <- lm(Försäljningspris ~ Miltal + Modellår + Hästkrafter, data = df_clean) # Skapar en modell där Pris förklaras av Miltal, Årsmodell och Hästkrafter
summary(modell) # Skriver ut resultatet (koefficienter, R-square och p-värden)

# Diagnos
par(mfrow = c(2, 2)) # # Delar upp skärmen för att visa fyra grafer samtidigt
plot(modell) # Genererar plottar (t.ex. QQ-plot)