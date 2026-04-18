#---------------------------packages & route----------------------------------#
###############################################################################
if (require(pacman) == F) {
  install.packages("pacman")}
pacman::p_load(pacman, update = T)
p_load(tidyverse)
p_load(openxlsx)


main <- "prep"


#-----------------------load files--------------------------------------------#
###############################################################################
source(file.path(main, "scripts", "proposal", "gather.R"))


#------------------what were they doing????-----------------------------------#
# linear model
lm1.0 <- lm(
  `Average ELA and Math Score` ~ `Average Free and Reduced-price Lunch`,
  data = exmpl0
)

# quadratic model
lm1.1 <- lm(
  `Average ELA and Math Score` ~ poly(`Average Free and Reduced-price Lunch`, 2),
  data = exmpl0
)

# scatterplot
plot(
  `Average ELA and Math Score` ~ `Average Free and Reduced-price Lunch`,
  data = exmpl0,
  pch = 16
)

# sorted x values
x <- exmpl0 %>%
  arrange(`Average Free and Reduced-price Lunch`) %>%
  pull(`Average Free and Reduced-price Lunch`)

# new data for prediction
nd <- data.frame(`Average Free and Reduced-price Lunch` = x)

# add linear fit
lines(
  x,
  predict(lm1.0, newdata = nd),
  col = "dodgerblue4",
  lwd = 2
)

# add quadratic fit
lines(
  x,
  predict(lm1.1, newdata = nd),
  col = "firebrick3",
  lwd = 2
)

# legend
legend(
  "topright",
  legend = c("Linear", "Quadratic"),
  col = c("dodgerblue4", "firebrick3"),
  lwd = 2,
  bty = "n"
)

int <- lm1.0$coefficients[["(Intercept)"]]
slo <- lm1.0$coefficients[["`Average Free and Reduced-price Lunch`"]]
exmpl1 <-
  exmpl0 %>% 
  mutate(Average_ELA_and_Math_Score = (`Average ELA Score` + `Average Math Score`) / 2,
         Average_ELA_and_Math_Score_dif = Average_ELA_and_Math_Score - `Average ELA and Math Score`,
         
         Expected_ELA_and_Math_Scores = int  + (`Average Free and Reduced-price Lunch` * slo),
         Expected_ELA_and_Math_Scores_dif = Expected_ELA_and_Math_Scores - `Expected ELA and Math Scores`)










