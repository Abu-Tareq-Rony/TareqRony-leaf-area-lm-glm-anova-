
library(openxlsx)

###########################
ind_dat <- read.xlsx("02_Individuals_2020_AS.xlsx", sheet = 1)
str(ind_dat)
ind_dat$management <- as.factor(ind_dat$management)
str(ind_dat)
names(ind_dat)
 leaflength <- c(ind_dat$Leaf.length1,ind_dat$Leaf.length2)
leaflength.mean <- mean(leaflength,trim = 0, na.rm = FALSE)
leafwidth <- c(ind_dat$Leaf.width1,ind_dat$Leaf.width2)
leaflength.mean <- mean(leafwidth,trim = 0, na.rm = FALSE)
 
leafarea <- (leaflength.mean * leaflength.mean)
  
####
# in this model I include area as explanatory variable. You may leave this out depending on your research quetsion.
model1 <- lm(Leaf.area ~ management + area + exposition + slope + soil_depth + soil_water + PAR_above + PAR_ground + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model1)
plot(model1)
# is the lm fitting?

library(MASS)
model2 <- glm(Leaf.area ~ management + area + exposition + slope + soil_depth + soil_water + PAR_above + PAR_ground + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model2)

model3 <- glm(Leaf.area ~ management + area + slope + soil_depth + soil_water + PAR_above + PAR_ground + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model3)

model4 <- glm(Leaf.area ~ management + area + slope + soil_depth + soil_water + PAR_above + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model4)

model5 <- glm(Leaf.area ~ management + area + soil_depth + soil_water + PAR_above + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model5)

model6 <- glm(Leaf.area ~ management + area + soil_depth + PAR_above + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model6)


model7 <- glm(Leaf.area ~ management + area + soil_depth + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model7)

model8 <- glm(Leaf.area ~ management + area + soil_depth + HL_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model8)

model9 <- glm(Leaf.area ~ management + area + soil_depth + HL_cover + soil_cover + vegHeigth_90, data = ind_dat)
summary(model9)


model10 <- glm(Leaf.area ~ management + area + soil_depth + HL_cover + soil_cover, data = ind_dat)
summary(model10)


model11 <- glm(Leaf.area ~ management + area + soil_depth + HL_cover, data = ind_dat)
summary(model11)

model12 <- glm(Leaf.area ~ management + area + HL_cover, data = ind_dat)
summary(model12)

model13 <- glm(Leaf.area ~ management + HL_cover, data = ind_dat)
summary(model13)

#########
#######
#### plot Number of Leaves explained by management

boxplot(Leaf.area ~ management, data = ind_dat, col = "green")

# are there significat differences between management types?
library(emmeans)
diff <- emmeans(model13, ~ management)
pairs(diff)

# how to get the significance letters for the graph?
library(multcomp)
library(multcompView)
cld(diff, Letters = letters)
#
boxplot(Leaf.area ~ management, data = ind_dat, col = "green", xlab = "Management type", ylab = "Leaf Area", ylim = c(0, 200))
text(1, 200, "a")
text(2, 200, "a")
text(3, 200, "a")
text(4, 200, "a")

#### plot Number of Leaves explained by HL cover
library(ggplot2)
ggplot(data = ind_dat, aes(x = HL_cover, y = Leaf.area))+ 
  geom_point(color = "black", alpha = 0.25, shape = 16, size = 1) +
  geom_smooth(method = MASS::glm.nb, formula = y ~ x, color = "darkgreen", se = TRUE, fill = "green", alpha = 0.35) +
  labs(x = "Percentage of HL cover", y = "Number of Leaves") +
  theme_bw()











