library(mgcv)

Study1F03b <- read.csv("Study1F03b.csv")

head(Study1F03b)

str(Study1F03b)

mod1 <- gam(valence ~ s(time), data=Study1F03b)

summary(mod1)

coef(mod1)

plot(mod1)

gam.check(mod1, pages=1)

mod2 <- gam(valence ~ s(time, k=15), data=Study1F03b)


summary(mod2)

coef(mod2)

plot(mod2)

gam.check(mod2, pages=1)

mod2 <- gam(valence ~ s(time, k=15), data=Study1F03b)


plot(mod2, se=TRUE, shade=TRUE, shade.col = "rosybrown2")

plot(mod2, se=TRUE, shade=TRUE, shade.col = "rosybrown2", residuals = TRUE)


mod3 <- gam(valence ~ s(time, k=15) + as.factor(Sex), data=Study1F03b)

summary(mod3)

coef(mod3)

plot(mod3, se=TRUE, shade=TRUE, shade.col = "rosybrown2", all.terms=TRUE)

gam.check(mod3, pages=1)


mod4 <- gam(valence ~ s(time, k=15, by = as.factor(Sex)), data=Study1F03b)

summary(mod4)

coef(mod4)

plot(mod4, se=TRUE, shade=TRUE, shade.col = "rosybrown2")

gam.check(mod4, pages=1)


mod5 <- gam(valence ~ s(time, k=15, by = as.factor(Sex)) + as.factor(Sex), data=Study1F03b)

summary(mod5)

coef(mod5)

plot(mod5, se=TRUE, shade=TRUE, shade.col = "rosybrown2")

gam.check(mod5, pages=1)
