# libraries - probably provided in the template file
library(mgcv)
library(gratia)

# assume datadrame df has x, y, and p, q, w elements ...
df <- read.csv("filename.csv")
head(df)
str(df)
summary(df)

### Linear/GL Models

# creating linear and gam models from a dataframe df, not all options needed
model <- lm(y ~ x + p, data = df)
model <- gam(y ~ s(x, p, k = 7) + s(x, q, k = 9, bs = "cr"), + w, data = df, method = "REML")

# extract and plot the basis functions
bs <- basis(s(x, p, k = 7), data = df)
draw(bs)

# functions to call on models
summary(model)
predict(model)
residuals(model)
coef(model)
gam.check(model)
k.check(model) # version of gam.check without plots
appraise(model) # version of gam.check with only (nice) plots
anova(model) # in file but not used

# plot models (without ggplot)
plot(model, residuals = TRUE, se = TRUE, rug = FALSE, pch = 1, cex = 1, shade = TRUE, shade.col = "rosybrown2")
draw(model, rug = FALSE)

# ggplot temaplte
df$predicted <- predict(model)
df$residuals <- residuals(model)
ggplot(data = df, aes(x = x, y = y)) +
geom_point(colour = "red") +
geom_smooth(method = "lm", se = FALSE, size = 1, colour = "green") +
geom_path(aes(x = x, y = predicted), colour = "blue") +
geom_point(aes(y = predicted), shape = 1) +
geom_segment(aes(xend = x, yend = predicted), alpha = .5) +
geom_text(aes(label = paste0("", round(residuals, 1), "")), size = 2.5, nudge_x = 0.1)

### IRR

# give (and remove) names from columns
# rows are subjects/observations, columns are raters
df <- read.csv("filename.csv")
names(df) <- c("A", "B")
df <- unname(df)
fd <- t(df)

# IRR measures
agree(df) # percentage agreement
kappa2(df) # Cohen's Kappa - fully crossed, two coders
icc(df, model = "oneway", type = "consistency", unit = "single")
icc(df, model = "twoway", type = "agreement", unit = "average")
iccNA(df) # does all six options
iccNA(df)$ICCs[2, ] # get second ICC results
kripp.alpha(t(df), "nominal")
kripp.alpha(t(df), "ordinal")
kripp.alpha(t(df), "ratio")
kripp.alpha(t(df), "interval") # same as ICC(1)

# Kappa Values
# 0.00 to 0.20 indicating slight agreement,
# 0.21 to 0.40 indicating fair agreement,
# 0.41 to 0.60 indicating moderate agreement,
# 0.61 to 0.80 indicating substantial agreement,
# 0.81 to 1.00 indicating almost perfect or perfect agreement.
# negative values same but for disagreement.

# Kappa Values by Krissendorf (not really used)
# 0.00 to 0.66 no real agreement,
# 0.67 to 0.80 tentative agreement,
# 0.81 to 1.00 indicating almost perfect or perfect agreement.

# ICC Values
# 0.00 to 0.39 poor agreement,
# 0.40 to 0.59 fair agreement,
# 0.60 to 0.74 good agreement,
# 0.75 to 1.00 excellent agreement.



# table template
knitr::kable(df, caption = "A caption", align = "cccccc") %>%
kable_styling(full_width = FALSE) %>%
column_spec(1, bold = TRUE, border_left = FALSE, border_right = TRUE) %>%
column_spec(2:7, background = "white")
