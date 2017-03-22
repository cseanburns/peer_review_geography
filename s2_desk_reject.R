source("libraries.R")

# Sent for Review ; looking at desk rejects in this section
dec0 <- dec %>% select(first_auth_geog, sent_for_review, submit_year)
dec0 <- dec0[complete.cases(dec0),]

dec0$sent_for_review <- relevel(dec0$sent_for_review, ref = "No")
dec0$first_auth_geog <- relevel(dec0$first_auth_geog, ref = "Europe")

summary(dec0)

contrasts(dec0$sent_for_review)
contrasts(dec0$first_auth_geog)

fit.0 <- glm(sent_for_review ~ first_auth_geog,
             data = dec0, family = "binomial")

summary(fit.0)

round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)


# Investigate ROC curve ; be sure to substitute "sent_for_review" out if using
# in other functions
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(sent_for_review ~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

roc_curve(fit.0, dec0)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 1)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 3)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 4)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 5)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 6)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 7)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7)

# Model fit
Anova(fit.0)

# Plotting least squares means by submit year
fit.1 <- glm(sent_for_review ~ first_auth_geog + submit_year,
             data = dec0, family = "binomial")

fit.1.ls <- lsmeans(fit.1, "first_auth_geog", by = "submit_year")
plot(fit.1.ls, xlab = "Least-Squares Means", ylab = "First Author Geography",
     main = "For Papers Sent for Review")

rm(dec0, fit.0, fit.chi, chi.df, chisq.prob, roc_curve)
rm(fit.1, fit.1.ls)
