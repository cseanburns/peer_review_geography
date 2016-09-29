# Recruiter difficulty by author region

# Mean Reviewer Days to Respond

source("libraries.R")

dec0 <- dec

dec0$paper_rejected       <- relevel(dec0$paper_rejected, ref = "Yes")
dec0$first_auth_geog      <- relevel(dec0$first_auth_geog, ref = "North America")

contrasts(dec0$paper_rejected)
contrasts(dec0$first_auth_geog)

fit.0 <- glm(paper_rejected ~ first_auth_geog + mean_reviewer_days_respond,
             data = dec0, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:8) # test the ranking
1 - pchisq(fit.0$deviance, fit.0$df.residual) # test the model

fit.chi.0     <- fit.0$null.deviance - fit.0$deviance
chi.df.0      <- fit.0$df.null - fit.0$df.residual
chisq.prob.0  <- 1 - pchisq(fit.chi.0, chi.df.0) 
fit.chi.0 ; chi.df.0 ; chisq.prob.0

dec0$prob.0    <- predict(fit.0, type = c("response"))

g.0 <- roc(paper_rejected ~ prob.0, data = dec0) ; g.0
plot(g.0)

# Mean Reviewer Days to Review

fit.1 <- glm(paper_rejected ~ first_auth_geog + mean_reviewer_days_review,
             data = dec0, family = "binomial")
summary(fit.1)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)

# Fix count of terms
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 2:8) # test the ranking
1 - pchisq(fit.1$deviance, fit.1$df.residual) # test the model

fit.chi.1     <- fit.1$null.deviance - fit.1$deviance
chi.df.1      <- fit.1$df.null - fit.1$df.residual

chisq.prob.1  <- 1 - pchisq(fit.chi.1, chi.df.1) 
fit.chi.1 ; chi.df.1 ; chisq.prob.1

dec0$prob.1   <- predict(fit.1, type = c("response"))

g.1 <- roc(paper_rejected ~ prob.1, data = dec0) ; g.1
plot(g.1)

# Proportion of Reviewers Responding

fit.2 <- glm(paper_rejected ~ first_auth_geog + prop_reviewers_responding,
             data = dec0, family = "binomial")
summary(fit.2)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)

# Fix term count
wald.test(b = coef(fit.2), Sigma = vcov(fit.2), Terms = 2:7) # test the ranking
1 - pchisq(fit.2$deviance, fit.2$df.residual) # test the model

fit.chi.2     <- fit.2$null.deviance - fit.2$deviance
chi.df.2      <- fit.2$df.null - fit.2$df.residual

chisq.prob.2  <- 1 - pchisq(fit.chi.2, chi.df.2) 
fit.chi.2 ; chi.df.2 ; chisq.prob.2

dec0$prob.2   <- predict(fit.2, type = c("response"))
g.2 <- roc(paper_rejected ~ prob.2, data = dec0) ; g.2
plot(g.2)

# Proportion of Reviewers Agreeing to Review

fit.3 <- glm(paper_rejected ~ first_auth_geog + prop_reviewers_agreeing,
             data = dec0, family = "binomial")
summary(fit.3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)

wald.test(b = coef(fit.3), Sigma = vcov(fit.3), Terms = 2:7) # test the ranking
1 - pchisq(fit.3$deviance, fit.3$df.residual) # test the model

fit.chi.3     <- fit.3$null.deviance - fit.3$deviance
chi.df.3      <- fit.3$df.null - fit.3$df.residual

chisq.prob.3  <- 1 - pchisq(fit.chi.3, chi.df.3) 
fit.chi.3 ; chi.df.3; chisq.prob.3

dec0$prob.3 <- predict(fit.3, type = c("response"))
g.3 <- roc(paper_rejected ~ prob.3, data = dec0) ; g.3
plot(g.3)

## Mean Reviewers Days to Review

fit.4 <- glm(paper_rejected ~ first_auth_geog + mean_reviewer_days_review,
             data = dec0, family = "binomial")
summary(fit.4)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)

wald.test(b = coef(fit.4), Sigma = vcov(fit.4), Terms = 2:8) # test the ranking
1 - pchisq(fit.4$deviance, fit.4$df.residual) # test the model

fit.chi.4      <- fit.4$null.deviance - fit.4$deviance
chi.chi.4      <- fit.4$df.null - fit.4$df.residual

chisq.prob.4   <- 1 - pchisq(fit.chi.4, chi.df.4) 
fit.chi.4 ; chi.df.4 ; chisq.prob.4

dec0$prob.4 <- predict(fit.4, type = c("response"))
g.4 <- roc(paper_rejected ~ prob.4, data = dec0) ; g.4
plot(g.4)

# ---

ggplot(dec0, aes(x = first_auth_geog, y = mean_reviewer_days_review)) +
        geom_boxplot() + facet_grid(. ~ paper_rejected) + theme_bw() +
        xlab("Geographical Region of First Author") +
        ylab("Mean Days to Review") +
        ggtitle("Paper Rejected") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

fit.5 <- glm(paper_rejected ~ 1, data = dec0, family = "binomial")

anova(fit.1, fit.5, test = "Chisq")
anova(fit.2, fit.5, test = "Chisq")
anova(fit.3, fit.5, test = "Chisq")
anova(fit.4, fit.5, test = "Chisq")

rm(fit.0, fit.1, fit.2, fit.3, fit.4, fit.5,
   fit.chi.0, fit.chi.1, fit.chi.2, fit.chi.3, fit.chi.4,
   chi.df.0, chi.df.1, chi.df.2, chi.df.3, chi.df.4,
   chisq.prob.0, chisq.prob.1, chisq.prob.2, chisq.prob.3, chisq.prob.4,
   g.0, g.1., g.2, g.3, g.4,
   dec0)
