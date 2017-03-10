source('libraries.R')

# Set up for comparing models
chi_compare <- function(model.1, model.2) {
       models.chi <- model.1$deviance - model.2$deviance
       models.df  <- model.1$df.residual - model.2$df.residual
       chi.prob   <- 1 - pchisq(models.chi, models.df)
       cat("models.chi is", models.chi, "\n")
       cat("models.df is", models.df, "\n")
       cat("chi.prob is", chi.prob, "\n")
       cat("If chi.prob is less than 0.05, then the second model is a signifcant improvement over the first model", "\n")
       cat("If applicable, then the second model is", exp((model.1$aic - model.2$aic)/2), "\n")
           cat("times as probable as the first model, minimizing information loss.")
}

# Build models for desk reject

# create working copy
dec0 <- dec

# Sent for Review ; looking at desk rejects in this section
dec0                 <- select(dec0, sent_for_review, first_auth_geog, english, HDI)
dec0                 <- dec0[complete.cases(dec0),]
dec0$HDI_10          <- dec0$HDI * 10
dec0$sent_for_review <- relevel(dec0$sent_for_review, ref = "No")
dec0$first_auth_geog <- relevel(dec0$first_auth_geog, ref = "Europe")
dec0$english         <- factor(dec0$english)
dec0$english         <- relevel(dec0$english, ref = "TRUE")

summary(dec0)

contrasts(dec0$sent_for_review)
contrasts(dec0$first_auth_geog)
contrasts(dec0$english)

fit.1 <- glm(sent_for_review ~ first_auth_geog,
             data = dec0, family = "binomial")
fit.2 <- glm(sent_for_review ~ first_auth_geog + english,
             data = dec0, family = "binomial")
fit.3 <- glm(sent_for_review ~ first_auth_geog + HDI_10,
             data = dec0, family = "binomial")
fit.4 <- glm(sent_for_review ~ first_auth_geog + english + HDI_10,
             data = dec0, family = "binomial")

summary(fit.1)
summary(fit.2)
summary(fit.3)
summary(fit.4)

round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)

# Investigate ROC curve ; be sure to substitute "sent_for_review" out if using
# in other functions
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(sent_for_review ~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

roc_curve(fit.1, dec0)
roc_curve(fit.2, dec0)
roc_curve(fit.3, dec0)
roc_curve(fit.4, dec0)

# Test the overall effect of the levels
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 1)
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 2)
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 3)
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 4)
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 5)
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 6)
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 7)
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 2:7)

# 1. The reduction in the deviance; results in the chi square statistic
# 2. The degrees of freedom for the chi square statistic
# 3. The probability associated with the chi-square statistic. If (e.g.) less
# than 0.05, we can reject the null hypothesis that the model is not better
# than chance at predicting the outcome
# 4. Display the results
fit.chi <- fit.1$null.deviance - fit.1$deviance
chi.df  <- fit.1$df.null - fit.1$df.residual
chisq.prob <- 1 - pchisq(fit.chi, chi.df)
fit.chi; chi.df; chisq.prob

chi_compare(fit.1, fit.2)
chi_compare(fit.1, fit.3)
chi_compare(fit.1, fit.4)
chi_compare(fit.3, fit.4)

# Test linearity of the logit
dec0$log_hdi10 <- log(dec0$HDI_10) * dec0$HDI_10
fit.5 <- glm(sent_for_review ~ first_auth_geog + HDI_10 + log_hdi10, data = dec0, family = "binomial")
fit.6 <- glm(sent_for_review ~ first_auth_geog + english + HDI_10 + log_hdi10,
             data = dec0, family = "binomial")
summary(fit.5)
summary(fit.6)

rm(dec0, fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, roc_curve)
rm(chi.df, chisq.prob, fit.chi)

# Build models for mean review score
# Resetart R to avoid package conflicts
require("ggplot2")
require("MASS")
require("Hmisc")
require("plyr")
library("pROC")
require("dplyr")

dec_score <- dplyr::filter(dec, sent_for_review == "Yes")
dec_score <- dplyr::select(dec_score, mean_review_score,
                           first_auth_geog, english, HDI)

dec_score$HDI10 <- dec_score$HDI * 10

dec_score <- dec_score[complete.cases(dec_score),]

dec_score$first_auth_geog <- relevel(dec_score$first_auth_geog, ref = "Europe")
dec_score$english         <- factor(dec_score$english)
dec_score$english         <- relevel(dec_score$english, ref = "TRUE")

mean.rs <- cut(dec_score$mean_review_score, breaks = 3)
mean.rs <- revalue(mean.rs, c("(0.997,2]" = "1",
                              "(2,3]" = "2",
                              "(3,4]" = "3"))

mean.rs <- factor(mean.rs, labels = c("Low", "Middle", "High"))
dec_score$mean.rs <- mean.rs

rm(mean.rs)

contrasts(dec_score$mean.rs)
contrasts(dec_score$first_auth_geog)
contrasts(dec_score$english)

ftable(xtabs(~ mean.rs + first_auth_geog, data = dec_score))

fit.1 <- polr(mean.rs ~ first_auth_geog, data = dec_score, Hess = TRUE)
fit.2 <- polr(mean.rs ~ first_auth_geog + english, data = dec_score, Hess = TRUE)
fit.3 <- polr(mean.rs ~ first_auth_geog + HDI10, data = dec_score, Hess = TRUE)
fit.4 <- polr(mean.rs ~ first_auth_geog + english + HDI10, data = dec_score, Hess = TRUE)

summary(fit.1, digits = 3)
summary(fit.2, digits = 3)
summary(fit.3, digits = 3)
summary(fit.4, digits = 3)

(ctable1 <- coef(summary(fit.1)))
(ctable2 <- coef(summary(fit.2)))
(ctable3 <- coef(summary(fit.3)))
(ctable4 <- coef(summary(fit.4)))

p1 <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
p2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
p3 <- pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2
p4 <- pnorm(abs(ctable4[, "t value"]), lower.tail = FALSE) * 2

(ctable1 <- cbind(ctable1, "p value" = p1))
(ctable2 <- cbind(ctable2, "p value" = p2))
(ctable3 <- cbind(ctable3, "p value" = p3))
(ctable4 <- cbind(ctable4, "p value" = p4))

round(ctable1, 3)
round(ctable2, 3)
round(ctable3, 3)
round(ctable4, 3)

(ci1 <- confint(fit.1))
(ci2 <- confint(fit.2))
(ci3 <- confint(fit.3))
(ci4 <- confint(fit.4))

round(exp(cbind(OR = coef(fit.1), ci1)), 3)
round(exp(cbind(OR = coef(fit.2), ci2)), 3)
round(exp(cbind(OR = coef(fit.3), ci3)), 3)
round(exp(cbind(OR = coef(fit.4), ci4)), 3)

chi_compare(fit.1, fit.2) # sig = yes
chi_compare(fit.1, fit.3) # sig = no
chi_compare(fit.1, fit.4) # sig = yes
chi_compare(fit.2, fit.4) # sig = no

rm(ci1, ci2, ci3, ci4)
rm(ctable1, ctable2, ctable3, ctable4)
rm(fit.1, fit.2, fit.3, fit.4)
rm(p1, p2, p3, p4)
rm(dec_score)

# Build models for paper reject
# Restart R after mean review score modeling
source('libraries.R')
dec_sent                 <- filter(dec, sent_for_review == "Yes")
dec_sent                 <- select(dec_sent, ms_id, paper_rejected,
                                   first_auth_geog, english, HDI, mean_review_score)
dec_sent$HDI10           <- dec_sent$HDI * 10
dec_sent                 <- dec_sent[complete.cases(dec_sent),]
dec_sent$paper_rejected  <- relevel(dec_sent$paper_rejected, ref = "Yes")
dec_sent$first_auth_geog <- relevel(dec_sent$first_auth_geog, ref = "Europe")
dec_sent$english         <- factor(dec_sent$english)
dec_sent$english         <- relevel(dec_sent$english, ref = "TRUE")

# Authors by regions and comparing to paper rejections
# Focus on data set filtered by sent for review

contrasts(dec_sent$paper_rejected)
contrasts(dec_sent$first_auth_geog)
contrasts(dec_sent$english)

fit.1 <- glm(paper_rejected ~ first_auth_geog,
             data = dec_sent, family = "binomial")

fit.2 <- glm(paper_rejected ~ first_auth_geog + english,
             data = dec_sent, family = "binomial")
fit.3 <- glm(paper_rejected ~ first_auth_geog + HDI10,
             data = dec_sent, family = "binomial")
fit.4 <- glm(paper_rejected ~ first_auth_geog + mean_review_score,
             data = dec_sent, family = "binomial")

fit.5 <- glm(paper_rejected ~ first_auth_geog + english + HDI10,
             data = dec_sent, family = "binomial")
fit.6 <- glm(paper_rejected ~ first_auth_geog + english + mean_review_score,
             data = dec_sent, family = "binomial")
 
fit.7 <- glm(paper_rejected ~ first_auth_geog + HDI10 + mean_review_score,
             data = dec_sent, family = "binomial")

fit.8 <- glm(paper_rejected ~ first_auth_geog + english + HDI10 + mean_review_score,
             data = dec_sent, family = "binomial")

# Investigate ROC curve ; be sure to substitute "sent_for_review" out if using
# in other functions
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(paper_rejected ~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

roc_curve(fit.1, dec_sent)
roc_curve(fit.2, dec_sent)
roc_curve(fit.3, dec_sent)
roc_curve(fit.4, dec_sent)
roc_curve(fit.5, dec_sent)
roc_curve(fit.6, dec_sent)
roc_curve(fit.7, dec_sent)
roc_curve(fit.8, dec_sent)

vif(fit.2)
vif(fit.4)

summary(fit.1)
summary(fit.2)
summary(fit.3)
summary(fit.4)
summary(fit.5)
summary(fit.6)
summary(fit.7)
summary(fit.8)

round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)
round(exp(cbind(OR = coef(fit.5), confint(fit.5))), 3)
round(exp(cbind(OR = coef(fit.6), confint(fit.6))), 3)
round(exp(cbind(OR = coef(fit.7), confint(fit.7))), 3)
round(exp(cbind(OR = coef(fit.8), confint(fit.8))), 3)

# Test the overall effect of the levels
wald.test(b = coef(fit.1), Sigma = vcov(fit.1), Terms = 2:7)

# The reduction in the deviance; results in the chi square statistic
fit.chi     <- fit.1$null.deviance - fit.1$deviance
# The degrees of freedom for the chi square statistic
chi.df      <- fit.1$df.null - fit.1$df.residual
# The probability associated with the chi-square statistic
# If (e.g.) less than 0.05, we can reject the null hypothesis that the model
# is not better than chance at predicting the outcome
chisq.prob  <- 1 - pchisq(fit.chi, chi.df) 
# display the results
fit.chi ; chi.df ; chisq.prob

chi_compare(fit.1, fit.2)
chi_compare(fit.1, fit.3)
chi_compare(fit.1, fit.4)
chi_compare(fit.1, fit.5)
chi_compare(fit.1, fit.6)
chi_compare(fit.1, fit.7)
chi_compare(fit.1, fit.8)

# no sig diff
chi_compare(fit.4, fit.6)
chi_compare(fit.4, fit.7)
chi_compare(fit.4, fit.8)

# 7 > 6 but no sig dif between 6,8 
chi_compare(fit.6, fit.7)
chi_compare(fit.6, fit.8)

# no sig diff
chi_compare(fit.7, fit.6)
chi_compare(fit.7, fit.8)

# linearity of the logit assumption test
dec_sent$log_hdi10 <- log(dec_sent$HDI10) * dec_sent$HDI10
dec_sent$log_mrs <- log(dec_sent$mean_review_score) * dec_sent$mean_review_score

fit.a <- glm(paper_rejected ~ first_auth_geog + HDI10 + log_hdi10,
             data = dec_sent, family = "binomial")

fit.b <- glm(paper_rejected ~ first_auth_geog + mean_review_score + log_mrs,
             data = dec_sent, family = "binomial")

fit.c <- glm(paper_rejected ~ first_auth_geog + english + HDI10 + log_hdi10,
             data = dec_sent, family = "binomial")
fit.d <- glm(paper_rejected ~ first_auth_geog + english + mean_review_score + log_mrs,
             data = dec_sent, family = "binomial")
 
fit.e <- glm(paper_rejected ~ first_auth_geog + HDI10 + mean_review_score + log_hdi10 + log_mrs,
             data = dec_sent, family = "binomial")

fit.f <- glm(paper_rejected ~ first_auth_geog + english + HDI10 + mean_review_score + log_hdi10 + log_mrs,
             data = dec_sent, family = "binomial")

summary(fit.a) # no violation
summary(fit.b) # no violation
summary(fit.c) # no violation
summary(fit.d) # no violation
summary(fit.e) # no violation
summary(fit.f) # no violation

rm(fit.chi, chi.df, chisq.prob, dec_sent)
rm(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8)
rm(chi_compare, roc_curve)
rm(fit.a, fit.b, fit.c, fit.d, fit.e, fit.f)