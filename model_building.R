source('libraries.R')

# Build models for desk reject

# create working copy
dec0 <- dec

# Sent for Review ; looking at desk rejects in this section
dec0                 <- select(dec0, sent_for_review, first_auth_geog, english, HDI)
dec0$HDI_10          <- dec0$HDI * 10
dec0                 <- dec0[complete.cases(dec0),]
dec0$sent_for_review <- relevel(dec0$sent_for_review, ref = "No")
dec0$first_auth_geog <- relevel(dec0$first_auth_geog, ref = "Europe")
dec0$english         <- factor(dec0$english)
dec0$english         <- relevel(dec0$english, ref = "TRUE")

summary(dec0)

contrasts(dec0$sent_for_review)
contrasts(dec0$first_auth_geog)
contrasts(dec0$english)

fit.0 <- glm(sent_for_review ~ first_auth_geog,
             data = dec0, family = "binomial")

fit.1 <- glm(sent_for_review ~ first_auth_geog + english,
             data = dec0, family = "binomial")

fit.2 <- glm(sent_for_review ~ first_auth_geog + english + HDI_10,
             data = dec0, family = "binomial")

fit.3 <- glm(sent_for_review ~ english + HDI_10,
             data = dec0, family = "binomial")

fit.4 <- glm(sent_for_review ~ HDI_10,
             data = dec0, family = "binomial")

fit.5 <- glm(sent_for_review ~ english,
             data = dec0, family = "binomial")

summary(fit.0)
summary(fit.1)
summary(fit.2)
summary(fit.3)
summary(fit.4)
summary(fit.5)

round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)
round(exp(cbind(OR = coef(fit.5), confint(fit.5))), 3)

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
roc_curve(fit.1, dec0)
roc_curve(fit.2, dec0)
roc_curve(fit.3, dec0)
roc_curve(fit.4, dec0)
roc_curve(fit.5, dec0)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 1)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 3)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 4)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 5)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 6)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 7)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7)

# 1. The reduction in the deviance; results in the chi square statistic
# 2. The degrees of freedom for the chi square statistic
# 3. The probability associated with the chi-square statistic. If (e.g.) less
# than 0.05, we can reject the null hypothesis that the model is not better
# than chance at predicting the outcome
# 4. Display the results
fit.chi <- fit.0$null.deviance - fit.0$deviance
chi.df  <- fit.0$df.null - fit.0$df.residual
chisq.prob <- 1 - pchisq(fit.chi, chi.df)
fit.chi; chi.df; chisq.prob

rm(dec0, fit.0, fit.1, fit.2, fit.3, fit.4, fit.5, roc_curve)

# Build models for paper reject

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

fit.0 <- glm(paper_rejected ~ first_auth_geog, data = dec_sent, family = "binomial")
fit.1 <- glm(paper_rejected ~ first_auth_geog + english, data = dec_sent, family = "binomial")
fit.2 <- glm(paper_rejected ~ first_auth_geog + english + HDI10, data = dec_sent, family = "binomial")
fit.3 <- glm(paper_rejected ~ first_auth_geog + english + HDI10 + mean_review_score,
             data = dec_sent, family = "binomial")
fit.4 <- glm(paper_rejected ~ english + HDI10 + mean_review_score, data = dec_sent, family = "binomial")
fit.5 <- glm(paper_rejected ~ english + HDI10, data = dec_sent, family = "binomial")
fit.6 <- glm(paper_rejected ~ english, data = dec_sent, family = "binomial")
fit.7 <- glm(paper_rejected ~ HDI10 + mean_review_score, data = dec_sent, family = "binomial")
fit.8 <- glm(paper_rejected ~ HDI10, data = dec_sent, family = "binomial")
fit.9 <- glm(paper_rejected ~ mean_review_score, data = dec_sent, family = "binomial")

# Investigate ROC curve ; be sure to substitute "sent_for_review" out if using
# in other functions
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(paper_rejected ~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

roc_curve(fit.0, dec_sent)
roc_curve(fit.1, dec_sent)
roc_curve(fit.2, dec_sent)
roc_curve(fit.3, dec_sent)
roc_curve(fit.4, dec_sent)
roc_curve(fit.5, dec_sent)
roc_curve(fit.6, dec_sent)
roc_curve(fit.7, dec_sent)
roc_curve(fit.8, dec_sent)
roc_curve(fit.9, dec_sent)

vif(fit.1)
vif(fit.2)
vif(fit.4)

summary(fit.0)
summary(fit.1)
summary(fit.2)
summary(fit.3)
summary(fit.4)
summary(fit.5)
summary(fit.6)
summary(fit.7)
summary(fit.8)
summary(fit.9)

round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)
round(exp(cbind(OR = coef(fit.5), confint(fit.5))), 3)
round(exp(cbind(OR = coef(fit.6), confint(fit.6))), 3)
round(exp(cbind(OR = coef(fit.7), confint(fit.7))), 3)
round(exp(cbind(OR = coef(fit.8), confint(fit.8))), 3)
round(exp(cbind(OR = coef(fit.9), confint(fit.9))), 3)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7)

# The reduction in the deviance; results in the chi square statistic
fit.chi     <- fit.0$null.deviance - fit.0$deviance
# The degrees of freedom for the chi square statistic
chi.df      <- fit.0$df.null - fit.0$df.residual
# The probability associated with the chi-square statistic
# If (e.g.) less than 0.05, we can reject the null hypothesis that the model
# is not better than chance at predicting the outcome
chisq.prob  <- 1 - pchisq(fit.chi, chi.df) 
# display the results
fit.chi ; chi.df ; chisq.prob

# reorder_size <- function(x) {
#         factor(x, levels = names(sort(table(x), decreasing = TRUE)))
# }
# 
# ggplot(dec_sent, aes(x = reorder_size(first_auth_geog), fill = paper_rejected)) +
#         geom_bar() + theme_bw() + 
#         scale_fill_grey(name = "Likely Not Published") +
#         labs(x = "Geographical Region of First Author",
#              y = "Count") +
#         theme(axis.text.y = element_text(size = 12,
#                                          colour = "black")) +
#         theme(axis.text.x = element_text(size = 12,
#                                          colour = "black")) +
#         theme(legend.position = c(.8,.8))

rm(fit.0, fit.chi, chi.df, chisq.prob, dec_sent)
rm(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, roc_curve)
