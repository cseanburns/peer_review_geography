source("libraries.R")

dec_sent                 <- filter(dec, sent_for_review == "Yes")
dec_sent                 <- select(dec_sent, ms_id, paper_rejected, first_auth_geog)
dec_sent                 <- dec_sent[complete.cases(dec_sent),]
dec_sent$paper_rejected  <- relevel(dec_sent$paper_rejected, ref = "Yes")
dec_sent$first_auth_geog <- relevel(dec_sent$first_auth_geog, ref = "Europe")

# Authors by regions and comparing to paper rejections
# Focus on data set filtered by sent for review

contrasts(dec_sent$paper_rejected)
contrasts(dec_sent$first_auth_geog)

fit.0 <- glm(paper_rejected ~ first_auth_geog, data = dec_sent, family = "binomial")

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

summary(fit.0)

round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 1)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 3)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 4)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 5)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 6)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 7)
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

rm(fit.0, fit.chi, chi.df, chisq.prob, dec_sent, roc_curve)
