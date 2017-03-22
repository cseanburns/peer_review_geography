source("libraries.R")

dec_sent                 <- filter(dec, sent_for_review == "Yes")
dec_sent                 <- select(dec_sent, ms_id, paper_rejected, first_auth_geog, submit_year)
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
Anova(fit.0)

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

# Plotting least squares means by submit year
fit.1 <- glm(paper_rejected ~ first_auth_geog + submit_year,
             data = dec_sent, family = "binomial")

fit.1.ls <- lsmeans(fit.1, "first_auth_geog", by = "submit_year")
plot(fit.1.ls, xlab = "Least-Squares Means", ylab = "First Author Geography",
     main = "For Papers Not Accepted or Invited to Revise")

rm(dec0, fit.0, fit.chi, chi.df, chisq.prob, roc_curve, fit.1.ls, fit.1)

rm(fit.0, fit.chi, chi.df, chisq.prob, dec_sent, roc_curve)
