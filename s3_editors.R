source("libraries.R")

# Editors by geographic regions, comparing to sent for review

dec0 <- dec
dec0 <- select(dec0, sent_for_review, handling_editor_geog)
dec0 <- dec0[complete.cases(dec0),]
dec0 <- filter(dec0, handling_editor_geog != "Latin America")

dec0$sent_for_review <- relevel(dec0$sent_for_review, ref = "No")
dec0$handling_editor_geog <- factor(dec0$handling_editor_geog)
dec0$handling_editor_geog <- relevel(dec0$handling_editor_geog,
                                     ref = "Europe")

contrasts(dec0$sent_for_review)
contrasts(dec0$handling_editor_geog)

fit.0 <- glm(sent_for_review ~ handling_editor_geog, data = dec0, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

table(dec0$sent_for_review, dec0$handling_editor_geog)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 1)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 3)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 4)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 5)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 6)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:6)

# The reduction in the deviance; results in the chi square statistic
fit.chi     <- fit.0$null.deviance - fit.0$deviance
# The degrees of freedom for the chi square statistic
chi.df      <- fit.0$df.null - fit.0$df.residual
# The probability associated with the chi-square statisitc
# If (e.g.) less than 0.05, then we can reject the null hypothesis that the model
# is not better than chance at predicting the outcome
chisq.prob  <- 1 - pchisq(fit.chi, chi.df) 
# Display the results
fit.chi ; chi.df ; chisq.prob

# ROC Curve
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(sent_for_review~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

roc_curve(fit.0, dec0)

rm(dec0, chi.df, chisq.prob, fit.chi, fit.0, roc_curve)

# Editors by geographic regions, comparing to paper rejections 
# Focus on sent for review data and not all data

dec0 <- dec
dec_sent <- filter(dec0, sent_for_review == "Yes")

# Get percentage of handling editors by region
sort(table(dec_sent$handling_editor_geog), decreasing = TRUE)
sort(round(table(dec_sent$handling_editor_geog) /
                   sum(table(dec_sent$handling_editor_geog)),3),
     decreasing = TRUE)

# Get percentage of first authors by region
sort(table(dec_sent$first_auth_geog), decreasing = TRUE)
sort(round(table(dec_sent$first_auth_geog) /
                   sum(table(dec_sent$first_auth_geog)),3),
     decreasing = TRUE)

# Get percentage of author geographies to handling editor geographies for
# papers that were sent for review
# Test chisq distribution of handling editors against composition of first author
# geographies
p <- table(dec_sent$first_auth_geog) / sum(table(dec_sent$first_auth_geog))
q <- table(dec_sent$handling_editor_geog)
round(p, 4) ; round(q, 4) ; round(q / sum(q), 4)
round(p / (q / sum(q)), 4)
chisq.test(q, p = p)
round(cbind(p, (q / sum(q))), 3)
round(p / (q / sum(q)), 3)

# Get percentage of author geographies to handling editor geographies for
# all submitted papers
# Test chisq distribution of handling editors against composition of first author
# geographies
p <- table(dec$first_auth_geog) / sum(table(dec$first_auth_geog))
q <- table(dec$handling_editor_geog)
round(p, 4) ; round(q, 4) ; round(q / sum(q), 4)
round(p / (q / sum(q)), 4)
chisq.test(q, p = p)
round(cbind(p, (q / sum(q))), 3)
round(p / (q / sum(q)), 3)

dec_sent$paper_rejected       <- relevel(dec_sent$paper_rejected, ref = "Yes")
dec_sent$handling_editor_geog <- relevel(dec_sent$handling_editor_geog,
                                         ref = "Europe")

contrasts(dec_sent$paper_rejected)
contrasts(dec_sent$handling_editor_geog)

fit.0 <- glm(paper_rejected ~ handling_editor_geog, data = dec_sent, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:7)

# The reduction in the deviance; results in the chi square statistic
fit.chi     <- fit.0$null.deviance - fit.0$deviance
# The degrees of freedom for the chi square statistic
chi.df      <- fit.0$df.null - fit.0$df.residual
# The probability associated with the chi-square statisitc
# If (e.g.) less than 0.05, then we can reject the null hypothesis that the model
# is not better than chance at predicting the outcome
chisq.prob  <- 1 - pchisq(fit.chi, chi.df) 
# Display the results
fit.chi ; chi.df ; chisq.prob

# ROC Curve
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(paper_rejected ~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

# dec_tmp <- select(dec_sent, handling_editor_geog, paper_rejected)
# table(dec_tmp)
# 
# reorder_size <- function(x) {
#         factor(x, levels = names(sort(table(x), decreasing = TRUE)))
# }
# 
# ggplot(dec_tmp, aes(x = reorder_size(handling_editor_geog), fill = paper_rejected)) +
#         geom_bar(stat = "count") + theme_bw() +
#         scale_fill_grey(name = "Revision Invited / Declined") +
#         labs(x = "Geographical Region of Handling Editor",
#         y = "Count") +
#         theme(axis.text.y = element_text(size = 12,
#                                     colour = "black")) +
#         theme(axis.text.x = element_text(size = 12,
#                                     colour = "black")) +
#         theme(legend.position = c(.8,.8))

rm(dec_sent, dec0, dec_sent, fit.0, fit.chi, chi.df, chisq.prob)
