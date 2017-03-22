source("libraries.R")

# Editors by geographic regions, comparing to sent for review
dec0 <- select(dec, sent_for_review, handling_editor_geog, handling_editor)
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
Anova(fit.0)

# Add handling_editor ID as a random effect
# Code derived from: http://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
fit.1 <- glmer(sent_for_review ~ handling_editor_geog + (1 | handling_editor),
             data = dec0, family = "binomial", control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 10)

summary(fit.1)
print(fit.1, corr = FALSE)

se <- sqrt(diag(vcov(fit.1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(fit.1), LL = fixef(fit.1) - 1.96 * se, UL = fixef(fit.1) + 1.96 *
                      se))
exp(tab)
Anova(fit.1)

# Test the overall effect of the levels
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 1)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 3)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 4)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 5)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 6)
wald.test(b = coef(fit.0), Sigma = vcov(fit.0), Terms = 2:6)

# ROC Curve
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(sent_for_review~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

roc_curve(fit.0, dec0)

rm(fit.0, fit.1, dec0, fit.chi, chi.df, chisq.prob, roc_curve, tab, se)

# Editors by geographic regions, comparing to paper rejections 
# Focus on sent for review data and not all data
dec_sent <- filter(dec, sent_for_review == "Yes")

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

# Add handling_editor ID as a random effect
fit.1 <- glmer(paper_rejected ~ handling_editor_geog + (1 | handling_editor),
             data = dec_sent, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 10)

summary(fit.1)
print(fit.1, corr = FALSE)

se <- sqrt(diag(vcov(fit.1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(fit.1), LL = fixef(fit.1) - 1.96 * se, UL = fixef(fit.1) + 1.96 *
                      se))
exp(tab)
Anova(fit.1)

# ROC Curve
roc_curve <- function(model, dataset) {
        prob <- predict(model, type = c("response"))
        dataset$prob <- prob
        g <- roc(paper_rejected ~ prob, data = dataset)
        pg <- plot(g)
        return(list(plot(pg)))
}

rm(dec_sent, dec0, dec_sent, fit.0)
rm(tab, fit.1, p, q, se)
