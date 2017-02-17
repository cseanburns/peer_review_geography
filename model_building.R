# 1. select all papers sent for review
# 2. remove papers with single authors
# 3. look at paper_rejected ~ geographic region of first authors +
#                             HDI + english

dec_sent <- dec %>% filter(sent_for_review == "Yes") %>%
        select(ms_id, first_auth_geog, paper_rejected)

dec_socio <- author_decisions %>%
        filter(author_order == 1) %>%
        select(ms_id, HDI, language)

# identify single authorships
singles.df <- select(author_decisions, ms_id)
singles.df <- data.frame(table(singles.df))
singles.df <- singles.df %>% filter(Freq == 1)
singles.df$singles.df <- as.integer(as.character(singles.df$singles.df))
singles <- singles.df$singles.df

dec_socio <- dec_socio[!(dec_socio$ms_id %in% singles),]

# join data frames for analysis
dec_sent_socio <- inner_join(dec_sent,
                             dec_socio,
                             by = "ms_id")

rm(singles, singles.df, dec_sent, dec_socio)

# convert language variable to English factor
dec_sent_socio$english <- ifelse(dec_sent_socio$language == "English", TRUE, FALSE)
dec_sent_socio$english <- factor(dec_sent_socio$english, ordered = FALSE)
dec_sent_socio$language <- NULL

# set contrasts
dec_sent_socio$first_auth_geog <- relevel(dec_sent_socio$first_auth_geog,
                                          ref = "Europe")
dec_sent_socio$paper_rejected  <- relevel(dec_sent_socio$paper_rejected,
                                          ref = "Yes")
dec_sent_socio$english <- relevel(dec_sent_socio$english, ref = "TRUE")

# remove NA cases
dec_sent_socio <- dec_sent_socio %>% filter(!is.na(first_auth_geog))

# build models
fit.1 <- glm(paper_rejected ~ first_auth_geog,
             data = dec_sent_socio,
             family = "binomial")

fit.2 <- glm(paper_rejected ~ first_auth_geog + HDI,
             data = dec_sent_socio,
             family = "binomial")

fit.3 <- glm(paper_rejected ~ first_auth_geog + english,
             data = dec_sent_socio,
             family = "binomial")

fit.4 <- glm(paper_rejected ~ first_auth_geog + HDI + english,
             data = dec_sent_socio,
             family = "binomial")

fit.5 <- glm(paper_rejected ~ HDI,
             data = dec_sent_socio,
             family = "binomial")

fit.6 <- glm(paper_rejected ~ english,
             data = dec_sent_socio,
             family = "binomial")

fit.7 <- glm(paper_rejected ~ HDI + english,
             data = dec_sent_socio,
             family = "binomial")

# Summary information
summary(fit.1)
summary(fit.2)
summary(fit.3)
summary(fit.4)
summary(fit.5)
summary(fit.6)
summary(fit.7)

# Odds ratios and confidence intervals, exponentiated
round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)
round(exp(cbind(OR = coef(fit.2), confint(fit.2))), 3)
round(exp(cbind(OR = coef(fit.3), confint(fit.3))), 3)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)
round(exp(cbind(OR = coef(fit.5), confint(fit.5))), 3)
round(exp(cbind(OR = coef(fit.6), confint(fit.6))), 3)
round(exp(cbind(OR = coef(fit.7), confint(fit.7))), 3)

# Compare model to null
chi_log_test <- function(log_model) {
        fit.chi <- log_model$null.deviance - log_model$deviance
        chi.df <- log_model$df.null - log_model$df.residual
        chisq.prob <- 1 - pchisq(fit.chi, chi.df)
        cat("The chi-square for the model is  ", fit.chi, "\n")
        cat("The degrees of freedom for the model chi-square is  ", chi.df, "\n")
        cat("The probability associated with the model chi-square is  ",
            chisq.prob, "\n")
}

chi_log_test(fit.1)
chi_log_test(fit.2)
chi_log_test(fit.3)
chi_log_test(fit.4)
chi_log_test(fit.5)
chi_log_test(fit.6)
chi_log_test(fit.7)

# Compare models
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

fit.1$call ; fit.2$call
chi_compare(fit.1, fit.2)

fit.1$call ; fit.3$call
chi_compare(fit.1, fit.3)

fit.1$call ; fit.4$call
chi_compare(fit.1, fit.4)

fit.5$call ; fit.7$call
chi_compare(fit.5, fit.7)

anova(fit.1, fit.2)
anova(fit.1, fit.3)
anova(fit.1, fit.4)
anova(fit.5, fit.7)

# Testing for linearity of the logit for the continuous variable in Model 7
# log_hdi_Int is not significant and the assumption is therefore not violated
dec_sent_socio$log_hdi_Int <- log(dec_sent_socio$HDI)*dec_sent_socio$HDI
fit.ll.1 <- glm(paper_rejected ~ HDI + english + log_hdi_Int,
                data = dec_sent_socio, family = "binomial")
summary(fit.ll.1)
rm(fit.ll.1)

rm(dec_lang, dec_lang_2, mixed.combined)
rm(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7)