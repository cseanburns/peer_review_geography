# Rejections

source("libraries.R")

dec0 <- dec

dec0 <- select(dec0, paper_rejected, first_auth_geog)
dec0 <- filter(dec0, !is.na(first_auth_geog))
dec0 <- filter(dec0, !is.na(paper_rejected))

dec0$paper_rejected_2 <- relevel(dec0$paper_rejected, ref = "Yes")
dec0$first_auth_geog  <- relevel(dec0$first_auth_geog, ref = "United Kingdom")

summary(dec0)

contrasts(dec0$paper_rejected)
contrasts(dec0$paper_rejected_2)
contrasts(dec0$first_auth_geog)

fit.0 <- glm(paper_rejected_2 ~ first_auth_geog, data = dec0, family = "binomial")
summary(fit.0)
round(exp(cbind(OR = coef(fit.0), confint(fit.0))), 3)

first.author.tbl <- table(decisions4$paper_rejected_2, decisions4$first_auth_geog)
first.author.tbl
round(first.author.tbl[2,]/first.author.tbl[1,],4)

ggplot(dec0, aes(x = first_auth_geog, fill = paper_rejected_2)) +
       geom_bar() + theme_bw() + scale_fill_grey(name = "Paper Rejected") +
       # facet_grid(. ~ first_auth_geog) +
       xlab("Paper Rejected") +
       ylab("Count") +
       ggtitle("First Author Region") +
       theme(legend.position = c(.9, .8))

rm(dec0, fit.0, first.author.tbl)

# Rejections by first/submitting author

dec0 <- dec

dec0 <- select(dec0, paper_rejected, first_auth_geog, submit_auth_first_auth, author_count)

accept_reject_fn <- function(x) {
  dec0 %>%
    filter(submit_auth_first_auth == "Yes",
           author_count > 1,
           paper_rejected == x) %>%
  select(first_auth_geog) %>%
  table()
}

acc_rej_no  <- accept_reject_fn("No") / (accept_reject_fn("No") + accept_reject_fn("Yes"))
acc_rej_yes <- accept_reject_fn("Yes") / (accept_reject_fn("Yes") + accept_reject_fn("No"))

round(acc_rej_no, 2) # acceptance rate
round(acc_rej_yes, 2) # rejection rate

#### this needs to be fixed -- senior author removed ####
geo_plot <- function(z) {
  data.frame(z) %>%
    ggplot(aes(x = senior_auth_geog,
               y = first_auth_geog)) + geom_tile(aes(fill = Freq),
           colour = "white") + theme_classic() +
           scale_fill_gradient(low = "white", high = "black") +
           xlab("Geographic Region of Last/Senior Author") +
           ylab("Geographic Region of First Author")
}

geo_plot(acc_rej_no)  + ggtitle("Acceptance Rate")
geo_plot(acc_rej_yes) + ggtitle("Rejection Rate")

# acceptance rate if editor is from same geographical location as the 
# submitting, first author

editor_same <- function(x) {
        decisions %>%
                filter(submit_auth_first_auth == "Yes",
                       !is.na(handling_editor_geog),
                       paper_rejected == x,
                       handlingEditorGeog == first_auth_geog) %>%
                select(first_auth_geog, senior_auth_geog) %>%
                table()
}

# acceptance / rejection rates
es.no <- editor_same("No") / (editor_same("No") + editor_same("Yes"))
es.yes <- editor_same("Yes") / (editor_same("Yes") + editor_same("No"))
round(es.no, 2)
round(es.yes, 2)

geo_plot(es.no)  + ggtitle("Acceptance Rate")
geo_plot(es.yes) + ggtitle("Rejection Rate")

# acceptance rate if editor is in a different geographical location than the 
# submitting, first author
        
editor_different <- function(x) {
  decisions %>%
    filter(submit_auth_first_auth == "Yes",
           !is.na(handlingEditorGeog),
           paper_rejected == x,
           handling_editor_geog != first_auth_geog) %>%
  select(first_auth_geog, senior_auth_geog) %>%
  table()
}

# acceptance / rejection rates
er.no  <- editor_different("No") / (editor_different("No") + editor_different("Yes"))
er.yes <- editor_different("Yes") / (editor_different("Yes") + editor_different("No"))

round(er.no, 2)
round(er.yes, 2)

geo_plot(er.no)  + ggtitle("Acceptance Rate")
geo_plot(er.yes) + ggtitle("Rejection Rate")

rm(dec0, accept_reject_fn, acc_rej_no, acc_rej_yes, geo_plot, editor_same, es.no
   es.yes, editor_different, er.no, er.yes)
