source("libraries.R")

## ---- Papers Rejected ----

decisions4 <- select(decisions, paperRejected, firstAuthGeog)
decisions4 <- filter(decisions4, !is.na(firstAuthGeog))
decisions4 <- filter(decisions4, !is.na(paperRejected))
decisions4$pr2 <- relevel(decisions4$paperRejected, ref = "Yes")
decisions4$firstAuthGeog <- relevel(decisions4$firstAuthGeog, ref = "United Kingdom")
summary(decisions4)
contrasts(decisions4$paperRejected)
contrasts(decisions4$pr2)
contrasts(decisions4$firstAuthGeog)
fit.4 <- glm(pr2 ~ firstAuthGeog, data = decisions4, family = "binomial")
summary(fit.4)
round(exp(cbind(OR = coef(fit.4), confint(fit.4))), 3)
# fasa <- table(decisions2$paperRejected, decisions2$firstAuthGeog)
# fasa
# round(fasa[2,]/fasa[1,],4)
fasa <- table(decisions4$pr2, decisions4$firstAuthGeog)
fasa
round(fasa[2,]/fasa[1,],4)

ggplot(decisions4, aes(x = firstAuthGeog, fill = paperRejected)) +
        geom_bar() + theme_bw() + scale_fill_grey(name = "Paper Rejected") +
        # facet_grid(. ~ firstAuthGeog) +
        xlab("Paper Rejected") +
        ylab("Count") +
        ggtitle("First Author Region") +
        theme(legend.position = c(.9, .8))

# decisions2 <- select(decisions, paperRejected, seniorAuthGeog)
# decisions2 <- filter(decisions2, !is.na(seniorAuthGeog))
# decisions2 <- filter(decisions2, !is.na(paperRejected))
# summary(decisions2)
# fit.1 <- glm(paperRejected ~ seniorAuthGeog, data = decisions2, family = "binomial")
# summary(fit.1)
# round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 3)
# ggplot(decisions2, aes(x = paperRejected)) +
#         geom_bar() + theme_bw() +
#         facet_grid(. ~ seniorAuthGeog) +
#         xlab("Paper Rejected") +
#         ylab("Count") +
#         ggtitle("Senior Author Region")
# 
# decisions2 <- select(decisions, paperRejected, firstAuthGeog, seniorAuthGeog)
# decisions2 <- filter(decisions2, firstAuthGeog != seniorAuthGeog)
# summary(decisions2)
# fit.1 <- glm(paperRejected ~ firstAuthGeog * seniorAuthGeog, family = "binomial", data = decisions2)
# summary(fit.1)
# round(exp(cbind(OR = coef(fit.1), confint(fit.1))), 2)
# ggplot(decisions2, aes(x = paperRejected)) +
#         geom_bar() + theme_bw() +
#         facet_grid(firstAuthGeog ~ seniorAuthGeog, labeller = label_both) +
#         xlab("Paper Rejected") +
#         ylab("Count")

# acceptance rates for same and international collaborations (first and senior author):

# x = Yes : paper is rejected
# x = No : paper is accepted

acceptReject <- function(x) {
        decisions %>%
                filter(submitAuthFirstAuth == "Yes",
                       authorCount > 1,
                       paperRejected == x) %>%
                select(firstAuthGeog, seniorAuthGeog) %>%
                table() }

arNo <- acceptReject("No") /
        (acceptReject("No") + acceptReject("Yes"))

arYes <- acceptReject("Yes") /
        (acceptReject("Yes") + acceptReject("No"))

round(arNo,2) # acceptance rate
round(arYes,2) # rejection rate

geoPlot <- function(z) {
        data.frame(z) %>%
        ggplot(aes(x = seniorAuthGeog,
                   y = firstAuthGeog)) +
        geom_tile(aes(fill = Freq),
                  colour = "white") +
        theme_classic() +
        scale_fill_gradient(low = "white", high = "black") +
        xlab("Geographic Region of Last/Senior Author") +
        ylab("Geographic Region of First Author")
}

geoPlot(arNo) + ggtitle("Acceptance Rate")
geoPlot(arYes) + ggtitle("Rejection Rate")

----

# acceptance rate if editor is from same geographical location as the submitting, first author

editorSame <- function(x) {
        decisions %>%
                filter(submitAuthFirstAuth == "Yes",
                       !is.na(handlingEditorGeog),
                       paperRejected == x,
                       handlingEditorGeog == firstAuthGeog) %>%
                select(firstAuthGeog, seniorAuthGeog) %>%
                table()
}

esNo <- editorSame("No") /                      # acceptance rate
        (editorSame("No") + editorSame("Yes"))

esYes <- editorSame("Yes") /                    # rejection rate
        (editorSame("Yes") + editorSame("No"))

round(esNo,2)
round(esYes,2)

geoPlot(esNo) + ggtitle("Acceptance Rate")
geoPlot(esYes) + ggtitle("Rejection Rate")

# acceptance rate if editor is in a different geographical location than the submitting, first author
        
editorDifferent <- function(x) {
        decisions %>%
                filter(submitAuthFirstAuth == "Yes",
                       !is.na(handlingEditorGeog),
                       paperRejected == x,
                       handlingEditorGeog != firstAuthGeog) %>%
                select(firstAuthGeog, seniorAuthGeog) %>%
                table()
}

erNo <- editorDifferent("No") /
        (editorDifferent("No") + editorDifferent("Yes"))
erYes <- editorDifferent("Yes") /
        (editorDifferent("Yes") + editorDifferent("No"))

round(erNo,2) # acceptance rate
round(erYes,2) # rejection rate

geoPlot(erNo) + ggtitle("Acceptance Rate")
geoPlot(erYes) + ggtitle("Rejection Rate")

# acceptance rate if editor is in the same geographic region as the submitting, senior author 

editorSame2 <- function(x) {
        decisions %>%
                filter(submitAuthSeniorAuth == "Yes",
                       !is.na(handlingEditorGeog),
                       paperRejected == x,
                       handlingEditorGeog == seniorAuthGeog) %>%
                select(firstAuthGeog, seniorAuthGeog) %>%
                table()
}

es2No <- editorSame2("No") /
        (editorSame2("No") + editorSame2("Yes"))
es2Yes <- editorSame2("Yes") /
        (editorSame2("Yes") + editorSame2("No"))

round(es2No,2) # rejection rate
round(es2Yes,2) # acceptance rate

geoPlot(es2No) + ggtitle("Acceptance Rate")
geoPlot(es2Yes) + ggtitle("Rejection Rate")

# acceptance rate if editor is in a different geographical location than the submitting, senior author

editorDifferent2 <- function(x) {
        decisions %>%
                filter(submitAuthSeniorAuth == "Yes",
                       !is.na(handlingEditorGeog),
                       paperRejected == x,
                       handlingEditorGeog != seniorAuthGeog) %>%
                select(firstAuthGeog, seniorAuthGeog) %>%
                table()
}

er2No <- editorDifferent2("No") /
        (editorDifferent2("No") + editorDifferent2("Yes"))

er2Yes <- editorDifferent2("Yes") /
        (editorDifferent2("Yes") + editorDifferent2("No"))

round(er2No,2)
round(er2Yes,2)

geoPlot(er2No) + ggtitle("Acceptance Rate")
geoPlot(er2Yes) + ggtitle("Rejection Rate")
