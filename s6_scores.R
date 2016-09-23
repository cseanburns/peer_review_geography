source("libraries.R")

dec0$paperRejected      <- relevel(dec0$paperRejected, ref = "Yes")
dec0$firstAuthGeog      <- relevel(dec0$firstAuthGeog, ref = "Europe")

contrasts(dec0$paperRejected)
contrasts(dec0$firstAuthGeog)

# ---

plot(density(dec0$meanReviewScore))
plot(cut(dec0$meanReviewScore, breaks = 3))
mrs <- cut(dec0$meanReviewScore, breaks = 3)
mrs <- revalue(mrs, c("(0.997,2]" = "1",
                      "(2,3]" = "2",
                      "(3,4]" = "3"))

mrs <- factor(mrs, labels = c("Low", "Middle", "High"))
dec0$mrs <- mrs
m <- polr(mrs ~ firstAuthGeog, data = dec0, Hess = TRUE)
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m))
exp(cbind(OR = coef(m), ci))
table(dec0$paperRejected, dec0$mrs)
table(m$model)

sf <- function(y) {
        c('Y>=1' = qlogis(mean(y >= 1)),
          'Y>=2' = qlogis(mean(y >= 2)),
          'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dec0, summary(as.numeric(mrs) ~ firstAuthGeog, fun = sf)))
(s <- with(dec0, summary(as.numeric(mrs) ~ firstAuthGeog, fun = sf)))
plot(s, which=1:3, pch=1:3,
     xlab = "Logit",
     ylab = "",
     main = " ", xlim=range(s[,3:4]))

ggplot(as.data.frame(table(m$model)),
       aes(x = firstAuthGeog, y = Freq, fill = mrs)) +
        geom_bar(stat="identity") + scale_fill_grey(name = "Review Score") +
        theme_bw() +
        xlab("Geographic Region of First Author") +
        ylab("Frequency") +
        theme(legend.position = c(.9,.8))

stbl <- as.table(s)
stbl <- stbl[1,]
sdf <- data.frame(stbl)
sdf <- filter(sdf, Var2 != "Y>=1" & Var2 != "N")
sdf <- filter(sdf, Var1 != "Europe")
names(sdf) <- c("Region", "Level", "OR")

nsdf <- data.frame(exp(coef(m)))
nsdf$Region <- rownames(nsdf)
rownames(nsdf) <- NULL
names(nsdf) <- c("OR", "Region")
Level <- rep("Overall", 6)
nsdf$Level <- Level
nsdf <- edit(nsdf) # Change Region names to match region names in SDF
nsdf$Region <- as.factor(nsdf$Region)
nsdf$Level <- as.factor(nsdf$Level)
sdf$Region <- droplevels(sdf$Region)
sdf$Level <- droplevels(sdf$Level)
tsdf <- rbind(sdf, nsdf)

ggplot(tsdf, aes(x = OR, y = Region)) +
        geom_point(aes(shape = factor(Level))) +
        scale_shape(solid = FALSE, name = "OR Level") + 
        theme_bw() +
        xlab("Logits") +
        ylab("Geographical Regions of First Authors")
        

# ggplot(as.data.frame(table(m$model)))