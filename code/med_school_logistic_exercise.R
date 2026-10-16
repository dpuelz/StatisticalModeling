# In-Class Exercise: Who gets an offer from medical school?
# Uses dell_med_admissions.csv

apps <- read.csv("../data/dell_med_admissions.csv", fileEncoding = "UTF-8-BOM",
                 na.strings = c("", "[REDACTED]"))
apps <- apps[!is.na(apps$Admissions_Status), ]
apps <- data.frame(cycle = apps$Admissions_Cycle,
                   status = trimws(apps$Admissions_Status),
                   sex = apps$Gender,
                   ethnicity = apps$TMDSASEthnicity,
                   mcat = apps$MCAT2015,
                   gpa_overall = apps$Overall_GPA,
                   gpa_undergrad = apps$Urg_Overall,
                   gpa_undergrad_sci = apps$UrgSci_GPA,
                   gpa_grad = apps$Grad_GPA)

dim(apps)
head(apps)

# ---- 1 ----

xtabs(~ status, data = apps)

offer_statuses <- c("Offered Admissions and Matriculated",
                    "Declined Offer of Admissions",
                    "Withdrew Application after Acceptance")
apps <- apps[apps$status != "Withdrew Application before Offer", ]
apps$offered <- ifelse(apps$status %in% offer_statuses, 1, 0)

xtabs(~ status + offered, data = apps)
mean(apps$offered)

# Q: What share of applicants get an offer? How often is "no offer" right?

summary(apps$mcat)
summary(apps$gpa_undergrad)

apps <- apps[!is.na(apps$mcat) & !is.na(apps$gpa_undergrad) &
               apps$mcat >= 472 & apps$gpa_undergrad > 0, ]
nrow(apps)

# Q: Why these filters?

# ---- 2 ----

hist(apps$mcat, breaks = 40, main = "", xlab = "MCAT")
boxplot(mcat ~ offered, data = apps, xlab = "offered", ylab = "MCAT")
boxplot(gpa_undergrad ~ offered, data = apps, xlab = "offered", ylab = "undergrad GPA")

n_by_mcat <- tapply(apps$offered, apps$mcat, length)
rate_by_mcat <- tapply(apps$offered, apps$mcat, mean)
by_mcat <- data.frame(mcat = as.numeric(names(n_by_mcat)),
                      n = as.vector(n_by_mcat),
                      rate = as.vector(rate_by_mcat))
by_mcat <- by_mcat[by_mcat$n >= 20, ]
plot(rate ~ mcat, data = by_mcat, pch = 19, cex = sqrt(by_mcat$n) / 8,
     xlab = "MCAT", ylab = "share offered")

# Q: What shape is this?

# ---- 3 ----

lm1 <- lm(offered ~ mcat, data = apps)
coef(lm1)
plot(jitter(apps$mcat, amount = 0.3), jitter(apps$offered, amount = 0.03),
     pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.1),
     xlab = "MCAT", ylab = "offered")
abline(lm1, col = "blue", lwd = 2)
predict(lm1, data.frame(mcat = c(475, 528)))

# Q: What's wrong here?

# ---- 4 ----

model1 <- glm(offered ~ mcat, data = apps, family = binomial)
summary(model1)

exp(coef(model1))
exp(5 * coef(model1)["mcat"])

predict(model1, data.frame(mcat = c(505, 510, 515, 520, 525)), type = "response")

# Q: Interpret each number above. Compare 505 -> 510 with 520 -> 525.

grid <- data.frame(mcat = seq(min(apps$mcat), max(apps$mcat), by = 0.5))
grid$p <- predict(model1, grid, type = "response")
plot(jitter(apps$mcat, amount = 0.3), jitter(apps$offered, amount = 0.03),
     pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.1),
     xlab = "MCAT", ylab = "P(offered)")
lines(grid$mcat, grid$p, col = "red", lwd = 2)
points(by_mcat$mcat, by_mcat$rate, pch = 19, col = "blue")

# Q: At what MCAT does the predicted chance of an offer reach 50%?

# ---- 5 ----

phat1 <- predict(model1, type = "response")

pred1 <- ifelse(phat1 >= 0.5, 1, 0)
xtabs(~ pred1 + apps$offered)
mean(pred1 == apps$offered)
mean(apps$offered == 0)

# Q: Is the model any good?

pred1b <- ifelse(phat1 >= 0.05, 1, 0)
xtabs(~ pred1b + apps$offered)
mean(pred1b == apps$offered)
prop.table(xtabs(~ pred1b + apps$offered), 2)

# Q: Read this table. Then try 0.1, 0.2, and 0.3. Which would you use, and why?

# ---- 6 ----

model2 <- glm(offered ~ mcat + gpa_undergrad + gpa_undergrad_sci,
              data = apps, family = binomial)
summary(model2)
exp(0.1 * coef(model2)[c("gpa_undergrad", "gpa_undergrad_sci")])
cor(apps$gpa_undergrad, apps$gpa_undergrad_sci, use = "complete.obs")
plot(apps$gpa_undergrad, apps$gpa_undergrad_sci, pch = 16, cex = 0.4,
     col = rgb(0, 0, 0, 0.2), xlab = "undergrad GPA", ylab = "undergrad science GPA")

# Q: Does science GPA matter?

keep <- !is.na(apps$gpa_undergrad_sci)
phat2 <- predict(model2, type = "response")
pred1c <- ifelse(phat1[keep] >= 0.2, 1, 0)
pred2 <- ifelse(phat2 >= 0.2, 1, 0)
xtabs(~ pred1c + apps$offered[keep])
xtabs(~ pred2 + apps$offered[keep])

# Q: What changed? Why do we need `keep`?

plot(grid$mcat, predict(model2, data.frame(mcat = grid$mcat, gpa_undergrad = 3.9,
                                           gpa_undergrad_sci = 3.9), type = "response"),
     type = "l", lwd = 2, col = "darkgreen", ylim = c(0, 1),
     xlab = "MCAT", ylab = "P(offered)")
lines(grid$mcat, predict(model2, data.frame(mcat = grid$mcat, gpa_undergrad = 3.6,
                                            gpa_undergrad_sci = 3.6), type = "response"),
      lwd = 2, col = "orange")
lines(grid$mcat, predict(model2, data.frame(mcat = grid$mcat, gpa_undergrad = 3.3,
                                            gpa_undergrad_sci = 3.3), type = "response"),
      lwd = 2, col = "red")
legend("topleft", c("GPA 3.9", "GPA 3.6", "GPA 3.3"),
       col = c("darkgreen", "orange", "red"), lwd = 2)

# Q: How many MCAT points is 0.3 of GPA worth?

# ---- 7 ----

apps <- apps[!is.na(apps$ethnicity) & apps$ethnicity != "International" &
               apps$sex %in% c("Female", "Male"), ]
apps$ethnicity <- relevel(factor(apps$ethnicity), ref = "White/Caucasian")
apps$cycle <- factor(apps$cycle)

prop.table(xtabs(~ ethnicity + offered, data = apps), 1)
prop.table(xtabs(~ sex + offered, data = apps), 1)

tapply(apps$mcat, apps$ethnicity, mean)
tapply(apps$gpa_undergrad, apps$ethnicity, mean)
boxplot(mcat ~ ethnicity, data = apps, las = 2, xlab = "", ylab = "MCAT",
        cex.axis = 0.7)

model3 <- glm(offered ~ mcat + gpa_undergrad + sex + ethnicity + cycle,
              data = apps, family = binomial)
summary(model3)
exp(coef(model3))

# Q: Compare the raw offer rates with the model's odds ratios.

last_cycle <- levels(apps$cycle)[nlevels(apps$cycle)]
applicant <- expand.grid(mcat = 512, gpa_undergrad = 3.8, sex = "Female",
                         cycle = last_cycle, ethnicity = levels(apps$ethnicity))
applicant$p <- predict(model3, applicant, type = "response")
applicant

applicant2 <- expand.grid(mcat = c(505, 512, 520), gpa_undergrad = 3.8, sex = "Female",
                          cycle = last_cycle, ethnicity = c("White/Caucasian", "Hispanic"))
applicant2$p <- predict(model3, applicant2, type = "response")
applicant2

# Q: One odds ratio, but the gap in probability changes. Why?

groups <- levels(apps$ethnicity)
cols <- rainbow(length(groups), v = 0.8)
plot(NULL, xlim = range(grid$mcat), ylim = c(0, 1), xlab = "MCAT", ylab = "P(offered)")
for (i in seq_along(groups)) {
  curve_i <- data.frame(mcat = grid$mcat, gpa_undergrad = 3.8, sex = "Female",
                        cycle = last_cycle, ethnicity = groups[i])
  lines(grid$mcat, predict(model3, curve_i, type = "response"), lwd = 2, col = cols[i])
}
legend("topleft", groups, col = cols, lwd = 2, cex = 0.7)

# Q: Where are the curves farthest apart?

# Q: Build your own applicant and predict their chance of an offer. How many
#    more MCAT points would double their odds?

# ---- 8 ----

model4 <- glm(offered ~ mcat * sex + gpa_undergrad + ethnicity + cycle,
              data = apps, family = binomial)
summary(model4)

# Q: What is the MCAT slope for women? For men?

# ---- 9 ----

# Q: What's missing from this data? Can we read model3 causally?
