# In-Class Exercise: Who gets an offer from medical school?
# Logistic regression on applicants to the Dell Medical School (UT Austin).
# Uses dell_med_admissions.csv
#
# Companion to the slides: logistic-regression.pdf
# Base R only -- no packages needed.
#
# Work through the sections in order. Lines marked  >> YOUR TURN  are for you
# and your neighbor to answer before we discuss as a class.

# ============================================================================
# Data Description
# ============================================================================
# dell_med_admissions.csv: the raw Texas Public Information Act release from
#   Dell Med covering its 2021 and 2022 admissions cycles. One row per
#   applicant: MCAT, GPAs, gender, TMDSAS ethnicity category, and the final
#   admissions outcome. The release is de-identified, and some cells are
#   marked [REDACTED] -- we read those as missing.
#
# Research question: how strongly do test scores and grades predict an offer
# of admission, and what is left over once we account for them?

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

# ============================================================================
# 1. Building the response
# ============================================================================

xtabs(~ status, data = apps)

# An applicant who withdrew BEFORE any decision never got a yes/no from the
# school, so drop them. Everyone who got an offer -- whether they enrolled,
# declined, or withdrew afterward -- counts as offered = 1.
offer_statuses <- c("Offered Admissions and Matriculated",
                    "Declined Offer of Admissions",
                    "Withdrew Application after Acceptance")
apps <- apps[apps$status != "Withdrew Application before Offer", ]
apps$offered <- ifelse(apps$status %in% offer_statuses, 1, 0)

xtabs(~ status + offered, data = apps)
mean(apps$offered)

# >> YOUR TURN: About what share of applicants get an offer? If you had to
#    guess "no offer" for everyone, how often would you be right?

summary(apps$mcat)
summary(apps$gpa_undergrad)

# The 2015 MCAT runs from 472 to 528. Anything outside that, and a GPA of 0,
# is a placeholder rather than a real value.
apps <- apps[!is.na(apps$mcat) & !is.na(apps$gpa_undergrad) &
               apps$mcat >= 472 & apps$gpa_undergrad > 0, ]
nrow(apps)

# ============================================================================
# 2. Look before you model
# ============================================================================

hist(apps$mcat, breaks = 40, main = "", xlab = "MCAT")
boxplot(mcat ~ offered, data = apps, xlab = "offered", ylab = "MCAT")
boxplot(gpa_undergrad ~ offered, data = apps, xlab = "offered", ylab = "undergrad GPA")

# Empirical offer rate at each MCAT score (scores with at least 20 applicants).
n_by_mcat <- tapply(apps$offered, apps$mcat, length)
rate_by_mcat <- tapply(apps$offered, apps$mcat, mean)
by_mcat <- data.frame(mcat = as.numeric(names(n_by_mcat)),
                      n = as.vector(n_by_mcat),
                      rate = as.vector(rate_by_mcat))
by_mcat <- by_mcat[by_mcat$n >= 20, ]
plot(rate ~ mcat, data = by_mcat, pch = 19, cex = sqrt(by_mcat$n) / 8,
     xlab = "MCAT", ylab = "share offered")

# >> YOUR TURN: Does the offer rate rise in a straight line? Where does it
#    take off? What shape does that remind you of?

# ============================================================================
# 3. Why not just run a regression?
# ============================================================================

lm1 <- lm(offered ~ mcat, data = apps)
coef(lm1)
plot(jitter(apps$mcat, amount = 0.3), jitter(apps$offered, amount = 0.03),
     pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.1),
     xlab = "MCAT", ylab = "offered")
abline(lm1, col = "blue", lwd = 2)

# >> YOUR TURN: What does the line predict for an MCAT of 475? Of 528?
#    Do those numbers make sense as probabilities?
predict(lm1, data.frame(mcat = c(475, 528)))

# ============================================================================
# 4. One predictor: MCAT
# ============================================================================

model1 <- glm(offered ~ mcat, data = apps, family = binomial)
summary(model1)

# Each extra MCAT point multiplies the odds of an offer by exp(beta).
exp(coef(model1))
exp(5 * coef(model1)["mcat"])

predict(model1, data.frame(mcat = c(505, 510, 515, 520, 525)), type = "response")

# >> YOUR TURN: The odds ratio per point is constant. Is the change in
#    PROBABILITY per point constant too? Compare 505 -> 510 with 520 -> 525.

# The fitted S-curve against the raw data and the empirical rates.
grid <- data.frame(mcat = seq(min(apps$mcat), max(apps$mcat), by = 0.5))
grid$p <- predict(model1, grid, type = "response")
plot(jitter(apps$mcat, amount = 0.3), jitter(apps$offered, amount = 0.03),
     pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.1),
     xlab = "MCAT", ylab = "P(offered)")
lines(grid$mcat, grid$p, col = "red", lwd = 2)
points(by_mcat$mcat, by_mcat$rate, pch = 19, col = "blue")

# >> YOUR TURN: Use the model to find the MCAT score where the predicted
#    chance of an offer reaches 50%. (Hint: log odds = 0.)

# ============================================================================
# 5. Classification: accuracy, thresholds, and the confusion matrix
# ============================================================================

phat1 <- predict(model1, type = "response")

pred1 <- ifelse(phat1 >= 0.5, 1, 0)
xtabs(~ pred1 + apps$offered)
mean(pred1 == apps$offered)
mean(apps$offered == 0)   # accuracy of the "nobody gets in" rule

# >> YOUR TURN: Is the model beating the "nobody gets in" rule? Why is
#    accuracy a poor yardstick when one outcome is rare?

# Lower the threshold.
pred1b <- ifelse(phat1 >= 0.05, 1, 0)
xtabs(~ pred1b + apps$offered)
mean(pred1b == apps$offered)
prop.table(xtabs(~ pred1b + apps$offered), 2)   # each column sums to 1

# >> YOUR TURN: Read the second table column by column. Of the applicants who
#    really got offers, what share did we flag? Of those who didn't, what
#    share did we wrongly flag?

# >> YOUR TURN: Try thresholds of 0.1, 0.2, and 0.3. As you lower the
#    threshold, what do you gain and what do you give up? If you were a
#    pre-med advisor using this model, which mistake would you rather make?

# ============================================================================
# 6. Adding grades
# ============================================================================

model2 <- glm(offered ~ mcat + gpa_undergrad + gpa_undergrad_sci,
              data = apps, family = binomial)
summary(model2)
exp(0.1 * coef(model2)[c("gpa_undergrad", "gpa_undergrad_sci")])
cor(apps$gpa_undergrad, apps$gpa_undergrad_sci, use = "complete.obs")
plot(apps$gpa_undergrad, apps$gpa_undergrad_sci, pch = 16, cex = 0.4,
     col = rgb(0, 0, 0, 0.2), xlab = "undergrad GPA", ylab = "undergrad science GPA")

# >> YOUR TURN: One GPA coefficient may look small or even flip sign. Use the
#    scatterplot and correlation to explain why. Does that mean science GPA
#    "doesn't matter"?

# Does adding grades change the confusion matrix? model2 drops applicants with
# a missing science GPA, so compare both models on the same applicants.
keep <- !is.na(apps$gpa_undergrad_sci)
phat2 <- predict(model2, type = "response")
pred1c <- ifelse(phat1[keep] >= 0.2, 1, 0)
pred2 <- ifelse(phat2 >= 0.2, 1, 0)
xtabs(~ pred1c + apps$offered[keep])
xtabs(~ pred2 + apps$offered[keep])

# >> YOUR TURN: At the same threshold, how many more admits does model2 catch,
#    and how many more false alarms does it raise?

# Predicted P(offer) by MCAT for three GPAs.
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

# >> YOUR TURN: Roughly how many MCAT points does a 3.6 student need to make
#    up to match a 3.9 student's chances?

# ============================================================================
# 7. Demographics: what is left over after scores and grades?
# ============================================================================

apps <- apps[!is.na(apps$ethnicity) & apps$ethnicity != "International" &
               apps$sex %in% c("Female", "Male"), ]
apps$ethnicity <- relevel(factor(apps$ethnicity), ref = "White/Caucasian")
apps$cycle <- factor(apps$cycle)

# Raw offer rates by group.
prop.table(xtabs(~ ethnicity + offered, data = apps), 1)
prop.table(xtabs(~ sex + offered, data = apps), 1)

# Groups also differ in their MCAT and GPA distributions.
tapply(apps$mcat, apps$ethnicity, mean)
tapply(apps$gpa_undergrad, apps$ethnicity, mean)
boxplot(mcat ~ ethnicity, data = apps, las = 2, xlab = "", ylab = "MCAT",
        cex.axis = 0.7)

model3 <- glm(offered ~ mcat + gpa_undergrad + sex + ethnicity + cycle,
              data = apps, family = binomial)
summary(model3)
exp(coef(model3))

# >> YOUR TURN: Compare the RAW gap in offer rates between two groups with the
#    gap in the model's odds ratios. Why can they differ so much, and even
#    point in opposite directions?

# Same applicant, different ethnicity box.
last_cycle <- levels(apps$cycle)[nlevels(apps$cycle)]
applicant <- expand.grid(mcat = 512, gpa_undergrad = 3.8, sex = "Female",
                         cycle = last_cycle, ethnicity = levels(apps$ethnicity))
applicant$p <- predict(model3, applicant, type = "response")
applicant

applicant2 <- expand.grid(mcat = c(505, 512, 520), gpa_undergrad = 3.8, sex = "Female",
                          cycle = last_cycle, ethnicity = c("White/Caucasian", "Hispanic"))
applicant2$p <- predict(model3, applicant2, type = "response")
applicant2

# >> YOUR TURN: The odds ratio for Hispanic is a single number, but the gap in
#    PROBABILITY changes with MCAT in applicant2. Why?

# One S-curve per group, same GPA, sex, and cycle.
groups <- levels(apps$ethnicity)
cols <- rainbow(length(groups), v = 0.8)
plot(NULL, xlim = range(grid$mcat), ylim = c(0, 1), xlab = "MCAT", ylab = "P(offered)")
for (i in seq_along(groups)) {
  curve_i <- data.frame(mcat = grid$mcat, gpa_undergrad = 3.8, sex = "Female",
                        cycle = last_cycle, ethnicity = groups[i])
  lines(grid$mcat, predict(model3, curve_i, type = "response"), lwd = 2, col = cols[i])
}
legend("topleft", groups, col = cols, lwd = 2, cex = 0.7)

# >> YOUR TURN: Where on the MCAT scale are the curves farthest apart? Where
#    do they nearly agree? What does that say about who these terms matter for?

# >> YOUR TURN: Build your own applicant. Pick an MCAT, a GPA, and the other
#    inputs, and use predict() to get their chance of an offer. Then find the
#    smallest MCAT increase that doubles their ODDS.

# ============================================================================
# 8. Does the MCAT slope differ by sex? An interaction
# ============================================================================

model4 <- glm(offered ~ mcat * sex + gpa_undergrad + ethnicity + cycle,
              data = apps, family = binomial)
summary(model4)

# >> YOUR TURN: Write out the MCAT slope (on the log-odds scale) for women and
#    for men. Is the difference large enough to matter in practice?

# ============================================================================
# 9. Discussion: prediction is not causation
# ============================================================================
# >> YOUR TURN (talk it through with your neighbor):
#  (a) The file has MCAT, GPA, sex, and ethnicity -- but admissions committees
#      also see interviews, essays, letters, research and clinical experience,
#      Texas residency, and more. List two omitted variables that could be
#      correlated with BOTH ethnicity and the chance of an offer. Which way
#      would each one bias the ethnicity coefficients?
#  (b) We dropped applicants with [REDACTED] or missing values. Could the
#      missingness itself be related to the outcome?
#  (c) Suppose someone reads model3 and says "the coefficient on group X is
#      the effect of being in group X on admission." What would you need to
#      make a claim like that credible?
