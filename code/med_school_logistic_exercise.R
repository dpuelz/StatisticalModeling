library(tidyverse)

apps <- read.csv("../data/dell_med_admissions.csv", fileEncoding = "UTF-8-BOM",
                 na.strings = c("", "[REDACTED]")) %>%
  filter(!is.na(Admissions_Status)) %>%
  transmute(cycle = Admissions_Cycle,
            status = trimws(Admissions_Status),
            sex = Gender,
            ethnicity = TMDSASEthnicity,
            mcat = MCAT2015,
            gpa_overall = Overall_GPA,
            gpa_undergrad = Urg_Overall,
            gpa_undergrad_sci = UrgSci_GPA,
            gpa_grad = Grad_GPA)

dim(apps)
head(apps)

xtabs(~ status, data = apps)

apps <- apps %>%
  filter(status != "Withdrew Application before Offer") %>%
  mutate(offered = ifelse(status %in% c("Offered Admissions and Matriculated",
                                        "Declined Offer of Admissions",
                                        "Withdrew Application after Acceptance"), 1, 0))

xtabs(~ status + offered, data = apps)
mean(apps$offered)

summary(apps$mcat)
summary(apps$gpa_undergrad)

apps <- apps %>% filter(mcat >= 472, gpa_undergrad > 0)
nrow(apps)

ggplot(apps, aes(x = factor(offered), y = mcat)) +
  geom_boxplot() +
  labs(x = "offered", y = "MCAT")

apps %>%
  group_by(mcat) %>%
  summarise(n = n(), rate = mean(offered)) %>%
  filter(n >= 20) %>%
  ggplot(aes(x = mcat, y = rate)) +
  geom_point(aes(size = n)) +
  labs(x = "MCAT", y = "share offered")

model1 <- glm(offered ~ mcat, data = apps, family = binomial)
summary(model1)

exp(coef(model1))
exp(5 * coef(model1)["mcat"])

predict(model1, data.frame(mcat = c(505, 510, 515, 520, 525)), type = "response")

grid <- data.frame(mcat = seq(min(apps$mcat), max(apps$mcat), by = 0.5))
grid$p <- predict(model1, grid, type = "response")
ggplot(apps, aes(x = mcat, y = offered)) +
  geom_jitter(height = 0.03, width = 0.3, alpha = 0.1) +
  geom_line(data = grid, aes(x = mcat, y = p), colour = "red", linewidth = 1) +
  labs(x = "MCAT", y = "P(offered)")

phat1 <- predict(model1, type = "response")
pred1 <- ifelse(phat1 >= 0.5, 1, 0)
mean(pred1 == apps$offered)
mean(apps$offered == 0)
xtabs(~ pred1 + apps$offered)

pred1b <- ifelse(phat1 >= 0.05, 1, 0)
mean(pred1b == apps$offered)
xtabs(~ pred1b + apps$offered)
xtabs(~ pred1b + apps$offered) %>% prop.table(2)

model2 <- glm(offered ~ mcat + gpa_undergrad + gpa_undergrad_sci,
              data = apps, family = binomial)
summary(model2)
exp(0.1 * coef(model2)[c("gpa_undergrad", "gpa_undergrad_sci")])
cor(apps$gpa_undergrad, apps$gpa_undergrad_sci)

apps <- apps %>%
  filter(!is.na(ethnicity), ethnicity != "International", sex %in% c("Female", "Male")) %>%
  mutate(ethnicity = relevel(factor(ethnicity), ref = "White/Caucasian"),
         cycle = factor(cycle))

xtabs(~ ethnicity + offered, data = apps) %>% prop.table(1)
xtabs(~ sex + offered, data = apps) %>% prop.table(1)

model3 <- glm(offered ~ mcat + gpa_undergrad + sex + ethnicity + cycle,
              data = apps, family = binomial)
summary(model3)
exp(coef(model3))

model3b <- glm(offered ~ mcat + gpa_undergrad + sex + cycle,
               data = apps, family = binomial)
anova(model3b, model3, test = "Chisq")

applicant <- expand.grid(mcat = 512, gpa_undergrad = 3.8, sex = "Female",
                         cycle = "2022", ethnicity = levels(apps$ethnicity))
applicant$p <- predict(model3, applicant, type = "response")
applicant

applicant2 <- expand.grid(mcat = c(505, 512, 520), gpa_undergrad = 3.8, sex = "Female",
                          cycle = "2022", ethnicity = c("White/Caucasian", "Hispanic"))
applicant2$p <- predict(model3, applicant2, type = "response")
applicant2
