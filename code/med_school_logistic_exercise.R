# In-Class Exercise: Are there racial preferences at Dell Medical School?
# Time: 30 minutes
#
# ============================================================================
# The data: dell_med_admissions.csv  (in the data/ folder)
# ============================================================================
# A Texas Public Information Act release from the Dell Medical School at
# UT Austin covering its 2021 and 2022 admissions cycles. One row per
# application. The file is de-identified, and some cells read "[REDACTED]".
#
#   DeIdent            de-identified applicant ID
#   Admissions_Status  final outcome of the application
#   Admissions_Cycle   admissions year
#   Gender             applicant gender
#   TMDSASEthnicity    race/ethnicity category reported through TMDSAS, the
#                      Texas medical school application service
#   MCAT2015           MCAT score (the post-2015 exam, scored 472-528)
#   Overall_GPA        overall GPA, all coursework
#   Urg_Overall        undergraduate GPA
#   UrgSci_GPA         undergraduate science GPA
#   Grad_GPA           graduate school GPA, if any
#
# ============================================================================
# The question
# ============================================================================
# Are there racial preferences in admissions at Dell Medical School?
#
# Use the data to make your case. Be ready to show the class your evidence
# and tell us how confident you are in your answer.

apps <- read.csv("../data/dell_med_admissions.csv", fileEncoding = "UTF-8-BOM")
apps$Gender <- relevel(factor(apps$Gender), ref = "Male")
apps$TMDSASEthnicity <- relevel(factor(apps$TMDSASEthnicity), ref = "Asian")

