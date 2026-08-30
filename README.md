# Statistical Modeling and Learning

Welcome to the Fall 2026 edition of Statistical Modeling and Learning (STM 2102, 4.5 credits)! All course materials can be found on this GitHub page. Please see the [course syllabus](syllabus/course_outline.pdf) for links and descriptions of the readings mentioned below.

**Instructor:**  
- Prof. David Puelz. Individual meetings can be booked at the following [link](https://calendly.com/dpuelz). Office hours: Please consult the webpage.

**Email:** [dpuelz@uaustin.org](mailto:dpuelz@uaustin.org)

**Meeting Schedule:**
- Section 1: M/W/F, 11:30a-12:45p
- Section 2: T/Th/F, 10:00a-11:15a

**Term:** Monday, August 31 - Monday, November 9, 2026. Final exams November 10-17.

## Course Description

This course trains students to wield data as both a scientific and predictive tool with an applied focus on business and economics. It develops mastery in two complementary approaches: supervised machine learning, which builds powerful models for prediction and decision making, and causal inference, which uncovers credible cause and effect relationships. Students will learn to design empirical studies, implement algorithms, and critically evaluate when data reveals insight.

## Course Objectives

- Understand the fundamentals of statistical modeling and supervised learning
- Fit and interpret regression models for prediction and inference
- Apply statistical methods to real-world data using appropriate software
- Evaluate model performance and understand the bias-variance tradeoff

## Required Readings

- _Introduction to Statistical Learning_ (ISL) -- Gareth James, et al. (Available for free online)
- _Mastering 'Metrics_ (MM) -- Joshua D. Angrist and Jörn-Steffen Pischke (Supplemental reading for causal inference)

## Assignments

There will be 5 homework assignments, submitted via Populi. Each will be posted here as it is
released, and is due at the **start of your section's class** on the Friday listed below.

| Homework | Due | Section 1 | Section 2 |
|---|---|---|---|
| Homework 1 | Friday, Sep 11 | 11:30a | 10:00a |
| Homework 2 | Friday, Sep 25 | 11:30a | 10:00a |
| Homework 3 | Friday, Oct 9 | 11:30a | 10:00a |
| Homework 4 | Friday, Oct 23 | 11:30a | 10:00a |
| Homework 5 | Friday, Nov 6 | 11:30a | 10:00a |

### Homework Rubric

1 = All answers incorrect or inadequately addressed and missing deliverables, severely lacking clarity, write-up unprofessional

2 = More than half of answers incorrect, severely lacking clarity, write-up unprofessional and/or missing deliverables

3 = The majority of answers are correct with a couple mistakes, write-up is not professionally compiled but legible

4 = All answers are correct and write-up is acceptable. This is the modal grade

5 = All answers are correct and write-up is exceptional. The student went above and beyond the prompts to investigate an area not explicitly requested

## Quizzes

There will be 5 quizzes on the Fridays of weeks 2, 4, 6, 8, and 10. The quizzes will be related to the homework, and we will mark up the quizzes in class directly after finishing the quiz.

- Quiz 1: Week 2, Friday, Sep 11
- Quiz 2: Week 4, Friday, Sep 25
- Quiz 3: Week 6, Friday, Oct 9
- Quiz 4: Week 8, Friday, Oct 23
- Quiz 5: Week 10, Friday, Nov 6

## Final Exam

The final exam will be held during the scheduled exam period, **November 10-17**. Note that Veterans Day, Wednesday November 11, is a university holiday and no exams are held that day.

## Software

### Local R (downloadable software on your computer)

You will need a local download of R to run our example code and for your assignments. Please install [R](https://cran.rstudio.com) and then [RStudio](https://posit.co/download/rstudio-desktop/) on your own computer (you want the "RStudio Desktop" version). Both are free and work on all platforms. R is the underlying data-analysis program we'll use in this course, while RStudio provides a nice front-end interface to R that makes certain repetitive steps (e.g. loading data, saving plots) very simple.

**Getting started with R**: If you're new to R, check out [Introduction to R](code/intro_to_R.R) for basic examples and concepts.

## Course Cadence

There will be 5 quizzes and 5 homework assignments. The quizzes will be on the Fridays of weeks 2, 4, 6, 8, and 10. The homeworks will be due at the start of class on the same Fridays as the quizzes -- 11:30a for Section 1, 10:00a for Section 2. The quiz content will be related to the homework, and we will mark up the quizzes in class directly after finishing the quiz. We will have a final exam during the scheduled exam time (on week 11 of the course).

## Rough Schedule

| Week | Week of | Topics | Reading |
|------|---------|--------|---------|
| 1 | Aug 31 | Intro and bias-variance tradeoff | ISL: Ch 1, 2 |
| 2 | Sep 7 | Inference for regression + Multiple regression | ISL: Ch 3.1-3.2, MM: Ch 2 |
| 3 | Sep 14 | Categorical predictors and interactions | ISL: Ch 3.3 |
| 4 | Sep 21 | Assumptions, diagnostics + Nonlinear regression | ISL: Ch 3.3, 7.1 |
| 5 | Sep 28 | Time series regression | ISL: Ch 3 (supplemental) |
| 6 | Oct 5 | Logistic regression | ISL: Ch 4.1-4.3 |
| 7 | Oct 12 | Model selection and penalized regression | ISL: Ch 6.1-6.5 |
| 8 | Oct 19 | Trees, ensembles, and neural networks | ISL: Ch 8.1-8.3, 11.1-11.3 |
| 9 | Oct 26 | Causal inference | MM: Ch 1, ISL: Ch 3.2 |
| 10 | Nov 2 | Causal inference | MM: Ch 3-5 |
| 11 | Nov 9 | Last day of instruction Nov 9; **final exams Nov 10-17** | |

Labor Day, Monday September 7, is a university holiday -- Section 1 does not meet that day.
Add/drop closes Thursday September 10; the last day to withdraw is Wednesday October 14.

## Outline of Topics

Slides and code for each unit will be posted here as we reach that week of class.

### (0) Introduction and Bias-Variance Tradeoff

Slides: [statistical-modeling.pdf](slides/statistical-modeling.pdf)

Supplemental: [Bias-Variance Tradeoff Derivation](slides/BV_tradeoff_derivation.pdf)

Code for class:
- [Introduction to R](code/intro_to_R.R) (includes example with [Austin restaurant data](data/austin_restaurants.csv))
- [Introduction to Probability and Regression](code/intro_probability-regression.R) (signal + noise, Normal, mean/SD, ±2 SD rule, *p*-values — the probability you'll need for regression)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 1, 2
- [Introduction to RMarkdown](http://rmarkdown.rstudio.com)
- [RMarkdown tutorial](https://rmarkdown.rstudio.com/lesson-1.html)

### (1) Inference for Regression and Multiple Regression

Slides: *coming soon*

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 3.1-3.2
- _Mastering 'Metrics_ (MM) -- Chapter 2

### (2) Categorical Predictors and Interactions

Slides: *coming soon*

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapter 3.3

### (3) Assumptions, Diagnostics, and Nonlinear Regression

Slides: *coming soon*

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 3.3, 7.1

### (4) Time Series Regression

Slides: *coming soon*

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapter 3 (supplemental)

### (5) Logistic Regression

Slides: *coming soon*

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 4.1-4.3

### (6) Model Selection and Penalized Regression

Slides: *coming soon*

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 6.1-6.5

### (7) Trees, Ensembles, Neural Networks, and Deep Neural Networks

Slides: *coming soon*

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 8.1-8.3, 11.1-11.3

### (8) Causal Inference

Slides: *coming soon*

Readings:
- _Mastering 'Metrics_ (MM) -- Chapters 1, 3-5
- _Introduction to Statistical Learning_ (ISL) -- Chapter 3.2
