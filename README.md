# Statistical Modeling and Learning

Welcome to the Winter 2026 edition of Statistical Modeling and Learning (STM 2102, 4.5 credits)! All course materials can be found on this GitHub page. Please see the [course syllabus](syllabus/course_outline.pdf) for links and descriptions of the readings mentioned below.

**Instructor:**  
- Prof. David Puelz. Individual meetings can be booked at the following [link](https://calendly.com/dpuelz). Office hours: Please consult the webpage.

**Email:** [dpuelz@uaustin.org](mailto:dpuelz@uaustin.org)

**Meeting Schedule:** M/W/F from 10:00a-11:15a

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

There will be 5 homework assignments to be turned in via Populi. They will be posted here.

- [Homework 1](assignments/HW1.pdf). Due Friday, Jan 16 at 10:00a.
  - Data: [drone_strikes_venezuela.csv](data/drone_strikes_venezuela.csv), [stock_returns.csv](data/stock_returns.csv), [College.csv](data/College.csv)
- [Homework 2](assignments/HW2.pdf). Due Friday, Jan 30 at 10:00a.
  - Data: [Boston.csv](data/Boston.csv), [beer-demand.csv](data/beer-demand.csv)
- [Homework 3](assignments/HW3.pdf). Due Friday, Feb 13 at 10:00a.
  - Data: [airline_passengers.csv](data/airline_passengers.csv), [daily_temperature.csv](data/daily_temperature.csv), [noisy_prices.csv](data/noisy_prices.csv), [bank-full.csv](data/bank-full.csv), [bank-names.txt](data/bank-names.txt)
- [Homework 4](assignments/HW4.pdf). Due Friday, Feb 27 at 10:00a.
  - Data: [greenbuildings.csv](data/greenbuildings.csv), [social_marketing.csv](data/social_marketing.csv), [wine.csv](data/wine.csv)
- [Homework 5](assignments/HW5.pdf). Due Friday, Mar 13 at 10:00a.
  - Data: [EuroSAT](https://pytorch.org/vision/stable/generated/torchvision.datasets.EuroSAT.html) (via `torchvision.datasets.EuroSAT`, downloads automatically); [castle](https://github.com/causaldata/causaldata) (via R package causaldata: `data(castle, package = "causaldata")`). Starter code: [HW5_neural_networks.ipynb](code/HW5_neural_networks.ipynb).

### Homework Rubric

1 = All answers incorrect or inadequately addressed and missing deliverables, severely lacking clarity, write-up unprofessional

2 = More than half of answers incorrect, severely lacking clarity, write-up unprofessional and/or missing deliverables

3 = The majority of answers are correct with a couple mistakes, write-up is not professionally compiled but legible

4 = All answers are correct and write-up is acceptable. This is the modal grade

5 = All answers are correct and write-up is exceptional. The student went above and beyond the prompts to investigate an area not explicitly requested

## Quizzes

There will be 5 quizzes on the Fridays of weeks 2, 4, 6, 8, and 10. The quizzes will be related to the homework, and we will mark up the quizzes in class directly after finishing the quiz.

- Quiz 1: Week 2, Friday (Jan 16)
- Quiz 2: Week 4, Friday (Jan 30)
- Quiz 3: Week 6, Friday (Feb 13)
- Quiz 4: Week 8, Friday (Feb 27)
- Quiz 5: Week 10, Friday (Mar 13)

## Final Exam

The final exam will be held during the scheduled exam time (week 11 of the course).

## Software

### Local R (downloadable software on your computer)

You will need a local download of R to run our example code and for your assignments. Please install [R](https://cran.rstudio.com) and then [RStudio](https://posit.co/download/rstudio-desktop/) on your own computer (you want the "RStudio Desktop" version). Both are free and work on all platforms. R is the underlying data-analysis program we'll use in this course, while RStudio provides a nice front-end interface to R that makes certain repetitive steps (e.g. loading data, saving plots) very simple.

**Getting started with R**: If you're new to R, check out [Introduction to R](code/intro_to_R.R) for basic examples and concepts.

## Course Cadence

There will be 5 quizzes and 5 homework assignments. The quizzes will be on the Fridays of weeks 2, 4, 6, 8, and 10. The homeworks will be due on Fridays at 10:00a (start of class) on the same days as the quizzes. The quiz content will be related to the homework, and we will mark up the quizzes in class directly after finishing the quiz. We will have a final exam during the scheduled exam time (on week 11 of the course).

## Rough Schedule

| Week | Dates | Topics | Reading |
|------|-------|--------|---------|
| 1 | Jan 5 | Intro and bias-variance tradeoff | ISL: Ch 1, 2 |
| 2 | Jan 12 | Inference for regression + Multiple regression | ISL: Ch 3.1-3.2, MM: Ch 2 |
| 3 | Jan 19 | Categorical predictors and interactions | ISL: Ch 3.3 |
| 4 | Jan 26 | Assumptions, diagnostics + Nonlinear regression | ISL: Ch 3.3, 7.1 |
| 5 | Feb 2 | Time series regression | ISL: Ch 3 (supplemental) |
| 6 | Feb 9 | Logistic regression | ISL: Ch 4.1-4.3 |
| 7 | Feb 16 | Model selection and penalized regression | ISL: Ch 6.1-6.5 |
| 8 | Feb 23 | Trees, ensembles, neural networks, and deep neural networks | ISL: Ch 8.1-8.3, 11.1-11.3 |
| 9 | Mar 2 | Causal inference | MM: Ch 1, ISL: Ch 3.2 |
| 10 | Mar 9 | Causal inference | MM: Ch 3-5 |
| 11 | Mar 16 | **Final exam week** | |

## Outline of Topics

### (0) Introduction and Bias-Variance Tradeoff

Slides: [statistical-modeling.pdf](slides/statistical-modeling.pdf)

Supplemental: [Bias-Variance Tradeoff Derivation](slides/BV_tradeoff_derivation.pdf)

Code for class:
- [Introduction to R](code/intro_to_R.R) (includes example with [Austin restaurant data](data/austin_restaurants.csv))
- [Introduction to Probability and Regression](code/intro_probability-regression.R) (learning probability through simulation)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 1, 2
- [Introduction to RMarkdown](http://rmarkdown.rstudio.com)
- [RMarkdown tutorial](https://rmarkdown.rstudio.com/lesson-1.html)

### (1) Inference for Regression and Multiple Regression

Slides: [linear-regression-simple-and-inference.pdf](slides/linear-regression-simple-and-inference.pdf), [multiple-regression.pdf](slides/multiple-regression.pdf)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 3.1-3.2
- _Mastering 'Metrics_ (MM) -- Chapter 2

Code for class:
- [Computing standard errors via bootstrapping](code/bootstrap.R)
- [Multiple regression example: stock returns](code/mlr_stock_returns.R)

### (2) Categorical Predictors and Interactions

Slides: [categorical-predictors.pdf](slides/categorical-predictors.pdf), [interactions.pdf](slides/interactions.pdf)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapter 3.3

Code for class:
- [Interactions example: stock returns](code/interactions_example.R)

### (3) Assumptions, Diagnostics, and Nonlinear Regression

Slides: [regression-assumptions-and-data-cleaning.pdf](slides/regression-assumptions-and-data-cleaning.pdf), [modeling-nonlinear-relationships.pdf](slides/modeling-nonlinear-relationships.pdf)

Supplemental: [Slope Interpretations in the Presence of Logs](slides/log_slope_derivations.pdf)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 3.3, 7.1

Code for class:
- [Regression diagnostics: Saratoga houses](code/saratoga_diag.R)

### (4) Time Series Regression

Slides: [time-series-regression.pdf](slides/time-series-regression.pdf), [dynamic-linear-models-and-kalman-filter.pdf](slides/dynamic-linear-models-and-kalman-filter.pdf)

Supplemental: [Gaussian posterior derivation](slides/gaussian_posterior_derivation.pdf), [Gaussian product and prior/likelihood/posterior (t=0→1, t=1→2)](slides/kalman_prior_likelihood_posterior.pdf)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapter 3 (supplemental)

Code for class:
- [Kalman filter on NVIDIA daily returns](code/kalman_filter_nvda.R)

### (5) Logistic Regression

Slides: [logistic-regression.pdf](slides/logistic-regression.pdf)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 4.1-4.3

Code for class:
- [TBA](code/)

### (6) Model Selection and Penalized Regression

Slides: [model-selection.pdf](slides/model-selection.pdf), [penalized-regression.pdf](slides/penalized-regression.pdf)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 6.1-6.5

Code for class:
- [doctor_shortage_model_selection.R](code/doctor_shortage_model_selection.R) (best subsets, stepwise regression; uses [counties.csv](data/counties.csv))
- [smallbeer.R](code/smallbeer.R) (price elasticity of demand with LASSO; uses [smallbeer.csv](data/smallbeer.csv))
- [congress109.R](code/congress109.R) (penalized logistic regression: predict party from speech; uses [congress109.csv](data/congress109.csv), [congress109members.csv](data/congress109members.csv))
- [congress114.R](code/congress114.R) (same analysis for 114th Congress 2015–2017; requires [congress114.csv](data/congress114.csv), [congress114members.csv](data/congress114members.csv)—see [data/congress114_README.md](data/congress114_README.md) for data instructions)

### (7) Trees, Ensembles, Neural Networks, and Deep Neural Networks

Slides: [regression-trees.pdf](slides/regression-trees.pdf), [intro-to-neural-networks.pdf](slides/intro-to-neural-networks.pdf)

Readings:
- _Introduction to Statistical Learning_ (ISL) -- Chapters 8.1-8.3, 11.1-11.3

Code for class:
- [trees_walkthrough.R](code/trees_walkthrough.R)
- [basic_nn.ipynb](code/basic_nn.ipynb)
- [classification_nn.ipynb](code/classification_nn.ipynb)
- [MNIST.ipynb](code/MNIST.ipynb)

### (8) Causal Inference (Part 1)

Slides: [TBA](slides/)

Readings:
- _Mastering 'Metrics_ (MM) -- Chapter 1
- _Introduction to Statistical Learning_ (ISL) -- Chapter 3.2

Code for class:
- [TBA](code/)

### (9) Causal Inference (Part 2)

Slides: [TBA](slides/)

Readings:
- _Mastering 'Metrics_ (MM) -- Chapters 3-5

Code for class:
- [TBA](code/)
