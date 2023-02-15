# Define the function
set.seed(1)

save_data_frames <- function() {
  examScoreData <- data.frame(math_scores = round(rnorm(100, mean = 50, sd = 10), digits=2),
                              biology_scores = round(rnorm(100, mean = 50, sd = 10), digits=2),
                              stress_level = sample(c("high", "middle", "low"), 100, replace = T),
                              sex = sample(c("male", "female"), 100, replace = T),
                              semester = sample(c("summer", "winter", "fall"), 100, replace = T))

  BMIData <-  data.frame(body_weight = rnorm(1000, mean = 175, sd = 20),
                         sex = sample(c("male", "female"), 1000, replace = T),
                         age_category = sample(c("10-20", "21-30", "31-40"), 1000, replace = T),
                         region = sample(c("urban", "rural"), 1000, replace = T),
                         state = sample(c("NY", "Ohio", "Chicago", "Alaska"), 1000, replace = T))

  ncd_binary_regression =data.frame(id=1:1000,
                                    age=sample(23:98, 10000, replace=T),
                                    sex=sample(c('Male', 'Female'), 1000, replace=T),
                                    smoking =sample(c('Yes', 'No'), 1000, replace=T),
                                    alcohol =sample(c('Yes', 'No'), 1000, replace=T),
                                    fruits_servings_a_day = sample(0:14, 1000, replace=T),
                                    vegetable_servings_a_day = sample(0:14, 1000, replace=T),
                                    diabetes =sample(c('Yes', 'No'), 1000, replace=T),
                                    cancer =sample(c('Yes', 'No'), 1000, replace=T),
                                    hypertension =sample(c('Yes', 'No'), 1000, replace=T),
                                    copd =sample(c('Yes', 'No'), 1000, replace=T),
                                    asthma =sample(c('Yes', 'No'), 1000, replace=T),
                                    arthritis =sample(c('Yes', 'No'), 1000, replace=T),
                                    high_cholesterol =sample(c('Yes', 'No'), 1000, replace=T),
                                    ckd =sample(c('Yes', 'No'), 1000, replace=T),
                                    bmi=sample(16.32:45.21, 1000, replace=T))
  cc<- c("Strongly Agree", "Agree" ,"Neutral","Disagree", "Strongly Disagree")
  personality_SEM <- data.frame(id=1:7564,
                                age=sample(15:98, 7564, replace=T),
                                sex=sample(c('Male', 'Female'), 7564, replace=T),
                                Extraversion = sample(cc, 7564, replace = T, prob = c(0.15, 0.17, 0.32, 0.21, 0.15)),
                                Agreeableness = sample(cc, 7564, replace = T, prob = c(0.19, 0.23, 0.20, 0.17, 0.21)),
                                Conscientiousness = sample(cc, 7564, replace = T, prob = c(0.13, 0.21, 0.19, 0.25, 0.22)),
                                Neuroticism = sample(cc, 7564, replace = T, prob = c(0.10, 0.15, 0.25, 0.30, 0.20)),
                                Openness = sample(cc, 7564, replace = T, prob = c(0.15, 0.20, 0.30, 0.25, 0.10)),
                                Assertiveness = sample(cc, 7564, replace = T, prob = c(0.20, 0.30, 0.25, 0.15, 0.10)),
                                Empathy = sample(cc, 7564, replace = T, prob = c(0.30, 0.25, 0.20, 0.15, 0.10)),
                                Impulsiveness = sample(cc, 7564, replace = T, prob = c(0.15, 0.20, 0.25, 0.30, 0.10)),
                                Curiosity = sample(cc, 7564, replace = T, prob = c(0.20, 0.25, 0.30, 0.20, 0.05)),
                                Honesty = sample(cc, 7564, replace = T, prob = c(0.30, 0.25, 0.20, 0.15, 0.10)),
                                Awareness = sample(cc, 7564, replace = T, prob = c(0.13, 0.21, 0.18, 0.27, 0.21)),
                                Compassion = sample(cc, 7564, replace = T, prob = c(0.15, 0.25, 0.19, 0.23, 0.18)),
                                Understanding = sample(cc, 7564, replace = T, prob = c(0.17, 0.23, 0.18, 0.29, 0.13)),
                                Recklessness = sample(cc, 7564, replace = T, prob = c(0.12, 0.25, 0.22, 0.28, 0.13)),
                                Exploration = sample(cc, 7564, replace = T, prob = c(0.15, 0.22, 0.24, 0.26, 0.13)),
                                Sincerity = sample(cc, 7564, replace = T, prob = c(0.19, 0.24, 0.20, 0.25, 0.12)),
                                Integrity = sample(cc, 7564, replace = T, prob = c(0.14, 0.26, 0.21, 0.27, 0.12)),
                                Interest = sample(cc, 7564, replace = T, prob = c(0.30, 0.25, 0.20, 0.15, 0.10)),
                                Inquisitive = sample(cc, 7564, replace = T, prob = c(0.16, 0.22, 0.24, 0.24, 0.14)),
                                Inspiration = sample(cc, 7564, replace = T, prob = c(0.30, 0.25, 0.20, 0.15, 0.10)),
                                Candor = sample(cc, 7564, replace = T, prob = c(0.14, 0.19, 0.21, 0.27, 0.19)),
                                Influence = sample(cc, 7564, replace = T, prob = c(0.13, 0.15, 0.24, 0.28, 0.20))
  )

  diabetes_binary <- data.frame(id=1:1000,
                                age = round(rnorm(1000, mean = 50, sd = 10)),
                                sex = sample(c("Male", "Female"), 1000, replace = T),
                                diabetes = sample(c("Yes", "No"), 1000, replace = T),
                                bmi = round(rnorm(1000, mean = 25, sd = 5), 1),
                                smoking = sample(c("Yes", "No"), 1000, replace = T),
                                alcohol = sample(c("Yes", "No"), 1000, replace = T),
                                exercise = sample(c("Yes", "No"), 1000, replace = T),
                                education = sample(c("High school", "College", "Graduate"), 1000, replace = T),
                                income = round(rnorm(1000, mean = 50000, sd = 20000)))

  cigar_poisson <- data.frame(id=1:1000,
                              age = round(rnorm(1000, mean = 50, sd = 10)),
                              sex = sample(c("Male", "Female"), 1000, replace = T),
                              diabetes = sample(c("Yes", "No"), 1000, replace = T),
                              bmi = rnorm(1000, mean = 25, sd = 5),
                              urban = sample(c("Urban", "Rural"), 1000, replace = T),
                              education = sample(c("High School", "College", "Graduate"), 1000, replace = T),                          no_cigars_day = sample(0:16, 1000, replace = T),
                              no_drinks_day = sample(0:13, 1000, replace = T),
                              hours_exercise_day = sample(0:9, 1000, replace = T))

  healthcare_multinomial <- data.frame(age = round(rnorm(1000, mean = 50, sd = 10)),
                                       sex = sample(c("Male", "Female"), 1000, replace = T),
                                       diabetes = sample(c("Yes", "No"), 1000, replace = T),
                                       bmi = rnorm(1000, mean = 25, sd = 5),
                                       blood_pressure = round(rnorm(1000, mean = 120, sd = 10)),
                                       cholesterol = round(rnorm(1000, mean = 200, sd = 50)),
                                       smoke = sample(c("Yes", "No"), 1000, replace = T),
                                       healthcare = sample(c("hospital", "clinic", "herbal"), 1000, replace = T))


  diabetes_survival <- data.frame(time = rexp(1000, rate = 0.1),
                                  age = round(rnorm(1000, mean = 50, sd = 10)),
                                  sex = sample(c("Male", "Female"), 1000, replace = T),
                                  diabetes = sample(c("Yes", "No"), 1000, replace = T),
                                  event=rbinom(1000, size = 1, prob = 0.5))

  gpa_manova <- data.frame(GPA = round(rnorm(1000, mean = 3.0, sd = 0.5), 2),
                           Study_Habits = sample(1:5, 1000, replace = T),
                           Motivation = sample(1:5, 1000, replace = T),
                           Age=sample(round(rnorm(1000, mean = 22, sd = 2), 0)))

  examscore_anova <- data.frame(section=rep(c("A", "B", "C"), each=200),
                                math_score=round(c(rnorm(180, mean=10), rnorm(210, mean=8), rnorm(210, mean=12)), digits = 2))

  sales_timeseries <-  data.frame(dates <- as.Date("1980-01-01") + sample(0:(365*32), 10000, replace=T),
                                  product_a=sample(1000:2000, size = 1000, replace = T),
                                  product_b=sample(500:1500, size = 1000, replace = T),
                                  product_c=sample(300:1300, size = 1000, replace = T))





  # Save the data frames as objects in the environment of the package
  assign("examScoreData", examScoreData, envir = .GlobalEnv)
  assign("BMIData", BMIData, envir = .GlobalEnv)
  assign("cigar_poisson", cigar_poisson, envir = .GlobalEnv)
  assign("healthcare_multinomial", healthcare_multinomial, envir = .GlobalEnv)
  assign("diabetes_survival", diabetes_survival, envir = .GlobalEnv)
  assign("gpa_manova", gpa_manova, envir = .GlobalEnv)
  assign("sales_timeseries", sales_timeseries, envir = .GlobalEnv)
  assign("examscore_anova", examscore_anova, envir = .GlobalEnv)
  assign("ncd_binary_regression", ncd_binary_regression, envir = .GlobalEnv)
  assign("personality_SEM", personality_SEM, envir = .GlobalEnv)
  assign("diabetes_binary", diabetes_binary, envir = .GlobalEnv)
}

# Call the function to save the data frames
save_data_frames()

# Define the function that returns the data frame
get_data_frame <- function(df_name) {
  return(get(df_name, envir = .GlobalEnv))
}

# Load the data frame by calling the `get_data_frame` function with the name of the data frame
examScoreData <- get_data_frame("examScoreData")
BMIData <- get_data_frame("BMIData")
cigar_poisson <- get_data_frame("cigar_poisson")
healthcare_multinomial <- get_data_frame("healthcare_multinomial")
diabetes_survival <- get_data_frame("diabetes_survival")
gpa_manova <- get_data_frame("gpa_manova")
sales_timeseries <- get_data_frame("sales_timeseries")
examscore_anova <- get_data_frame("examscore_anova")
ncd_binary_regression <- get_data_frame("ncd_binary_regression")
personality_SEM <- get_data_frame("personality_SEM")
diabetes_binary <- get_data_frame("diabetes_binary")


