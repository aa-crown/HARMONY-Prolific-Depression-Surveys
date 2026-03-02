#test-retest analyses
library(dplyr)
library(ggplot2)
library(psych)
library(plotly)
library(irr)

#import data -------------------------------------------------
survey_questions <- read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/full_survey/combo_feb18.csv") %>% 
  select(-contains("timestamp"),
         -contains("complete"),
         -contains("diagnosis"),
         -contains("meds"),
         -contains("atten"),
         -contains("X"),
         -contains("info"),
         -contains("redcap")) %>%  
  rename_with(~ paste0(., "_t1"), -prolific_id)

retest_questions <- read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/retest/combo_retest_feb18.csv") %>% 
  select(-contains("timestamp"),
         -contains("complete"),
         -contains("diagnosis"),
         -contains("meds"),
         -contains("atten"),
         -contains("X"),
         -contains("info"),
         -contains("redcap")) %>%  
  rename_with(~ paste0(., "_t2"), -prolific_id)



survey_scores <- read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/summary_scores/prolific_scores_Feb18.csv") %>% 
  select(-contains("timestamp"),
         -contains("info")) %>% 
  rename_with(~ paste0(., "_t1"), -prolific_id)

retest_scores <- read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/summary_scores/prolific_scores_RETEST_Feb18.csv") %>% 
  select(-contains("timestamp"),
         -contains("info")) %>% 
  rename_with(~ paste0(., "_t2"), -prolific_id)


dems_retest <- read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/demographic_data/combo_retest_dem_feb18.csv")


combo_wide_questions <- inner_join(survey_questions, retest_questions, by="prolific_id")
combo_wide_scores <- inner_join(survey_scores, retest_scores, by="prolific_id")

#need to go back and talk about people who are missing GAD7 & BDI
#!!!!!!!!!!!!!!!
summary(complete.cases(combo_wide_questions))
combo_wide_questions <- na.omit(combo_wide_questions)

summary(complete.cases(combo_wide_scores))

#test-retest analyses---------------------------------------------------------------

#question level------------------------------------------------------------------------
items <- gsub("_t1", "", grep("_t1$", names(combo_wide_questions), value = TRUE))

questions_icc <- sapply(items, function(question) {
  
  df <- data.frame(
    combo_wide_questions[[paste0(question, "_t1")]],
    combo_wide_questions[[paste0(question, "_t2")]]
  )
  
  icc(df,
      model = "twoway",
      type = "agreement",
      unit = "single")$value
})

#visualize
icc_df <- data.frame(
  question = items,
  ICC = questions_icc
)


icc_df <- icc_df[order(icc_df$ICC), ]
icc_df$question <- factor(icc_df$question, levels = icc_df$question)

p2 <- ggplot(icc_df,
             aes(x = reorder(question, ICC),
                 y = ICC,
                 text = paste("Scale:", question,
                              "<br>ICC:", round(ICC, 3)))) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  geom_hline(yintercept = 0.60,
             linetype = "dashed") +
  labs(title = "Question-Level Test–Retest ICC",
       x = "Survey",
       y = "ICC")

ggplotly(p2, tooltip = "text")


#scales/scores (survey level)--------------------------------------------------------------------
scores <- gsub("_t1", "", grep("_t1$", names(combo_wide_scores), value = TRUE))

scale_icc <- sapply(scores, function(survey) {
  
  df <- data.frame(
    combo_wide_scores[[paste0(survey, "_t1")]],
    combo_wide_scores[[paste0(survey, "_t2")]]
  )
  
  icc(df,
      model = "twoway",
      type = "agreement",
      unit = "single")$value
})


#visualize
icc_df <- data.frame(
  score = scores,
  ICC = scale_icc
)


icc_df <- icc_df[order(icc_df$ICC), ]
icc_df$sco <- factor(icc_df$score, levels = icc_df$score)

p2 <- ggplot(icc_df,
             aes(x = reorder(score, ICC),
                 y = ICC,
                 text = paste("Scale:", score,
                              "<br>ICC:", round(ICC, 3)))) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Scale-Level Test–Retest ICC",
       x = "Survey",
       y = "ICC")

ggplotly(p2, tooltip = "text")

#survey level with error bars---------------------------------------------------
scores <- gsub("_t1", "", grep("_t1$", names(combo_wide_scores), value = TRUE))

icc_results <- sapply(scores, function(survey) {
  
  df <- data.frame(
    combo_wide_scores[[paste0(survey, "_t1")]],
    combo_wide_scores[[paste0(survey, "_t2")]]
  )
  
  out <- icc(df,
             model = "twoway",
             type = "agreement",
             unit = "single")
  
  c(
    ICC   = out$value,
    lower = out$lbound,
    upper = out$ubound
  )
})


#visualize
icc_df <- data.frame(
  scores = scores,
  ICC    = icc_results["ICC", ],
  lower  = icc_results["lower", ],
  upper  = icc_results["upper", ]
)

icc_df$flag_low <- icc_df$lower < 0.60

icc_df <- icc_df[order(icc_df$ICC), ]
icc_df$sco <- factor(icc_df$score, levels = icc_df$score)


p <- ggplot(icc_df,
aes(x = reorder(scores, ICC),
    y = ICC,
    text = paste("Scale:", scores,
                 "<br>ICC:", round(ICC, 3),
                 "<br>95% CI:",
                 paste0("(", round(lower, 2),
                        ", ", round(upper, 2), ")")))) +
  
  #CI band
  geom_linerange(aes(ymin = lower, ymax = upper),
                 size = 6,      # controls thickness
                 alpha = 0.2,   # transparency
                 color = "blue") +
  
  # ICC point
  geom_point(aes(color = flag_low),
             size = 2.5) +
  
  geom_hline(yintercept = 0.60,
             linetype = "dashed") +
  
  coord_flip() +
  theme_minimal() +
  labs(title = "Test–Retest Reliability (ICC with 95% CI)",
       x = "Scale",
       y = "ICC") +
  scale_color_manual(values = c("FALSE" = "black",
                                "TRUE"  = "red"),
                     name = "Lower CI < .60")

ggplotly(p, tooltip = "text")
