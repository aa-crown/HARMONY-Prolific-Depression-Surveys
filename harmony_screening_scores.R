library(descr)
library(dplyr)
library(ggplot2)

#initial harmony screening data set
#harmony_screening <- read.csv("/Users/angelicacrown/Downloads/HARMONYScreeningProl_DATA_2025-12-03_1211.csv")

#screen 02 (300 people, Feb 17th 2026)
harmony_screening <- read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/screeners/HARMONYScreeningProl_DATA_2026-02-17_SCREEN02.csv")

#removing two participants who revoked consent
#screener test
#harmony_screening <- harmony_screening[harmony_screening$prolific_id != "65f0a43ed1d258cc4c97805d", ]
#harmony_screening <- harmony_screening[harmony_screening$prolific_id != "5db48b4d14dab8000de45fdf", ]

#screen 02 feb 17th
harmony_screening <- harmony_screening %>%
  filter(!prolific_id %in% c(#returned
                             "699470f58c5f30338818a092",
                             "6612b3449105fbfa6c67d9c1",
                             "5f3ac1732efa0a74f975b1a8",
                             "63e630188c3e4ac4a92b1ff5",
                             "697135c23c7970c52efbf29a",
                             "5f2a5f187bf471245f7dc096",
                             "5f4df2b4bdb2449bd751ce51",
                             '69944e0e4be8b356d67c9129',
                             #timed out
                             "6521ab1c4a2ffd6c21a3b662")) %>% 
  filter(phq10_complete==2)

#filter out non-white participants
#dem <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/screeners/prolific_demographic_SCREEN02.csv') %>% 
#  mutate(prolific_id = Participant.id)

#dem_screen <- inner_join(harmony_screening, dem, by="prolific_id")

#harmony_screening <- dem_screen %>% 
#  filter(!Ethnicity.simplified=="White")

#change to normal 0 index
harmony_screening[,c("phq10_1", "phq10_2", "phq10_3", "phq10_4", "phq10_5", "phq10_6", "phq10_7", "phq10_8", "phq10_9", "phq10_10")]<-
  harmony_screening[,c("phq10_1", "phq10_2", "phq10_3", "phq10_4", "phq10_5", "phq10_6", "phq10_7", "phq10_8", "phq10_9", "phq10_10")] -1

#creating overall score
harmony_screening <- harmony_screening %>%
  mutate(
    summary_score = rowSums(across(c(phq10_1:phq10_9)))
  )

#frequency table
freq(harmony_screening$summary_score)

#creating var for if met threshold
above_5 <- sum(harmony_screening$summary_score >= 5)
harmony_screening$met_threshold <- ifelse(harmony_screening$summary_score >= 5, 1, 0)

#dataset with only people who met the threshold of 5
prolific_screen01_thresholded <- harmony_screening %>% 
  filter(met_threshold == 1)

#visualize
ggplot(prolific_screen01_thresholded, aes(summary_score))+
  geom_bar(fill="lightblue")+
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  )+
  scale_x_continuous(breaks = seq(min(prolific_screen01_thresholded$summary_score), max(prolific_screen01_thresholded$summary_score), by = 2))+
  ggtitle("Screen 02 After Thresholding")

#export dataset
#write.csv(prolific_screen01_thresholded, file = "/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/thresholded_screeners/prolific_screen02_thresholded.csv", row.names = FALSE)

