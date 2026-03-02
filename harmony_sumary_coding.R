#summary coding HARMONY Prolific instruments 
library(dplyr)
library(descr)

#import dataset-------------------------------------------------
#df <- read.csv("/Users/angelicacrown/Downloads/HARMONYProlificSeque_DATA_2025-12-11_1322.csv")
#df <- read.csv("/Users/angelicacrown/Downloads/HARMONYProlificSeque_DATA_2025-12-29_1122.csv")
#df <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/total questions data/combo_jan23.csv')

#df <- read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/full_survey/combo_feb18.csv")
#df <-  read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/retest/combo_retest_feb18.csv")


#remove participants who didn't finish full survey
df <- df %>% 
  filter(!is.na(atten03))


#removing "complete" vars, not needed
df <- df %>%
  select(-ends_with("complete"))

#initializing summary dataset with participants as rows
summary_df <- df[,c("info_sheet", "info_timestamp", "prolific_id")]

##define functions---------------------------------------------------------------------

#sum cols together using prefix
#TEST
#data <- df %>% 
#  select(prolific_id, apath01_timestamp, apath01_apath_01, apath01_apath_02)
#summed <- sum_by_prefix(df=data, prefix="apath01", summary_df, new_col_name="apathy_sum)

sum_by_prefix <- function(df, prefix, summary_df, new_col_name = NULL) {
  
  # If user doesn't provide a new column name, default to prefix_sum
  if (is.null(new_col_name)) {
    new_col_name <- paste0(prefix, "_sum")
  }
  
  # Columns to exclude
  exclude_cols <- paste0(prefix, "_timestamp")
  
  # Identify numeric columns that start with prefix but not timestamp
  numeric_cols <- df %>%
    select(starts_with(prefix)) %>%
    select(-all_of(exclude_cols)) %>%
    select(where(is.numeric)) %>%
    names()
  
  #sum instrument items together
  grouped_sum <- df %>%
    group_by(prolific_id) %>%
    summarise(
      "{new_col_name}" := rowSums(across(all_of(numeric_cols)), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merge with summary_df
  summary_df <- summary_df %>%
    left_join(grouped_sum, by = "prolific_id")
  
  return(summary_df)
}


#sub 1 for 0 index
#TEST
#data <- data.frame(xyz=c(1,2,3,4), xy_timestamp=c(4,3,2,1), yz=c(1,2,3,4))
#indexed <- subtract_one_by_prefix(data, prefix = "xy")
subtract_one_by_prefix <- function(df, prefix) {
  
  # exclude timestamp column
  exclude_cols <- paste0(prefix, c("_timestamp"))
  
  # Identify cols to sub 1 from
  cols_to_modify <- df %>%
    select(starts_with(prefix)) %>%
    select(-all_of(exclude_cols)) %>%
    names()
  
  # Subtract 1 from those columns
  df <- df %>%
    mutate(across(all_of(cols_to_modify), ~ . - 1))
  
  return(df)
}

#reverse coding variables 
#args are df, vector of vars to be reverse coded, max of current scale (integer)
#TEST
#data <- data.frame(x=c(1,2,3,4), y=c(4,3,2,1))
#reversed <- reverse_code(data, c("x","y"),4)
reverse_code <- function(df, vars, max) {
  for (v in vars) {
    if (!v %in% names(df)) {
      warning(paste("Column", v, "not found in dataframe. Skipping."))
      next
    }
    col <- df[[v]]
    
    # Only reverse if numeric
    if (!is.numeric(col)) {
      warning(paste("Column", v, "is not numeric. Skipping."))
      next
    }
    
    # Reverse code: new = max + 1 − old
    #https://www.statology.org/reverse-coding-in-excel/
    df[[v]] <- max +1 - col
    
  }
  return(df)
}

#initializing function to binarize attention columns
#==0 if answer we are looking for, 1 otherwise
#TEST
#data <- df %>% 
#    select(prolific_id, atten01, atten02, atten03)
#summary_df <- atten_binary(data, "atten01", true_answer = 1, summary_df, "atten01_biiined")
atten_binary <- function(df, column_name, true_answer, summary_df, new_col_name = NULL) {
  
  # Default name col_code
  if (is.null(new_col_name)) {
    new_col_name <- paste0(column_name, "_code")
  }
  
  #dummy code
  coded <- df %>%
    group_by(prolific_id) %>%
    summarise(
      "{new_col_name}" := ifelse(.data[[column_name]] == true_answer, 0, 1),
      .groups = "drop"
    )
  
  # Merge with summary_df
  summary_df <- summary_df %>%
    left_join(coded, by = "prolific_id")
  
  return(summary_df)
}

#fxn to get max response from subset of items 
#needed for qids
#args: df, items to get max between, summary_df, new_col_name (optional)
max_by_items <- function(df, items, summary_df, new_col_name) {
  
  #get max score
  domain_scores <- df %>%
    group_by(prolific_id) %>%
    summarise(
      "{new_col_name}" := pmax(!!!syms(items), na.rm = TRUE),
      .groups = "drop"
    )
  
  #join dataframes 
  summary_df %>%
    left_join(domain_scores, by = "prolific_id")
}


####apath-----done--------------------------------------------------------

#https://www.dementiaresearch.org.au/wp-content/uploads/2016/06/AES_Guidelines.pdf
#normally 1=not at all .... 4=a lot
#we coded it as 1==very true, 4=not at all

#normally, would need to reverse code all except 6,10,11. We do the opposite
#6: "getting things done during the day is important to me"
#10: "I put little effort into anything"
#11: "I approach life with intensity"

#reverse code #6, #10, #11 -- according to link above
#negativly worded ?s for us are 10, 12, 13
#10- "I put little effort into anything"
#12- "Someone has to tell me what to do each day"
#13 - "I am less concerned about my problems than I should be"


df <- reverse_code(df, c("apath01_apath_10", "apath01_apath_12", "apath01_apath_13"), 4)
summary_df <- sum_by_prefix(df, summary_df, prefix="apath01")

####bisbas----done---------------------------------------------------------
#https://www.psy.miami.edu/faculty/ccarver/bisbas.html

#yields 1 BIS score and 3 BAS scores
# Items other than 2 and 22 are reverse-scored.
# 
# BAS Drive:  3, 9, 12, 21
# BAS Fun Seeking:  5, 10, 15, 20
# BAS Reward Responsiveness:  4, 7, 14, 18, 23
# BIS:  2, 8, 13, 16, 19, 22, 24
# Items 1, 6, 11, 17,  are fillers.
df <- reverse_code(df, c("bisbas01_bisbas1",
                                       
                                       "bisbas01_bisbas3",
                                       "bisbas01_bisbas4",
                                       "bisbas01_bisbas5",
                                       "bisbas01_bisbas6",
                                       "bisbas01_bisbas7",
                                       "bisbas01_bisbas8",
                                       "bisbas01_bisbas9",
                                       "bisbas01_bisbas10",
                                       "bisbas01_bisbas11",
                                       "bisbas01_bisbas12",
                                       "bisbas01_bisbas13",
                                       "bisbas01_bisbas14",
                                       "bisbas01_bisbas15",
                                       "bisbas01_bisbas16",
                                       "bisbas01_bisbas17",
                                       "bisbas01_bisbas18",
                                       "bisbas01_bisbas19",
                                       "bisbas01_bisbas20",
                                       "bisbas01_bisbas21",
                                      
                                       "bisbas01_bisbas23",
                                       "bisbas01_bisbas24"), 4)


#BIS (Punishment Sensitivity Scale): 2, 8, 13, 16, 19, 22, 24
bis <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
             "bisbas01_timestamp",
             "bisbas01_bisbas2",
             "bisbas01_bisbas8",
             "bisbas01_bisbas13",
             "bisbas01_bisbas16",
             "bisbas01_bisbas19",
             "bisbas01_bisbas22",
             "bisbas01_bisbas24")]
summary_df <- sum_by_prefix(bis, "bisbas01", summary_df, "bisbas_bis_sum")

#BAS Reward Responsiveness: 4, 7, 14, 18, 23
bas_reward <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
             "bisbas01_timestamp",
             "bisbas01_bisbas4",
             "bisbas01_bisbas7",
             "bisbas01_bisbas14",
             "bisbas01_bisbas18",
             "bisbas01_bisbas23")]
summary_df <- sum_by_prefix(bas_reward, "bisbas01", summary_df, "bisbas_BASreward_sum")

#BAS Drive: 3, 9, 12, 21
bas_drive <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                    "bisbas01_timestamp",
                    "bisbas01_bisbas3",
                    "bisbas01_bisbas9",
                    "bisbas01_bisbas12",
                    "bisbas01_bisbas21")]
summary_df <- sum_by_prefix(bas_drive, "bisbas01", summary_df, "bisbas_BASdrive_sum")

#BAS Fun Seeking: 5, 10, 15, 20
bas_fun <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                   "bisbas01_timestamp",
                   "bisbas01_bisbas5",
                   "bisbas01_bisbas10",
                   "bisbas01_bisbas15",
                   "bisbas01_bisbas20")]
summary_df <- sum_by_prefix(bas_fun, "bisbas01", summary_df, "bisbas_BASfun_sum")



####rcads-----done--------------------------------------------------------
#https://www.camh.ca/-/media/files/rcads-quick-guide-pdf.pdf

#total summary score, coded regularly
summary_df<-sum_by_prefix(df=df, prefix="rcads01", summary_df=summary_df, new_col_name = "rcads_total")

#subscales:
#MDD: 10 items
#OCD: 6 items
#Social Phobia: 9 items
#SAD: 7 items
#Panic Disorder: 9 items
#GAD: 6 items

#MDD: 2, 6, 11, 15, 19, 21, 25, 29, 40, 47
rcads_mdd <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                  "rcads01_timestamp",
                  "rcads01_rcads_2",
                  "rcads01_rcads_6",
                  "rcads01_rcads_11",
                  "rcads01_rcads_15",
                  "rcads01_rcads_19",
                  "rcads01_rcads_21",
                  "rcads01_rcads_25",
                  "rcads01_rcads_29",
                  "rcads01_rcads_40",
                  "rcads01_rcads_47")]
summary_df <- sum_by_prefix(rcads_mdd, "rcads01", summary_df, new_col_name = "rcads_mdd")

#OCD: 10,16, 23, 31, 42, 44 
rcads_ocd <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                   "rcads01_timestamp",
                   "rcads01_rcads_10",
                   "rcads01_rcads_16",
                   "rcads01_rcads_23",
                   "rcads01_rcads_31",
                   "rcads01_rcads_42",
                   "rcads01_rcads_44")]
summary_df <- sum_by_prefix(rcads_ocd, "rcads01", summary_df, new_col_name = "rcads_ocd")

#Social Phobia: 4, 7, 8, 12, 20, 30, 32, 38, 43 
rcads_social <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                   "rcads01_timestamp",
                   "rcads01_rcads_4",
                   "rcads01_rcads_7",
                   "rcads01_rcads_8",
                   "rcads01_rcads_12",
                   "rcads01_rcads_20",
                   "rcads01_rcads_30",
                   "rcads01_rcads_32",
                   "rcads01_rcads_38",
                   "rcads01_rcads_43")]
summary_df <- sum_by_prefix(rcads_social, "rcads01", summary_df, new_col_name = "rcads_social")

#Separation anxiety: 5, 9, 17, 18, 33, 45, 46
rcads_separate <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                      "rcads01_timestamp",
                      "rcads01_rcads_5",
                      "rcads01_rcads_9",
                      "rcads01_rcads_17",
                      "rcads01_rcads_18",
                      "rcads01_rcads_33",
                      "rcads01_rcads_45",
                      "rcads01_rcads_46")]
summary_df <- sum_by_prefix(rcads_separate, "rcads01", summary_df, new_col_name = "rcads_separate")

#Panic Disorder: 3, 14, 24, 26, 28, 34, 36, 39, 41
rcads_panic <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                      "rcads01_timestamp",
                      "rcads01_rcads_3",
                      "rcads01_rcads_14",
                      "rcads01_rcads_24",
                      "rcads01_rcads_26",
                      "rcads01_rcads_28",
                      "rcads01_rcads_34",
                      "rcads01_rcads_36",
                      "rcads01_rcads_39",
                      "rcads01_rcads_41")]
summary_df <- sum_by_prefix(rcads_panic, "rcads01", summary_df, new_col_name = "rcads_panic")

#GAD: 1, 13, 22, 27, 35, 37
rcads_GAD <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                        "rcads01_timestamp",
                        "rcads01_rcads_1",
                        "rcads01_rcads_13",
                        "rcads01_rcads_22",
                        "rcads01_rcads_27",
                        "rcads01_rcads_35",
                        "rcads01_rcads_37")]
summary_df <- sum_by_prefix(rcads_GAD, "rcads01", summary_df, new_col_name = "rcads_GAD")

####mfq-------done------------------------------------------------------
#https://www.corc.uk.net/outcome-measures-guidance/directory-of-outcome-measures/mood-and-feelings-questionnaire-mfq/

#Scores on the long version range from 0 to 66. 
#Scoring 27 or higher on the long version may indicate the presence of depression in the respondent.

#need to subtract 1 from all scores such that "not true"=0, "sometimes"=1, "true"=2
#add together
df <- subtract_one_by_prefix(df, "mfq01")
summary_df <- sum_by_prefix(df, "mfq01", summary_df)

####nffi------done-------------------------------------------------------
#https://nesdo.onderzoek.io/wp-content/uploads/2016/08/NEO_NO1_205.pdf

#neuroticism subscale
#(6+11+21+26+36+41+51+56) - (1+16+31+46)
##we have a 5 point scale, should be a 7 pt scale ----- need to fix for future survey releases

#add together positively coded items
nffi_neuro_pos <- df[,c("info_sheet", "info_timestamp", "prolific_id",
                        "nffi01_timestamp",
                        "nffi01_nffi_6", 
                        "nffi01_nffi_11", 
                        "nffi01_nffi_21",
                        "nffi01_nffi_26",
                        "nffi01_nffi_36",
                        "nffi01_nffi_41",
                        "nffi01_nffi_51",
                        "nffi01_nffi_56")]
nffi_pos_temp <- sum_by_prefix(nffi_neuro_pos, "nffi01", summary_df, "pos_tmp")

#add together negatively coded items
nffi_neuro_neg <- df[,c("info_sheet", "info_timestamp", "prolific_id",
                        "nffi01_timestamp",
                        "nffi01_nffi_1",
                        "nffi01_nffi_16",
                        "nffi01_nffi_31",
                        "nffi01_nffi_46")]
nffi_neg_temp <- sum_by_prefix(nffi_neuro_neg, "nffi01", summary_df, "neg_tmp")

#create temporary df for scoring
nffi_scoring <- df %>% 
  select(info_sheet, 
         prolific_id,
         nffi01_timestamp)
nffi_scoring$nffi01_pos <- nffi_pos_temp$pos_tmp
nffi_scoring$nffi01_neg <- nffi_neg_temp$neg_tmp

#subtract neg from pos and include summary score in summary df
summary_df <- summary_df %>%
  left_join(
    nffi_scoring %>%
      transmute(prolific_id, nffi_score = nffi01_pos - nffi01_neg),
    by = "prolific_id"
  )

####shaps-----done------------------------------------------------------
#https://www.recoveryanswers.org/assets/snaith-hamilton_pleasure_scale_shaps.pdf

#2 or less is normal score

#sub 1 from all to get 0 index
#reverse code 2,4,5,7,9 so that 0="strongly agree"
df <- reverse_code(df, c("shaps01_shaps2", 
                         "shaps01_shaps4", 
                         "shaps01_shaps5", 
                         "shaps01_shaps7", 
                         "shaps01_shaps9" ), 4)
df <- subtract_one_by_prefix(df, "shaps01")
summary_df <- sum_by_prefix(df, "shaps01", summary_df)


####rbqa------done?-------------------------------------------------------
#https://nda.nih.gov/data-structure/rbqa01

#sub 1 for 0 index
df <- subtract_one_by_prefix(df, "rbqa01")
#add together? Unclear if this is what we should do, but seems natural
summary_df <- sum_by_prefix(df, "rbqa01", summary_df)

####stai------done-------------------------------------------------------
#https://arc.psych.wisc.edu/self-report/state-trait-anxiety-inventory-sta/

#no need to initially change any coding

#SAI (state) all items, reverse coded: 1,2, 5, 8, 10, 11,15, 16, 19, 20
df <- reverse_code(df, c("stai01_stai1", "stai01_stai2", "stai01_stai5", "stai01_stai_state8_i", "stai01_stai10", "stai01_stai15", "stai01_stai16", "stai01_stai_state19_i", "stai01_stai20"), 
                   max=4) 
SAI_subset <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                    "stai01_timestamp",
                    "stai01_stai1", 
                    "stai01_stai2", 
                    "stai01_stai3", 
                    "stai01_stai_state4_i",
                    "stai01_stai5", 
                    "stai01_stai6", 
                    "stai01_stai7", 
                    "stai01_stai_state8_i", 
                    "stai01_stai_state9_i",
                    "stai01_stai10", 
                    "stai01_stai11", 
                    "stai01_stai12", 
                    "stai01_stai13", 
                    "stai01_stai_state14_i",
                    "stai01_stai15", 
                    "stai01_stai16", 
                    "stai01_stai17", 
                    "stai01_stai_state18_i",
                    "stai01_stai_state19_i", 
                    "stai01_stai20")
                 ]
summary_df <- sum_by_prefix(SAI_subset, "stai01", summary_df, new_col_name = "SAI_sum")


#TAI (trait) all items, reverse coded:  1,3, 6, 7, 10, 13,14, 16, 19
df <- reverse_code(df, c("stai01_stai21", "stai01_stai_trait3_i", "stai01_stai26", "stai01_stai27", "stai01_stai30", "stai01_stai13", "stai01_stai_trait14_i", "stai01_stai36", "stai01_stai39"), 
                   max=4) 
TAI_subset <- df[ ,c("info_sheet", "info_timestamp", "prolific_id", 
                    "stai01_timestamp",
                    "stai01_stai21", 
                    "stai01_stai_trait2_i", 
                    "stai01_stai_trait3_i",
                    "stai01_stai24", 
                    "stai01_stai_trait5_i",
                    "stai01_stai26", 
                    "stai01_stai27", 
                    "stai01_stai28", 
                    "stai01_stai29", 
                    "stai01_stai30",
                    "stai01_stai_trait11_i",
                    "stai01_stai32", 
                    "stai01_stai33", 
                    "stai01_stai_trait14_i",
                    "stai01_stai_trait15_i",
                    "stai01_stai36", 
                    "stai01_stai37", 
                    "stai01_stai38", 
                    "stai01_stai39", 
                    "stai01_stai40")
                 ]
summary_df <- sum_by_prefix(TAI_subset, "stai01", summary_df, new_col_name = "TAI_sum")

####MASQ-------------------------------------------------------------
#https://arc.psych.wisc.edu/self-report/mood-and-anxiety-symptom-questionnaire-masq/ 
#--scoring notes pdf at bottom of ^^
#https://www.scribd.com/document/700865494/Mini-MASQ
#--for 26 ?s ^^
#FINAL REFERENCE: MAPI RESEARCH TRUST 1991 Watson & Clark copyright 

# Anxious Arousal (AA, 17 items):
masq_AA <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                 "masq01_timestamp",
                 'masq01_71',
                 'masq01_72',
                 'masq01_73',
                 'masq01_74',
                 'masq01_75',
                 'masq01_76',
                 'masq01_77',
                 'masq01_78',
                 'masq01_79',
                 'masq01_80',
                 'masq01_81',
                 'masq01_82',
                 'masq01_83',
                 'masq01_84',
                 'masq01_85',
                 'masq01_86',
                 'masq01_87')]
summary_df <- sum_by_prefix(masq_AA, "masq01", summary_df, new_col_name = "masq_AA_sum")

# General Distress: Mixed Symptoms: GDM 15 items
#-- not available in short form
df <- reverse_code(df, c('masq01_36'), 5)

masq_GDM <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                 "masq01_timestamp",
                 #pos coded (normal)
                 'masq01_2',
                 'masq01_12',
                 'masq01_16',
                 'masq01_17',
                 'masq01_20',
                 'masq01_23',
                 'masq01_25',
                 'masq01_26',
                 'masq01_27',
                 'masq01_28',
                 'masq01_29',
                 'masq01_30',
                 'masq01_31',
                 'masq01_34',
                 #reverse coded
                 'masq01_36'
                 )]
summary_df <- sum_by_prefix(masq_GDM, "masq01", summary_df, new_col_name = "masq_GDM_sum")


# GD: Anxious Symptoms (GDA, 11 items)
masq_GDA <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                 "masq01_timestamp",
                 'masq01_7',
                 'masq01_10',
                 'masq01_13',
                 'masq01_15',
                 'masq01_18',
                 'masq01_22',
                 'masq01_24',
                 'masq01_32',
                 'masq01_35',
                 'masq01_37',
                 'masq01_38')]
summary_df <- sum_by_prefix(masq_GDA, "masq01", summary_df, new_col_name = "masq_GDA_sum")

# GD: Depressive Symptoms (GDD, 12 items): 
masq_GDD <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                 "masq01_timestamp",
                 'masq01_1',
                 'masq01_3',
                 'masq01_4',
                 'masq01_5',
                 'masq01_6',
                 'masq01_8',
                 'masq01_9',
                 'masq01_11',
                 'masq01_14',
                 'masq01_19',
                 'masq01_21',
                 'masq01_33')]
summary_df <- sum_by_prefix(masq_GDD, "masq01", summary_df, new_col_name = "masq_GDD_sum")

# Anhedonic Depression (AD, 22 items)
# Positively keyed items: 18, 25, 33, 41, 50, 51, 57, 61
# Negative keyed items: 3, 7, 10, 15, 22, 27, 39, 43, 47, 49, 53, 56, 58, 60
df <- reverse_code(df, c('masq01_39',
                         'masq01_40',
                         'masq01_42',
                         'masq01_43',
                         'masq01_44',
                         'masq01_45',
                         'masq01_46',
                         'masq01_48',
                         'masq01_50',
                         'masq01_51',
                         'masq01_53',
                         'masq01_55',
                         'masq01_57',
                         'masq01_58'
                         ), 5)

masq_AD <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                 "masq01_timestamp",
                 ##pos coding (normal)
                 'masq01_62',
                 'masq01_63',
                 'masq01_64',
                 'masq01_65',
                 'masq01_66',
                 'masq01_67',
                 'masq01_68',
                 'masq01_70',
                 #reverse coding
                 'masq01_39',
                 'masq01_40',
                 'masq01_42',
                 'masq01_43',
                 'masq01_44',
                 'masq01_45',
                 'masq01_46',
                 'masq01_48',
                 'masq01_50',
                 'masq01_51',
                 'masq01_53',
                 'masq01_55',
                 'masq01_57',
                 'masq01_58'
                 )]
summary_df <- sum_by_prefix(masq_AD, "masq01", summary_df, new_col_name = "masq_AD_sum")

####gad7------done-------------------------------------------------------
#https://www.hiv.uw.edu/page/mental-health-screening/gad-7

#sub 1 to get 0 index
#add together
df <- subtract_one_by_prefix(df, "gad701")
summary_df <- sum_by_prefix(df, "gad701", summary_df)

####bdi-------done------------------------------------------------------
#https://www.ismanet.org/doctoryourspirit/pdfs/Beck-Depression-Inventory-BDI.pdf

#already is 0 index
#add together
summary_df <- sum_by_prefix(df, "bdi01", summary_df)

####rrs-------done------------------------------------------------------

#just add together
#scale ranges 22 - 88
summary_df <- sum_by_prefix(df, "rrs01", summary_df)

####qids------done-------------------------------------------------------
#https://alnursing.org/wp-content/uploads/2020/03/Depression-Questionnaire-QIDS-SR-16.pdf
#total score 0-27
#get 0 index
df <- subtract_one_by_prefix(df, "qids01")
 
#create temporary df for intermediate calculations
qids_scoring <- df %>% 
  select(info_sheet, 
         prolific_id,
         qids01_timestamp,
         qids01_vmdsd,
         qids01_vintr,
         qids01_vengy,
         qids01_vvwsf,
         qids01_vcntr,
         qids01_vsuic
         )

#1. Enter the highest score on any 1 of the 4 sleep items (items 1 to 4).
    #qids01_vsoin
    #qids01_vmnin
    #qids01_vemin
    #qids01_vhysm
qids_scoring <- max_by_items(df, 
                             items=c("qids01_vsoin", "qids01_vmnin", "qids01_vemin", "qids01_vhysm"), 
                             summary_df=qids_scoring, 
                             new_col_name = "qids01_sleep")

#Enter the highest score on any 1 of the 4 weight items (items 6 to 9).
    #qids01_vapdc
    #qids01_vapin
    #qids01_vwtdc
    #qids01_vwtin
qids_scoring <- max_by_items(df, 
                             items=c("qids01_vapdc", "qids01_vapin", "qids01_vwtdc" , "qids01_vwtin"), 
                             summary_df=qids_scoring, 
                             new_col_name = "qids01_weight")


#Enter the highest score on either of the 2 psychomotor items (15 and 16).
    #qids01_vslow
    #qids01_vagit
qids_scoring <- max_by_items(df, 
                             items=c("qids01_vslow", "qids01_vagit"), 
                             summary_df=qids_scoring, 
                             new_col_name = "qids01_psymotor")

#2. There will be one score for each of the 9 DSM-IV Major Depressive Disorder symptom domains.
#3. Add the scores of the 9 items below to obtain the total score   .
  #sleep, c("qids01_vsoin", "qids01_vmnin", "qids01_vemin", "qids01_vhysm")
  #weight, c("qids01_vapdc", "qids01_vapin", "qids01_vwtdc" , "qids01_vwtin")
  #psychomotor changes, c("qids01_vslow", "qids01_vagit")
  #depressed mood, c("qids01_vmdsd")
  #decreased interest, c("qids01_vintr")
  #fatigue, c("qids01_vengy")
  #guilt, c("qids01_vvwsf")
  #concentration, c("qids01_vcntr")
  #suicidal ideation c("qids01_vsuic")
summary_df <- sum_by_prefix(qids_scoring, "qids01", summary_df, "qids_score")


####who01-----done--------------------------------------------------------
#https://www.who.int/standards/classifications/international-classification-of-functioning-disability-and-health/who-disability-assessment-schedule

#sub 1 for 0 index
#add together
df <- subtract_one_by_prefix(df, "who01")
summary_df <- sum_by_prefix(df, "who01", summary_df)

####dass------done-------------------------------------------------------
#https://novopsych.com/assessments/depression/depression-anxiety-stress-scales-long-form-dass-42/

#sub 1 for 0 index
#add
df <- subtract_one_by_prefix(df, "dass01")
summary_df <- sum_by_prefix(df, "dass01", summary_df, new_col_name = "dass_total_sum")

# Each of the three DASS-42 scales contains 14 items:

#   Depression (Items 3, 5, 10, 13, 16, 17, 21, 24, 26, 31, 34, 37, 38, 42)
# Symptoms such as dysphoria, hopelessness, devaluation of life, self-deprecation, lack of interest/involvement, anhedonia, and inertia. 
dass_dep <- df[,c("info_sheet", "info_timestamp", "prolific_id","dass01_timestamp", 
                  "dass01_dass_3", 
                  "dass01_dass_5", 
                  "dass01_dass_10",
                  "dass01_dass_13",
                  "dass01_dass_16",
                  "dass01_dass_17",
                  "dass01_dass_21",
                  "dass01_dass_24",
                  "dass01_dass_26",
                  "dass01_dass_31",
                  "dass01_dass_34",
                  "dass01_dass_37",
                  "dass01_dass_38",
                  "dass01_dass_42")]
summary_df <- sum_by_prefix(dass_dep, "dass01", summary_df, new_col_name = "dass_dep_sum")

# Anxiety (Items 2, 4, 7, 9, 15, 19, 20, 23, 25, 28, 30, 36, 40, 41)
# Symptoms such as physiological arousal and fear components of anxiety. It assesses autonomic arousal typical of anxiety, such as trembling, sweating, feelings of panic, and the fear of losing control. The anxiety items are intended to measure the respondent’s experience of anxious arousal, and are not focussed on the worry typical of Generalised Anxiety Disorder.
dass_anx <- df[,c("info_sheet", "info_timestamp", "prolific_id",
                  "dass01_timestamp", 
                  "dass01_dass_2", 
                  "dass01_dass_4", 
                  "dass01_dass_7",
                  "dass01_dass_9",
                  "dass01_dass_15",
                  "dass01_dass_19",
                  "dass01_dass_20",
                  "dass01_dass_23", #MISSING Q 23 IN QUESTIONAIRE
                  "dass01_dass_25",
                  "dass01_dass_28",
                  "dass01_dass_30",
                  "dass01_dass_36",
                  "dass01_dass_40",
                  "dass01_dass_41")]
summary_df <- sum_by_prefix(dass_anx, "dass01", summary_df, new_col_name = "dass_anx_sum")


# Stress (Items 1, 6, 8, 11, 12, 14, 18, 22, 27, 29, 32, 33, 35, 39)
# Chronic symptoms of non-specific arousal. It assesses difficulty relaxing, nervous arousal, and being easily upset/agitated, irritable/over-reactive, and impatient. Stress items are focused on the respondent’s state of tension and chronic general arousal, capturing how much the respondent feels overburdened or overwhelmed by life’s stressors.
dass_stress <- df[,c("info_sheet", "info_timestamp", "prolific_id","dass01_timestamp", 
                  "dass01_dass_1", 
                  "dass01_dass_6", 
                  "dass01_dass_8",
                  "dass01_dass_11",
                  "dass01_dass_12",
                  "dass01_dass_14",
                  "dass01_dass_18",
                  "dass01_dass_22",
                  "dass01_dass_27",
                  "dass01_dass_29", #MISSING
                  "dass01_dass_32",
                  "dass01_dass_33",
                  "dass01_dass_35",
                  "dass01_dass_39" #MISSING
      )]
summary_df <- sum_by_prefix(dass_stress, "dass01", summary_df, new_col_name = "dass_stress_sum")


####bricope---done----------------------------------------------------------
#https://novopsych.com/assessments/formulation/brief-cope/

#3 subscales
#score is average (sum item scores/# of items)
#avoidant coping: Items 1, 3, 4, 6, 8, 11, 16, 19
avoid <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
               "briefcope01_timestamp", 
               "briefcope01_bc_1", 
               "briefcope01_bc_3",
               "briefcope01_bc_4",
               "briefcope01_bc_6",
               "briefcope01_bc_8", 
               "briefcope01_bc_11",
               "briefcope01_bc_16",
               "briefcope01_bc_19")]
summary_df <- sum_by_prefix(avoid, "briefcope01", summary_df, "briefcope_avoid_avg") %>% 
  mutate(briefcope_avoid_avg = briefcope_avoid_avg / 8)


#problem focused: Items 2, 7, 10, 12, 14, 17, 23, 25
problem <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
               "briefcope01_timestamp", 
               "briefcope01_bc_2", 
               "briefcope01_bc_7",
               "briefcope01_bc_10",
               "briefcope01_bc_12",
               "briefcope01_bc_14", 
               "briefcope01_bc_17",
               "briefcope01_bc_23",
               "briefcope01_bc_25")]
summary_df <- sum_by_prefix(problem, "briefcope01", summary_df, "briefcope_problem_avg") %>% 
  mutate(briefcope_problem_avg = briefcope_problem_avg / 8)


#emotion focused: Items 5, 9, 13, 15, 18, 20, 21, 22, 24, 26, 27, 28
emotion <- df[,c("info_sheet", "info_timestamp", "prolific_id", 
                 "briefcope01_timestamp", 
                 "briefcope01_bc_5", 
                 "briefcope01_bc_9",
                 "briefcope01_bc_13",
                 "briefcope01_bc_15",
                 "briefcope01_bc_18", 
                 "briefcope01_bc_20",
                 "briefcope01_bc_21",
                 "briefcope01_bc_22",
                 "briefcope01_bc_24",
                 "briefcope01_bc_26",
                 "briefcope01_bc_27",
                 "briefcope01_bc_28")]
summary_df <- sum_by_prefix(emotion, "briefcope01", summary_df, "briefcope_emotion_avg") %>% 
  mutate(briefcope_emotion_avg = briefcope_emotion_avg / 8)

####bai-------done------------------------------------------------------
#https://res.cloudinary.com/dpmykpsih/image/upload/great-plains-health-site-358/media/1087/anxiety.pdf

#sub 1 for 0 index
#add
df <- subtract_one_by_prefix(df, "bai01")
summary_df <- sum_by_prefix(df, "bai01", summary_df)

####panas-----done--------------------------------------------------------
#https://ogg.osu.edu/media/documents/MB%20Stream/PANAS.pdf

#Positive Affect Score: Add the scores on items 1, 3, 5, 9, 10, 12, 14, 16, 17, and 19. 
#Scores can range from 10 – 50, with higher scores representing higher levels of positive affect
pos_panas <- df[,c("info_sheet", "info_timestamp", "prolific_id", "panas01_timestamp", "panas01_interested_q1", "panas01_excited_q3", "panas01_strong_q5", "panas01_enthusiastic_q9", "panas01_proud_q10", "panas01_alert_q12", "panas01_inspired_q14", "panas01_determined_q16", "panas01_attentive_q17", "panas01_active_q19")]
summary_df <- sum_by_prefix(pos_panas, "panas01", summary_df, new_col_name = "panas_pos")

#Negative Affect Score: Add the scores on items 2, 4, 6, 7, 8, 11, 13, 15, 18, and 20. 
#Scores can range from 10 – 50, with lower scores representing lower levels of negative affect.
neg_panas <- df[, c("info_sheet", "info_timestamp", "prolific_id", "panas01_timestamp", "panas01_distressed_q2", "panas01_upset1_q4", "panas01_guilty_q6", "panas01_scared_q7", "panas01_hostile_q8", "panas01_irritable_q11", "panas01_ashamed_q13", "panas01_nervous_q15", "panas01_jittery_q18", "panas01_afraid_q20")]
summary_df <- sum_by_prefix(pos_panas, "panas01", summary_df, new_col_name = "panas_neg")
####rds-------done------------------------------------------------------
#https://pubmed.ncbi.nlm.nih.gov/34708477/ - invented by 1st WAPIAW

#sum together
summary_df <- sum_by_prefix(df, "rds4", summary_df)

####scl-------done------------------------------------------------------
#average score--divide by number of items

#can't find documentation-  only for 90 pt version
summary_df <- sum_by_prefix(df, "scl20", summary_df, "scl_20_avg") %>% 
  mutate(scl_20_avg = scl_20_avg / 20)

####wsas------done-------------------------------------------------------
#https://ebchelp.blueprint.ai/en/articles/9939604-work-and-social-adjustment-scale-wsas

#sub 1 to get 0 index
#add
df <- subtract_one_by_prefix(df, "wsas")
summary_df <- sum_by_prefix(df, "wsas", summary_df)

####whoqol----done---------------------------------------------------------
#https://novopsych.com/assessments/formulation/who-quality-of-life-brief-whoqol-bref/

#The total score is presented between 26 and 156, where higher scores represent higher levels of quality of life.

# fix weird naming issue
names(df)[names(df) == "whoqolbref_timestamp"] <- "wwhoqolbref_timestamp"

#total score
summary_df <- sum_by_prefix(df, "wwhoqolbref", summary_df, "whoqol_total_sum")

#Physical health (items 3, 4, 10, 15, 16, 17, 18). Raw scores between 7 and 35
qol_phys <- df[,c("info_sheet", "info_timestamp", "prolific_id", "wwhoqolbref_timestamp", "wwhoqolbref_3", "wwhoqolbref_4", "wwhoqolbref_10", "wwhoqolbref_15", "wwhoqolbref_16", "wwhoqolbref_17", "wwhoqolbref_18")]
summary_df <- sum_by_prefix(qol_phys, "wwhoqolbref", summary_df, "whoqol_phys_sum")

#Psychological Health (items 5, 6, 7, 11, 19, 26). Raw score between 6 and 30
qol_psych <- df[,c("info_sheet", "info_timestamp", "prolific_id", "wwhoqolbref_timestamp", "wwhoqolbref_5", "wwhoqolbref_6", "wwhoqolbref_7", "wwhoqolbref_11", "wwhoqolbref_19", "wwhoqolbref_26")]
summary_df <- sum_by_prefix(qol_psych, "wwhoqolbref", summary_df, "whoqol_psych_sum")

#Social relationships (items 20, 21, 22). Raw score between 3 and 15
qol_social <- df[,c("info_sheet", "info_timestamp", "prolific_id", "wwhoqolbref_timestamp", "wwhoqolbref_20", "wwhoqolbref_21", "wwhoqolbref_22")]
summary_df <- sum_by_prefix(qol_social, "wwhoqolbref", summary_df, "whoqol_social_sum")

#Environment (items 8, 9, 12, 13, 14, 23, 24, 25). Raw score between 8 and 40
qol_enviro <- df[,c("info_sheet", "info_timestamp", "prolific_id", "wwhoqolbref_timestamp", "wwhoqolbref_8", "wwhoqolbref_9", "wwhoqolbref_12", "wwhoqolbref_13", "wwhoqolbref_14", "wwhoqolbref_23", "wwhoqolbref_24", "wwhoqolbref_25")]
summary_df <- sum_by_prefix(qol_enviro, "wwhoqolbref", summary_df, "whoqol_enviro_sum")

####phq10-----done--------------------------------------------------------
#zero index
#add together 9 questions, exclude 10th
#for comparing phq10 scores--only run initialization and then phq

df <- subtract_one_by_prefix(df, "phq10")

#select 9 relevant questions
phq9 <- df[,c("info_sheet", "info_timestamp", "prolific_id", "phq10_timestamp","phq10_1", "phq10_2", "phq10_3", "phq10_4", "phq10_5", "phq10_6", "phq10_7", "phq10_8", "phq10_9")]
summary_df <- sum_by_prefix(phq9, "phq10", summary_df)


####atten01---done----------------------------------------------------------
#Select "strongly agree" if you read this question
summary_df_atten <- atten_binary(df, "atten01", true_answer = 4, summary_df = summary_df, new_col_name = "atten01_code")


####atten02---done----------------------------------------------------------
#I work 15 months out of every year
summary_df_atten <- atten_binary(df, "atten02", true_answer = 1, summary_df = summary_df_atten, new_col_name = "atten02_code")

####atten03---done----------------------------------------------------------
#I have never used a computer
summary_df_atten <- atten_binary(df, "atten03", true_answer = 1, summary_df = summary_df_atten, new_col_name = "atten03_code")



####medications---------------------------------------------------------


####diagnoses-----------------------------------------------------------



###export dataset-----------------------------------------------------------
#write.csv(summary_df, file = "/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/summary_scores/prolific_scores_Feb18.csv", row.names = FALSE)
#write.csv(summary_df_atten, file = "/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/attention/prolific_atten_scores_Feb18.csv", row.names = FALSE)
#write.csv(summary_df, file = "/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/phq9_reliability/prolific_phq_Feb18.csv")
#write.csv(summary_df, file = "/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/summary_scores/prolific_scores_RETEST_Feb18.csv", row.names = FALSE)





