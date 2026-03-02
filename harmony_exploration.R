#HARMONY Prolific data analysis/exploration
library(ggplot2)
library(reshape2)
library(Rtsne)
library(dplyr)
library(knitr)
library(kableExtra)
library(sass)
library(heatmaply)
library(fields)
library(psych)
library(ggpubr)
library(RColorBrewer)
library(DT)
library(stringr)
library(ggrepel)
library(uwot)
library(tidyr)

###import dataset------------------------------------------
#made using summary_coding_prolific.R file
#scores <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/summary scores/prolific_scores_1-30-26.csv')
scores <- read.csv("/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/summary_scores/prolific_scores_Feb18.csv")


#remove ids to get pure numeric matrix
scores_noID <- scores[, -(1:3)]

#import question by question 
questions <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/full_survey/combo_feb18.csv') 

questions_noID <- questions[, -(1:4)] %>% 
  filter(!is.na(atten03)) 

questions_noID <- questions_noID %>% 
  select(-contains("timestamp"),
         -contains("complete"),
         -contains("diagnosis"),
         -contains("meds"),
         -contains("atten"),
         -contains("id"))

#first prolific screener demographics dataset
dem_df <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/demographic data/feb18_combo_demographics.csv')

dem_survey <- dem_df  %>% 
  mutate(Time.taken=Time.taken/60) %>% 
  rename(prolific_id = Participant.id)

#scores and demographics combined
scores_dem <- left_join(scores, dem_survey %>% 
                          select(prolific_id, 
                                 Age, 
                                 Time.taken), 
                        by="prolific_id")

scores_dem_noID <- scores_dem[, -(1:4)] %>% 
  mutate(Age = as.numeric(Age))
  


#instrument x instrument heatmap-------------------------
#https://r-graph-gallery.com/heatmap.html

spearman_cors <- cor(scores_noID,
                     method = "spearman",
                     use = "pairwise.complete.obs")

heatmaply(spearman_cors,
          dendrogram = "both",
          hclust_method = "ward.D2",
          dist_method = "euclidean",
          colors = RdYlGn, limits = c(-1, 1),
          grid_color = "white",
          grid_width = 0.00001,
          titleX = FALSE,
          branches_lwd = 0.1,
          fontsize_row = 10, fontsize_col = 10,
          labCol = colnames(spearman_cors),
          labRow = rownames(spearman_cors),
          heatmap_layers = theme(axis.line=element_blank()),
  #        reorderfun = ward.cl
          main = "Instrument by Instrument \n\ Spearman"
)

#instrument x instrument heatmap w/ demographics-------------------------

cors_scores_dem <- cor(scores_dem_noID,
                     method = "pearson",
                     use = "pairwise.complete.obs")

heatmaply(cors_scores_dem,
          dendrogram = "both",
          hclust_method = "ward.D2",
          dist_method = "euclidean",
          colors = RdYlGn, limits = c(-1, 1),
          grid_color = "white",
          grid_width = 0.00001,
          titleX = FALSE,
          branches_lwd = 0.1,
          fontsize_row = 10, fontsize_col = 10,
          labCol = colnames(cors_scores_dem),
          labRow = rownames(cors_scores_dem),
          heatmap_layers = theme(axis.line=element_blank()),
          main= "Instrument by Instrument + Demographics \n\ Spearman"
)

#question by question heatmap-------------------------------------

#spearman correlation
cors_questions <- cor(questions_noID,
                      method = "spearman",
                      use = "pairwise.complete.obs")


heatmaply(
  cors_questions,
  dendrogram = "both",
  hclust_method = "ward.D2",
  dist_method = "euclidean",
  colors = RdYlGn, limits = c(-1, 1),
  fontsize_row = 1,
  fontsize_col = 1,
  grid_color = NA,
  plot_method = "plotly",
  main = "Question by Question"
)

#tSNE plot------------------------------------------------

scores_scaled <- scale(scores_noID)
scores_scaled <- t(scores_scaled)

set.seed(123)

#get tSNE results in a df
tsne_out <- Rtsne(
  scores_scaled,
  dims = 2,
  perplexity = 15,
  theta = 0.5,
  verbose = TRUE,
  check_duplicates = FALSE
)
tsne_df <- data.frame(
  Dim1 = tsne_out$Y[, 1],
  Dim2 = tsne_out$Y[, 2]
)

#can add in demographic variables for color visualization later maybe
#tsne_df$group <- t(scores_dem$Age)

# add row names as a label column
tsne_df$label <- rownames(scores_scaled)

p_tsne <- ggplot(tsne_df, aes(Dim1, Dim2, text = label)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(
    title = "t-SNE plot",
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2"
  )

# make it interactive
ggplotly(p_tsne, tooltip = "text")

#uMAP plot------------------------------------------------
set.seed(123)

umap_out <- umap(
  scores_scaled,
  n_neighbors = 15,   # roughly analogous to perplexity
  n_components = 2,
  metric = "euclidean"
)

umap_df <- data.frame(
  Dim1 = umap_out[, 1],
  Dim2 = umap_out[, 2],
  label = rownames(scores_scaled)
)

p_umap <- ggplot(umap_df, aes(Dim1, Dim2, text = label)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(
    title = "uMAP plot",
    x = "uMAP Dimension 1",
    y = "uMAP Dimension 2"
  )

ggplotly(p_umap, tooltip = "text")

#PCA on questions w/ scree plot--------------------------
questions_noNA <- na.omit(questions_noID)

#perform PCA
pca_questions <- prcomp(questions_noNA, 
                  center = TRUE, 
                  scale. = TRUE)

#extract eigenvalues 
eigenvals <- pca_questions$sdev^2

#create df with PC #s with correspoding eigenvalues
scree_df <- data.frame(
  PC = seq_along(eigenvals),
  Eigenvalue = eigenvals
)

#plot PCA
ggplot(scree_df, aes(x = PC, y = Eigenvalue)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Scree Plot at Question Level",
    x = "Principal Component",
    y = "Eigenvalue"
  ) +
  theme_minimal()

#score level PCA w/ scree plot---------------------------------
scores_noNA <- na.omit(scores_noID)
pca_questions <- prcomp(scores_noNA, 
                        center = TRUE, 
                        scale. = TRUE)

#extract eigenvalues 
eigenvals <- pca_questions$sdev^2

#create df with PC #s with correspoding eigenvalues
scree_df <- data.frame(
  PC = seq_along(eigenvals),
  Eigenvalue = eigenvals
)

#plot PCA
ggplot(scree_df, aes(x = PC, y = Eigenvalue)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Scree Plot at Score Level",
    x = "Principal Component",
    y = "Eigenvalue"
  ) +
  theme_minimal()

#calculate variance explained
var_explained <- eigenvals / sum(eigenvals)

plot(cumsum(var_explained),
     type = "b",
     xlab = "Number of Components",
     ylab = "Cumulative Proportion of Variance Explained")

#plot cumlative variance explained with individual variance explained
cum_var_explained <- cumsum(var_explained)

pca_var_df <- data.frame(
  PC = seq_along(eigenvals),
  Variance = var_explained,
  CumulativeVariance = cum_var_explained
)

ggplot(pca_var_df, aes(x = PC)) +
  geom_line(aes(y = Variance, color = "Individual"), linewidth = 1) +
  geom_point(aes(y = Variance, color = "Individual")) +
  geom_line(aes(y = CumulativeVariance, color = "Cumulative"), linewidth = 1) +
  geom_point(aes(y = CumulativeVariance, color = "Cumulative")) +
  scale_color_manual(
    name = "Variance",
    values = c("Individual" = "steelblue", "Cumulative" = "darkred")
  ) +
  labs(
    title = "Variance Explained by Principal Components",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()

ggplot(pca_var_df, aes(x = PC)) +
  geom_line(aes(y = Variance, color = "Individual"), linewidth = 1) +
  geom_point(aes(y = Variance, color = "Individual")) +
  geom_line(aes(y = CumulativeVariance, color = "Cumulative"), linewidth = 1) +
  geom_point(aes(y = CumulativeVariance, color = "Cumulative")) +
  scale_color_manual(
    name = "Variance",
    values = c("Individual" = "steelblue", "Cumulative" = "darkred")
  ) +
  labs(
    title = "Variance Explained by Principal Components \n\ First 15 Components",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()+
  coord_cartesian(xlim = c(1, 15))
#varimax rotation for interpretability--------------------
#perform PCA
pca_res <- principal(
  questions_noNA,
  nfactors = 30,
  rotate = "varimax",
  scores = TRUE
)

#visualize variables
#extract loadings
loadings <- as.data.frame(unclass(pca_res$loadings))
loadings$variable <- rownames(loadings)

#plot
fig <- plot_ly(
  loadings,
  x = ~RC1,
  y = ~RC2,
  type = "scatter",
  mode = "markers",
  text = ~paste(
    "Variable:", variable,
    "<br>RC1:", round(RC1, 2),
    "<br>RC2:", round(RC2, 2)
  ),
  hoverinfo = "text"
)

fig

#visualize components - hover plot
top_n <- 30
threshold <- 0.3

component_summary <- loadings %>%
  pivot_longer(
    cols = starts_with("RC"),
    names_to = "component",
    values_to = "loading"
  ) %>%
  mutate(abs_loading = abs(loading)) %>%
  filter(abs_loading >= threshold) %>%
  group_by(component) %>%
  arrange(desc(abs_loading)) %>%
  slice_head(n = top_n) %>%
  summarise(
    hover_text = paste(
      paste0(variable, ": ", round(loading, 2)),
      collapse = "<br>"
    )
  )


component_df <- data.frame(
  component = names(pca_res$Vaccounted["Proportion Var", ]),
  variance = as.numeric(pca_res$Vaccounted["Proportion Var", ])
)


component_df <- left_join(component_df, component_summary, by = "component")

fig <- plot_ly(
  component_df,
  x = ~component,
  y = ~variance,
  type = "scatter",
  mode = "markers",
  text = ~hover_text,
  hoverinfo = "text"
)

fig

component_df %>%
  kable(
    format = "html",
    escape = FALSE,
    caption = "Variance Explained by Each Component - Varimax Rotation",
    align = "c"
  ) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )


###distribution of demographics-----------------------------
#sex-----------------------------------------
ggplot(dem_survey, aes(Sex))+
  geom_bar(fill="lightblue", color = "black")+
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  )+
  ggtitle("Biological Sex - Taken Full Survey")

#age-----------------------------------------
ggplot(dem_survey, aes(Age))+
  geom_bar(fill="lightblue", color = "black") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45)  # Tilt x-axis labels
  )+
  ggtitle("Age Distribution \n\ Taken Full Survey")

#time-----------------------------------------
boxplot(dem_survey$Time.taken)
outliers <- boxplot(dem_survey$Time.taken, plot=FALSE)$out
x<-dem_survey
x<- x[-which(x$Time.taken %in% outliers),]

ggplot(x, aes(x = Time.taken)) +
  geom_histogram(binwidth = 4, fill = "lightblue", color = "black") +
  scale_x_continuous(breaks = seq(0, max(dem_df$Time.taken, na.rm = TRUE), by = 2)) +
  xlab("Time Taken (Minutes)") +
  ylab("Count") +
  theme_minimal()+
  ggtitle("Time Taken Distrubution \n\ Full Survey")

#ethnicity screener-----------------------------------------
screen02 <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/screeners/prolific_demographic_SCREEN02.csv')
ggplot(screen02, aes(Ethnicity.simplified))+
  geom_bar(fill="lightblue", color = "black")+
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  )+
  ggtitle("Ethnicity Distribution \n\ Screener 02")

#ethnicity full survey-----------------------------------------
ggplot(dem_survey, aes(Ethnicity.simplified))+
  geom_bar(fill="lightblue", color = "black")+
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  )+
  ggtitle("Ethnicity Distribution \n\ Taken Full Survey")

#age x time taken scatter plot-----------------------------
ggplot(x, aes(x = Age, y = Time.taken)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  labs(
    x = "Age",
    y = "Time Taken",
    title = "Age vs Time Taken"
  )

#overall severity-----------------------------------
ggplot(data=scores)+
  geom_bar(aes(x=phq10_sum), fill="lightblue", color = "black")+
  scale_x_continuous(breaks = seq(0, max(scores$phq10_sum, na.rm = TRUE), by = 1))+
  ggtitle("Overall Severity by PHQ9 Sum Score \n\ Taken Full Survey")
  


###QC----------------------------
#attention check ?s----------------------------------
atten_df <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/attention questions/prolific_atten_scores_screen02_97participants.csv')
summary_table_atten <- atten_df %>%
  count(atten01_code, atten02_code, atten03_code)

summary_table_atten


#reliability of PHQ by question----------------------------------
#screeners vs full survey, absolute score difference
# screen <- read.csv("/Users/angelicacrown/Downloads/prolific_screen01_thresholded.csv")%>% 
#   select(prolific_id, contains("phq10"), -contains("timestamp"))
# survey <- read.csv("/Users/angelicacrown/Downloads/HARMONYProlificSeque_DATA_2025-12-11_1322.csv") %>% 
#   select(prolific_id, contains("phq10"), -contains("timestamp"))

screen <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/thresholded screeners/prolific_screen02_thresholded.csv')%>% 
  select(prolific_id, contains("phq10"), -contains("timestamp"))
survey <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/total questions data/combo_jan23.csv') %>% 
  select(prolific_id, contains("phq10"), -contains("timestamp"))

common <- screen %>%
  inner_join(survey, by = "prolific_id",
             suffix = c("_screen", "_survey"))

common <- common %>% 
  select(prolific_id, contains("phq10"), -contains("timestamp"), -contains("complete")) %>% 
  filter(!is.na(phq10_1_survey))

phq10_diff <- common %>%
  mutate(across(ends_with("_survey"), 
                ~ . - get(sub("_survey$", "_screen", cur_column())),
                .names = "{sub('_survey$', '_diff', .col)}")) %>%
  select(prolific_id, ends_with("_diff"))

#make a table
phq10_diff %>%
  kable(
    caption = "PHQ-10 Score Differences by Participant",
    align = "c"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))



#reliability of PHQ by score------------------------------------
PHQ_scores_inital <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/thresholded screeners/prolific_screen02_thresholded.csv') %>% 
  select(prolific_id, 
         sum.summary_score)
PHQ_scores_survey <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/phq9 reliability/phq9_scores_surveys2_3_4_5_1.2.csv') %>% 
  select(prolific_id, 
         phq10_sum)

scores_combined <- inner_join(PHQ_scores_inital, PHQ_scores_survey, by = "prolific_id") %>% 
  rename(initial = sum.summary_score,
         survey = phq10_sum) %>% 
  mutate(surv_minus_init = survey - initial)



#table
# scores_combined %>%
#   kable(
#     caption = "PHQ-10 Score Differences by Participant",
#     align = "c"
#   ) %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
# 

#scatter plot
ggplot(scores_combined, aes(x = initial, y = survey)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  labs(
    x = "Initial",
    y = "Survey",
    title = "Survey vs Initial"
  ) +
  theme_minimal()

#ICC
icc_data <- scores_combined %>%
  select(initial, survey)

ICC(icc_data)

#meds---------------------------------------------------
ggplot(questions, aes(meds01_1))+
  geom_bar(fill="lightblue")+
  ggtitle("Medication: Yes/No")+
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3) +
  theme_minimal()

questions %>% 
  count(meds01_2, name = "Count") %>%
  datatable(options = list(pageLength = 100))

questions <- questions %>%
  mutate(
    sertraline = as.integer(str_detect(meds01_2, regex("sertraline", ignore_case = TRUE))),
    
    lexapro = as.integer(
      str_detect(meds01_2, regex("lexapro", ignore_case = TRUE))),
    
    prozac = as.integer(
      str_detect(meds01_2, regex("fluoxetine|prozac", ignore_case = TRUE))),
    
    zoloft = as.integer(str_detect(meds01_2, regex("zoloft", ignore_case = TRUE))),
    
    abilify = as.integer(str_detect(meds01_2, regex("abilify", ignore_case = TRUE)))
  )

questions %>% 
  select(sertraline, lexapro, prozac, zoloft, abilify) %>% 
  summarise(across(everything(), sum))

#diagnoses---------------------------------------------------
ggplot(questions, aes(diagnosis01_1))+
  geom_bar(fill="lightblue")+
  ggtitle("Diagnosis: Yes/No")+
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.3) +
  theme_minimal()

questions %>% 
  count(diagnosis01_2, name = "Count") %>%
  datatable(options = list(pageLength = 100))

questions <- questions %>%
  mutate(
    anxiety = as.integer(str_detect(diagnosis01_2, regex("anxiety|gad", ignore_case = TRUE))),
    
    depression = as.integer(
      str_detect(diagnosis01_2, regex("depression|depressive|depressed|\\bmdd\\b", ignore_case = TRUE))),
    
    ptsd = as.integer(
      str_detect(diagnosis01_2, regex("\\bptsd\\b|post[- ]?traumatic stress disorder", ignore_case = TRUE))),
    
    adhd = as.integer(str_detect(diagnosis01_2, regex("adhd|add", ignore_case = TRUE))),
    
    bipolar = as.integer(str_detect(diagnosis01_2, regex("bipolar|bi polar|bi pol", ignore_case = TRUE)))
  )

questions %>% 
  select(anxiety, depression, ptsd, adhd, bipolar) %>% 
  summarise(across(everything(), sum))

#meds vs severity------------------------------------------
scores <- read.csv('/Users/angelicacrown/Desktop/WUSTL/Personomics/Harmony/Prolific/DATA/summary scores/prolific_scores_94_full_survey.csv')
meds <- questions %>% 
  select(prolific_id, meds01_1)
scores_meds <- merge(scores, meds, by="prolific_id")

ggplot(scores_meds) +
  geom_jitter(aes(phq10_sum, meds01_1, color=meds01_1))+
  labs(title = "Severity vs Medication Status",
       x="PHQ 9 Sum",
       y="Medication Status (Yes/No")
