# HARMONY: Prolific Depression Surveys

This repo contains R files pertaining to the coding of depression survey data collected via the online platform Prolfic. 

## Overview of files
> harmony_screening_scores.R

The PHQ10 survey was used as a screener in this study. This file contains code for thresholding participants based on PHQ10 scores. The cutoff used here is scores of 5 or higher.

> harmony_sumary_coding.R

This file contains code to calculate summary scores of the surveys used in this study. Each survey has different summary coding methods.

Functions are created to\
    1. Sum responses together\
    2. Subtract one from all responses to achieve 0 index on responses\
    3. Inverse code responses\
    4. Binarize responses based on response wanted\
    5. Get max response from subset of items

> [!NOTE]
> Not all functions (if any) will be used on any one survey

Surveys included in this study:
* apath: Apathy Evaluation Scale (18 questions)
* bisbas: Behavioral Inhibition/Activation Scale (24 questions)
* rcads: Revised Children's Anxiety and Depression Scale (47 questions)
* mfq: Mood and Feeling Questionaire: Long Version Self Report (34 questions)
* nffi: Five Factor Personality Inventory: Neuroticism Subscale (20 questions)
* shaps: Snaith-Hamilton Pleasure Scale (14 questions)
* rbqa: Risy Behavior Questionaire for Adolescents (20 questions)
* stai: State-Trait Anxiety Inventory (40 questions)
* masq: Mood and Anxiety Symptom Form Questionaire (90 questions)
* gad7: Generalized Anxiety Disorder (7 questions)
* bdi: Beck Depression Inventory (21 questions)
* rrs: Ruminative Responses Scale (22 questions)
* qids: Quick Inventory of Depressive Symptoms - Self Report (16 questions)
* who: WHO Disability Assessment Schedule (12 questions)
* dass: Depression Anxiety Stress Scales (42 questions)
* briefcope: Brief Coping Orientation to Problems Experienced Inventory (28 questions)
* bai: Beck Anxiety Inventory (21 questions)
* panas: Positive and Negative Affect Schedule (20 questions)
* rds: Recent Depressive Symptom (4 questions)
* scl: Symptom Checklist (20 questions)
* wsas: Work and Social Adjustement Scale (5 questions)
* whoqolbref: WHO Quality of Life BREF (26 questions)
* phq10: Patient Health Questionaire 9 +1 (10 questions)\
  
Additionally, three attention check questions were employed in addition to four questions asking about medications and diagnoses.

> harmony_exploration.R

This file contains code to create graphs for
* survey x survey heatmap
* question x question heatmap
* tSNE plot
* uMAP plot
* PCA on questions + scree plot
* PCA on scores + scree plot
* Sex distribution plot
* Age distribution plot
* Ethnicity distribution plot
* Age x Time_taken scatter plot
* Overall severity distribution plot
* Attention Check Questions
* Reliability of PHQ10 from screener -> question  by question
* Reliability of PHQ10 from screener -> scores

> test_retest.R

This file contains code to explore the reliability of questions across time. Participants were asked to retake the full survey three weeks after they initially completed it. 
* reliablity based on scores
* reliablity based on questions

Input files: Full data set from first full survey, full data set from second full survey, demographics file
* contains code to merge datasets from the two time points

  
> example_data/HARMONYProlificSequenceVersion_DataDictionary_2026-01-03-rev_for_seq3+.csv

This folder contains a data dictionary with all questions used in each survey. File formatted for RedCAP survey distribution.
