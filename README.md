# my-r-project
machine learning projects (01.2025)
This project explores the factors that influence logical addition in the translation field."Logical addition" refers to the addition of words or phrases in the target text that are not explicitly present in the source text but are necessary to convey the meaning accurately.

Our study aims to answer the following questions:

1.Can "logical addition" (logic), with binary outcomes of 1 (addition present) or 0 (addition absent), be effectively modeled as a dependent variable?
We will investigate the relationship between "logical addition" and the following independent variables: the number of sentences in the original text (sent_ct), lexical difficulty of the original text (complexity), lexical diversity of the original text (ttr), average sentence length of the original text (sent_leng), average dependency depth of the original text (dept), original text output speed (SPM), [clear definition of orin_wd], and [clear definition of orin_sec].

2.Which of these factors are the most influential, and which have a less significant impact on logical addition?

3.Do original text output speed (SPM), original text length (in words), and original text length (in seconds) exert a phased influence on logical addition?

4.What motivated the selection of the chosen model, and what criteria were used to include or exclude specific variables from the analysis?

This document contains the conclusions derived from our modeling analysis (logistic regression, a basic tree model, random forest),
the original R code used in the study, and the client's responses to the research questions.
