# BIO-539-Final-Project
Investigating the effects of variables on the meat weight for Atlantic sea scallops through GLMMs. Atlantic sea scallops were collected at 50 stations throughout Georges Bank by a seasonal survey. GLMMs with a poisson distribution and log link and a variety of different variables to find the best performing model predicting meat weight. Knowing the dynamics of scallop meats weight will allow managers to be more informed and create better management for the whole fishery.

### Instructions
All analysis was done with ```R``` and the final paper was created through an Rmarkdown file. The Final_Project script goes through data exploration and creation of the models. The Final_Project_Writeup contains all the code needed to create the figures as well as the writeup of the results. Additonal packages were used in these scripts and will need to be installed if you do not have them. How to install them is coded below.
```
install.packages("tidyverse", "lme4", "AICcmodavg", "viridis", "kableExtra")
```
Once those packages are installed, you can knit the Final_Project_Writeup to create the final html paper.
