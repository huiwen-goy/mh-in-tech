#### Report

[https://huiwen-goy.github.io/mh-in-tech/mental_health_genpop_tech.html](https://huiwen-goy.github.io/mh-in-tech/mental_health_genpop_tech.html)

[Team poster](https://huiwen-goy.github.io/mh-in-tech/Team_18_Datafolio-v2-ch.pdf)

#### About this project
 
The goals of this project were to:   
1. Understand which demographic factors are associated with higher risk of poor mental health, in the general population and in tech workers.    
2. Examine how much mental health support tech workers feel that they have in the workplace.  
 
#### About the data

1. BRFSS original data source and documentation: https://www.cdc.gov/brfss/data_documentation/index.htm  
2. OSMI original data source: https://osmihelp.org/research  

#### About the code

BRFSS_compile_5y_data.R:   
read BRFSS xport data file, calculate survey weight adjustments, merge 5 years of BRFSS data into one dataset  

BRFSS_LR_analysis.R:   
recode variables and perform logistic regression, incorporating survey design  

mental_health_tech.ipynb:  
code for same regressions done in R  
