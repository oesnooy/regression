lifestyle <- read.csv(file.choose())
str(lifestyle)

lifestyle$Gender_Male <- as.factor(lifestyle$Gender_Male) # Male(1), Female(0)
lifestyle$Stress_Level_Low <- as.factor(lifestyle$Stress_Level_Low) # Stress_Level_Low(1), others(0)
lifestyle$Stress_Level_Moderate <- as.factor(lifestyle$Stress_Level_Moderate)  # Stress_Level_Moderate(1), others(0)
str(lifestyle)

n=nrow(lifestyle) # 데이터 개수
pairs(lifestyle)  # plot


lm_result=lm(Grades~Study_Hours_Per_Day+Extracurricular_Hours_Per_Day+Sleep_Hours_Per_Day+Social_Hours_Per_Day+Physical_Activity_Hours_Per_Day+Gender_Male+Stress_Level_Low+Stress_Level_Moderate,data=lifestyle) # Grades만 종속변수, 나머지는 독립변수
summary(lm_result)


# Backward AIC : y=5+0.380*Study_Hours_Per_Day-0.02*Extracurricular_Hours_Per_Day-0.054*Stress_Level_Moderate1
lm_result=lm(Grades~Study_Hours_Per_Day+Extracurricular_Hours_Per_Day+Sleep_Hours_Per_Day+Social_Hours_Per_Day+Physical_Activity_Hours_Per_Day+Gender_Male+Stress_Level_Low+Stress_Level_Moderate,data=lifestyle) 
step1<-step(lm_result,direction='backward')
summary(step1)


# Backward BIC : y=4.913+0.385*Study_Hours_Per_Day 
lm_result=lm(Grades~Study_Hours_Per_Day+Extracurricular_Hours_Per_Day+Sleep_Hours_Per_Day+Social_Hours_Per_Day+Physical_Activity_Hours_Per_Day+Gender_Male+Stress_Level_Low+Stress_Level_Moderate,data=lifestyle) 
step(lm_result,direction='backward',k=log(n))


# Forward AIC : y=5.006+0.380*Study_Hours_Per_Day-0.054*Stress_Level_Moderate1-0.02*Extracurricular_Hours_Per_Day  
lm_result=lm(Grades~1,data=lifestyle)
step(lm_result,direction='forward',scope=(~Study_Hours_Per_Day+Extracurricular_Hours_Per_Day+Sleep_Hours_Per_Day+Social_Hours_Per_Day+Physical_Activity_Hours_Per_Day+Gender_Male+Stress_Level_Low+Stress_Level_Moderate))


# Forward BIC : y=4.913+0.385*Study_Hours_Per_Day 
lm_result=lm(Grades~1,data=lifestyle)
step(lm_result,direction='forward',k=log(n),scope=(~Study_Hours_Per_Day+Extracurricular_Hours_Per_Day+Sleep_Hours_Per_Day+Social_Hours_Per_Day+Physical_Activity_Hours_Per_Day+Gender_Male+Stress_Level_Low+Stress_Level_Moderate))