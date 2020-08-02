d = read.csv("C:/Users/0295s/Desktop/NRS/Dr Hasim Reza PGT/mastersheetcsv.csv")
summary(d)

d$Sex = as.factor(d$Sex)
d$CS_Headache = as.numeric(d$CS_Headache)
d$CS_Vomitting = as.numeric(d$CS_Vomitting)
d$Meningeal.sign = as.factor(d$Meningeal.sign)
d$Neurologic.deficit = as.factor(d$Neurologic.deficit)
d$CSF_Gstrain = as.factor(d$CSF_Gstrain)
d$CSF_AFB = as.factor(d$CSF_AFB)
d$CSF_Culture = as.factor(d$CSF_Culture)
d$CSF_HSV.PCR = as.factor(d$CSF_HSV.PCR)
d$CSF_ADA = as.factor(d$CSF_ADA)
d$Outcome = as.factor(d$Outcome)
d$Diagnosis = as.factor(d$Diagnosis)

summary(d)

attach(d)

#under the assumption of normality
# Suppose if we want to check if there is a difference in Diagnosis (i.e. Improved or Expired) for some variable.
# In simple words we want to see if the CSF levels is different for people who have Improved and those who have expired.

###########################
#cytological parameters
############################
ar1= aov(CSF_TC~Diagnosis)
summary(ar1)

ar2 = aov(CSF_PMN~Diagnosis)
summary(ar2)

ar3 = aov(CSF_Lymphocyte~Diagnosis)
summary(ar3)

###########################
#biochemical parameters
############################
ar4 = aov(CSF_Sugar~Diagnosis)
summary(ar4)

ar5 = aov(CSF_Protein~Diagnosis)
summary(ar5)

ar6 = aov(CSF_Lac_d1~Diagnosis)
summary(ar6)

ar7 = aov(CSF_Lac_d5~Diagnosis)
summary(ar7)

###########################
#blood parameter parameters
############################
ar8 = aov(BP_TLC~Diagnosis)
summary(ar8)

ar9 = aov(BP_Neutrophil~Diagnosis)
summary(ar9)

ar10 = aov(BP_Lymphocyte~Diagnosis)
summary(ar10)

ar11 = aov(BP_RBS~Diagnosis)
summary(ar11)

ar12 = aov(BP_Lac_d1~Diagnosis)
summary(ar12)

#there are several ways to check for normality in the data
#1. You can check for density plot for bell shaped curve
#2. If the scattered plot is densely accumulated at some small region etc.

#If the data is not normally distributed then we would opt for Mann Whitney U test which is done when our data is non-parametric.
wilcox.test(CSF_Lac_D1_exp,CSF_Lac_D1_imp)

#check for p-value to accept or reject
