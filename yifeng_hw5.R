library(UBCRM)

set.seed(2022)

s1 = c(0.1, 0.2, 0.3, 0.4, 0.5)
s2 = c(0.02, 0.06, 0.1, 0.2, 0.3)
s3 = c(0.3, 0.35, 0.4, 0.45, 0.5)

se1 = ssim3p3(s1, 5000)
se2 = ssim3p3(s2, 5000)
se3 = ssim3p3(s3, 5000)


help("Crm")
test = CreData(5)
test <- updata(test, 1, 3, 0)
test <- updata(test, 2, 3, 1)
test <- updata(test, 2, 3, 1)

p = c(0.1,0.15,0.25,0.35,0.45)
Crm(test, p, target = 0.3, nextlevel = "ntarget", nptmax = 24, nmaxmtd =6)


q2_se1 = ssimCrm(s1, n = 5000, firstdose = 1, cohortsize = 3, target = 1/3,
                         nptmax = 30, approach = "bayes", model = "logistic")
q2_se1


new = c(0.2, 0.3, 0.35, 0.4, 0.45)
q2_se1 = ssimCrm(new, n = 5000, firstdose = 1, cohortsize = 3, target = 1/3,
                 nptmax = 30, approach = "bayes", model = "logistic")

