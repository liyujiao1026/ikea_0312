source('https://raw.githubusercontent.com/liyujiao1026/ikea_0312/master/0_source.R')

savepath <- ''

invYear = 2007
treat_ID =  1780
clusterNumber <- 6
predictors = c( "Productivity", "Population",  "Percent_University","SalesIndex","EmployeeIndex",
                "Infrast","Border", "Patent")
treatCity <-  data_firm[,3][data_firm[,1] == treat_ID][1]  
inv_th <- which(2001:2012 == invYear)

#==========================================================================#
# Run all of analysis
result <- Func_paper(invYear = invYear ,treat_ID = treat_ID , clusterNumber = clusterNumber , predictors = predictors, data_firm = data_firm , savepath = savepath)

