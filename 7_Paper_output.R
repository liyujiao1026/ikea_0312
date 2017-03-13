rm(list = ls())
source('/Users/Yujiao/Desktop/SCM_nru/2017/R_project/IKEA_2017/0_source.R')

savepath <- '/Users/Yujiao/Desktop/SCM_nru/2017/R_project/IKEA_2017/Empirical_study_0312/Empirical_results/'
setwd(savepath)
treat_ID =  1480
invYear = 2004
treatCity <-  data_firm[,3][data_firm[,1] == treat_ID][1]  
inv_th <- which(2001:2012 == invYear)

ylim.path = c(0,280); ylim.gap = c(-200, 200)
ylim.path = c(60,160); ylim.gap= c(-60, 60)
#---------------#

clusterNumber <- 4
# predictors = c( "Productivity", "Population",  "Percent_University","SalesIndex","EmployeeIndex", "Infrast","Border", "Patent")

predictors_gothenburg = c( "Productivity", "Percent_University")

#==========================================================================#
# Run all of analysis

# result <- Func_paper(invYear = invYear ,treat_ID = treat_ID , clusterNumber = clusterNumber , predictors = predictors_gothenburg, data_firm = data_firm , savepath = savepath)
#==========================================================================#



# output plot and table in paper
Donor_ID <- readRDS(paste0( "Donor_",treat_ID))
result_SCM <- readRDS(paste0("SCM_",treat_ID))
result_DonorWeight <- result_SCM$synth.tables$tab.w[order(result_SCM$synth.tables$tab.w[,1], decreasing = T),]
result_placebo <- readRDS(paste0( "placebo_",treat_ID))

result_bootstrap_NonPar <- readRDS( paste0("Bootstrap_NonPar_",treat_ID)) 

result_bootstrap_Par <- readRDS(paste0("Bootstrap_Par_",treat_ID)) 


# 1. SCM plot

SCM_plot_Func(treat_ID = treat_ID ,invYear = invYear, 
              Y.obs = result_SCM$Y.obs, Y.sc = result_SCM$Y.sc, gaps = -result_SCM$gaps,
              ylim.path = ylim.path, ylim.gap = ylim.gap)

# Output weighted units
weightData <- result_DonorWeight[1:7,]
saveRDS(weightData, paste0("Weight_", treat_ID))

#=======================================================================#
# 2. Placebo 

ratioResult <- plot_placebo_Func(result_placebo = result_placebo, inv_th = inv_th,
                  result_SCM = result_SCM, ylim.gap = ylim.gap, Donor_ID = Donor_ID )

placebo_ratio0 <- sum(ratioResult$ratio_treat < ratioResult$ratio_ctrl,na.rm = T) / (length(ratioResult$ratio_ctrl) + 1)

mean_placebo <- replicate(500, mean(sample(ratioResult$ratio_ctrl, replace = T) ) )
placebo_ratio1 <- sum(ratioResult$ratio_treat < mean_placebo,na.rm = T) / (length(mean_placebo)+1)


#=======================================================================#
# 3. Bootstrap
qFunc <- function(x){c( mean(x), quantile(x, c(0.025,0.975), na.rm = T))}
result_Par <- plot_bootstrap_Func(result_bootstrap_NonPar = result_bootstrap_NonPar, 
                    result_bootstrap_Par = result_bootstrap_Par,
                                result_SCM = result_SCM, ylim.path = ylim.path, ylim.gap = ylim.gap,
                                inv_th = inv_th, invYear = invYear, treatCity = "Gothenburg")

table(unlist(result_Par$model_par))
# most_model <- unique(c(which(result_Par$model_par != "M"), 
#                        which(result_Par$alpha_inf_par > 5),
#                        which(result_Par$b_inf_par > 5),
#                      which(result_Par$r_inf_par > 0)
#                      ))
# 
# ava <- setdiff(1:length(result_Par$model_par), most_model)
# result_Par$alpha_inf_par

post_par <- qFunc(apply(result_Par$post_gap_par, 2, mean))
alpha_inf <- qFunc(result_Par$alpha_inf_par)
saveRDS(alpha_inf, paste0("alpha_inf_",treat_ID))



# table 2
tableOutput_nonPar <- Table_bootstrap_Func(result_bootstrap_NonPar = result_bootstrap_NonPar,
                                           inv_th = inv_th)



# fill in paper

weightData
placebo_ratio0
placebo_ratio1
lapply(tableOutput_nonPar, round, 2)

round(alpha_inf, 2)
round(post_par, 2)

