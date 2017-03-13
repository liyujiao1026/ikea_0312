rm(list = ls())
source('/Users/Yujiao/Desktop/SCM_nru/2017/R_project/IKEA_2017/0_source.R')
savepath <- '/Users/Yujiao/Desktop/SCM_nru/2017/R_project/IKEA_2017/Empirical_study_Employee_0312/'
setwd(savepath)
treat_ID =  1480
invYear = 2004
treatCity <-  data_firm[,3][data_firm[,1] == treat_ID][1]  
inv_th <- which(2001:2012 == invYear)

ylim.path = c(1800,3000); ylim.gap = c(-3000, 3000)
subset(data_firm, Kommun_code == treat_ID)
#ylim.path = c(60,160); ylim.gap= c(-60, 60)
#---------------#

clusterNumber <- 4
predictors = c( "Productivity", "Population",  "Percent_University","SalesIndex","EmployeeIndex", "Infrast","Border", "Patent")

predictors = c( "Productivity",  "Percent_University","EmployeeIndex")
#predictors_gothenburg = c( "Productivity", "Percent_University")



# output plot and table in paper

Donor_ID <- readRDS(paste0( "../Empirical_study_0312/Empirical_results/Donor_",treat_ID))

# 2. Running SCM  ----------------#
result_SCM <- SCM_estimate_cmp( invYear = invYear, treat_ID = treat_ID, ctrl_ID = Donor_ID ,
                                predictors =  predictors)

SCM_plot_Func(treat_ID = treat_ID ,invYear = invYear, 
              Y.obs = result_SCM$Y.obs, Y.sc = result_SCM$Y.sc, gaps = -result_SCM$gaps,
              ylim.path = ylim.path, ylim.gap = ylim.gap)


saveRDS(result_SCM, paste0(savepath,"SCM_",treat_ID))

weight_paper <- result_SCM$synth.tables$tab.w[order(result_SCM$synth.tables$tab.w[,1], 
                                                    decreasing = T),]

print("running scm is done")

# 3. Placebo  ----------------#
result_placebo <- sapply(Donor_ID, function(x){SCM_estimate_cmp( invYear = invYear, treat_ID = x, 
                                                                 ctrl_ID = setdiff(Donor_ID,x),
                                                                 predictors =  predictors)})

saveRDS(result_placebo, paste0(savepath, "Placebo_", treat_ID))

# 4. Bootstrap  ----------------#
# (4.1) non-para
result_bootstrap_NonPar <- Func_Bootstrap_cmp(treat_ID = treat_ID, invYear = invYear,
                                              repTimes = 500, Donor_ID = Donor_ID, predictors = predictors)

saveRDS(result_bootstrap_NonPar, paste0(savepath, "Bootstrap_NonPar_",treat_ID))

# (4.2) para

gap_bootstrap <- -do.call(cbind, result_bootstrap_NonPar[2,])[(inv_th):12,]
gap_bootstrap[1,] <- 0


tryFun <- function(x, initPar){
            result_try <- try(Func_parametric(x, initPar = initPar))
            if ("try-error" %in% class(result_try)){
                        result_try <- NA
            }
            return(result_try)
}

result_bootstrap_Par0 <- apply(gap_bootstrap, 2, tryFun, initPar = c(1,1,1))
result_bootstrap_Par <- result_bootstrap_Par0[!is.na(result_bootstrap_Par0)]



saveRDS(result_bootstrap_Par, paste0(savepath, "Bootstrap_Par_",treat_ID))












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
                                  inv_th = inv_th, invYear = invYear, treatCity = treatCity)

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
#saveRDS(alpha_inf, paste0("alpha_inf_",treat_ID))



# table 2
tableOutput_nonPar <- Table_bootstrap_Func(result_bootstrap_NonPar = result_bootstrap_NonPar,
                                           inv_th = inv_th)



# fill in paper

weightData
placebo_ratio0
placebo_ratio1
table0 <- do.call(rbind, lapply(tableOutput_nonPar, round, 2))
colnames(table0) <- c("mean","Q_2.5%","Q_97.5%")
tableResult <- round(rbind(rbind(table0, alpha_inf), post_par), 4)

tableResult

