source('https://raw.githubusercontent.com/liyujiao1026/ikea_0312/master/0_source.R')

savepath <- './Empirical_study_0312/Empirical_results/'

invYear = 2006
treat_ID =  2583
clusterNumber <- 6
predictors = c( "Productivity", "Population",  "Percent_University","SalesIndex","EmployeeIndex",
                "Infrast","Border", "Patent")
treatCity <-  data_firm[,3][data_firm[,1] == treat_ID][1]  
inv_th <- which(2001:2012 == invYear)


ylim.path = c(60,160)
ylim.gap= c(-60, 60)

#==========================================================================#
# Run all of analysis
result <- Func_paper(invYear = invYear ,treat_ID = treat_ID , clusterNumber = clusterNumber , predictors = predictors, data_firm = data_firm , savepath = savepath)

setwd(savepath)
Donor_ID <- readRDS(paste0("./Donor_",treat_ID))
result_SCM <- readRDS(paste0("./SCM_",treat_ID))
result_DonorWeight <- result_SCM$synth.tables$tab.w[order(result_SCM$synth.tables$tab.w[,1], decreasing = T),]
result_placebo <- readRDS(paste0("./placebo_",treat_ID))
result_bootstrap_NonPar <- readRDS(paste0("./Bootstrap_NonPar_",treat_ID)) 
result_bootstrap_NonPar_SC <- readRDS(paste0("./Bootstrap_NonPar_SC_",treat_ID)) 
result_bootstrap_Par <- readRDS(paste0("./Bootstrap_Par_",treat_ID)) 

#==========================================================================#
# 1. SCM 

SCM_plot_Func(treat_ID = treat_ID ,invYear = invYear, 
              Y.obs = result_SCM$Y.obs, Y.sc = result_SCM$Y.sc, gaps = -result_SCM$gaps,
              ylim.path = c(60,160), ylim.gap = c(-60, 60 ))

# Output weighted units
result_DonorWeight

#=======================================================================#
# 2. Placebo 
MSE_pre <- unlist(result_placebo[1,])
MSE_post <- unlist(lapply(result_placebo[2,], function(x){ sum((x[(inv_th+1):12])^2) }))
ratio_ctrl <- sqrt(MSE_post/MSE_pre)
MSE_pre_treat <- unlist(result_SCM[[1]])
MSE_post_treat <- sum(result_SCM[[2]][(inv_th+1):12] ^2 )
ratio_treat <- sqrt(MSE_post_treat/MSE_pre_treat)
print(ratio_treat)

par(mfrow = c(1,2))
plot(x = 2001:2012, y = -result_placebo[[2]], main = "",
     ylim = c(-200,200), 
     type = "l", lwd = 2 , ylab = "gap", xlab = "Year")

for (i in 1:length(Donor_ID)){
            lines(x = 2001:2012, y = -result_placebo[2,][[i]], 
                  ylim = c(-60,100), type = "l", lwd = 1, col = "gray")
}

lines(x = 2001:2012, y = -result_placebo[[2]], ylim = c(-200,200), 
      type = "l", lwd = 2 , ylab = "", xlab = "")
abline(v = invYear, lty = 3)
abline(h = 0, lty = 2, lwd = 1.5)

hist(c(ratio_treat,ratio_ctrl), xlim = c(0, ratio_treat),
     breaks = 50, xlab = expression('Ratio'['post']), main = "")
text(ratio_treat , 1.9, treatCity, cex = 0.7)
arrows(ratio_treat , 1.2,ratio_treat - 1, 1.7, length = 0.08 )

sum(ratio_treat < ratio_ctrl, na.rm = T) / (length(ratio_ctrl)+1)
MSE_post
MSE_post_treat

#=======================================================================#
# 3. Bootstrap

# 3.1 non-para
SC_interval <- apply(result_bootstrap_NonPar_SC, 1, quantile, c(0.025,0.975), na.rm = T)
SC_mean <- apply(result_bootstrap_NonPar_SC, 1, mean, na.rm = T)
#plot

par(mfrow = c(1,2))
plot(x = 2001:2012,  result_SCM$Y.obs, type = "l", ylim = ylim.path, 
     xlab = "Year", ylab = "Productivity", lwd = 1.5)
abline(v = invYear, lty = 3)

lines(x = 2006:2012, y = c(  result_SCM$Y.obs[inv_th],SC_mean[(inv_th + 1):12]) , 
      ylim = ylim.path ,lty = 2, lwd = 1.5)
lines(x = 2006:2012, y = c(  result_SCM$Y.obs[inv_th], SC_interval[1,(inv_th + 1):12] ) , 
      ylim = ylim.path ,lty = 3, lwd = 1.5, col = "blue")
lines(x = 2006:2012, y = c(  result_SCM$Y.obs[inv_th], SC_interval[2,(inv_th + 1):12] ) , 
      ylim = ylim.path ,lty = 3, lwd = 1.5, col = "blue")



# 3.2 para
model_par <- lapply(result_bootstrap_Par, function(x){x[[1]]})
alpha_inf_par <- unlist(lapply(result_bootstrap_Par, function(x){x[[2]][1]}))
gap_est_par <- do.call(cbind, lapply(result_bootstrap_Par, function(x){x[[3]][,2]}))

Y_est_par <- result_SCM$Y.obs[inv_th:12] + gap_est_par
Y_interval_par <- apply(Y_est_par,1, quantile, c(0.025,0.975))[,-1]
Y_mean_par <- apply(Y_est_par,1, mean)[-1]

y.obs_plot <- result_SCM$Y.obs
plot( x = 2001:2012, y.obs_plot, ylim = c(0, 250), 
      lwd = 1.5, 
      type = "l", xlab = "Year", ylab = "Productivity")
abline(v = invYear, lty = 3)
lines(x = 2006:2012 , y = c(y.obs_plot[inv_th], Y_mean_par), 
      ylim = c(0, 250), lwd = 1.5, lty = 2)

lines(x = 2006:2012 , y = c(y.obs_plot[inv_th], Y_interval_par[1,]), 
      ylim = c(0, 250), lty = 3,lwd = 1.5,  col = "blue")

lines(x = 2006:2012 , y = c(y.obs_plot[inv_th], Y_interval_par[2,]), 
      ylim = c(0, 250), lty = 3, lwd = 1.5, col = "blue")


# histogram
hist(alpha_inf_par, xlab = expression(alpha[infinity]),  breaks = 15, main = "",freq = F, col = "lightgray")
lines(density(alpha_inf_par,adjust = 3))
abline(v = quantile(alpha_inf_par, c(0.025,0.975)), col = "blue", lty = 3)



# table 2
qFunc <- function(x){c(quantile(x, 0.025), mean(x), quantile(x, 0.975))}
post1 <- gap_bootstrap[inv_th + 1,]
pre1 <- gap_bootstrap[inv_th - 1,]
pre3 <- apply(gap_bootstrap[(inv_th - 3):(inv_th - 1),], 2, mean)
post3 <- apply(gap_bootstrap[(inv_th + 1):(inv_th + 3),], 2, mean)
preall <- apply(gap_bootstrap[1:(inv_th - 1),], 2, mean)
postall <- apply(gap_bootstrap[(inv_th + 1):12,], 2, mean)

MSPE_pre_bootstrap <- as.vector(unlist(do.call(cbind, result_bootstrap[1,])))
MSPE_post_bootstrap <- apply( do.call(cbind, result_bootstrap[2,])[(inv_th+1):12,], 2, function(x){sum(x^2)})
MSPE_ratio <- sqrt(MSPE_post_bootstrap /MSPE_pre_bootstrap)



fitting_Statistic <- list(pre3, pre1, post1, post3, preall, postall, sqrt(MSPE_pre_bootstrap),MSPE_ratio)
fitting_CI <- round(do.call(cbind, lapply(fitting_Statistic, qFunc)), 2)
colnames(fitting_CI) <- c("pre3", "pre1", "post1", "post3", "preall", "postall", "MSPE","MSPE_ratio")
row.names(fitting_CI) <- c("Q_2.5", "mean", "Q_97.5")
saveRDS(fitting_CI, paste0(savepath,"Bootstrap_NonPar_CI_", treat_ID))

fitting_CI_haparanda <- readRDS(savepath,"Bootstrap_NonPar_CI_2583")


postall_par <- qFunc( apply(gap_est_par, 2, mean) )
post3_par <- qFunc( apply(gap_est_par[1:3,], 2, mean) )
qFunc(gap_est_par[1,])
qFunc(alpha_inf_par)
