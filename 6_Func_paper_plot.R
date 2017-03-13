

# scm plot ===========================================================#
SCM_plot_Func <- function(treat_ID, invYear,
                          Y.obs, Y.sc, gaps,
                          ylim.path, ylim.gap,
                          treatCity = NULL){
            
            if (missing(treatCity)){
                        treatCity = data_firm[,3][data_firm[,1] == treat_ID][1]
            }
            
            
            par(mfrow=c(1,2),oma = c(0, 0, 2, 0) )
            # path
            plot(x = 2001:2012, Y.obs, type = "l", ylim = ylim.path,
                 xlab = "Year", ylab = "Productivity", lwd = 1.5)
            lines(x = 2001:2012,Y.sc, ylim = ylim.path ,lty = 2, lwd = 1.5)
            abline(v = invYear, lty = 3)
            # gap
            plot(x = 2001:2012, gaps, type = "l", ylim = ylim.gap,
                 xlab = "Year", ylab = "Gap", lwd = 1.5)
            abline(h = 0 ,lty =2, lwd = 1.5)
            abline(v = invYear, lty = 3)
            
            mtext(treatCity, outer = T, cex = 1.5)
}



# SCM_plot_Func(result1$Y.obs,result1$Y.sc,result1$gaps,
#               invYear, treatCity = "Haparanda", ylim.path = c(0,280), ylim.gap= c(-200, 200))



# placebo plot ===========================================================#
plot_placebo_Func <- function(result_placebo, inv_th,result_SCM, ylim.gap, Donor_ID ){ 
            
            MSE_pre <- unlist(result_placebo[1,])
            MSE_post <- unlist(lapply(result_placebo[2,], function(x){ sum((x[(inv_th+1):12])^2) }))
            ratio_ctrl <- sqrt(MSE_post/MSE_pre)
            MSE_pre_treat <- unlist(result_SCM[[1]])
            MSE_post_treat <- sum(result_SCM[[2]][(inv_th+1):12] ^2 )
            ratio_treat <- sqrt(MSE_post_treat/MSE_pre_treat)
            print(ratio_treat)
            
            par(mfrow = c(1,2))
            plot(x = 2001:2012, y = -result_SCM[[2]], main = "",
                 ylim = ylim.gap, 
                 type = "l", lwd = 2 , ylab = "gap", xlab = "Year")
            
            y.placebo <- result_placebo[2,]
            y.placebo1 <- y.placebo[!is.na(y.placebo)]
            
            
            for (i in 1:length(y.placebo1)){
                        lines(x = 2001:2012, y = y.placebo1[[i]]*(-1), 
                              ylim = ylim.gap, type = "l", lwd = 1, col = "gray")
            }
            
            
            lines(x = 2001:2012, y = -result_SCM[[2]], ylim = ylim.gap, 
                  type = "l", lwd = 2 , ylab = "", xlab = "")
            abline(v = invYear, lty = 3)
            abline(h = 0, lty = 2, lwd = 1.5)
            
            hist(c(ratio_treat,ratio_ctrl), xlim = c(0, ratio_treat),
                 breaks = 50, xlab = expression('Ratio'['post']), main = "")
            text(ratio_treat - 1, 1.9, treatCity, cex = 0.7)
            arrows(ratio_treat , 1.2,ratio_treat - 1, 1.7, length = 0.08 )
            
            sum(ratio_treat < ratio_ctrl, na.rm = T) / (length(ratio_ctrl) + 1)
            
            return(list("ratio_treat" = ratio_treat,"ratio_ctrl" = ratio_ctrl))
            
}





# bootstrap plot ===========================================================#

plot_bootstrap_Func <- function(result_bootstrap_NonPar, result_bootstrap_Par,
                                result_SCM, ylim.path,ylim.gap,
                                inv_th, invYear, treatCity = NULL
){ 
            qFunc <- function(x){c(mean(x, na.rm = T), quantile(x, c(0.025,0.975), na.rm = T))}
            
            result_bootstrap_NonPar_SC <- do.call(cbind, result_bootstrap_NonPar[6,])
            SC_interval <- apply(result_bootstrap_NonPar_SC, 1, qFunc)
            
            #plot
            
            par(mfrow = c(1,2),oma = c(0, 0, 2, 0) )
            plot(x = 2001:2012,  result_SCM$Y.obs, type = "l", ylim = ylim.path,
                 main = "non-parametric",
                 xlab = "Year", ylab = "Productivity", lwd = 1.5)
            
            lines(x = invYear:2012, y = c(  result_SCM$Y.obs[inv_th],SC_interval[1,(inv_th + 1):12]) , 
                  ylim = ylim.path ,lty = 2, lwd = 1.5)
            lines(x = invYear:2012, y = c(  result_SCM$Y.obs[inv_th], SC_interval[2,(inv_th + 1):12] ) , 
                  ylim = ylim.path ,lty = 3, lwd = 1.5, col = "blue")
            lines(x = invYear:2012, y = c(  result_SCM$Y.obs[inv_th], SC_interval[3,(inv_th + 1):12] ) , 
                  ylim = ylim.path ,lty = 3, lwd = 1.5, col = "blue")
            abline(v = invYear, lty = 3)
            
            
            # 3.2 para
            model_par <- lapply(result_bootstrap_Par, function(x){x[[1]]})
            alpha_inf_par <- unlist(lapply(result_bootstrap_Par, function(x){x[[2]][1]}))
            post_gap_par <- do.call(cbind, lapply(result_bootstrap_Par, function(x){x[[3]][,2]})) # predicted gap of post-intervention
            
            Y_est_par <- result_SCM$Y.obs[inv_th:12] + post_gap_par
            Y_interval_par <- apply(Y_est_par,1, quantile, c(0.025,0.975))[,-1]
            Y_mean_par <- apply(Y_est_par,1, mean)[-1]
            
            y.obs_plot <- result_SCM$Y.obs
            plot( x = 2001:2012, y.obs_plot, ylim = ylim.path,  main = "parametric",
                  lwd = 1.5, 
                  type = "l", xlab = "Year", ylab = "Productivity")
            abline(v = invYear, lty = 3)
            lines(x = invYear:2012 , y = c(y.obs_plot[inv_th], Y_mean_par), 
                  ylim = ylim.gap, lwd = 1.5, lty = 2)
            
            lines(x = invYear:2012 , y = c(y.obs_plot[inv_th], Y_interval_par[1,]), 
                  ylim = ylim.gap, lty = 3,lwd = 1.5,  col = "blue")
            
            lines(x = invYear:2012 , y = c(y.obs_plot[inv_th], Y_interval_par[2,]), 
                  ylim = ylim.gap, lty = 3, lwd = 1.5, col = "blue")
            
            mtext(treatCity, outer = T, cex = 1.5)
            
            # histogram
            par(mfrow = c(1,1))
            hist(alpha_inf_par, xlab = expression(alpha[infinity]),  breaks = 15, main = "",freq = F, col = "lightgray")
            lines(density(alpha_inf_par,adjust = 3))
            abline(v = quantile(alpha_inf_par, c(0.025,0.975)), col = "blue", lty = 3)
            
            r_inf_par <- unlist(lapply(result_bootstrap_Par, function(x){x[[2]][2]}))
            b_inf_par <- unlist(lapply(result_bootstrap_Par, function(x){x[[2]][3]}))
            
            return(list("model_par"= model_par, "alpha_inf_par" = alpha_inf_par,
                        "r_inf_par"= r_inf_par,"b_inf_par"= b_inf_par,
                        "post_gap_par" = post_gap_par))
            
            
            
}


# table output ---------------------------------------#

Table_bootstrap_Func <- function(result_bootstrap_NonPar, inv_th){
            qFunc <- function(x){c(mean(x, na.rm = T), quantile(x, c(0.025,0.975), na.rm = T))}
            
            
            gap_bootstrap <- -1 * do.call(cbind, result_bootstrap_NonPar[2,])
            post1 <- gap_bootstrap[inv_th + 1,]
            pre1 <- gap_bootstrap[inv_th - 1,]
            pre3 <- apply(gap_bootstrap[(inv_th - 3):(inv_th - 1),], 2, mean)
            post3 <- apply(gap_bootstrap[(inv_th + 1):(inv_th + 3),], 2, mean)
            preall <- apply(gap_bootstrap[1:(inv_th - 1),], 2, mean)
            postall <- apply(gap_bootstrap[(inv_th + 1):12,], 2, mean)
            
            MSPE_pre_bootstrap <- sqrt(unlist(result_bootstrap_NonPar[1,]))
            post_data <- do.call(cbind, result_bootstrap_NonPar[2,])[(inv_th+1):12,]   
            MSPE_post_bootstrap <- as.vector(apply(post_data, 2, function(x){sum(x^2)}))
            MSPE_ratio <- sqrt(MSPE_post_bootstrap) / MSPE_pre_bootstrap
            
            fitting_Statistics <- list("pre3" = pre3, "pre1" = pre1, "post1" = post1,
                                       "post3" = post3, "preall" =  preall, "postall" = postall,
                                       "MSPE_pre_bootstrap" = MSPE_pre_bootstrap, "MSPE_ratio" = MSPE_ratio)
            
            quantile_Statistics <- lapply(fitting_Statistics, qFunc)
            
            saveRDS(quantile_Statistics, paste0("Bootstrap_NonPar_CI_", treat_ID))
            
            return(quantile_Statistics )
            
}



