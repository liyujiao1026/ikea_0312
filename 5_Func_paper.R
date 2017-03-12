# Integrate all functions into this funciton

Func_paper <- function(invYear,treat_ID , clusterNumber, predictors, data_firm, savepath ){
            
            if (missing(savepath)) {
                        savepath <-  './Empirical_study_0312/Empirical_results/'
            }
            
            inv_th <- which(2001:2012 == invYear)
            treatCity <-  data_firm[,3][data_firm[,1] == treat_ID][1]  
            
            print(treatCity)
            # 1. Clustering ----------------#
            cluster_result <- Func_cluster(treat_ID, invYear, clusterNumber =  clusterNumber, 
                                           var.cluster = predictors)
            
            Donor_ID <- setdiff( cluster_result$clustered_UnitId, c(880, 1480, 2583, 1780))
            print(length(Donor_ID))
            saveRDS(Donor_ID, paste0(savepath,"Donor_",treat_ID))
            
            # 2. Running SCM  ----------------#
            result_SCM <- SCM_estimate_cmp( invYear = invYear, treat_ID = treat_ID, ctrl_ID = Donor_ID ,
                                            predictors =  predictors)
            
            
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
            gap_bootstrap <- -do.call(cbind, result_bootstrap_NonPar[2,])
            result_bootstrap_par <- apply(gap_bootstrap[inv_th:12,], 2, Func_parametric)
            saveRDS(result_bootstrap_par, paste0(savepath, "Bootstrap_Par_",treat_ID))
            
            #===========================================================#
            return(list("Donor_ID" = Donor_ID,
                        "result_SCM" = result_SCM, "weight_paper" = weight_paper, 
                        "result_placebo" = result_placebo,
                        "result_bootstrap_NonPar" = result_bootstrap_NonPar, 
                        "result_bootstrap_par" = result_bootstrap_par))
}

      

