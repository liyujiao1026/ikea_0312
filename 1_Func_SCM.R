# function of running SCM




SCM_estimate <- function(treat_ID, ctrl_ID, invYear, predictors){
            
            opt_from <- 2001
            opt_to <- invYear - 1
            
            validCheck <- data_firm[data_firm[,1] %in% ctrl_ID,][, c("Infrast", "Border","Patent")]
            tb <- apply(validCheck, 2, table)
            removePre <- which(sapply(tb, length) == 1) %>% names
            predictors_in <- setdiff(predictors, removePre)
            
            dataprep.out <-
                        dataprep(
                                    foo = data_firm,
                                    predictors = predictors_in,
                                    predictors.op = "mean",
                                    
                                    # 
                                    # special.predictors = list(
                                    #             list("Productivity", (invYear - 3):(invYear - 1), "mean")
                                    #             
                                    # ),
                                    
                                    #dependent = "Productivity",
                                    dependent = "SalesIndex",
                                    unit.variable = "Kommun_code",
                                    time.variable = "Year",
                                    treatment.identifier = treat_ID,
                                    controls.identifier = ctrl_ID,
                                    time.predictors.prior = c(opt_from:opt_to),
                                    time.optimize.ssr = c(opt_from:opt_to),
                                    unit.names.variable = "Kommun_name",
                                    time.plot = 2001:2012
                        )
            
            
            
            synth.out <- try(
                        synth(dataprep.out) ,
                        silent = T)
            
            if ("try-error" %in% class(synth.out)) {
                        MSPE <-  gaps <-  synth.tables <- NA
                        Y.obs <-  Y.sc <- NA
            } else { 
                        synth.tables <- synth.tab(
                                    dataprep.res = dataprep.out,
                                    synth.res = synth.out)
                        
                        
                        
                        
                        Y.sc <- dataprep.out$Y0plot %*% synth.out$solution.w
                        Y.obs <- dataprep.out$Y1plot
                        gaps <- Y.obs - Y.sc
                        
                        preYear <- invYear > row.names(gaps)%>%as.numeric
                        
                        MSPE <- sum(gaps[preYear]^2)
            }
            
            
            return(list("MSPE" = MSPE, "gaps" = gaps, "synth.tables" = synth.tables,
                        "remove_predictor" = removePre, "Y.obs" = Y.obs, "Y.sc" = Y.sc))
}


SCM_estimate_cmp <- cmpfun(SCM_estimate)



#test
# invYear = 2006
# treat_ID = 2583
# clusterNumber <- 7
# inv_th <- which(2001:2012 == invYear)
# predictors = c( "Productivity","Population",  "Percent_University","SalesIndex","EmployeeIndex","Infrast","Border", "Patent")
# 
# cluster_result <- Func_cluster(treat_ID, invYear, clusterNumber = 7,  var.cluster = predictors)
# 
# Donor_ID <- setdiff( cluster_result$clustered_UnitId, c(880, 1480, 2583, 1780))
# length(Donor_ID)
# res1 <- SCM_estimate_cmp( invYear = invYear, treat_ID = treat_ID, ctrl_ID = Donor_ID, predictors = predictors)
# res1$MSPE
# 
# plot(x = 2001:2012, res1$Y.obs, type = "l", ylim = c(0, 280), xlab = "Year", ylab="Productivity", lwd = 1)
# lines(x = 2001:2012,res1$Y.sc, ylim = c(0, 280) ,lty = 2)


