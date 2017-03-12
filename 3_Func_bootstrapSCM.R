# function of bootstrap



Func_Bootstrap <- function(treat_ID, invYear, repTimes, Donor_ID, predictors){
            
            f_Bootstrap <- function(treat_ID, invYear, Donor_ID){
                        
                        donor_id <- setdiff(Donor_ID , treat_ID)
                        ctrl_ID = unique(sample(donor_id, size = length(donor_id), replace = T))
                        tes <- try(SCM_estimate_cmp( invYear = invYear,
                                                     treat_ID = treat_ID, ctrl_ID = ctrl_ID, predictors = predictors))
                        return(tes)
            }
            
            result_bootstrap <- replicate(n = repTimes, 
                                          expr = f_Bootstrap(treat_ID = treat_ID , invYear = invYear, Donor_ID))
            return(result_bootstrap)
}

Func_Bootstrap_cmp <- cmpfun(Func_Bootstrap)

# repTimes = 3
# invYear = 2006
# treat_ID <- 2583
# predictors
# tes <- Func_Bootstrap_cmp(treat_ID = treat_ID, invYear = invYear, 
#                           repTimes = repTimes, Donor_ID = Donor_ID, predictors = predictors)

