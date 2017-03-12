# fuction for growu curve estimation of intervention

Func_parametric <- function(gap.obs){ 
            
            time <- 1:length(gap.obs)
            
            f.beta <- function(par, gap.obs, type){
                        a <- par[1]
                        r <- par[2]
                        B <- par[3]
                        
                        gap.pre_Monomolecular <- a * (1 - B * exp((-1) * r * time)) 
                        gap.pre_Logistic <- a / (1 + B * exp((-1) * r * time))
                        gap.pre_Gompertz <- a * exp((-1) * B * exp((-1) * r * time))
                        
                        
                        if (type == "M") { gap.pre <- gap.pre_Monomolecular
                        } else if (type == "L") {
                                    gap.pre <- gap.pre_Logistic
                        } else {
                                    gap.pre <- gap.pre_Gompertz        
                        }
                        
                        
                        gaps <-  gap.obs - gap.pre
                        
                        AIC.v <- length(gap.obs)*(log(2*pi) + 1 + log((sum(gaps^2)/length(gap.obs)))) + 8
                        
                        
                        return(AIC.v)
                        
            }
            
            init <- -90 #quantile(gap.obs, 0.05)
            
            M.model <- optim(par = c(init, 2, 10), f.beta , gap.obs = gap.obs , type = "M")
            L.model <- optim(par = c(init, 2, 10), f.beta , gap.obs = gap.obs , type = "L")
            G.model <- optim(par = c(init, 2, 10), f.beta , gap.obs = gap.obs , type = "G")
            
            optimal <- which.min(c(M.model$value, L.model$value, G.model$value))
            
            
            model.optimal <- c("M","L","G")[optimal]
            par.optimal <- list(M.model$par, L.model$par, G.model$par )[[optimal]]
            names(par.optimal) <- c("a","r","B")
            
            a <- par.optimal[1]
            r <- par.optimal[2]
            B <- par.optimal[3]
            
            
            time.plot <- seq(from = 1, to = length(gap.obs), by = 1)
            predict.Y <- if (optimal == 1){
                        a * (1 - B * exp((-1) * r * time.plot)) 
            }else if (optimal == 2){
                        a / (1 + B * exp((-1) * r * time.plot))
            }else{
                        a * exp((-1) * B * exp((-1) * r * time.plot))
            }
            predict.Y <- round(predict.Y, 2)
            
            return(list( "model.optimal" = model.optimal, 
                         "par.optimal" = par.optimal,  
                         "predict.Y" = data.frame(time.plot,predict.Y)))
}


# 
# gap <- -haparanda[[2]]
# gap.post <- gap[row.names(gap)%>%as.numeric >= invYear]
# gap.pre <- Func_parametric(gap.post)$predict.Y
# 
# y.pre <- haparanda[[5]][inv_th:12] + gap.pre[,2]
# plot( x = 2001:2012, haparanda[[5]], ylim = c(0, 250), type = "l")
# lines(x = 2006:2012 , y = c(haparanda[[5]][inv_th],y.pre[-1]), ylim = c(0, 250))


