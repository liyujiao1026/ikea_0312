# fuction for growu curve estimation of intervention

Func_parametric <- function(gap.obs, initPar){ 
            
            time <- 1:length(gap.obs)
            
            f.beta <- function(par, gap.obs, type){
                        a <- par[1]
                        r <- par[2]
                        B <- par[3]
                        
                        gap.pre_Monomolecular <- a * (1 - B * exp((-1) * r * time)) 
                        gap.pre_Logistic <- a / (1 + B * exp((-1) * r * time))
                        gap.pre_Gompertz <- a * exp((-1) * B * exp((-1) * r * time))
                        gap.pre_constant <- mean(gap.obs)
                        
                        
                        if (type == "M") { gap.pre <- gap.pre_Monomolecular
                        } else if (type == "L") {
                                    gap.pre <- gap.pre_Logistic
                        } else if (type  == "G"){
                                    gap.pre <- gap.pre_Gompertz        
                        } else{
                                    gap.pre <- gap.pre_constant
                        }
                        
                        
                        gaps <-  gap.pre - gap.obs
                        
                        AIC.v <- length(gap.obs)*(log(2*pi) + 1 + log((sum(gaps^2)/length(gap.obs)))) + 8
                        
                        
                        return(c(AIC.v))
                        
            }
            
            
            #init <- 0#quantile(gap.obs, 0.5, na.rm = T) #quantile(gap.obs, 0.05)
            # initPar_gothenburg: c(0,0,0)
            # initPar_Karlstad <- c(-1,-1,0)
            
            M.model <- optim(par = initPar, f.beta , gap.obs = gap.obs , type = "M")
            L.model <- optim(par = initPar, f.beta , gap.obs = gap.obs , type = "L")
            G.model <- optim(par = initPar, f.beta , gap.obs = gap.obs , type = "G")
            C.model <- f.beta(par= initPar, gap.obs, type = "C")
            
            optimal <- which.min(c(M.model$value, L.model$value, G.model$value,  C.model))
            
            
            model.optimal <- c("M","L","G","C")[optimal]
            par.optimal <- list(M.model$par, L.model$par, G.model$par, mean(gap.obs) )[[optimal]]
            
            if(length(par.optimal) == 1){
                        r <- B <- NA
                        a <- mean(gap.obs)
                        par.optimal <- c(a,r,B)
            }
            
            
            names(par.optimal) <- c("a","r","B")
            a <- par.optimal[1]
            r <- par.optimal[2]
            B <- par.optimal[3]
            
            
            time.plot <- seq(from = 1, to = length(gap.obs), by = 1)
            predict.Y <- if (optimal == 1){
                        a * (1 - B * exp((-1) * r * time.plot)) 
            }else if (optimal == 2){
                        a / (1 + B * exp((-1) * r * time.plot))
            }else if(optimal == 3){
                        a * exp((-1) * B * exp((-1) * r * time.plot))
            }else{
                        rep(a, length(time.plot))
                        
            }
            predict.Y <- round(predict.Y, 2)
            
            return(list( "model.optimal" = model.optimal, 
                         "par.optimal" = par.optimal,  
                         "predict.Y" = data.frame(time.plot,predict.Y)) )
}


# 
# gap <- -haparanda[[2]]
# gap.post <- gap[row.names(gap)%>%as.numeric >= invYear]
# gap.pre <- Func_parametric(gap.post)$predict.Y
# 
# y.pre <- haparanda[[5]][inv_th:12] + gap.pre[,2]
# plot( x = 2001:2012, haparanda[[5]], ylim = c(0, 250), type = "l")
# lines(x = 2006:2012 , y = c(haparanda[[5]][inv_th],y.pre[-1]), ylim = c(0, 250))


