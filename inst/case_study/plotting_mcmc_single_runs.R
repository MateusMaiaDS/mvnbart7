# Reading the dim = 3, n = 1000
library(ggplot2)
mvbart_mod <- readRDS("~/Documents/lab_spBART/mvbart_runs/rep_1_single_run1000_ntree_50_mvn_3_task_regression_sim_friedman1.Rds")
sigma1_post <- sqrt(mvbart_mod$Sigma_post[1,1,])# %>% plot(type = 'l', main = expression(sigma[1]))
sigma2_post <- sqrt(mvbart_mod$Sigma_post[2,2,])# %>% plot(type = 'l', main = expression(sigma[2]))
sigma2_post <- sqrt(mvbart_mod$Sigma_post[3,3,])# %>% plot(type = 'l', main = expression(sigma[2]))

rho12_post <- (mvbart_mod$Sigma_post[1,2,]/(sqrt(mvbart_mod$Sigma_post[2,2,])*sqrt(mvbart_mod$Sigma_post[1,1,])))# %>% plot(type = 'l', main = expression(rho[12]))
rho13_post <- (mvbart_mod$Sigma_post[1,3,]/(sqrt(mvbart_mod$Sigma_post[3,3,])*sqrt(mvbart_mod$Sigma_post[1,1,])))# %>% plot(type = 'l', main = expression(rho[12]))
rho23_post <- (mvbart_mod$Sigma_post[2,3,]/(sqrt(mvbart_mod$Sigma_post[2,2,])*sqrt(mvbart_mod$Sigma_post[3,3,])))# %>% plot(type = 'l', main = expression(rho[12]))

# Creating the GGPLOT for each one of them
sigma1_post_plot <- ggplot()+
     geom_line(data = data.frame(n_post = 1:length(sigma1_post),
                                 sigma1 = sigma1_post),
               mapping = aes(x = n_post, y = sigma1), col = c("black"), alpha = 0.5)+
     geom_hline(yintercept = 1.0, linetype = 'dashed', col = 'blue')+
     xlab("Posterior iter")+
     ylab(expression(sigma[1]))+
     theme_bw()+
     theme(text = element_text(size = 14))  # Adjust the size as needed
sigma1_post_plot

sigma2_post_plot <- ggplot()+
     geom_line(data = data.frame(n_post = 1:length(sigma1_post),
                                 sigma1 = sigma2_post),
               mapping = aes(x = n_post, y = sigma1), col = c("#B51700"), alpha = 0.5)+
     geom_hline(yintercept = 125, linetype = 'dashed', col = 'black')+
     xlab("Posterior iter")+
     ylab(expression(sigma[2]))+
     theme_bw()+
     theme(text = element_text(size = 14))  # Adjust the size as needed
