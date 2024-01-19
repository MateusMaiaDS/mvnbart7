# Loading plotting libraries
library(ggplot2)
mvbart_mod <- readRDS("~/Documents/lab_spBART/mvnbart6/inst/case_study/rep_1_single_run1000_ntree_50_mvn_3_task_classification_sim_friedman1.Rds")
sigma1_post <- sqrt(mvbart_mod$Sigma_post[1,1,])# %>% plot(type = 'l', main = expression(sigma[1]))
sigma2_post <- sqrt(mvbart_mod$Sigma_post[2,2,])# %>% plot(type = 'l', main = expression(sigma[2]))
sigma3_post <- sqrt(mvbart_mod$Sigma_post[3,3,])# %>% plot(type = 'l', main = expression(sigma[2]))

rho12_post <- (mvbart_mod$Sigma_post[1,2,]/(sqrt(mvbart_mod$Sigma_post[2,2,])*sqrt(mvbart_mod$Sigma_post[1,1,])))# %>% plot(type = 'l', main = expression(rho[12]))
rho13_post <- (mvbart_mod$Sigma_post[1,3,]/(sqrt(mvbart_mod$Sigma_post[1,1,])*sqrt(mvbart_mod$Sigma_post[3,3,])))# %>% plot(type = 'l', main = expression(rho[12]))
rho23_post <- (mvbart_mod$Sigma_post[2,3,]/(sqrt(mvbart_mod$Sigma_post[2,2,])*sqrt(mvbart_mod$Sigma_post[3,3,])))# %>% plot(type = 'l', main = expression(rho[12]))

sigma1_post_plot <- ggplot()+
     geom_line(data = data.frame(n_post = 1:length(sigma1_post),
                                 sigma1 = sigma1_post),
               mapping = aes(x = n_post, y = sigma1), col = c("#B51700"), alpha = 0.5)+
     geom_hline(yintercept = 1.0, linetype = 'dashed', col = 'black')+
     xlab("Posterior iter")+
     ylab(expression(sigma[1]))+
     theme_bw()+
     theme(text = element_text(size = 14))  # Adjust the size as needed


sigma2_post_plot <- ggplot()+
     geom_line(data = data.frame(n_post = 1:length(sigma1_post),
                                 sigma1 = sigma2_post),
               mapping = aes(x = n_post, y = sigma1), col = c("#B51700"), alpha = 0.5)+
     geom_hline(yintercept = 125, linetype = 'dashed', col = 'black')+
     xlab("Posterior iter")+
     ylab(expression(sigma[2]))+
     theme_bw()+
     theme(text = element_text(size = 14))  # Adjust the size as needed

sigma3_post_plot <- ggplot()+
     geom_line(data = data.frame(n_post = 1:length(sigma1_post),
                                 sigma1 = sigma3_post),
               mapping = aes(x = n_post, y = sigma1), col = c("#B51700"), alpha = 0.5)+
     geom_hline(yintercept = 0.1, linetype = 'dashed', col = 'black')+
     xlab("Posterior iter")+
     ylab(expression(sigma[3]))+
     theme_bw()+
     theme(text = element_text(size = 14))  # Adjust the size as needed

rho12_post_plot <- ggplot()+
     geom_line(data = data.frame(n_post = 1:length(sigma1_post),
                                 sigma1 = rho12_post),
               mapping = aes(x = n_post, y = sigma1), col = c("#B51700"), alpha = 0.5)+
     geom_hline(yintercept = 0.8, linetype = 'dashed', col = 'black')+
     xlab("Posterior iter")+
     ylab(expression(rho[12]))+
     theme_bw()+
     theme(text = element_text(size = 14))  # Adjust the size as needed

rho13_post_plot <- ggplot()+
     geom_line(data = data.frame(n_post = 1:length(sigma1_post),
                                 sigma1 = rho13_post),
               mapping = aes(x = n_post, y = sigma1), col = c("#B51700"), alpha = 0.5)+
     geom_hline(yintercept = 0.5, linetype = 'dashed', col = 'black')+
     xlab("Posterior iter")+
     ylab(expression(rho[13]))+
     theme_bw()+
     theme(text = element_text(size = 14))  # Adjust the size as needed

rho23_post_plot <- ggplot()+
     geom_line(data = data.frame(n_post = 1:length(sigma1_post),
                                 sigma1 = rho23_post),
               mapping = aes(x = n_post, y = sigma1), col = c("#B51700"), alpha = 0.5)+
     geom_hline(yintercept = 0.25, linetype = 'dashed', col = 'black')+
     xlab("Posterior iter")+
     ylab(expression(rho[23]))+
     theme_bw()+
     theme(text = element_text(size = 14))  # Adjust the size as needed

cowplot::plot_grid(sigma1_post_plot,sigma2_post_plot,sigma3_post_plot,
                   rho12_post_plot,rho13_post_plot,rho23_post_plot,
                   ncol = 3, nrow = 2)

cowplot::plot_grid(rho12_post_plot,rho13_post_plot,rho23_post_plot,
                   ncol = 1, nrow = 3)
rho12_post_plot
# Visualzing the variable importance
mvn_dim <- 3
mod <- mvbart_mod
df_x <- mvbart_mod$data$x_train
par(mfrow=c(1,mvn_dim))
for( y_j_plot in 1:mvn_dim){
     total_count <- apply(mod$var_importance[,,y_j_plot],2,sum)
     norm_count <- total_count/sum(total_count)
     names(norm_count) <- paste0("x.",1:ncol(df_x))
     sort <- sort(norm_count,decreasing = TRUE)
     barplot(sort,main = paste0("Var importance for y.",y_j_plot),las = 2)
}

# Variable importance in ggplot
varimport_plots <- list()
for (y_j_plot in 1:mvn_dim) {
     total_count <- apply(mod$var_importance[(mod$mcmc$n_burn+1):(mod$mcmc$n_mcmc),,y_j_plot], 2, sum)
     norm_count <- total_count / sum(total_count)
     names(norm_count) <- paste0("x.", 1:ncol(df_x))

     # Create a data frame for ggplot
     plot_data <- data.frame(variable = names(norm_count), importance = norm_count)

     # Sort the data frame by importance
     plot_data <- plot_data[order(-plot_data$importance), ]

     # Create the ggplot bar plot
     varimport_plots[[y_j_plot]] <- ggplot(plot_data, aes(x = reorder(variable, -importance), y = importance)) +
          geom_bar(stat = "identity",fill = c("#B51700"), alpha = 0.75) +
          labs(title = paste0("Var importance for y.", y_j_plot),
               x = "Variable",
               y = "Var. Importance") +
          theme_bw() +
          theme(text = element_text(size  = 14),
                axis.text.x = element_text(angle = 90, hjust = 1))


}
cowplot::plot_grid(varimport_plots[[1]],varimport_plots[[2]],varimport_plots[[3]], ncol = mvn_dim)
