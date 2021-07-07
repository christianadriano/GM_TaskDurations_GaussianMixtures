"
Initialize Gaussian Mixture Model by running a k-Means

This code was inspired on:
https://tinyheero.github.io/2016/01/03/gmm-em.html


"

prior.df <- function(wait){
  
  wait.kmeans <- kmeans(wait, 2)
  wait.kmeans.cluster <- wait.kmeans$cluster
  
  wait.df <- data_frame(x = wait, cluster = wait.kmeans.cluster)
  
  wait.df %>%
    mutate(num = row_number()) %>%
    ggplot(aes(y = num, x = x, color = factor(cluster))) +
    geom_point() +
    ylab("Values") +
    ylab("Data Point Number") +
    scale_color_discrete(name = "Cluster") +
    ggtitle("K-means Clustering")
  
  wait.summary.df <- wait.df %>%
    group_by(cluster) %>%
    summarize(mu = mean(x), variance = var(x), std = sd(x), size = n())
  
  #Initialize alpha
  wait.summary.df <- wait.summary.df %>%
    mutate(alpha = size / sum(size))
  
  wait.summary.df %>%
    dplyr::select(cluster, mu, variance, std, alpha)
  
  return(wait.summary.df)
  
}
