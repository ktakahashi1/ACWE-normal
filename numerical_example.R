## This is an execution file for the numerical examples in the paper.
## Load "function" file first, and then execute following code to perform the analyses.

source(".../function.R")

## Common settings
n_star<-100
v_s<-rep(1,3)
sigma<-1
alpha<-0.025
B<-100000
seed<-5
size_axistitle<-18
size_axistext<-15
size_legend<-18

## Example 1
set.seed(seed)
n_s<-c(50,100,100)
mean_s<-cbind(0, seq(-2,2,0.01), 0)
weight<-matrix(NA, nrow=nrow(mean_s), ncol=ncol(mean_s))
phi<-rep(NA, nrow(mean_s))
for (i in 1:nrow(mean_s)) {
  result<-ACWE(n_s=n_s, mean_s=mean_s[i,], v_s=v_s,
               n_star=n_star, sigma=sigma, alpha=alpha, B=B)
  weight[i,]<-result$weight
  phi[i]<-result$correction_factor
}
for_plot_example_1<-tibble(mean_1=mean_s[,2], w1=weight[,2], w2=weight[,3], phi=phi)
for_plot_example_1<-for_plot_example_1 %>% pivot_longer(cols=c("w1", "w2", "phi"), names_to="legend", values_to="value")
for_plot_example_1$legend<-factor(for_plot_example_1$legend, levels=c("w1", "w2", "phi"))

plot_example_1<-for_plot_example_1 %>% ggplot(aes(x=mean_1, color=legend)) +
  geom_line(aes(y=value), linewidth=1) +
  scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38", "phi"="#619CFF"),
                     labels=c(w1=expression(hat(italic(w))^(1)),
                              w2=expression(hat(italic(w))^(2)),
                              phi=expression(italic(phi)))) +
  ggtitle("Example 1") + ylab("Estimates") + xlab(expression(bar(italic(x))^(1))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom") + scale_y_continuous(limits=c(0,2))

## Example 2
set.seed(seed)
mean_s<-rep(0,3)
n_s<-cbind(50, seq(1,200,1), 100)
weight<-matrix(NA, nrow=nrow(n_s), ncol=ncol(n_s))
phi<-rep(NA, nrow(n_s))
for (i in 1:nrow(n_s)) {
  result<-ACWE(n_s=n_s[i,], mean_s=mean_s, v_s=v_s,
               n_star=n_star, sigma=sigma, alpha=alpha, B=B)
  weight[i,]<-result$weight
  phi[i]<-result$correction_factor
}
for_plot_example_2<-tibble(n_1=n_s[,2], w1=weight[,2], w2=weight[,3], phi=phi)
for_plot_example_2<-for_plot_example_2 %>% pivot_longer(cols=c("w1", "w2", "phi"), names_to="legend", values_to="value")
for_plot_example_2$legend<-factor(for_plot_example_2$legend, levels=c("w1", "w2", "phi"))

plot_example_2<-for_plot_example_2 %>% ggplot(aes(x=n_1, color=legend)) +
  geom_line(aes(y=value), linewidth=1) +
  scale_color_manual(values=c("w1"="#F8766D", "w2"="#00BA38", "phi"="#619CFF"),
                     labels=c(w1=expression(hat(italic(w))^(1)),
                              w2=expression(hat(italic(w))^(2)),
                              phi=expression(italic(phi)))) +
  ggtitle("Example 2") + ylab("Estimates") +  xlab(expression(italic(n[1]))) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title.x=element_text(size=15), axis.title.y=element_text(size=size_axistitle),
        axis.text.x=element_text(size=size_axistext), axis.text.y=element_text(size=size_axistext),
        legend.title=element_blank(), legend.text=element_text(size=size_legend),
        legend.position="bottom") + scale_y_continuous(limits=c(0,2))

plot_example_all<-ggarrange(plot_example_1, plot_example_2,
                            common.legend=T, legend="bottom", ncol=2)

stopCluster(cl)
