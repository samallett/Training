### Show 95% CIs for two studies (manually defined) one above the other

library(tidyverse)

df1 <-  tibble(
  diff = 1.2,
  ci_lower = 1.09,
  ci_upper = 1.33
)

df2 <-  tibble(
    diff = 1.2,
    ci_lower = 0.97,
    ci_upper = 1.48
  )


p <- ggplot() +
  # 95% CI with whiskers
  geom_segment(data=df1, 
    aes(x = ci_lower, xend = ci_upper, y = 1.5, yend = 1.5),
    linewidth = 1.0, color = "blue"
  ) +
  geom_segment(data=df1, 
    aes(x = ci_lower, xend = ci_lower, y = 1.4, yend = 1.6),
    linewidth = 1.0, color = "blue"
  ) +
  geom_segment(data=df1, 
    aes(x = ci_upper, xend = ci_upper, y = 1.4, yend = 1.6),
    linewidth = 1.0, color = "blue"
  ) +

  geom_point(data=df1, aes(x = diff, y = 1.5), color = "blue", size = 8) +
  
  geom_segment(data=df2, 
               aes(x = ci_lower, xend = ci_upper, y = 0.5, yend = 0.5),
               linewidth = 1.0, color = "red"
  ) +
  geom_segment(data=df2, 
               aes(x = ci_lower, xend = ci_lower, y = 0.4, yend = 0.6),
               linewidth = 1.0, color = "red"
  ) +
  geom_segment(data=df2, 
               aes(x = ci_upper, xend = ci_upper, y = 0.4, yend = 0.6),
               linewidth = 1.0, color = "red"
  ) +
  
  geom_point(data=df1, aes(x = diff, y = 0.5), color = "red", size = 8) +
  
  geom_hline(yintercept = 0, color = "#595959", size = 0.3) +
  geom_vline(xintercept = 1,
             color = "#595959",
             linetype = 1,
             linewidth = 0.3) +
  labs(
    title = " ",
    x = " ",
    y = NULL   # remove y-axis label
  ) +
  # scale_x_continuous(limits = c(0.67, 1.5)) +   
  scale_x_log10() +
  scale_y_continuous(limits = c(0, 2)) +  # set x-axis range
  theme_minimal(base_size = 24) +
  theme(
    axis.text.y = element_blank(),    # remove y-axis tick labels
    axis.ticks.y = element_blank(),    # remove y-axis ticks
    plot.background = element_rect(fill = "#FCF7E4", color = NA),   # background color
    panel.background = element_rect(fill = "#FCF7E4", color = NA)) 

ggsave(p, filename = "ci2.png",width=7, height=5)


