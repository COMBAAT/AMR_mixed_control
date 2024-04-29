

df <- test1 %>% mutate(W = as.factor(W_st), ps = as.factor(prop.insecticide), K = as.factor(K))

plot2a <- df %>% filter(W == 0, K == 6000) %>% ggplot(aes(x = treat_prop, y = Incidence, colour = ps, group = interaction(ps, K))) + 
  geom_point(aes(shape = K), size = 3) + 
  geom_line()
plot2a

plot2b <- df %>% filter(W == 100, K == 6000) %>% ggplot(aes(x = treat_prop, y = Incidence, colour = ps, group = interaction(ps, K))) + 
  geom_point(aes(shape = K), size = 3) + 
  geom_line()
plot2b

plot2c <- df %>% filter(W == 250, K == 6000) %>% ggplot(aes(x = treat_prop, y = Incidence, colour = ps, group = interaction(ps, K))) + 
  geom_point(aes(shape = K), size = 3) + 
  geom_line()
plot2c

print(plot_grid(plot2a, plot2b, plot2c))
  

plot3a <- df %>% filter(W == 0, K == 6000) %>% ggplot(aes(x = treat_prop, y = prevalence, colour = ps, group = interaction(ps, K))) + 
  geom_point(aes(shape = K), size = 3) + 
  geom_line()
plot3a

plot3b <- df %>% filter(W == 100, K == 6000) %>% ggplot(aes(x = treat_prop, y = prevalence, colour = ps, group = interaction(ps, K))) + 
  geom_point(aes(shape = K), size = 3) + 
  geom_line()
plot3b

plot3c <- df %>% filter(W == 250, K == 6000) %>% ggplot(aes(x = treat_prop, y = prevalence, colour = ps, group = interaction(ps, K))) + 
  geom_point(aes(shape = K), size = 3) + 
  geom_line()
plot3c

print(plot_grid(plot3a, plot3b, plot3c))
