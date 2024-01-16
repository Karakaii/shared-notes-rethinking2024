library(tidyverse)

# Get the data
library(rethinking)
data(Howell1)
# detach this package so it doesn't mask other things
detach(package:rethinking, unload = T)
Howell1
rethinking::precis(Howell1)

d2 <- Howell1[Howell1$age >= 18,]

set.seed(100)

# Run a model where we add the effect of height
# Use centring to make for a more interpretable intercept and prior
# Here a is the average weight for someone of the average height
m_SHW <- rethinking::quap(
    alist(
        W ~ dnorm(mu,sigma),
        mu <- a[S] + b[S]*(H-Hbar),
        a[S] ~ dnorm(60,10),
        b[S] ~ dunif(0,1),
        sigma ~ dunif(0,10)
    ),
    data = list(W = d2$weight, H = d2$height, Hbar = mean(d2$height), S = d2$male+1)
)
rethinking::precis(m_SHW, depth = 2)

# Running the linear model for each value of the posterior, for each value of the data
# e.g., across each of these possible values of height and sex, what is the weight predicted
height_seq <- seq(130, 190, length=100)

post_lines_S1 <- rethinking::link(fit = m_SHW, data = list(H=height_seq, S=rep(1, 100), Hbar = mean(d2$height)))
post_lines_S1 <- as.data.frame(post_lines_S1)
post_lines_S1$line <- rownames(post_lines_S1)
post_lines_S1 <- post_lines_S1 %>% pivot_longer(
    cols = -line, names_to = "height", values_to = "weight"
)
post_lines_S1$height <- as.numeric(str_remove(post_lines_S1$height, "V")) # remove the V string
post_lines_S1$height <- height_seq[post_lines_S1$height] # get height based on index of the sequence
post_lines_S1_mean <- post_lines_S1 %>% group_by(height) %>% summarise(weight = mean(weight))

post_lines_S2 <- rethinking::link(fit = m_SHW, data = list(H=height_seq, S=rep(2, 100), Hbar = mean(d2$height)))
post_lines_S2 <- as.data.frame(post_lines_S2)
post_lines_S2$line <- rownames(post_lines_S2)
post_lines_S2 <- post_lines_S2 %>% pivot_longer(
    cols = -line, names_to = "height", values_to = "weight"
)
post_lines_S2$height <- as.numeric(str_remove(post_lines_S2$height, "V"))
post_lines_S2$height <- height_seq[post_lines_S2$height] # get height based on index of the sequence
post_lines_S2_mean <- post_lines_S2 %>% group_by(height) %>% summarise(weight = mean(weight))

post_lines <- bind_rows(
    post_lines_S1 %>% mutate(sex = 1),
    post_lines_S2 %>% mutate(sex = 2)
) %>% mutate(
    sex = as.factor(sex),
    line_sex = paste0(line, "_", sex)
)

post_lines_mean <- bind_rows(
    post_lines_S1_mean %>% mutate(sex = 1),
    post_lines_S2_mean %>% mutate(sex = 2)
) %>% mutate(
    sex = as.factor(sex)
)

post_pop_S1 <- rethinking::sim(fit = m_SHW, data = list(H=height_seq, S=rep(1, 100), Hbar = mean(d2$height)))
post_pop_S1 <- as.data.frame(post_pop_S1)
post_pop_S1$id <- rownames(post_pop_S1)
post_pop_S1 <- post_pop_S1 %>% pivot_longer(
    cols = -id, names_to = "height", values_to = "weight"
)
post_pop_S1$height <- as.numeric(str_remove(post_pop_S1$height, "V")) # remove the V string
post_pop_S1$height <- height_seq[post_pop_S1$height] # get height based on index of the sequence
post_pop_S1_pi <- post_pop_S1 %>% group_by(height) %>% summarise(
    low = tidybayes::qi(weight, 0.89)[1],
    high = tidybayes::qi(weight, 0.89)[2]
)

post_pop_S2 <- rethinking::sim(fit = m_SHW, data = list(H=height_seq, S=rep(1, 100), Hbar = mean(d2$height)))
post_pop_S2 <- as.data.frame(post_pop_S2)
post_pop_S2$id <- rownames(post_pop_S2)
post_pop_S2 <- post_pop_S2 %>% pivot_longer(
    cols = -id, names_to = "height", values_to = "weight"
)
post_pop_S2$height <- as.numeric(str_remove(post_pop_S2$height, "V")) # remove the V string
post_pop_S2$height <- height_seq[post_pop_S2$height] # get height based on index of the sequence
post_pop_S2_pi <- post_pop_S2 %>% group_by(height) %>% summarise(
    low = tidybayes::qi(weight, 0.89)[1],
    high = tidybayes::qi(weight, 0.89)[2]
)

post_post_pi <- bind_rows(
    post_pop_S1_pi %>% mutate(sex = 1),
    post_pop_S2_pi %>% mutate(sex = 2)
) %>% mutate(
    sex = as.factor(sex)
)

# plot the mean lines from the posterior across the data
# (around the mean of the posterior? doesn't include sigma?)
d2 %>% mutate(sex=as.factor(male+1)) %>%
    ggplot(aes(x=height, y=weight, color=sex)) +
    geom_line(
        data = post_lines_mean
    ) +
    geom_point() +
    theme_bw()

# plot the uncertainty around the averages lines across the data
# (around the mean of the posterior - doesn't include sigma)
# and plot the uncertainty around which the population could fall
#  (89% percentile interval - includes sigma)
d2 %>% mutate(sex=as.factor(male+1)) %>%
    ggplot(aes(x=height, y=weight, color=sex)) +
    geom_line(
        data = post_lines,
        aes(group=line_sex),
        alpha=0.05
    ) +
    geom_line(
        data = post_post_pi %>% select(height, sex, low) %>% rename(weight=low),
        alpha=1,
        linetype=2
    ) +
    geom_line(
        data = post_post_pi %>% select(height, sex, high) %>% rename(weight=high),
        alpha=1,
        linetype=2
    ) +
    geom_point() +
    theme_bw()

