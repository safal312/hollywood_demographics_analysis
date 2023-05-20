library(stargazer)
library(lfe)

# ----------
model = lm("domestic_box_office ~ W_NL_male + W_NL_female + HL_male + HL_female + B_NL_male + B_NL_female + A_male + A_female",
           data=domestic_data)
stargazer(model, type="text")

model = lm("domestic_box_office ~ W_NL_male + W_NL_female + HL_male + HL_female + B_NL_male + B_NL_female + A_male + A_female +
           W_NL_male * B_NL_male",
           data=domestic_data)
stargazer(model, type="text")

# ---------------
model = lm("domestic_box_office ~  white + minorities + white * minorities",
           data=domestic_data)
stargazer(model, type="text",
          covariate.labels = c("White Cast", "Minority Cast", "White X Minorities"),
          dep.var.labels = c("Domestic Box Office"))

lmtest::bptest(model)
# ---------

minorities = seq(0,15)
effect = summary(model)$coefficients[2, 1] + 
  minorities*summary(model)$coefficients[4, 1]
margin_se = sqrt(vcov(model)[2,2] + 
                   minorities^2*vcov(model)[4,4] + 
                   2*minorities*vcov(model)[2,4] )
low = effect - 1.96 *margin_se
high = effect + 1.96 *margin_se
temp = cbind(minorities, effect, low, high)
temp = as.data.frame(temp)
ggplot(data = temp, aes(x=minorities, y = effect)) +
  # adding geom_point, note that there is no aes() function
  # that's because we specified x and y coordinates above
  geom_point(size = 3) +
  # adding the errorbars, note that aes() only has ymin and ymax args
  # that's because x and y were specified above
  geom_errorbar(aes(ymin = low, ymax = high), width=0.05) + 
  geom_hline(yintercept = 0, # adding horizontal line
             color = "deeppink4",
             size = 0.75,
             linetype = 2) + 
  xlab("No. of Minorities") + 
  ylab("Effect on Box Office") +
  ggtitle("Marginal effect of white casts on box office") + 
  theme_light() + 
  theme(plot.title = element_text(hjust=0.5))

max(domestic_data$minorities)

model = lm("box_office ~ white + minorities + Territory + white*minorities*Territory",
           data=demo_cuk_agg)
stargazer(model, type="text")

# now with fixed effects
m = felm(box_office ~ white + minorities + white*minorities | Territory | 0 | Territory, data = demo_cuk_agg)
getfe(m)
summary(m)

# stargazer(m, type="text")


# ggplot(demo_cuk) +
#   geom_bar(aes(x=movie, y = diff, fill = Territory), stat="identity", alpha = 0.75) +
#   scale_fill_manual(values = c("indianred4", "dodgerblue4"),
#                     labels = c("China", "UK"),
#                     name = "Territory") +
#   labs(x = "Box Office Earnings Per Capita",
#        y = "Count of Movies",
#        title = "Distribution of Box Office Earnings Per Capita in China vs UK") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# ggplot(demo_cuk) +
#   geom_histogram(aes(x = diff, fill = Territory), alpha = 0.75, bins=30) +
#   scale_fill_manual(values = c("indianred4", "dodgerblue4"),
#                     labels = c("China", "UK"),
#                     name = "Territory") +
#   labs(x = "Box Office Earnings Per Capita",
#        y = "Count of Movies",
#        title = "Distribution of Box Office Earnings Per Capita in China vs UK") +
#   theme_minimal()


# demo_cuk %>% subset(Territory == "China") %>% pull(diff) %>% mean()
# demo_cuk %>% subset(Territory == "United Kingdom") %>% pull(diff) %>% mean()
# 
# t.test(demo_cuk %>% subset(Territory == "China") %>% pull(diff),
#        demo_cuk %>% subset(Territory == "United Kingdom") %>% pull(diff))






