##### Tidy Regression in R 

library(tidyverse)
library(cowplot)
# These packages are installed w/ tidyverse
library(broom)
library(modelr)

##### Simple Linear Regression: #####
# We're going to be using some data that comes with the modelr package; sim1
sim1
# Visualize the relationship
ggplot(sim1, aes(x,y)) + geom_point()

# Let's run a regression
# The lm() function can run most linear models that have a continuous response (y) variable: 
# linear regression, t-tests, ANOVA, ANCOVA, etc. 
sim1_mod <- 
  lm(formula = y ~ x, # "~" indicates a formula
     data = sim1) 
# The formula determines which variable is the response/y (right hand side) and which is the predictor/x (right hand side).  More complicated formulas can specify the relationship between several x variables.

# Some basic information:
sim1_mod
# More details
summary(sim1_mod)

# Tidyverse views of output
tidy(sim1_mod)
glance(sim1_mod)

# Combine model predictions with original data
augment(sim1_mod)

# Plot regression results
augment(sim1_mod) %>% 
  ggplot() +
  # Note we're declaring aes() inside the geom_ functions, since it's different between the layers
  geom_ribbon(aes(x, ymin = .fitted - .se.fit, 
    # This is creates the standard error around the prediction
                  ymax = .fitted + .se.fit),
              fill = grey(.8))+
  geom_point(aes(x, y = y)) +
  geom_line(aes(x = x, y = .fitted),
            color = "red")

augment(sim1_mod) %>% 
  ggplot() +
  # Note we're declaring aes() inside the geom_ functions, since it's different between the layers
  geom_point(aes(x, y = y)) +
  geom_line(aes(x = x, y = .fitted),
            color = "red")
# Check residuals
augment(sim1_mod) %>% 
  ggplot(aes(x=x, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0,
             color = "red")

###########################
# Lizard Exercise:
lizards = read_csv("data/anoles.csv")
# One would expect a relationship between limb length (Limb) perch characteristics (Diameter & Height)
# Are these relationship present in the data?  Use linear models and visualize the results.
# Are there strong relationships between morphological measurements (SVL, Tail, Mass, Limb)?  

#### ANOVA/t-test (Categorical predictors) #####
install.packages("cld")
# lm works with categorical predictors too
sim2
# Visualize it
ggplot(sim2) + aes(x, y) + geom_violin(color = gray(.8)) + geom_jitter(width = .2)

sim2_mod <- # Now let's run an ANOVA; note the code is exactly the same as sim1
  lm(formula = y ~ x, data = sim2) 
sim2_mod # Note the coefficients: xb, xc, xd
# These define how the mean value of categories b:d differ from the mean of category a (which is the intercept)
anova(sim2_mod) # gives you the ANOVA table
tidy(anova(sim2_mod)) # converts it to a tidy data frame
tidy(sim2_mod) # Gives you the effect-level estimates

augment(sim2_mod) 
# install.packages("multcomp")
library(multcomp)

# Plot means with confidence intervals
augment(sim2_mod) %>% 
  # Calculate the location of the error bars
  mutate(y_lower  = .fitted - 1.96*.se.fit, 
         y_upper  = .fitted +  1.96*.se.fit) %>% 
  ggplot(aes(x = x)) +  
  # We're using the same x variables for everything, so we define it in the main ggplot function. We use different y variables, so we define the aes() in each geom
  geom_errorbar(aes(ymin = y_lower, ymax = y_upper), 
                width = .3, color = "red") +
  geom_point(aes(y = .fitted), size = 2, color = "red") +
  geom_jitter(aes(y = y), width = .2)

# Exercise: 
# 95% confidence intervals are the group mean +/- 1.96 * the standard error.
  # Modify the above code to show them.  
  # Which groups have non-overlapping confidence intervals?

# You can run post-hoc tests to determine which groups are 'significantly' different.
  # We aren't going to go into the details of that here, but let's say we had the following results, suggesting groups a and d are grouped together, with b and c being unique
signif_groups = 
  tribble(~"x", ~"group",
            "a", "A",
            "b", "B",
            "c", "C",
            "d", "A")
sim2_anova_results = augment(sim2_mod) %>% 
  mutate(y_lower  = .fitted - .se.fit, 
         y_upper  = .fitted +  .se.fit) %>% 
  left_join(signif_groups, by = "x") # join the table of groupings
sim2_anova_results
sim2_anova_results %>% 
  mutate(y_lower  = .fitted - 1.96*.se.fit, 
         y_upper  = .fitted +  1.96*.se.fit) %>% 
  ggplot(aes(x = x)) +  
  # We're using the same x variables for everything, so we define it in the main ggplot function. We use different y variables, so we define the aes() in each geom
  geom_errorbar(aes(ymin = y_lower, ymax = y_upper), 
                width = .3, color = "red") +
  geom_point(aes(y = .fitted), size = 2, color = "red") +
  geom_jitter(aes(y = y), width = .2) +
  geom_text(aes(label = group), y = 12, size = 12) + ylim(0, 12)
# Since there's no multi-membership groups, you map group to color or another aesthetic instead.  

#####################
# Lizard Exercise:
 
# Do different colored anoles differ in perch use (Height and Diameter?)) Visualize group means and confidence intervals. You may wish to use geom_violin() to summarize the raw data, since there's quite a lot of it.
# Are there similar differences among sites?

##### Multiple predictors #####

sim3
sim3 %>% ggplot(aes(x1, y, color = x2)) + 
  geom_point()
sim3 %>% ggplot(aes(x1, y)) + facet_wrap(~x2)+ 
  geom_point()

## To fit two independent variables without interactions, use +
sim3_mod1 <- lm(y ~ x1 + x2, data = sim3) 
# To fit two predictors AND their interaction, use *
sim3_mod2 <- lm(y ~ x1 * x2, data = sim3)

# The first model assumes that there's a single slope between x1 and y, and that the intercept differs between the groups of x2; this is a traditional ANCOVA
# The interaction model has different intercepts y~x1 slopes for each group in x2; it's essentially 4 different regressions.

summary(sim3_mod1)
summary(sim3_mod2)
# You can use AIC for model comparison
AIC(sim3_mod1, sim3_mod2)
# Model 2 has a much lower AIC, indicating a better fit

# We can look at this visually, too.
model_comparison_data = 
  bind_rows( # combines two data frames
    augment(sim3_mod1) %>% mutate(Model = "No Interaction"),
    augment(sim3_mod2) %>% mutate(Model = "Interaction")
  ) %>% 
  mutate(ci_lower = .fitted - 1.96 * .se.fit, ci_upper = .fitted + 1.96 * .se.fit)

model_comparison_data %>% ggplot(aes(x = x1, color = Model)) + facet_wrap(~x2) +
  geom_point(aes(y = y), color = "black") + # raw data
    # we're defining a fixed color here because the raw data isn't part of a model
  geom_line(aes(y = .fitted)) +
  # We're defining confidence intervals as lines this time, with different line patterns
  geom_line(aes(y = ci_lower), linetype = 2) + 
  geom_line(aes(y = ci_upper), linetype = 2) 

################################
# Lizards Exercise
# Let's look back at the relationships between limb length and perch Height/Diameter. 
# Would considering interactions improve the fit of the models? Visualize he results

##### Bonus: GLM example (if we get there?) ####
lizards_2 = lizards %>% # Define a binary variable
  mutate(Pattern = if_else(Color == "Brown", "Vermiculated", "Spotted") %>% as.factor)
logsitic_regression = glm(Pattern ~ SVL*Site, family = binomial(), data = lizards_2)

augment(logsitic_regression)
# Plot this