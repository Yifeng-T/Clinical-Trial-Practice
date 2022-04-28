# Latent class mixed model analysis for blood_pressure
*Yifeng Tang*   
*2022-04*    
   
   
We have done the latent class mixed model for rice diet blood pressure data. Following is the procedure how I performed the method.
## The R code are stores in
**Training the data:** P/Rice/final_latent_mixed_model_for_MH/train_MHmodel_fullmodel.Rmd   
**Plotting:** P/Rice/full_7days_avg_plot.Rmd   
Run the training file first, then run the plotting file.


## 1.	Data preparation
The density of the blood pressure (systolic blood pressure) data is very high, so our strategy is to take the weekly mean value of the blood pressure less than 365 days since entering the study. 
* a.	Less than 365 days
* b.	Calculate the mean value     
    
Following is the code to implement these two functions:
```{r}
table_l = table1%>%filter(DateInStudy<365) # less than 365 days

table_l = table_l%>%mutate(week = DateInStudy %/% 7)%>%group_by(record_id, week)%>%mutate(week_mean_bp_sys = mean(bp_sys), 
                                                                                week_mean_bp_dia = mean(bp_dia),
                                                                                week_mean_bp_wei = mean(weight_lbs))
#mutate the new variable week_index and weekly mean values
table_l = table_l%>%filter(!duplicated(cbind(record_id, week)))
```
* c.	To create the new variable :the covariate representing the grouping structure (here is each subject). Since the new variable should be numeric value, we may not use record_id here. Following is the code I create the variable called “idd”, which could be used as the covariate grouping structure:    
```{r}
table_temp = table%>%filter(nchar(record_id) ==4)
for (row in 1:nrow(table_temp)) {
table_temp [row, "record_id"] = paste("U0", table_temp[row, "record_id"], sep = "")
    
}
table[1:18676,]=table_temp

table = table%>%mutate(idd = substr(record_id, 2, nchar(record_id)))
table$idd = as.numeric(table$idd)
```
## 2.	Train the model
We used the [lcmm function](https://www.rdocumentation.org/packages/lcmm/versions/1.8.1.1/topics/lcmm) to train the model. In this part, we assume the model is a polynomial that have different terms of different powers. When you train the model, you could try different combinations.   
Following is the code to train the model:
```{r}
model_bpdia_3 = lcmm(fixed = week_mean_bp_dia~poly(DateInStudy, degree = 3), subject="idd", mixture = ~poly(DateInStudy, degree = 3), ng = 3, data=table_l,link="linear")
#fixed: the responders; subject: is the covariate structure variable that we created previously
```
[The optimal model for weekly_blood_pressure is degree=3 with 3 potential groups ng=3]
You can use this code to check the details of the model:
```{r}
postprob(sys2)#sys2 is the model name
```
## 3.	Plotting
Given the date variable, we can predict the weekly_avg_bp:
```{r}
#setwd("P:/Rice/yifeng/latent_class_mixed_models")
#dm5 = my_model <- readRDS("dm5.rds")
# Generate Predictions from Optimal Model for Plotting
data_new <- data.frame(DateInStudy = seq(from = 0, to = 365, by = 0.01))
preds <- predictY(sys2, newdata = data_new, draws = TRUE, ndraws = 500) # set the predicted model

#preds_data: stored the predicted information based on the given data in study variable
preds_data <- as.data.frame(preds$pred)
preds_data$new_time <- preds$times$DateInStudy
p1 = plot(preds) # p1 is the plot includes simple trajectory lines
```
Following is how to generate a fancy plot by using ggplot2
```{r}
preds_data <- 
  preds_data %>%
  pivot_longer(cols = contains("Ypred_"),
               names_prefix = "Ypred_",
               names_to = "class") %>%
  separate(class, into = c("quant", "class"), sep = "_") %>%
  mutate(class = str_remove(class, "class")) %>%
  pivot_wider(id_cols = c(new_time, class), #here, the  new_time is the variable name in preds_data
              names_from = c(quant),
              values_from = value,
              names_prefix = "quant_")


```

```{r}
plot_traj <- 
  function(data, metric){
    ggplot(data = data) +
      geom_ribbon(aes(x = new_time,
                      ymin = quant_2.5,
                      ymax = quant_97.5,
                      group = class,
                      fill = class),
                  alpha = 0.2) + 
      geom_smooth(aes(x = new_time,
                      y = quant_50,
                      group = class,
                      color = class),
                  lwd = 1.5,
                  method = "loess") + 
      labs(x = "Days since entering the study",
           y = metric,
           color = "Latent Class",
           fill = "Latent Class") +
      theme_classic()
  }
```
```{r}
plot_traj(data = preds_data, metric = "BP_sys") +ggtitle("predicted trajectory lines for systolic blood pressure (2 latents and cubed model)")
```

