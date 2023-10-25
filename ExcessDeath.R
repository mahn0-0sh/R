library(data.table)
library(ggplot2)

table = fread("iranprovs_mortality_monthly.csv")
table$ym_number = table$y + table$m/12 - 1/24

#here we categorize people by their age into three groups (young, middle, old)
table[, age_category := ifelse(age_group=="0" |age_group=="01-04" | age_group=="05-09"
                                | age_group == "10-14" | age_group == "15-19"|age_group=="20-24","young",
                                ifelse ( age_group=="25-29" | age_group=="30-34"
                                        | age_group == "35-39" | age_group == "40-44" | age_group == "45-49"
                                        | age_group == "50-54" | age_group == "55-59","middle","old"))]

dtable = table[, .(n = sum(n)), .(y, m, ym_number,prov,age_category)]

start_date = 1398 + 10/12 - 1/24
model_start = start_date - 5


deaths_table = dtable[ym_number>=start_date]
deaths_table[, upper_bound := 0 ]
deaths_table[, predicted_deaths := 0]


#here we find the predicted deaths in the time of corona and we found an upper bound for it (with confidence interval)
#and if number of deaths are more than this upper bound it means the difference is significant and we have excess mortality
for (pr in 1:31){
  for(ac in 1:3){
    for(mt in 1:12){
      prv = unique(dtable$prov)[pr]
      age_cat = unique(dtable$age_category)[ac]
      prv_mt = dtable[prov == prv & m == mt & ym_number>model_start & age_category==age_cat]
      
      fit = lm(n ~ ym_number, prv_mt[ym_number< start_date] )
      p_value = summary(fit)$coefficients[2,4]
      
      #if p-value<0.1 then our model is good and the coefficients are significant and we can have a good prediction
      if (p_value < 0.1){
        prediction = predict(fit, data.frame(ym_number = prv_mt[ym_number>=start_date,ym_number]),
                              interval = "prediction")
        deaths_table[prov == prv & m == mt  & age_category==age_cat, predicted_deaths := prediction[,1]]
        deaths_table[prov == prv & m == mt  & age_category==age_cat, upper_bound := prediction[,3]]
      }
      else{
        #here we use the mean instead.
        prediction = mean(prv_mt[ym_number<start_date ,n])
        confidence = confint(lm(n ~ 1, prv_mt[ym_number< start_date]),level = 0.95)
        deaths_table[prov == prv & m == mt  & age_category==age_cat, predicted_deaths := prediction]
        deaths_table[prov == prv & m == mt  & age_category==age_cat, upper_bound := confidence[2]]
      }
    }
  }
}

#we add rows to show how many excess mortality we have and the percentage of it
deaths_table[, excess_mortality := ifelse(n>upper_bound,n - predicted_deaths, 0)]
deaths_table[, em_percentage := excess_mortality/predicted_deaths * 100]

#here we calculate total excess mortality in provs and total in total!
total_deaths_prov = deaths_table[, .(n =floor(sum(excess_mortality))) , prov]
total_deaths = sum(total_deaths_prov$n)

#نتایج:
#تهران،خراسان رضوی و اصفحان بیشترین تعداد فوت اضافه را دارند که با توجه به جمعیت بالای آنها منطقی است.
#تعداد کل فوتی های اضافه با تخمین انجام شده 314305 تا بوده است

