#Задание 2, выполнила Шитенков А.А
#создайте модель множественной линейной регрессии потоков дневных потоков углекислого газа за период 2013 года по данным измеренийметодом турбулентной пульсации
setwd("C:/GitHub/mathmod-n-1")
getwd() #проверяем рабочую директорию

library("tidyverse")
#считываем данные из файла
data = read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
data = data[-1,] #убираем строку
#преобразуем строковые значения в факторные
data = data %>% mutate_if(is.character, factor)
#заменяем конфликтующие знаки колонок
names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>%
  str_replace_all("[?]","_quest_") %>%  
  str_replace_all("[*]","_star_") %>%  
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
#Посмотрим, что получилось
glimpse(data)
#оставим данные только по весеннему периоду 2013 года:
data$daytime = as.logical(data$daytime)
data = data[data$DOY>=60 & data$DOY<=151 & data$daytime == TRUE, c(1:ncol(data))] 
#выберем все переменные типа numeric
data_numeric = data[,sapply(data,is.numeric) ]
#все остальные переменные:
data_non_numeric = data[,!sapply(data,is.numeric) ]
# создадим матрицу для корелляционного анализа и преобразуем ее в таблицу, выбрав нужный столбец (потоки паров воды)
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux)
#выберем имена переменных (строк) с коэффициентом детерминации больше 0.2
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .2] %>% na.exclude; vars
#соберем переменные из вектора в одну формулу:
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")); formula
#создадим обучающую и тестирующую выборки:
row_numbers = 1:length(data$date)
teach = sample(row_numbers, floor(length(data$date)*.7))
test = row_numbers[-teach]
#непересекающиеся подвыборки:
teaching_tbl_unq = data[teach,]
testing_tbl_unq = data[test,]
# МОДЕЛЬ 1
#создаем модель линейной регрессии
model = lm(formula, data = data);model
#коэффициенты
coef(model)
#остатки
resid(model)
#доверительный интервал
confint(model)
#P-значения по модели
summary(model)
#дисперсионный анализ
anova(model)
#графическое представление модели:
plot(model)

# МОДЕЛЬ 2
formula = as.formula(paste("h2o_flux~", "(", paste(vars,collapse = "+"), ")^2", sep="", collapse = NULL));formula
#создаем модель линейной регрессии
model1 = lm(formula, data = data);model
#коэффициенты
coef(model1)
#остатки
resid(model1)
#доверительный интервал
confint(model1)
#P-значения по модели
summary(model1)
#дисперсионный анализ
anova(model1)
#графическое представление модели:
plot(model1)

 anova(model1)
 formula2 =h2o_flux~ v_var+w_var+h2o_var+w.ts_cov+co2+co2.1
model2=lm(formula2, data=tbl)
anova(model2)
summary(model2)
cor_td = cor(drop_na(select(data_numeric,v_var,w_var,h2o_var,w.ts_cov,co2,co2.1 )))

anova(model3)
formula3=h2o_flux~ v_var+h2o_var+w.ts_cov+co2
model3=lm (formula3, data=tbl)
anova(model3)
summary(model3)

 
              