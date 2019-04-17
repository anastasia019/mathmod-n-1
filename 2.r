#������� 2, ��������� �������� �.�
#�������� ������ ������������� �������� ��������� ������� ������� ������� ����������� ���� �� ������ 2013 ���� �� ������ ���������������� ������������ ���������
setwd("C:/GitHub/mathmod-n-1")
getwd() #��������� ������� ����������

library("tidyverse")
#��������� ������ �� �����
data = read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
data = data[-1,] #������� ������
#����������� ��������� �������� � ���������
data = data %>% mutate_if(is.character, factor)
#�������� ������������� ����� �������
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
#���������, ��� ����������
glimpse(data)
#������� ������ ������ �� ��������� ������� 2013 ����:
data$daytime = as.logical(data$daytime)
data = data[data$DOY>=60 & data$DOY<=151 & data$daytime == TRUE, c(1:ncol(data))] 
#������� ��� ���������� ���� numeric
data_numeric = data[,sapply(data,is.numeric) ]
#��� ��������� ����������:
data_non_numeric = data[,!sapply(data,is.numeric) ]
# �������� ������� ��� ��������������� ������� � ����������� �� � �������, ������ ������ ������� (������ ����� ����)
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux)
#������� ����� ���������� (�����) � ������������� ������������ ������ 0.2
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .2] %>% na.exclude; vars
#������� ���������� �� ������� � ���� �������:
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")); formula
#�������� ��������� � ����������� �������:
row_numbers = 1:length(data$date)
teach = sample(row_numbers, floor(length(data$date)*.7))
test = row_numbers[-teach]
#���������������� ����������:
teaching_tbl_unq = data[teach,]
testing_tbl_unq = data[test,]
# ������ 1
#������� ������ �������� ���������
model = lm(formula, data = data);model
#������������
coef(model)
#�������
resid(model)
#������������� ��������
confint(model)
#P-�������� �� ������
summary(model)
#������������� ������
anova(model)
#����������� ������������� ������:
plot(model)

# ������ 2
formula = as.formula(paste("h2o_flux~", "(", paste(vars,collapse = "+"), ")^2", sep="", collapse = NULL));formula
#������� ������ �������� ���������
model1 = lm(formula, data = data);model
#������������
coef(model1)
#�������
resid(model1)
#������������� ��������
confint(model1)
#P-�������� �� ������
summary(model1)
#������������� ������
anova(model1)
#����������� ������������� ������:
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

 
              