#54.193687, 45.183491
#Шитенкова Анастасия – для региона 13 рассчитайте урожайность пшеницы в период 
#с 1995 по 2015 год взяв для рассчета средние суммы активных температур за эти годы, 
#с 13 ближайших метеостанций но убирая из рассчета активных температур дни с температурой 
#ниже 10 градусов

library(tidyverse)
library(rnoaa)
library(lubridate)
#Данные для расчета:
ai = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# station_data = ghcnd_stations()
# write.csv(station_data, "station_data.csv")
station_data = read.csv("station_data.csv")

# После получения списка всех станций, получим список станций ближайших к столице региона,
# создав таблицу с именем региона и координатами его столицы.
saransk = data.frame(id = "SARANSK", latitude = 54.193687,  longitude = 45.183491)
saransk_around = meteo_nearby_stations(lat_lon_df = saransk, station_data = station_data,
                                       limit = 13, var = "TAVG", year_min = 1995, year_max = 2015)
all_data = tibble()
# цикл для каждой станции
for (i in 1:13)
{
  #Для одной из станций зададим ее параметры
  saransk_id = saransk_around[["SARANSK"]][["id"]][i]
  # загрузим данные для станции
  data = meteo_tidy_ghcnd(stationid = saransk_id,
                          var="TAVG",
                          date_min="1995-01-01",
                          date_max="2015-12-31")
  #добавим строки в таблицу со всеми данными
  all_data = bind_rows(all_data, data)
}


 #создадим еще одну таблицу
clean_data = tibble()
  
  clean_data =   all_data %>% mutate(year = year(date), month = month(date)) %>%
                           mutate(tavg = tavg/10) %>%
                           filter(tavg > 10) %>%
                           group_by(year, month, id) %>%
                           summarize(summ = sum(tavg, na.rm=T), n=length(tavg) ) %>%
                           group_by(month) %>%
                           # ... и нахождения средних месячных активных температур:
                           summarize(s = mean(summ, na.rm = TRUE)) %>%
                           # создадим колонки для расчета:
                           mutate (a = ai[3:12], b = bi[3:12], d = di[3:12]) %>%
                           # и рассчитаем урожайность для каждого месяца:
                           mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
  
#Урожайность:
Yield = sum(clean_data$fert); Yield
