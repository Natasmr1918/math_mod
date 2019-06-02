# 1
# Смирнова Наталья – для региона 42 рассчитайте урожайность пшеницы в 2008 году, 
# взяв для рассчета средние суммы активных температур за предыдущие 5 лет, с метеостанций на расстоянии от 50 до 250 км
# Координаты столицы: Кемерово, Кемеровская обл., 650055  55.340206, 86.061170

# Подключим библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# station_data = ghcnd_stations()
# write.csv(station_data, "station_data.csv")

station_data = read.csv("station_data.csv")
# Получим список метеостанций
kemerovo = data.frame(id = "KEMEROVO", latitude = 55.340206,  longitude = 86.061170)
#найдем станции, соответствующие критериям
kemerovo_around = meteo_nearby_stations(lat_lon_df = kemerovo, station_data = station_data,
                                           radius = 250, var = "TAVG", 
                                           year_min = 2003, year_max = 2007)
#создадим таблицу
all_data = tibble()
#запишем в таблицу данные для станций, начиная со станции, расположенной на расстоянии 85.7 км
for (i in 3:length(kemerovo_around))
{
  # Определим станцию:
  kemerovo_id = kemerovo_around[["KEMEROVO"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = kemerovo_id,
                          var="TAVG",
                          date_min="2003-01-01",
                          date_max="2007-12-31")
  #объединим данные в таблице
  all_data = bind_rows(all_data, data %>%
                         #добавим колонки для группировки по году и месяцу
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         #найдем суммарную среднюю активную температуру по месяцу за каждый год для станции
                         summarise (tavg = sum(tavg[tavg>50], na.rm = TRUE)/10 )
  )
}

# Изменения в таблице сохранятся в векторе clean_data.
clean_data = all_data %>%
  # Добавим колонку month для группировки данных:
  group_by(month) %>%
  # Найдем месячный d и cумму активных тмператур для каждой станции:
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  #добавим данные из таблицы с показателем d
  # Добавим колонки для расчета:
  mutate (a = af, b = bf, d = df) %>%
  # Рассчитаем урожайность для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#Согласно расчету, урожайность пшеницы в Кемерово в 2008 году составила (ц/га):
Yield = sum(clean_data$fert); Yield
