# 1
# �������� ������� � ��� ������� 42 ����������� ����������� ������� � 2008 ����, 
# ���� ��� �������� ������� ����� �������� ���������� �� ���������� 5 ���, � ������������ �� ���������� �� 50 �� 250 ��
# ���������� �������: ��������, ����������� ���., 650055  55.340206, 86.061170

# ��������� ����������:
library(tidyverse)
library(rnoaa)
library(lubridate)

# �������� ������� � ������� ��� �������:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 #  ����������� ������������� ���
Qj = 1600 # ������������ ������ ��������
Lj = 2.2 #  ����� ������ �������� � �������� ���������
Ej = 25 #   ����������� ��������� ��������

# station_data = ghcnd_stations()
# write.csv(station_data, "station_data.csv")

station_data = read.csv("station_data.csv")
# ������� ������ ������������
kemerovo = data.frame(id = "KEMEROVO", latitude = 55.340206,  longitude = 86.061170)
#������ �������, ��������������� ���������
kemerovo_around = meteo_nearby_stations(lat_lon_df = kemerovo, station_data = station_data,
                                           radius = 250, var = "TAVG", 
                                           year_min = 2003, year_max = 2007)
#�������� �������
all_data = tibble()
#������� � ������� ������ ��� �������, ������� �� �������, ������������� �� ���������� 85.7 ��
for (i in 3:length(kemerovo_around))
{
  # ��������� �������:
  kemerovo_id = kemerovo_around[["KEMEROVO"]][["id"]][i]
  # �������� ������ ��� �������:
  data = meteo_tidy_ghcnd(stationid = kemerovo_id,
                          var="TAVG",
                          date_min="2003-01-01",
                          date_max="2007-12-31")
  #��������� ������ � �������
  all_data = bind_rows(all_data, data %>%
                         #������� ������� ��� ����������� �� ���� � ������
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         #������ ��������� ������� �������� ����������� �� ������ �� ������ ��� ��� �������
                         summarise (tavg = sum(tavg[tavg>50], na.rm = TRUE)/10 )
  )
}

# ��������� � ������� ���������� � ������� clean_data.
clean_data = all_data %>%
  # ������� ������� month ��� ����������� ������:
  group_by(month) %>%
  # ������ �������� d � c���� �������� ��������� ��� ������ �������:
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  #������� ������ �� ������� � ����������� d
  # ������� ������� ��� �������:
  mutate (a = af, b = bf, d = df) %>%
  # ���������� ����������� ��� ������� ������:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#�������� �������, ����������� ������� � �������� � 2008 ���� ��������� (�/��):
Yield = sum(clean_data$fert); Yield
