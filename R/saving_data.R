library(dplyr)
library(readxl)
library(hms)
library(lubridate)

filesELPI <- c("C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250620_095706.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250620_134957.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250623_091259.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250623_133554.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250624_093940.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250624_125342.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250625_084846.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250625_091846.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250625_140744.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250605_120257.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250605_140830.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250606_102855.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250609_102855.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250609_140935.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250610_092329.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250610_140818.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250612_110918.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250612_164404.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250617_095950.dat",
                "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/ELPI Data Files/elpi_20250617_131908.dat")

dataELPI <- combine_elpi_data(filesELPI, add_filename = TRUE)
saveRDS(dataELPI, "C:/Users/katie/OneDrive - UBC/Research/Tirewear-particulate/R/elpiR/chassisData/data/dataELPI.rds")

filesTruck <- c("C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June5.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June6.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June9.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June10.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June12.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June17_Truck.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June20_TruckData.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June23_Truckdata.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June24_Truckdata.xlsx",
           "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/TruckData/June25_Truckdata.xlsx")

###
dataTruck <- filesTruck %>%
  map_df(~ read_excel(.x, col_types = "text") %>%
           mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
                  Time = ifelse(grepl(":", Time),
                           Time,
                           format(hms::as_hms(as.numeric(Time) * 86400), "%H:%M:%S")),
                 DateTime = ymd_hms(paste(Date, Time), tz = "America/Los_Angeles"),
                 Seconds = as.numeric(Seconds),
                 WheelBasedVehicleSpeed = as.numeric(WheelBasedVehicleSpeed),
                 WheelBasedVehicleSpeed = ifelse(WheelBasedVehicleSpeed < 1, 0, WheelBasedVehicleSpeed),
                 WheelBasedVehicleSpeed = if_else( WheelBasedVehicleSpeed > 150, lag(WheelBasedVehicleSpeed), WheelBasedVehicleSpeed)))

saveRDS(dataTruck, "C:/Users/katie/OneDrive - UBC/Research/Tirewear-particulate/R/elpiR/chassisData/data/dataTruck.rds")

###
filesAQ <- c("C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/AirQuality Sensor Data/QUANTAQ_sensor_data_June3-09.csv",
                 "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/AirQuality Sensor Data/QUANTAQ_sensor_data_June10-16.csv",
                 "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/AirQuality Sensor Data/QUANTAQ_sensor_data_June16-23.csv",
                 "C:/Users/katie/OneDrive - UBC/Research/Gurcharan - Chassis Dyno Sampling Campaign 2025/Instrument Data/AirQuality Sensor Data/QUANTAQ_sensor_data_Jun23-25.csv")

dataAQ <- filesAQ %>%
  map_df(~read.csv(.x)) %>%
  mutate(DateTime_utc = ymd_hms(timestamp, tz = "UTC"),
          DateTime_local = ymd_hms(timestamp_local, tz = "America/Los_Angeles"),
          DateTime_rounded = round_date(DateTime_local, unit = "second"))

saveRDS(dataAQ, "C:/Users/katie/OneDrive - UBC/Research/Tirewear-particulate/R/elpiR/chassisData/data/dataAQ.rds")

