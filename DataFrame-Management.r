# Library init ----------------------------------------------------------

library(gridExtra)
library(tidyverse)
library(ggplot2)
library(scales)

# Dataframe init ----------------------------------------------------------

per_capita_energy_use <- read_csv("Dataframes/per-capita-energy-use.csv")
coin_Aave <- read_csv("Dataframes/coin_Aave.csv")
coin_BinanceCoin <- read_csv("Dataframes/coin_BinanceCoin.csv")
coin_Bitcoin <- read_csv("Dataframes/coin_Bitcoin.csv")
coin_Cardano <- read_csv("Dataframes/coin_Cardano.csv")
coin_ChainLink <- read_csv("Dataframes/coin_ChainLink.csv")
coin_Cosmos <- read_csv("Dataframes/coin_Cosmos.csv")
coin_CryptocomCoin <- read_csv("Dataframes/coin_CryptocomCoin.csv")
coin_Dogecoin <- read_csv("Dataframes/coin_Dogecoin.csv")
coin_EOS <- read_csv("Dataframes/coin_EOS.csv")
coin_Ethereum <- read_csv("Dataframes/coin_Ethereum.csv")
coin_Iota <- read_csv("Dataframes/coin_Iota.csv")
coin_Litecoin <- read_csv("Dataframes/coin_Litecoin.csv")
coin_Monero <- read_csv("Dataframes/coin_Monero.csv")
coin_NEM <- read_csv("Dataframes/coin_NEM.csv")
coin_Polkadot <- read_csv("Dataframes/coin_Polkadot.csv")
coin_Solana <- read_csv("Dataframes/coin_Solana.csv")
coin_Stellar <- read_csv("Dataframes/coin_Stellar.csv")
coin_Tether <- read_csv("Dataframes/coin_Tether.csv")
coin_Tron <- read_csv("Dataframes/coin_Tron.csv")
coin_USDCoin <- read_csv("Dataframes/coin_USDCoin.csv")
coin_Uniswap <- read_csv("Dataframes/coin_Uniswap.csv")
coin_WrappedBitcoin <- read_csv("Dataframes/coin_WrappedBitcoin.csv")
coin_XRP <- read_csv("Dataframes/coin_XRP.csv")

# Dataframe Cleaning ----------------------------------------------------------

coins_ALL_Vector <- c(list(coin_Aave),
                      list(coin_BinanceCoin),
                      list(coin_Bitcoin),
                      list(coin_Cardano),
                      list(coin_ChainLink),
                      list(coin_ChainLink),
                      list(coin_Cosmos),
                      list(coin_CryptocomCoin),
                      list(coin_Dogecoin),
                      list(coin_EOS),
                      list(coin_Ethereum),
                      list(coin_Iota),
                      list(coin_Litecoin),
                      list(coin_Monero),
                      list(coin_NEM),
                      list(coin_Polkadot),
                      list(coin_Solana),
                      list(coin_Stellar),
                      list(coin_Tether),
                      list(coin_Tron),
                      list(coin_Uniswap),
                      list(coin_USDCoin),
                      list(coin_WrappedBitcoin),
                      list(coin_XRP))



coins_ALL_DF <- data.frame()
for (DF in coins_ALL_Vector) {
  coins_ALL_DF <- rbind(coins_ALL_DF,DF)}
coins_ALL_DF %>% mutate(Date=as.Date(Date, format = "%y-%m-%d"))


power_df <- data.frame()
power_df <- na.omit(per_capita_energy_use)
power_df <- filter(power_df, Year %in% c(seq(2013,2021,1)))

coins_df_summary <- coins_ALL_DF %>% 
  group_by(Date = lubridate::floor_date(Date, 'year')) %>%
  summarize(Avg_Value = (mean(High)+mean(Low))/2)

power_df_summary <- power_df %>% 
  group_by(Year) %>%
  summarize(Avg_Consumption = mean(`Primary energy consumption per capita (kWh/person)`)) 

power_df_summary$Year <- paste(power_df_summary$Year,"01","01", sep="-")

volumen_df_summary <- coins_ALL_DF %>% 
  group_by(Date = lubridate::floor_date(Date, 'year')) %>%
  summarize(Volume = mean(Volume))

# Summary  ----------------------------------------------------------

summary(coins_ALL_DF)
nrow(coins_ALL_DF)
ncol(coins_ALL_DF)

summary(power_df)
nrow(power_df)
ncol(power_df)


# Visualization  ----------------------------------------------------------

value <- coins_df_summary %>% 
  ggplot() +
  geom_col(aes(x = as.Date(Date,format = "%y-%m-%d"), y = Avg_Value),color="Black",fill="Red") + 
  labs(y="Valor Promedio de las criptomonedas(USD)", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  ggtitle("Promedio del valor de las crypto monedas por año ")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

volumen <- volumen_df_summary %>%  
  ggplot() +
  geom_col(aes(x = as.Date(Date,format = "%y-%m-%d"), y = Volume),color="Black",fill="Green") + 
  labs(y="Volumen Promedio de transacciones", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  scale_y_log10(label=label_log())+
  ggtitle("Promedio de transacciones por año")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power <- power_df_summary %>% 
  ggplot() +
  geom_col(aes(x = as.character(Year), y = Avg_Consumption),color="Black",fill='Blue') + 
  labs(y="Consumo promedio de energia per capita (kWh/persona) ", x="Años")+
  ggtitle("Promedio de consumo de energia por año")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_value <-  ggplot() + 
  geom_line(data=power_df_summary, aes(x=as.character(Year), y=Avg_Consumption, group=1), color='blue') + 
  geom_line(data=coins_df_summary, aes(x=as.character(Date), y=Avg_Value ,group=2), color='red')+
  labs(y="Consumo(Azul) vs Precio(Rojo)", x="Años")

power_volume <-  ggplot() + 
  geom_line(data=power_df_summary, aes(x=as.character(Year), y=Avg_Consumption, group=1), color='blue') + 
  geom_line(data=volumen_df_summary, aes(x=as.character(Date), y=Volume ,group=2), color='green')+
  labs(y="Consumo(Azul) vs Volumen de transacciones(Verde)", x="Años")
