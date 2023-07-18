# Library init ----------------------------------------------------------

library(gridExtra)
library(tidyverse)
library(ggplot2)
library(scales)
library(readxl)
library(zoo)

# Dataframe init ----------------------------------------------------------

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
MES_0423 <- read_csv("Dataframes/MES_0423.csv")

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



COINS_ALL_DF <- data.frame()
for (DF in coins_ALL_Vector) {
  COINS_ALL_DF <- rbind(COINS_ALL_DF,DF)}
COINS_ALL_DF <- COINS_ALL_DF %>% mutate(Date=as.yearmon(Date, format = "%y-%m-%d"))

COINS_ALL_DF_VOL <- COINS_ALL_DF %>% 
  group_by(Name) %>% 
  summarise(Volume=sum(Volume)) 

COINS_ALL_DF_VOL <- COINS_ALL_DF_VOL %>% mutate(Percentage = 100*Volume/sum(Volume))

coins <- COINS_ALL_DF[COINS_ALL_DF$Name %in% c('Ethereum','Bitcoin','Tether'),]

power_df <- MES_0423 
names(power_df) <- power_df[8,]
colnames(power_df)[5] <- "Amount"
power_df <- power_df[-c(1:8),]
power_df <- power_df %>%
  mutate(Time = Time %>% 
           str_replace_all(" ", "-"))
power_df <- power_df %>% mutate(Time=as.yearmon(Time,"%b-%Y"))
power_df <- power_df %>% 
            filter(!(Country %in% c("IEA Total","OECD Americas","OECD Asia Oceania","OECD Europe","OECD Total"))) %>% 
            filter(Product == "Total Combustible Fuels")

power_df_total <- MES_0423 
names(power_df_total) <- power_df_total[8,]
colnames(power_df_total)[5] <- "Amount"
power_df_total <- power_df_total[-c(1:8),]
power_df_total <- power_df_total %>%
  mutate(Time = Time %>% 
           str_replace_all(" ", "-"))
power_df_total <- power_df_total %>% mutate(Time=as.yearmon(Time,"%b-%Y"))
power_df_total <- power_df_total %>% 
  filter(!(Country %in% c("IEA Total","OECD Americas","OECD Asia Oceania","OECD Europe","OECD Total"))) %>% 
  filter(Product == "Electricity" & Balance == "Final Consumption (Calculated)")


power_df <- power_df %>% 
  group_by(Time,Product,Unit) %>% 
  summarize(Avg_amount = sum(Amount))

power_df_total <- power_df_total %>% 
  group_by(Time,Product,Unit) %>% 
  summarize(Avg_amount = sum(Amount))

value_df_summary <- coins %>% 
  group_by(Date) %>%
  summarize(Avg_Value = (mean(High)+mean(Low))/2)

volumen_df_summary <- coins %>% 
  group_by(Date)  %>%
  summarize(Volume = mean(Volume))

power_value_df <- left_join(value_df_summary,power_df,by=c("Date"="Time"))

power_volume_df <- left_join(volumen_df_summary,power_df,by=c("Date"="Time"))

power_total_value_df <- left_join(value_df_summary,power_df_total,by=c("Date"="Time"))

power_total_volume_df <- left_join(volumen_df_summary,power_df_total,by=c("Date"="Time"))
# Summary  ----------------------------------------------------------

summary(coins_ALL_DF)
nrow(coins_ALL_DF)
ncol(coins_ALL_DF)


# Visualization  ----------------------------------------------------------

value <- value_df_summary %>% 
  ggplot() +
  geom_col(aes(x = as.Date(Date,format = "%y-%m-%d"), y = Avg_Value),color="black",fill="red")+ 
  labs(y="Valor Promedio de las criptomonedas(USD)", x="Años")+
  ggtitle("Promedio del valor de las crypto monedas por año ")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

volumen <- volumen_df_summary %>%  
  ggplot() +
  geom_col(aes(x = as.Date(Date,format = "%y-%m-%d"), y = Volume),color="black",fill="green") + 
  labs(y="Volumen Promedio de transacciones", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  ggtitle("Promedio de transacciones por año")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power <- power_df %>%  
  ggplot() +
  geom_col(aes(x = as.Date(Time,format = "%y-%m-%d"), y = Avg_amount),color="black",fill="yellow") + 
  labs(y="Energia Promedio Generada por Combustibles", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  ggtitle("Promedio de energia creada con fuentes combustibles por año")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_total <- power_df_total %>%  
  ggplot() +
  geom_col(aes(x = as.Date(Time,format = "%y-%m-%d"), y = Avg_amount),color="black",fill="magenta") + 
  labs(y="Energia Promedio Generada ", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  ggtitle("Promedio de energia creada con fuentes combustibles por año")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_value <-power_value_df %>%  
  ggplot() +
  geom_point(aes(x = Avg_amount, y = Avg_Value),color="blue") + 
  labs(y="Valor de cripto promedio", x="Energia Consumida")+
  ggtitle("Promedio de energia creada vs Valor de cripto")+
  stat_smooth(aes(x = Avg_amount, y = Avg_Value),method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_volume <-power_volume_df %>%  
  ggplot() +
  geom_point(aes(x = Avg_amount, y = Volume),color="orange") + 
  labs(y="Volumen de transacciones promedio", x="Energia Consumida")+
  ggtitle("Promedio de energia creada vs Volumen de transacciones")+
  stat_smooth(aes(x = Avg_amount, y = Volume),method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_total_value <-power_total_value_df %>%  
  ggplot() +
  geom_point(aes(x = Avg_amount, y = Avg_Value),color="dark green") + 
  labs(y="Valor de cripto promedio", x="Energia Consumida total")+
  ggtitle("Promedio de energia creada total vs Valor de cripto")+
  stat_smooth(aes(x = Avg_amount, y = Avg_Value),method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_total_volume <-power_total_volume_df %>%  
  ggplot() +
  geom_point(aes(x = Avg_amount, y = Volume),color="dark blue") + 
  labs(y="Volumen de transacciones promedio", x="Energia Consumida total")+
  ggtitle("Promedio de energia creada total vs Volumen de transacciones")+
  stat_smooth(aes(x = Avg_amount, y = Volume),method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

#Correlation---------------------------------------------------------
cor(power_value_df$Avg_Value,power_value_df$Avg_amount)
cor(power_volume_df$Volume,power_volume_df$Avg_amount)

cor(power_total_value_df$Avg_Value,power_total_value_df$Avg_amount)
cor(power_total_volume_df$Volume,power_total_volume_df$Avg_amount)

# Check  ----------------------------------------------------------

# value
# power
# volumen
# power_value
# power_volume
