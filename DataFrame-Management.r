# Library init ----------------------------------------------------------

library(gridExtra)
library(tidyverse)
library(ggplot2)
library(scales)
library(readxl)
library(openxlsx)
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
BTC_Footprints_v1 <- read_excel("Dataframes/BTC_Footprints_v1.xlsx")
owid_co2_data <- read_excel("Dataframes/owid-co2-data.xlsx")


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


##DIOS MIO QUE DOLOR DE CABEZA LIMPIAR ESTO 

power_bt_df <- BTC_Footprints_v1

##Algunas de las fechas del dataframe se transforman en numeros porque excel es especial
##Si todas se transformaran no seria un problema pero en el df original las fechas del 1 al 12 estan en
##otro formato (ni idea), Asi que las separo en las fechas normales y las numero
power_bt_df_g <- dplyr::filter(power_bt_df, grepl('/', Date))
power_bt_df_w <- dplyr::filter(power_bt_df, !grepl('/', Date))

##Le digo a R que deje de tratar esas fechas como fechas y que las vuelva numero
power_bt_df_w$Date <- as.numeric(power_bt_df_w$Date)
##Ahora los vulevo fechas en base a la fecha de excel
power_bt_df_w$Date <- as.Date(power_bt_df_w$Date,origin = "1899-12-30")


##Estas ya estaban bien, solo las vuelvo year month
power_bt_df_g <- power_bt_df_g %>% mutate(Date=as.yearmon(Date,"%m/%d/%Y"))

##Las vuelvo char para que R se olvide del formato original en la que las tenia
##Las vuelvo fecha otra vez pero con distinto formato
##Las vuelvo yearmon (No se porque funciona, pero no funciono los otros metodos que busque)
power_bt_df_w <- power_bt_df_w %>%
  mutate(Date = Date %>% 
           str_replace_all("-", "/"))
power_bt_df_w <- power_bt_df_w %>% mutate(Date=as.character(Date))
power_bt_df_w <- power_bt_df_w %>% mutate(Date=as.Date(Date,"%Y/%d/%m"))
power_bt_df_w <- power_bt_df_w %>% mutate(Date=as.yearmon(Date))

##Las fechas raras (Weird) + las fechas buenas (Good) = dataframe original
power_bt_df <- rbind(power_bt_df_g,power_bt_df_w)


##Le quito las lineas en blanco, le doy nombre a las columnas, quito aquellas cosas que no sean paises
##formateo la fecha para que sea yearmon
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

power_bt_df_summary <- power_bt_df %>% 
  group_by(Date) %>%
  summarize(Avg_Energy = (mean(BTCENEGUE)))

value_df_summary <- coins %>% 
  group_by(Date) %>%
  summarize(Avg_Value = (mean(High)+mean(Low))/2)

volumen_df_summary <- coins %>% 
  group_by(Date)  %>%
  summarize(Volume = mean(Volume))

power_bt_power_total_df <- left_join(power_df_total,power_bt_df_summary,by=c("Time"="Date")) 

power_value_df <- left_join(value_df_summary,power_df,by=c("Date"="Time"))

power_volume_df <- left_join(volumen_df_summary,power_df,by=c("Date"="Time"))

power_total_value_df <- left_join(value_df_summary,power_df_total,by=c("Date"="Time"))

power_total_volume_df <- left_join(volumen_df_summary,power_df_total,by=c("Date"="Time"))
# Summary  ----------------------------------------------------------

summary(COINS_ALL_DF)
nrow(COINS_ALL_DF)
ncol(COINS_ALL_DF)

summary(power_df)
nrow(power_df)
ncol(power_df)

# Visualization  ----------------------------------------------------------

value <- value_df_summary %>% 
  ggplot() +
  geom_line(aes(x = as.Date(Date,format = "%y-%m-%d"), y = Avg_Value),color="red")+ 
  labs(y="Valor Promedio de las criptomonedas(USD)", x="Años")+
  ggtitle("Promedio del valor de las crypto monedas por año ")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

volumen <- volumen_df_summary %>%  
  ggplot() +
  geom_line(aes(x = as.Date(Date,format = "%y-%m-%d"), y = Volume),color="green") + 
  labs(y="Volumen Promedio de transacciones", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  ggtitle("Promedio de transacciones por año")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power <- power_df %>%  
  ggplot() +
  geom_line(aes(x = as.Date(Time,format = "%y-%m-%d"), y = Avg_amount),color="yellow") + 
  labs(y="Energia Generada por Combustibles", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  ggtitle("Energia creada con fuentes combustibles por año")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_bt <- power_bt_df_summary %>%  
  ggplot() +
  geom_line(aes(x = as.Date(Date,format = "%y-%m-%d"), y = Avg_Energy),color="purple") + 
  labs(y="Energia Consumida por BT", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  ggtitle("Energia consumida por BT")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_total <- power_df_total %>%  
  ggplot() +
  geom_line(aes(x = as.Date(Time,format = "%y-%m-%d"), y = Avg_amount),color="magenta") + 
  labs(y="Energia  Generada ", x="Años")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  ggtitle("Energia creada por año")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_value <-power_value_df %>%  
  ggplot() +
  geom_point(aes(x = Avg_amount, y = Avg_Value),color="blue") + 
  labs(y="Valor de cripto promedio", x="Energia Consumida")+
  ggtitle("Energia creada por combustibles vs Valor de cripto promedio")+
  stat_smooth(aes(x = Avg_amount, y = Avg_Value),method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_volume <-power_volume_df %>%  
  ggplot() +
  geom_point(aes(x = Avg_amount, y = Volume),color="orange") + 
  labs(y="Volumen de transacciones promedio", x="Energia Creada por combustibles")+
  ggtitle("Energia creada por combustibles vs Volumen promedio de transacciones")+
  stat_smooth(aes(x = Avg_amount, y = Volume),method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_total_value <-power_total_value_df %>%  
  ggplot() +
  geom_point(aes(x = Avg_amount, y = Avg_Value),color="dark green") + 
  labs(y="Valor de cripto promedio", x="Energia Consumida total")+
  ggtitle("Energia creada total vs Valor de cripto")+
  scale_y_continuous(trans='log2')+
  stat_smooth(aes(x = Avg_amount, y = Avg_Value),method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_total_volume <-power_total_volume_df %>%  
  ggplot() +
  geom_point(aes(x = Avg_amount, y = Volume),color="dark blue") + 
  labs(y="Volumen de transacciones promedio (Log2)", x="Energia Consumida total")+
  ggtitle("Energia creada total vs Volumen de transacciones")+
  scale_y_continuous(trans='log2')+
  stat_smooth(aes(x = Avg_amount, y = Volume),method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))

power_bt_power <- power_bt_power_total_df  %>%  
  ggplot() +
  labs(y="Energia Creada y Consumida (log2)", x="Años")+
  ggtitle("Energia creada por paises de la IEA  vs Energia Requerida por la cripto")+
  geom_line(aes(x = as.Date(Time,format = "%y-%m-%d"), y = Avg_amount,color="Poder calculado")) +
  geom_line(aes(x = as.Date(Time,format = "%y-%m-%d"), y = Avg_Energy,color="Poder Requerido")) +
  scale_y_continuous(trans='log2')+
  scale_color_manual(name="Fuente de poder",breaks=c('Poder calculado','Poder Requerido'),values = c('Poder calculado'='orange','Poder Requerido'='blue'))+
  theme(axis.title=element_text(size=10,face="bold"),axis.text.x = element_text(size = 8,angle = 60))
  

#Correlation---------------------------------------------------------

cor(power_value_df$Avg_Value,power_value_df$Avg_amount)
cor(power_volume_df$Volume,power_volume_df$Avg_amount)

cor(power_total_value_df$Avg_Value,power_total_value_df$Avg_amount)
cor(power_total_volume_df$Volume,power_total_volume_df$Avg_amount)


