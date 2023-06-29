# Librarys
library(openxlsx)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(gridExtra)

# Import Dataframe ACTIVE PRODUCT ID
active_product_id <- data.frame(read.xlsx("ACTIVE_PRODUCT_ID.xlsx", sheet = 2))

# Import Dataframe MASTER CIENT
master_client <- data.frame(read.xlsx("MASTER_CLIENT.xlsx", sheet = 2))

# Import Datafarme MASTER PRODUCT
master_product <- data.frame(read.xlsx("MASTER_PRODUCT.xlsx", sheet = 2))

# Import Dataframe ACTUALS
actuals <- data.frame(read.csv("ACTUALS.csv"))

# Import Dataframe FORECAST
forecasts <- data.frame(read_csv("FORECASTS.csv")) 
forecasts <- subset(forecasts, FINAL_SO != 0)

"
===========================================================================
ANALYSING THE VARIABLES TO SEE IF THE ACCURACY DECREASES IN THE LAST YEARS
===========================================================================
"

# Changing the column name CNPJ to CNPJ ACUTALS and CNPJ FORECASTS
# Changing the column name SKU_ACTIVO to PRODUCT_ID
colnames(actuals) <- c("YEAR", "MONTH", "BCODE", "CNPJ", "HOLDING", "SI_TON")
colnames(forecasts) <- c("PRODUCT_ID", "CNPJ", "PERIOD", "FINAL_SO")

# Transforming the data of columns
forecasts$YEAR <- as.numeric(substr(forecasts$PERIOD, 1, 4))
forecasts$MONTH <- as.numeric(substr(forecasts$PERIOD, 5, 6))

# Analysing the quantity of PRODUCT_ID in MASTER_PRODUCT CHOCOLATE
length(unique(master_product$PRODUCT_ID))

# Using only the 350 PRODUCT_ID from MASTER_PRODUCT CHOCOLATE
# Creating a new data frame for ACTUALS, by filtering the PRODUCT_ID CHOCOLATE, by mergning with ACTIVE_PRODUCT_ID AND MASTER_PRODUCT
new_actuals <- merge((merge(actuals, active_product_id, by = "BCODE", all.x = TRUE)), master_product, by = "PRODUCT_ID", all.x = TRUE) %>% filter(PRODUCT_ID %in% master_product$PRODUCT_ID)

# Creating a new data frame for FORECASTS, by filtering and merging with the PRODUCT_ID CHOCOLATE from MASTER_PRODUCT (4.7M down to 1.3M)
new_forecasts <- merge(forecasts, master_product, by = "PRODUCT_ID", all.x = TRUE) %>% filter(PRODUCT_ID %in% master_product$PRODUCT_ID)

# Biding the NEW_ACTULAS and NEW_FORECASTS dataframes, based only in the PRODUCT_ID CHOCOLATE
merge <- bind_rows(new_actuals, new_forecasts) %>% select(-PERIOD, -MONTH, -HOLDING)
merge <- replace(merge, is.na(merge), 0)

# Formulating the variable SFA
si_ton_final_so_2020 <- merge %>% filter(YEAR == 2020) %>% summarize(sum(SI_TON), sum(FINAL_SO))
si_ton_final_so_2021 <- merge %>% filter(YEAR == 2021) %>% summarize(sum(SI_TON), sum(FINAL_SO))
si_ton_final_so_2022 <- merge %>% filter(YEAR == 2022) %>% summarize(sum(SI_TON), sum(FINAL_SO))
si_ton_final_so_2023 <- merge %>% filter(YEAR == 2023) %>% summarize(sum(SI_TON), sum(FINAL_SO))

sfa_2020 <- 1 - ((abs(si_ton_final_so_2020[1] - si_ton_final_so_2020[2])) / si_ton_final_so_2020[1])
sfa_2021 <- 1 - ((abs(si_ton_final_so_2021[1] - si_ton_final_so_2021[2])) / si_ton_final_so_2021[1])
sfa_2022 <- 1 - ((abs(si_ton_final_so_2022[1] - si_ton_final_so_2022[2])) / si_ton_final_so_2022[1])
sfa_2023 <- 1 - ((abs(si_ton_final_so_2023[1] - si_ton_final_so_2023[2])) / si_ton_final_so_2023[1])

print(c(sfa_2020, sfa_2021, sfa_2022, sfa_2023))

"
=========================================================================================
ANALYSING THE VARIABLES TO SEE IF THE ACCURACY BEHAVIOR CHANGES BASED ON FLAVOR AND SIZE
=========================================================================================
"

# Filtering the SI_TON and FINAL_SO for columns SIZES and FLAVORS
# Formulating the variable SFA
sfa_small <- 1 - ((abs((merge %>% filter(SIZE == "SMALL") %>% summarize(sum(SI_TON))) - (merge %>% filter(SIZE == "SMALL") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(SIZE == "SMALL") %>% summarize(sum(SI_TON))))
sfa_medium <- 1 - ((abs((merge %>% filter(SIZE == "medium") %>% summarize(sum(SI_TON))) - (merge %>% filter(SIZE == "medium") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(SIZE == "medium") %>% summarize(sum(SI_TON))))
sfa_large <- 1 - ((abs((merge %>% filter(SIZE == "large") %>% summarize(sum(SI_TON))) - (merge %>% filter(SIZE == "large") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(SIZE == "large") %>% summarize(sum(SI_TON))))
sfa_regular <- 1 - ((abs((merge %>% filter(FLAVOR == "regular") %>% summarize(sum(SI_TON))) - (merge %>% filter(FLAVOR == "regular") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(FLAVOR == "regular") %>% summarize(sum(SI_TON))))
sfa_candy <- 1 - ((abs((merge %>% filter(FLAVOR == "candy") %>% summarize(sum(SI_TON))) - (merge %>% filter(FLAVOR == "candy") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(FLAVOR == "candy") %>% summarize(sum(SI_TON))))
sfa_milk <- 1 - ((abs((merge %>% filter(FLAVOR == "milk") %>% summarize(sum(SI_TON))) - (merge %>% filter(FLAVOR == "milk") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(FLAVOR == "milk") %>% summarize(sum(SI_TON))))
sfa_darkchocolate <- 1 - ((abs((merge %>% filter(FLAVOR == "dark chocolate") %>% summarize(sum(SI_TON))) - (merge %>% filter(FLAVOR == "dark chocolate") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(FLAVOR == "dark chocolate") %>% summarize(sum(SI_TON))))
sfa_withmilk <- 1 - ((abs((merge %>% filter(FLAVOR == "WITH MILK") %>% summarize(sum(SI_TON))) - (merge %>% filter(FLAVOR == "WITH MILK") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(FLAVOR == "WITH MILK") %>% summarize(sum(SI_TON))))
sfa_strawberry <- 1 - ((abs((merge %>% filter(FLAVOR == "StrawberrY") %>% summarize(sum(SI_TON))) - (merge %>% filter(FLAVOR == "StrawberrY") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(FLAVOR == "StrawberrY") %>% summarize(sum(SI_TON))))
sfa_other <- 1 - ((abs((merge %>% filter(FLAVOR == "Other") %>% summarize(sum(SI_TON))) - (merge %>% filter(FLAVOR == "Other") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(FLAVOR == "Other") %>% summarize(sum(SI_TON))))
sfa_almond  <- 1 - ((abs((merge %>% filter(FLAVOR == "Almond") %>% summarize(sum(SI_TON)))  - (merge %>% filter(FLAVOR == "Almond") %>% summarize(sum(FINAL_SO))))) / (merge %>% filter(FLAVOR == "Almond") %>% summarize(sum(SI_TON))))

print(c(sfa_small, sfa_medium, sfa_large))
print(c(sfa_regular, sfa_candy, sfa_milk, sfa_darkchocolate, sfa_withmilk, sfa_strawberry, sfa_other, sfa_almond))


"
====================================
EXPORTING AND PLOTTING ALL THE DATA
====================================
"

# Exporting the merge dataframe as .csv file (PowerBI)
write.table(merge, file = "merge.csv", sep = ",", row.names = FALSE)

# Plots
plot1 <- ggplot(new_actuals, aes(x = YEAR, y = SI_TON)) + geom_bar(stat = "identity")
plot2 <- ggplot(new_forecasts, aes(x = YEAR, y = FINAL_SO)) + geom_bar(stat = "identity")
plot3 <- grid.arrange(plot1, plot2, nrow = 1)
plot4 <- ggplot() + geom_bar(data = new_actuals, aes(x = YEAR, y = SI_TON, fill = "SI_TON"), stat = "identity", position = "stack") +
geom_bar(data = new_forecasts, aes(x = YEAR, y = FINAL_SO, fill = "FINAL_SO"), stat = "identity") +
scale_fill_manual(values = c("SI_TON" = "red", "FINAL_SO" = "blue"))
print(plot4)

