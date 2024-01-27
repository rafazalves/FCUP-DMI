library(tidyr)
library(dplyr)
library(pROC)
library(randomForest)

card <- read.csv("card_comp.csv", sep = ";")
loan <- read.csv("loan_comp.csv", sep = ";")
trans <- read.csv("trans_comp.csv", sep = ";")
account <- read.csv("account.csv", sep = ";")
card_dev <- read.csv("card_dev.csv", sep = ";")
client <- read.csv("client.csv", sep = ";")
disp <- read.csv("disp.csv", sep = ";")
district <- read.csv("district.csv", sep = ";")
loan_dev <- read.csv("loan_dev.csv", sep = ";")
trans_dev <- read.csv("trans_dev.csv", sep = ";")

filtered_disp <- subset(disp, type == "OWNER")

kaggle <- loan %>% inner_join(filtered_disp, by = "account_id") %>% inner_join(client, by = "client_id") %>% select(loan_id, client_id, birth_number, date, amount, duration, payments, district_id)
kaggle <- kaggle %>% distinct(loan_id, .keep_all = TRUE)

#Calcular idade do cliente ao pedir o loan
kaggle$client_age <- as.integer(substr(kaggle$date, 1, 2)) - as.integer(substr(kaggle$birth_number, 1, 2))
condition <- as.integer(substr(kaggle$date, 3, 4)) < as.integer(substr(kaggle$birth_number, 3, 4)) | (as.integer(substr(kaggle$date, 3, 4)) > as.integer(substr(kaggle$birth_number, 3, 4))) & (as.integer(substr(kaggle$date, 5, 6)) < as.integer(substr(kaggle$birth_number, 5, 6)))
kaggle$client_age <- ifelse(condition, kaggle$client_age - 1, kaggle$client_age)
kaggle$client_age <- as.integer(kaggle$client_age)

#Adicionar o AverageSalary e TaxaDesemprego e calcular qual usar
kaggle <- kaggle %>% inner_join(district, by = c("district_id" = "code")) %>% select(loan_id, client_id, client_age, date, amount, duration, payments, district_id, average.salary, unemploymant.rate..95, unemploymant.rate..96)
kaggle <- kaggle %>% mutate( unemploymant.rate = ifelse(substr(as.character(date), 1, 2) <= "95", unemploymant.rate..95, unemploymant.rate..96) )
#Remover date porque já não é necessário
kaggle <- subset(kaggle, select = -c(unemploymant.rate..95, unemploymant.rate..96))

disp_filtered <- disp[disp$client_id %in% kaggle$client_id, ]
disp_filtered <- subset(disp_filtered, select = -type)
disp_filtered <- merge(disp_filtered, kaggle[, c("client_id", "date")], by = "client_id", all.x = TRUE)
disp_filtered <- subset(disp_filtered, select = -disp_id)

filtered_trans <- trans %>% semi_join(disp_filtered, by = c("account_id")) %>% filter(date < disp_filtered$date)
filtered_trans <- filtered_trans %>% group_by(account_id) %>% filter(date == max(date))
filtered_trans <- filtered_trans %>% group_by(account_id) %>% arrange(desc(type == "withdrawal")) %>% slice(1)
filtered_trans <- filtered_trans %>% select(-trans_id, -date, -type, -amount, -account)

disp_filtered <- left_join(disp_filtered, filtered_trans, by = "account_id")
disp_filtered <- disp_filtered %>% select(-account_id, -date)

kaggle <- left_join(kaggle, disp_filtered, by = "client_id")
kaggle <- kaggle %>% select(-date, -district_id)

kaggle <- subset(kaggle, select = -c(operation, k_symbol, bank, status))

