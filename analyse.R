# Настройки вывода
options(
  digits = 2,
  width = 100
)

# Загрузка библиотек
library("psych")

# Импорт данных
dt <- read.table(file="artra.csv", sep=";", dec=",", header=TRUE)
dt$Сопутс.патология[dt$Сопутс.патология == ""] <- NA

# Описателдьные статистики
desc.all <- describe(dt[c(-2, -3, -5, -13, -24)], skew=FALSE)[c(-1, -5)]
desc.groups <- describe.by(dt[c(-2, -3, -5, -13, -24)], dt$Группа, skew=FALSE)
# 
# Проверка соответствия нормальному закону распределения
norm.stats <- t(sapply(dt[7:12], shapiro.test))[, 1:2]

# Проверка гомогенности групп до начала исследований
homo.groups <- t(sapply(dt[7:12], wilcox.test))[, c(1, 3)]

# Подготовка данных для ANOVA с повторными измерениями
# Выбриаем только нужные переменные
wdt <- dt[, c(
  "Группа",
  "РСЩ_1", "РСЩ_3",
  "УЗ.хрящ_1", "УЗЩ_3",
  "ВАШ_1", "ВАШ_3",
  "WOMAC_1", "WOMAC_3",
  "HAQ_1", "HAQ_3",
  "Индекс.Лекена_1", "Индекс.Лекена_3"
)]
# Преобразуем в длинный формат
ldt <- reshape(wdt, varying=c(list(2:3), list(4:5), list(6:7), list(8:9), list(10:11), list(12:13)), direction="long")
colnames(ldt)[2] <- "Замер"