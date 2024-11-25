library(readxl)

num <- seq(1, 37)
a <- c(0.46, 0.52, 0.86, 0.91, 0.54, 
       0.64, 0.67, 0.74, 0.69, 0.61, 
       0.65, 0.53, 0.76,
       0.41, 0.33, 0.41, 0.28, 0.39, 
       0.36, 0.40, 0.67, 0.39, 0.33, 
       0.56, 0.61, 0.18, 0.59, 0.23,
       0.09, 0.22, 0.27, 0.40, 0.27,
       0.06, 0.30, 0.36, 0.42)
b <- c("Северный", "Северный", "Северный", 
       "Северный", "Северный", "Северный", 
       "Северный", "Северный", "Северный", 
       "Северный", "Северный", "Северный", "Северный",
       "Средняя полоса", "Средняя полоса", "Средняя полоса",
       "Средняя полоса", "Средняя полоса", "Средняя полоса",
       "Средняя полоса", "Средняя полоса", "Средняя полоса",
       "Средняя полоса", "Средняя полоса", "Средняя полоса",
       "Средняя полоса", "Средняя полоса", "Средняя полоса",
       "Южный", "Южный", "Южный",
       "Южный", "Южный", "Южный",
       "Южный", "Южный", "Южный")

b_factor <- factor(b)

my_frame <- data.frame(num = num, measurement = a, climate_region = b_factor)

print(my_frame)
print(dim(my_frame))        # Размерность таблицы
print(str(my_frame))        # Структура таблицы
print(names(my_frame))      # Названия признаков
head(my_frame, 4)           # Первые 4 строки таблицы

subset(my_frame, measurement > 0.2 & measurement < 0.7)
subset(my_frame, climate_region == "Северный")
my_frame[c(1, 3, 6, 9, 10), ]
my_frame[my_frame$measurement == min(my_frame$measurement), ]

new_rows <- data.frame(num = c(16, 17, 18), 
                       measurement = c(0.5, 0.4, 0.6), 
                       climate_region = factor(c("Северный", "Средняя полоса", "Южный")))
my_frame <- rbind(my_frame, new_rows)


mean_val <- mean(my_frame$measurement)
sd_val <- sd(my_frame$measurement)

write.table(data.frame(mean = mean_val, sd = sd_val), "mean_sd.txt", row.names = FALSE)

my_frame$New <- rnorm(n = nrow(my_frame), mean = mean_val, sd = sd_val)
print(my_frame)

flat <- read_excel("Flat98.xlsx")

write.table(head(flat, 5), "flat_head.txt", row.names = FALSE)

f <- function(vec) {
  return(sort(vec, decreasing = TRUE)[1:5])
}

most_expensive <- flat[order(flat$price, decreasing = TRUE)[1:5], ]
write.table(most_expensive, "most_expensive.txt", row.names = FALSE)

two_room_flats <- flat[flat$rooms == 2, ]
largest_livsp <- two_room_flats[order(two_room_flats$livsp, decreasing = TRUE)[1:5], ]
write.table(largest_livsp, "largest_livsp_2rooms.txt", row.names = FALSE)


