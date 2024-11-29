# 1. Чтение данных
data_sample <- as.numeric(read.csv("/home/luckhost/programming/probab_theory_sfu_2nd_course/2nd/var1.csv", header = TRUE)$x)
sample1 <- data_sample[1:10]
sample2 <- data_sample[1:50]
sample3 <- data_sample

# Проверка данных
print(head(sample1))
print(head(sample2))
print(head(sample3))

# Функция для анализа выборки
analyze_sample <- function(sample, name) {
  cat("\nАнализ выборки:", name, "\n")
  
  # Основные численные характеристики
  sample_mean <- mean(sample)
  sample_unbaised_var <- var(sample)
  sample_var <- sample_unbiased_var * (length(sample) - 1) / length(sample)
  sample_sd <- sd(sample)
  sample_quantiles <- quantile(sample, probs = c(0.25, 0.5, 0.75))
  
  cat("Среднее выборочное:", sample_mean, "\n")
  cat("Смещённая выборочная дисперсия:", sample_var, "\n")
  cat("Несмещённая выборочная дисперсия:", sample_unbiased_var, "\n")
  cat("Выборочные квантили:", sample_quantiles, "\n")
  
  # Точечные оценки параметров нормального распределения
  a_hat <- sample_mean
  sigma_hat <- sample_sd
  
  # Построение гистограммы и плотности
  hist(sample, probability = TRUE, breaks = 10, 
       main = paste("Гистограмма и плотность\n", name),
       xlab = "Значения", ylab = "Относительная частота", col = "lightblue", border = "black")
  curve(dnorm(x, mean = a_hat, sd = sigma_hat), 
        col = "red", lwd = 2, add = TRUE)
  legend("topright", legend = c("Гистограмма", "Теоретическая\nплотность"), 
         fill = c("lightblue", NA), border = c("black", NA), lty = c(NA, 1), col = c("black", "red"))
  
  # Построение эмпирической и теоретической функций распределения
  plot(ecdf(sample), 
       main = paste("Эмпирическая и теоретическая функции\nраспределения ", name),
       xlab = "Значения", ylab = "Функция распределения", col = "blue", lwd = 2)
  curve(pnorm(x, mean = a_hat, sd = sigma_hat), 
        col = "red", lwd = 2, add = TRUE)
  legend("bottomright", legend = c("Эмпирическая функция", "Теоретическая функция"), 
         col = c("blue", "red"), lty = 1, lwd = 2)
}

# Анализ выборок
results1 <- analyze_sample(sample1, "Выборка 1-10")
results2 <- analyze_sample(sample2, "Выборка 1-50")
results3 <- analyze_sample(sample3, "Вся выборка")

# 4. Доверительные интервалы
confidence_intervals <- function(sample, q) {
  n <- length(sample)
  mean_sample <- mean(sample)
  sigma_sample <- sqrt(var(sample) * (n / (n - 1)))
  
  t_val <- qt(1 - (1 - q) / 2, df = n - 1)
  mean_ci <- c(mean_sample - t_val * sigma_sample / sqrt(n), 
               mean_sample + t_val * sigma_sample / sqrt(n))
  
  chi2_low <- qchisq((1 - q) / 2, df = n - 1)
  chi2_high <- qchisq(1 - (1 - q) / 2, df = n - 1)
  variance_ci <- c((n - 1) * var(sample) / chi2_high, 
                   (n - 1) * var(sample) / chi2_low)
  
  return(list(mean_ci = mean_ci, variance_ci = variance_ci))
}

# Пример расчета для выборки 1
ci_sample1 <- confidence_intervals(sample1, 0.95)
cat("\nДоверительный интервал для матожидания (q=0.95): ", ci_sample1$mean_ci, "\n")
cat("Доверительный интервал для дисперсии (q=0.95): ", ci_sample1$variance_ci, "\n")

# 6 и 7. Графики изменения длины доверительного интервала
plot_confidence_intervals <- function(sample, param = "mean", name) {
  n <- length(sample)
  q_vals <- seq(0.9, 0.99999, length.out = 100)
  interval_lengths <- numeric(length(q_vals))
  
  for (i in seq_along(q_vals)) {
    ci <- confidence_intervals(sample, q_vals[i])
    if (param == "mean") {
      interval_lengths[i] <- diff(ci$mean_ci)
    } else {
      interval_lengths[i] <- diff(ci$variance_ci)
    }
  }
  
  plot(q_vals, interval_lengths, type = "l", col = "blue", lwd = 2,
       main = paste("Длина доверительного интервала\n", param, name),
       xlab = "Уровень доверия q", ylab = "Длина интервала")
}

# Построение графиков
par(mfrow = c(3, 2))
plot_confidence_intervals(sample1, "mean", "Выборка 1-10")
plot_confidence_intervals(sample1, "variance", "Выборка 1-10")
plot_confidence_intervals(sample2, "mean", "Выборка 1-50")
plot_confidence_intervals(sample2, "variance", "Выборка 1-50")
plot_confidence_intervals(sample3, "mean", "Вся выборка")
plot_confidence_intervals(sample3, "variance", "Вся выборка")

