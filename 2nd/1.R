# 1. Чтение данных
data_sample <- read.csv("/home/luckhost/programming/probab_theory_sfu_2nd_course/2nd/var1.csv", header = TRUE)$x # Предполагается, что данные в первом столбце
sample1 <- data_sample[1:10]
sample2 <- data_sample[1:50]
sample3 <- data_sample

# Проверка данных
print(head(sample1))
print(head(sample2))
print(head(sample3))

# Функция для расчетов и построения графиков
analyze_sample <- function(sample, name) {
  library(ggplot2)
  
  # Основные числовые характеристики
  sample_mean <- mean(sample)
  sample_var <- var(sample)
  sample_unbiased_var <- sample_var * (length(sample) / (length(sample) - 1))
  sample_quantiles <- quantile(sample)
  
  cat(paste0("\n", name, ": \n"))
  cat("Среднее выборочное: ", sample_mean, "\n")
  cat("Выборочная дисперсия: ", sample_var, "\n")
  cat("Несмещенная выборочная дисперсия: ", sample_unbiased_var, "\n")
  cat("Квантили: \n")
  print(sample_quantiles)
  
  # Точечные оценки параметров нормального распределения
  a_hat <- sample_mean
  sigma2_hat <- sample_unbiased_var
  sigma_hat <- sqrt(sigma2_hat)
  
  # Построение гистограммы и плотности
  plot <- ggplot(data.frame(sample), aes(x = sample)) +
    geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", alpha = 0.7, color = "black") +
    stat_function(fun = dnorm, args = list(mean = a_hat, sd = sigma_hat), color = "red", size = 1) +
    ggtitle(paste("Гистограмма и плотность для", name)) +
    xlab("Значения") +
    ylab("Относительная частота") +
    theme_minimal()
  
  # Сохранение графика
  ggsave(paste0("hist_density_", name, ".png"), plot = plot, width = 7, height = 7)
  
  
  # Построение эмпирической и теоретической функции распределения
  ecdf_sample <- ecdf(sample)
  x_vals <- seq(min(sample), max(sample), length.out = 100)
  theoretical_cdf <- pnorm(x_vals, mean = a_hat, sd = sigma_hat)
  
  plot(ecdf_sample, main = paste("Эмпирическая и теоретическая функции распределения для", name),
       xlab = "Значения", ylab = "F(x)", col = "blue", lwd = 2)
  lines(x_vals, theoretical_cdf, col = "red", lwd = 2)
  legend("bottomright", legend = c("Эмпирическая", "Теоретическая"), col = c("blue", "red"), lwd = 2)
  dev.copy(png, paste0("cdf_", name, ".png"))
  dev.off()
  
  return(list(mean = sample_mean, var = sigma2_hat, linewidth = length(sample)))
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
       main = paste("Длина доверительного интервала для", param, "(", name, ")"),
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

