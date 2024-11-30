# 1. Чтение данных
data_sample <- as.numeric(read.csv("/home/luckhost/programming/probab_theory_sfu_2nd_course/2nd/var1.csv", header = TRUE)$x)
sample1 <- sort(data_sample[1:10])
sample2 <- sort(data_sample[1:50])
sample3 <- sort(data_sample)

# Функция для анализа выборки
analyze_sample <- function(sample, name) {
  cat("\nАнализ выборки:", name, "\n")
  
  # Основные численные характеристики
  sample_mean <- mean(sample) # ср выборочное 
  sample_unbiased_var <- var(sample)
  sample_var <- sample_unbiased_var * (length(sample) - 1) / length(sample)
  sample_sd <- sd(sample) # ср квадратичное отклонение
  sample_quantiles <- quantile(sample, probs = c(0.25, 0.5, 0.75))
  
  cat("Среднее выборочное:", sample_mean, "\n")
  cat("Смещённая выборочная дисперсия:", sample_var, "\n")
  cat("Несмещённая выборочная дисперсия:", sample_unbiased_var, "\n")
  cat("Выборочные квантили:", sample_quantiles, "\n")
  
  # Точечные оценки параметров нормального распределения
  a <- sample_mean
  sigma_2 <- sample_unbiased_var
  
  # Среднеквадратическое отклонение.
  sigma <- sample_sd
  cat("Точечные оценки. ", name, "\n")
  cat("Оценка среднего: ", a, "\n") 
  cat("Оценка дисперсии: ", sigma_2, "\n")
  
  len <- seq(sample_mean - 3*sigma, sample_mean + 3*sigma, length.out=200)
  normal_dist <- dnorm(len, mean=a, sd=sigma) # Значение плотности 
  
  
  hist(
    sample,
    freq = FALSE, # Гистограмма относительных частот
    breaks = "Sturges", # Алгоритм вычисления
    main = paste("Гистограмма относительных частот\n", name),
    xlab = "Значения",  
    ylab = "h_i",
    col = "#2fa0d7",
    border = "#070C21",
    ylim=c(0, 1.4*max(normal_dist)),
    xlim=range(len)
  )
  
  # Кривая нормального распределения
  curve(
    dnorm(x, mean = a, sd = sigma), 
    col = "#51c8b1",
    lwd = 2, 
    add = TRUE
  )
  
  legend(
    "topright", 
    legend = c("Гистограмма", "Теоретическая\nплотность"), 
    fill = c("#2fa0d7", NA), 
    border = c("black", NA), 
    lty = c(NA, 1), 
    col = c("black", "#51c8b1")
  )
  
  # Построение эмпирической и теоретической функций распределения
  plot(
    ecdf(sample), 
    main = paste("Эмпирическая и теоретическая функции\nраспределения ", name),
    xlab = "Значения", 
    ylab = "Функция распределения", 
    col = "#2fa0d7", 
    lwd = 2
  )
  
  curve(
    pnorm(x, mean = a, sd = sigma),
    col = "#50c8b1", 
    lwd = 2, 
    add = TRUE
  )
  legend(
    "bottomright", 
    legend = c("Эмпирическая функция", "Теоретическая функция"), 
    col = c("#2fa0d7", "#50c8b1"), 
    lty = 1, lwd = 2
  )
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

# Расчет для выборок
ci_sample1 <- confidence_intervals(sample1, 0.95)
cat("\nДов. интервал для матожидания (q=0.95): ", ci_sample1$mean_ci, "\n")
cat("Дов. интервал для дисперсии (q=0.95): ", ci_sample1$variance_ci, "\n")

ci_sample2 <- confidence_intervals(sample2, 0.95)
cat("\nДов. интервал для матожидания (q=0.95): ", ci_sample2$mean_ci, "\n")
cat("Дов. интервал для дисперсии (q=0.95): ", ci_sample2$variance_ci, "\n")

ci_sample3 <- confidence_intervals(sample3, 0.95)
cat("\nДов. интервал для матожидания (q=0.95): ", ci_samp3e1$mean_ci, "\n")
cat("Дов. интервал для дисперсии (q=0.95): ", ci_sample3$variance_ci, "\n")


# Функция для вычисления и построения длины доверительных интервалов
plot_combined_confidence_intervals <- function(samples, param = "mean", names) {
  q_vals <- seq(0.9, 0.99999, length.out = 100)
  
  # Установка цветов для выборок
  colors <- c("#2fa0d7", "#f04f39", "#4caf50")
  
  # Создание пустого графика
  plot(NULL, xlim = range(q_vals), ylim = c(0, max(sapply(samples, function(sample) {
    n <- length(sample)
    max_lengths <- sapply(q_vals, function(q) {
      ci <- confidence_intervals(sample, q)
      if (param == "mean") diff(ci$mean_ci) else diff(ci$variance_ci)
    })
    max(max_lengths)
  }))),
  type = "n", xlab = "Уровень доверия q", 
  ylab = "Длина доверительного интервала", 
  main = ifelse(param == "mean", 
                "Длина доверительных интервалов для среднего (a)", 
                "Длина доверительных интервалов для дисперсии (σ²)"))
  
  # Добавление графиков для каждой выборки
  for (i in seq_along(samples)) {
    sample <- samples[[i]]
    interval_lengths <- sapply(q_vals, function(q) {
      ci <- confidence_intervals(sample, q)
      if (param == "mean") diff(ci$mean_ci) else diff(ci$variance_ci)
    })
    lines(q_vals, interval_lengths, col = colors[i], lwd = 2, type = "l")
  }
  
  # Добавление легенды
  legend("topleft", legend = names, col = colors, lty = 1, lwd = 2)
}

# Построение двух графиков: для среднего и дисперсии
samples <- list(sample1, sample2, sample3)
names <- c("Выборка 1-10", "Выборка 1-50", "Вся выборка")

# Для среднего (a)
plot_combined_confidence_intervals(samples, param = "mean", names)

# Для дисперсии (σ²)
plot_combined_confidence_intervals(samples, param = "variance", names)


