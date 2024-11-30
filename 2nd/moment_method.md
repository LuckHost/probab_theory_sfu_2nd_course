Метод моментов для нормального распределения позволяет оценить неизвестные параметры распределения случайной величины в математической статистике. 2

Суть метода заключается в следующем: 2

К реальным данным распределения подбирают теоретическое распределение (функцию плотности вероятностей). 2
Чтобы найти неизвестные параметры, считают числовые характеристики (моменты) теоретического распределения, то есть математическое ожидание, дисперсию. 2
Затем сопоставляют посчитанные параметры по реальным данным с теоретическими параметрами для конкретного распределения. 2
В итоге получают систему уравнений относительно неизвестных параметров распределения. 2
Решая систему, находят эти параметры. 2
Для нормального закона N (m, s) удобно взять первый начальный и второй центральный моменты. Для этого случая получается система из двух уравнений, из которой находятся оценки двух параметров по методу моментов: m$ = x; s$ = s.

Рассмотрим пример оценки параметров для распределения Пуассона \( P(\lambda) \) с помощью метода моментов.  

### Задача
Предположим, у нас есть выборка объёма \( n = 5 \):  
\[
x = \{2, 3, 1, 4, 2\}
\]  
Нужно оценить параметр \( \lambda \), который является математическим ожиданием распределения Пуассона.

---

### Шаги метода моментов

1. **Теоретический момент**:  
   Для распределения Пуассона математическое ожидание \( \mathbb{E}[X] \) равно \( \lambda \).  
   Значит, теоретический момент первого порядка:  
   \[
   \mu_1' = \lambda
   \]

2. **Выборочный момент**:  
   Вычисляем выборочное среднее:  
   \[
   m_1' = \frac{1}{n} \sum_{i=1}^n x_i = \frac{2 + 3 + 1 + 4 + 2}{5} = \frac{12}{5} = 2.4
   \]

3. **Приравнивание моментов**:  
   Приравниваем теоретический и выборочный моменты:  
   \[
   \lambda = m_1'
   \]  
   Таким образом, оценка параметра \( \lambda \) равна:  
   \[
   \hat{\lambda} = 2.4
   \]

---

### Итог
Согласно методу моментов, оценка параметра \( \lambda \) для распределения Пуассона по данной выборке равна \( \hat{\lambda} = 2.4 \).

Этот пример демонстрирует, насколько просто использовать метод моментов: достаточно найти выборочное среднее и сопоставить его с теоретическим моментом.