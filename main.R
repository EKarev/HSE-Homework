library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)
library(cowplot)
library(lubridate)
library(stringr)
library(gridExtra)

data <- read.csv("dataset.csv")
colnames(data) <- gsub("\\.", "_", colnames(data))

data$Gender <- as.factor(data$Gender)
data$City <- as.factor(data$City)
data$Profession <- as.factor(data$Profession)
data$Degree <- as.factor(data$Degree)
data$Dietary_Habits <- as.factor(data$Dietary_Habits)
data$Have_you_ever_had_suicidal_thoughts <- as.factor(data$Have_you_ever_had_suicidal_thoughts)
data$Sleep_Duration <- factor(data$Sleep_Duration, levels = c("Less than 5 hours", "5-6 hours", "7-8 hours"))

data_long <- data %>%
  gather(key = "variable", value = "value", -id, -Gender, -Age, -City, -Profession, -Degree, -Dietary_Habits, -Have_you_ever_had_suicidal_thoughts, -Sleep_Duration)

p1 <- ggplot(data, aes(x = Age, y = CGPA, color = Gender, size = Academic_Pressure)) +
  geom_point(alpha = 0.8) +
  scale_color_brewer(palette = "Set3") +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "Точки с разными размерами и цветами", x = "Возраст", y = "CGPA", color = "Пол") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  )

p2 <- ggplot(data, aes(x = CGPA, y = Academic_Pressure)) +
  geom_line(aes(color = Profession), size = 1.2) +
  geom_point(aes(shape = Have_you_ever_had_suicidal_thoughts, size = Work_Pressure), color = "black", alpha = 0.6) +
  labs(title = "Линии с шумом и точками", x = "CGPA", y = "Академическое давление", color = "Профессия") +
  theme_light() +
  scale_size_continuous(range = c(4, 7)) +
  scale_shape_manual(values = c(16, 17)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "italic", size = 16)
  )

p3 <- ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Сложные бары по категории", x = "Переменная", y = "Значение", fill = "Переменная") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    panel.grid.major = element_line(color = "lightgrey", linetype = "dotted"),
    plot.title = element_text(size = 14, face = "bold")
  )

p4 <- ggplot(data, aes(x = Work_Pressure, y = Financial_Stress)) +
  geom_tile(aes(fill = Depression)) +
  scale_fill_viridis_c() +
  labs(title = "Тепловая карта", x = "Рабочее давление", y = "Финансовый стресс", fill = "Депрессия") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

p5 <- ggplot(data, aes(x = Dietary_Habits, y = Financial_Stress)) +
  geom_boxplot(aes(color = Dietary_Habits), outlier.colour = "red", outlier.size = 3) +
  labs(title = "Коробчатая диаграмма", x = "Пищевые привычки", y = "Финансовый стресс") +
  scale_color_brewer(palette = "Set2") +
  theme_light() +
  theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  )

p6 <- ggplot(data, aes(x = Age, y = Work_Pressure)) +
  geom_point(aes(color = Profession), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  labs(title = "Диаграмма рассеяния с трендом", x = "Возраст", y = "Рабочее давление", color = "Профессия") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

p7 <- ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Гистограмма по возрасту", x = "Возраст", y = "Частота") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_blank()
  )

grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 3)



