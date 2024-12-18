# Анализ данных и визуализация в R

## Описание

Данный проект включает в себя анализ данных и создание различных визуализаций с использованием языка программирования R. Проект выполнен для учебного задания, и включает в себя несколько графиков, которые помогают визуализировать зависимости между различными переменными в наборе данных.

## Структура проекта

1.  **Основной код**: Скрипт на R, который выполняет загрузку, обработку данных и создает визуализации.
2.  **Данные**: Файл `dataset.csv` содержит данные о студентах, включая различные характеристики, такие как возраст, пол, профессия, уровень академического давления, рабочий стресс, депрессия, пищевые привычки, продолжительность сна и наличие суицидальных мыслей. Эти данные были используются для построения графиков.
3.  **`.gitignore`**: Конфигурация для исключения временных файлов и каталогов из репозитория.
4.  **renv.lock** Для работы с проектом используются пакеты R, которые перечислены в этом файле.

## Зависимости

Для работы с проектом необходимо установить следующие библиотеки в R:

-   `ggplot2` — для построения графиков.
-   `dplyr` — для обработки данных.
-   `tidyr` — для преобразования данных.
-   `scales` — для работы с масштабированием осей.
-   `RColorBrewer` — для цветовых палитр.
-   `cowplot` — для комбинирования нескольких графиков.
-   `lubridate` — для работы с датами.
-   `stringr` — для работы с текстовыми данными.
-   `gridExtra` — для размещения нескольких графиков.

Для установки всех зависимостей используйте следующую команду:

`r install.packages(c("ggplot2", "dplyr", "tidyr", "scales", "RColorBrewer", "cowplot", "lubridate", "stringr", "gridExtra"))`

## Как запустить проект

1.  **Скачайте или клонируйте репозиторий**: `https://github.com/EKarev/HSE-Homework.git`
2.  **Откройте файл с кодом в вашей среде разработки R**.
3.  **Запустите скрипт**, который загрузит данные и создаст графики.
4.  **Графики будут отображены** в панели визуализации RStudio или в терминале, в зависимости от вашей среды.

## Структура данных

Данные были предоставлены в формате CSV и содержат информацию о различных характеристиках студентов, таких как:

-   Возраст

-   Пол

-   Город

-   Профессия

-   Уровень академического давления

-   Рабочий стресс

-   Депрессия

-   Пищевые привычки

-   Продолжительность сна

-   Наличие суицидальных мыслей

## Примечания

-   Датасет был взят c [Kaggle](https://www.kaggle.com/datasets/hopesb/student-depression-dataset)
-   Графики нарочито были сделаны нерепрезентативными

## Лицензия

Этот проект был выполнен для учебных целей и доступен для использования и модификации в рамках образовательных нужд.
