View(Sleep_Health_and_Lifestyle)
head(Sleep_Health_and_Lifestyle)

install.packages("readxl") #excell dosyasını okumak için
installed.packages("ggplot2") #ggplot görselleştirme araçlarını kullanmak için
install.packages("dplyr") #veri manipülasyonları için
install.packages("gridExtra") #grafik düzenleme paketi
install.packages("MetBrewer") #farkli renk paletleri sağlar
install.packages("tidyr") #veri çerçevelerindeki verilerin şeklini değiştirmek, düzenlemek ve temizlemek için
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(MetBrewer)
library(tidyr)

data <- read.csv("/cloud/project/Sleep Health and Lifestyle.csv")

colnames(data) #data verisi içerisindeki sütunların başlıklarını verir

#GRAFİK1
# Cinsiyet ve uyku bozukluğu sütunlarını içeren bir alt veri kümesi oluşturma
sleep_disorder_by_gender <- data[, c("Gender", "Sleep.Disorder")]

# Renk paletini tanımlama
blue_palette <- colorRampPalette(c("#C7E9C0", "#4A90E2"))(3)  # 3 renk için palet oluştur


# Çubuk Grafiği
ggplot(sleep_disorder_by_gender, aes(x = Gender , fill = Sleep.Disorder)) +
  geom_bar(position = "dodge", width = 0.7, color = "white") +
  scale_fill_manual(values = blue_palette,
                    labels = c("Uykusuzluk", "Hiçbiri", "Uyku Apnesi")) +
  labs(title = "Cinsiyete Göre Uyku Bozukluğu Dağılımı", x = "Cinsiyet", y = "Kişi Sayısı", fill = "Uyku Bozukluğu") +
  scale_x_discrete(labels = c("Kadın", "Erkek"))+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14,),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14,),
    legend.text = element_text(size = 12))+
  ylim(0,150)

#GRAFİK2
# Meslek gruplaması yap
data <- data %>%
  mutate(Occupation_Grouped = case_when(
    grepl("Software Engineer", Occupation) ~ "Yazılım Mühendisi",
    grepl("Engineer", Occupation) ~ "Mühendis",
    grepl("Lawyer", Occupation) ~ "Avukat",
    grepl("Accountant", Occupation) ~ "Muhasebeci",
    grepl("Doctor", Occupation) ~ "Doktor",
    grepl("Salesperson", Occupation) ~ "Satış Personeli",
    grepl("Teacher", Occupation) ~ "Öğretmen",
    grepl("Nurse", Occupation) ~ "Hemşire",
    TRUE ~ Occupation  # "Diğer" kategorisinde olmayan meslekleri direkt olarak kullan
  )) %>%
  na.omit() 

# Meslek sıralaması
sorted_data <- data %>%
  group_by(Occupation_Grouped) %>%
  mutate(Count = n()) %>%
  arrange(Count, .by_group = TRUE)

# Renk paleti
blue_palette <- colorRampPalette(c("#C7E9C0", "#4A90E2"))(3)


# "insomnia" olanları filtrele
insomnia_data <- sorted_data %>%
  filter(Sleep.Disorder == "Insomnia")

# Görseli çizdirelim
ggplot(insomnia_data, aes(x = Count, y = reorder(Occupation_Grouped, Count), fill = as.factor(Sleep.Disorder))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = blue_palette,
                    labels = c("Uykusuzluk"))+
  labs(
    title = "Meslek Kategorisine Göre Uykusuzluk (Insomnia) Dağılımı",
    y = "Meslek",
    x = "Gözlem Sayısı",
    fill = "Uyku Durumu"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+  # Meslek isimlerini çevirerek düzenleme
  xlim(0, 90)

#GRAFİK3
# Stres düzeyi ve uyku kalitesi için verileri hazırlayalım
data_long <- data %>%
  select(Age, Stress.Level, Quality.of.Sleep) %>%
  gather(key = "Variable", value = "Value", -Age)

# Stres düzeyi ve uyku kalitesi için faktör seviyelerini ayarlama
data_long$Variable <- factor(data_long$Variable, levels = c("Stress.Level", "Quality.of.Sleep"),
                             labels = c("Stres Düzeyi", "Uyku Kalitesi"))

# Renk paletini oluşturduk
blue_palette <- c("#CAF0F8", "#90E0EF", "#00B4D8", "#0096C7", "#0077B6", "#023E8A", "#03045E")

# Yaş ortalamasını hesaplayalım
age_mean <- mean(data$Age, na.rm = TRUE)

# Birleştirilmiş violin grafiği oluşturma
ggplot(data_long, aes(x = as.factor(Value), y = Age, fill = as.factor(Value))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  scale_fill_manual(values = blue_palette) +
  facet_wrap(~ Variable, scales = "free_x") + 
  geom_hline(yintercept = age_mean, linetype = "solid", color = "red", linewidth = 0.5) +
  labs(
    title = "Yaşa Göre Stres Düzeyi ve Uyku Kalitesi",
    x = "Düzeyler",
    y = "Yaş",
    fill = "Düzeyler"
  ) +
  theme_minimal()

#GRAFİK4
# Filtrelenmiş veriyi oluştur(Uykusuzluk ve uyku apnesi)
filtered_data <- data %>% filter(Sleep.Disorder %in% c("Insomnia", "Sleep Apnea"))

# Renk paletini belirleyelim
palette <- met.brewer("Hokusai3", 2)

# Veri çerçevesini hazırlama 
long_data <- filtered_data %>%
  pivot_longer(cols = c(Quality.of.Sleep, Sleep.Duration, Stress.Level),
               names_to = "Measure",
               values_to = "Value")

# Görseli oluşturalım 
ggplot(long_data, aes(x = Value, fill = Sleep.Disorder)) +
  geom_density(alpha = 0.5) +
  labs(x = "Değer",
       y = "Yoğunluk",
       title = "Uyku Bozukluklarının Farklı Ölçütlere Etkisi",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Uyku Bozukluğu") +
  scale_fill_manual(values = palette,
                    labels = c("Uykusuzluk", "Uyku Apnesi")) +
  facet_grid(rows = vars(Measure), scales = "free_x", labeller = as_labeller(c(
    "Quality.of.Sleep" = "Uyku Kalitesi",
    "Sleep.Duration" = "Uyku Süresi",
    "Stress.Level" = "Stres Düzeyi"
  ))) +
  theme_bw()
  