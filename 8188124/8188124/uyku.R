View(Sleep_Health_and_Lifestyle)
head(Sleep_Health_and_Lifestyle)

install.packages("readxl") #excell dosyasını okumak için
installed.packages("ggplot2") #ggplot görselleştirme araçlarını kullanmak için
install.packages("dplyr") #veri manipülasyonları için
install.packages("gridExtra") #grafik düzenleme paketi
install.packages("MetBrewer") #farkli renk paletleri sağlar
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(MetBrewer)

data <- read.csv("/cloud/project/Sleep Health and Lifestyle.csv")

colnames(data) #data verisi içerisindeki sütunların başlıklarını verir

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
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12))+
    ylim(0,150)
 
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
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+  # Meslek isimlerini çevirerek düzenleme
  xlim(0, 90)

 

# Yaş ortalamasını hesaplayalım
age_mean <- mean(data$Age, na.rm = TRUE)

# Stres düzeylerine göre renk ataması
#blue_palette <- colorRampPalette(c("#C7E9C0", "#1F75FE"))(length(levels(data$Stress.Level)))

blue_palette[1:6] <- c("#CAF0F8","#90E0EF","#00B4D8", "#0077B6","#023E8A","#03045E")  

# Stres düzeylerine göre renk paletini kullanarak grafik oluşturma
g1 <- ggplot(data, aes(x = as.factor(Stress.Level), y = Age, fill = as.factor(Stress.Level))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  scale_fill_manual(values = blue_palette) +
  
  # Yaş ortalamasını gösteren düz çizgi ekleyelim   
  geom_hline(yintercept = age_mean, linetype = "solid", color = "red", size = 0.5) +
  
  labs(
    title = "Yaşa Göre Stres Düzeyi",
    x = "Stres Düzeyi",
    y = "Yaş",
    fill = "Stres Düzeyi"
  ) +
  theme_minimal()
print(g1)

# Yaş ortalamasını hesaplayalım
age_mean <- mean(data$Age, na.rm = TRUE)


blue_palette[1:6] <- c("#CAF0F8","#90E0EF","#00B4D8", "#0077B6","#023E8A","#03045E")   

# Uyku kalitesine göre renk paletini kullanarak grafik oluşturma
g2 <- ggplot(data, aes(x = as.factor(Quality.of.Sleep), y = Age, fill = as.factor(Quality.of.Sleep))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  scale_fill_manual(values = blue_palette) +
  
  # Yaş ortalamasını gösteren düz çizgi ekleyelim   
  geom_hline(yintercept = age_mean, linetype = "solid", color = "red", size = 0.5) +
  
  labs(
    title = "Yaşa Göre Uyku Kalitesi",
    x = "Uyku Kalitesi",
    y = "Yaş",
    fill = "Uyku Kalitesi"
  ) +
  theme_minimal()
 
 grid.arrange(g1,
              g2)   
 
 #bunu kullanmayabilirim. ÖRNEK OLARAK KALSIN*****
 ggplot(data, aes(x = Quality.of.Sleep, fill = Gender )) +
   geom_density(alpha = 0.5) +
   labs(x = "Uyku Kalitesi ",
        y = "Yoğunluk",
        title = "Cinsiyetin Uyku Kalitesi Üzerindeki Etkisi",
        subtitle = "Kernel Yoğunluk Tahmini",
        fill= "Cinsiyet")+
   scale_fill_manual(values= met.brewer("Hiroshige",2),
                     labels = c("Kadın", "Erkek")) +
                     
   theme_bw() 
   
 
 
 # Uykusuzluk ve Uyku Apnesi olan verileri filtrele
 filtered_data <- data %>% filter(Sleep.Disorder %in% c("Insomnia", "Sleep Apnea"))
 
 g3 <- ggplot(filtered_data, aes(x = Quality.of.Sleep, fill = Sleep.Disorder )) +
   geom_density(alpha = 0.5) +
   labs(x = "Uyku Kalitesi ",
        y = "Yoğunluk",
        title = "Uyku Bozukluklarının Uyku Kalitesine Etkisi",
        subtitle = "Kernel Yoğunluk Tahmini",
        fill= "Uyku Bozukluğu")+
   scale_fill_manual(values= met.brewer("Hokusai3",2),
                     labels = c("Uykusuzluk", "Uyku Apnesi")) +
   
   theme_bw() 
 
 
  g4 <-  ggplot(filtered_data, aes(x = Sleep.Duration, fill = Sleep.Disorder )) +
   geom_density(alpha = 0.5) +
   labs(x = "Uyku Süresi ",
        y = "Yoğunluk",
        title = "Uyku Bozukluklarının Uyku Süresine Etkisi",
        subtitle = "Kernel Yoğunluk Tahmini",
        fill= "Uyku Bozukluğu")+
   scale_fill_manual(values= met.brewer("Hokusai3",2),
                     labels = c("Uykusuzluk", "Uyku Apnesi")) +
   
   theme_bw() 
   
 g5 <-  ggplot(filtered_data, aes(x = Stress.Level, fill = Sleep.Disorder )) +
     geom_density(alpha = 0.5) +
     labs(x = "Stres Düzeyi ",
          y = "Yoğunluk",
          title = "Uyku Bozukluklarının Stres Düzeyine Etkisi",
          subtitle = "Kernel Yoğunluk Tahmini",
          fill= "Uyku Bozukluğu")+
     scale_fill_manual(values= met.brewer("Hokusai3",2),
                       labels = c("Uykusuzluk", "Uyku Apnesi")) +
     
     theme_bw() 
   
g6 <-   ggplot(filtered_data, aes(x = Physical.Activity.Level, fill = Sleep.Disorder )) +
     geom_density(alpha = 0.5) +
     labs(x = "Fiziksel Aktivite Düzeyi ",
          y = "Yoğunluk",
          title = "Uyku Bozukluklarının Fiziksel Aktiviteye Etkisi",
          subtitle = "Kernel Yoğunluk Tahmini",
          fill= "Uyku Bozukluğu")+
     scale_fill_manual(values= met.brewer("Hokusai3",2),
                       labels = c("Uykusuzluk", "Uyku Apnesi")) +
     
     theme_bw() 
   
grid.arrange(g3, g4, g5, g6) 
   


 