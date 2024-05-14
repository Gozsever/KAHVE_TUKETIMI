# Uyku Bozuklukları ve Yaşam Tarzı
Uyku bozuklukları ve yaşam tarzı arasındaki ilişki, günümüzde giderek artan bir ilgi konusu haline gelmiştir. Özellikle modern yaşamın getirdiği stres, düzensiz çalışma saatleri, teknoloji kullanımı ve hareketsiz yaşam tarzı gibi faktörler, uyku düzeni üzerinde önemli etkilere sahip olabilir. Örneğin, yetersiz fiziksel aktivite ve düzensiz uyku alışkanlıkları, uyku kalitesini olumsuz yönde etkileyebilir ve uyku bozukluklarının ortaya çıkmasına zemin hazırlayabilir.

Düzenli egzersiz yapmak, sağlıklı bir diyet benimsemek ve stres yönetimi tekniklerini uygulamak gibi yaşam tarzı değişiklikleri, uyku kalitesini artırabilir ve uyku bozukluklarının riskini azaltabilir. 

Bu nedenle, bireylerin uyku düzenleri ve yaşam tarzı seçimleri üzerinde bilinçli olmaları ve gerekli önlemleri almaları, genel sağlık ve iyi bir yaşam kalitesi için önemlidir.

# Amaç ve Önem
Uyku düzenleri, fiziksel aktivite, stres düzeyi ve genel sağlık durumları arasındaki ilişkileri incelemek amacıyla kullanılabilir.

Veri seti bizlere, bireylerin uyku düzenleri ve yaşam tarzı seçimlerinin sağlık üzerindeki etkilerini anlamak için zengin bir kaynak sunar. Bireylerin uyku kalitesi ve süresine etki eden faktörleri analiz ederek, daha sağlıklı yaşam tarzı önerileri ve uyku düzenlemeleri yapılabilir. Bu tür çalışmalar, uyku bozuklukları ile mücadele ve genel sağlık iyileştirme stratejileri geliştirmek için önemli veriler sağlayabilir.

# Veri Seti
Bu veri seti, uyku düzenleri, fiziksel aktivite, stres düzeyi ve genel sağlık durumları arasındaki ilişkileri incelemek amacıyla çevrimiçi olarak paylaşılan bir çalışmanın sonuçlarını içermektedir. Veri seti, uyku bozuklukları ve diğer sağlık parametreleri arasındaki ilişkileri incelemek amacıyla kullanılabilir. Kısaca özetleyecek olursak;

Toplam Gözlemler (Satırlar):
374 (Başlık satırı hariç),
Değişkenler (Sütunlar):
14,
Cinsiyet Dağılımı:
Erkek: 189
Kadın: 185

https://www.kaggle.com/datasets/vincentchege/sleep-and-lifestyle/data



# Grafikler

## Grafiklerin Çizdirilmesinde Yardımcı Paketlerin Yüklenmesi Ve Çalıştırılması

```

install.packages("readxl") #excell dosyasını okumak için
installed.packages("ggplot2") #ggplot görselleştirme araçlarını kullanmak için
install.packages("dplyr") #veri manipülasyonları için
install.packages("gridExtra") #grafik düzenleme paketi
install.packages("MetBrewer") #farklı renk paletleri sağlar
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(MetBrewer)

```


## Grafik-1 Cinsiyete Göre Uyku Bozukluğu Dağılımı

```

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

```
![Cinsiyet](https://github.com/Gozsever/Uyku_Yasam_Tarz-/blob/main/G%C3%B6rseller/Cinsiyet.png)

YORUM

## Grafik-2 Meslek Kategorisine Göre Uykusuzluk (Insomnia) Dağılımı

```

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

# Görselleştirme
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

```
![Meslek ve Uykusuzluk](https://github.com/Gozsever/Uyku_Yasam_Tarz-/blob/main/G%C3%B6rseller/Meslek%20ve%20Uykusuzluk.png)


YORUM

