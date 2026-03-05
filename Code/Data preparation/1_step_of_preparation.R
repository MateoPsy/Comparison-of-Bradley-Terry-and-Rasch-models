############################################
#### Pairwise comparisons analysis 2025 ####
####     Data preparation - part 1      ####
############################################

rm(list=ls())

### packages ####
library(tidyverse)
library(ggplot2)
library(dplyr)


### loading data ####
data_wright <- read.csv("~/Desktop/Pairwise comparisons_study/Data/Data for 1_step_of_preparation/data_wright.csv")
data_order <- read.csv("~/Desktop/Pairwise comparisons_study/Data/Data for 1_step_of_preparation/Data_W_Order.csv")
data_b <- read.csv("~/Desktop/Pairwise comparisons_study/Data/Data for 1_step_of_preparation/Data_pairwise.csv")


data_order <- data_order[-c(1:2, 49),-c(10:13,23)] # second loading of questionnaire w/out data
data <- data_order %>%
  arrange(id)


data_b_full <- merge(data_b, data_wright, by = "id")
data_b_full <- data_b_full[-2,] # second loading of questionnaire w/out data
data_order_full <- merge(data_order, data_wright, by = "id")


### RECODING ####

# Define recoding dictionary
recode_map_aut <- c("Můžu se sám/sama rozhodovat, jak chci žít svůj život." = 1,
                    "Mám pocit, že mě pořád někdo někam tlačí." = 2,
                    "Můžu volně vyjadřovat své nápady a názory." = 3,
                    "V běžném životě musím často dělat, co mi druzí řeknou." = 4,
                    "Lidé, se kterými se běžně potkávám, berou v potaz, jak se cítím." = 5,
                    "V běžných, každodenních situacích mám pocit, že se nemusím přetvařovat." = 6,
                    "V běžném životě nemám moc příležitostí rozhodovat, jak chci co dělat." = 7
)


recode_map_hi <- c("Jsem spíš vyšší než muži mého věku." = 1,
                   "Jsem spíš vyšší než ženy mého věku." = 2,
                   "Ostatní mě občas žádají, ať jim něco podám, protože jsem vyšší." = 3,
                   "Slýchávám narážky na to, že jsem vysoký/á." = 4,
                   "Lidem, kteří na koncertě stojí za mnou, většinou má postava dost brání ve výhledu." = 5,
                   "Obyčejné postele jsou pro mě příliš krátké." = 6,
                   "Když chci někoho obejmout, většinou se musím sklonit." = 7,
                   "Často si musím dávat pozor, abych se neuhodil(a) hlavou např. o nízký strop nebo rám dveří." = 8
)



recode_map_dass <- c("Bylo pro mě těžké se zklidnit." = 1,
                     "Měl(a) jsem sklon jednat v některých situacích přehnaně." = 2,
                     "Cítil(a) jsem, že jsem napjatý/á a nervózní." = 3,
                     "Přistihl(a) jsem se, jak jsem rozrušený/rozrušená." = 4,
                     "Bylo pro mě těžké relaxovat (odpočívat)." = 5,
                     "Nesnášel(a) jsem, když mě něco odvádělo od práce (od toho, co právě dělám)." = 6,
                     "Cítil(a) jsem se docela podrážděně." = 7
)

recode_map_bmpn <- c("K lidem, se kterými trávím čas, zažívám silný pocit blízkosti." = 1,
                     "Udělal/a jsem nějakou hloupost, po které jsem se cítil/a neschopně." = 2,
                     "Dařilo se mi i v náročných/obtížných věcech." = 3,
                     "Někteří lidé mi radili, co bych měl/a dělat." = 4,
                     "Musel/a jsem dělat věci proti své vůli." = 5
)

recode_map_dem <- c("Demonstranti, kteří neposlouchají policii, by vždy měli být tvrdě potrestáni." = 1,
                    "Demonstrace a protesty na náměstích by měly probíhat pod přísnější kontrolou." = 2,
                    "Měli bychom omezit tzv. aktivisty, kteří jen kritizují vládu, ale sami nic nedělají." = 3,
                    "Pokud si většina občanů nepřeje v naší zemi nějakou menšinu, měla by tato menšina poslechnout a odejít." = 4,
                    "Menšiny v naší zemi by si neměly příliš vyskakovat, protože v naší zemi rozhoduje většina." = 5
)

recode_map_ders <- c("Když jsem rozrušený/á, těžko věnuji pozornost něčemu jinému." = 1,
                     "Když jsem rozrušený/á, špatně se soustředím." = 2,
                     "Když jsem rozrušený/á, jsem celý/á bez sebe." = 3,
                     "Když jsem rozrušený/á, stěží se dokážu ovládat." = 4,
                     "Když jsem rozrušený/á, ztrácím kontrolu nad svým chováním." = 5,
                     "Když jsem rozrušený/á, dělá mi potíže něco dokončit." = 6,
                     "Když jsem rozrušený/á, jsem z toho podrážděný/á." = 7
)


recode_map_ders01 <- data.frame(c("Když jsem rozrušený/á, těžko věnuji pozornost něčemu jinému.",
                                  "Když jsem rozrušený/á, špatně se soustředím.",
                                  "Když jsem rozrušený/á, jsem celý/á bez sebe.",
                                  "Když jsem rozrušený/á, stěží se dokážu ovládat.",
                                  "Když jsem rozrušený/á, ztrácím kontrolu nad svým chováním.",
                                  "Když jsem rozrušený/á, dělá mi potíže něco dokončit.",
                                  "Když jsem rozrušený/á, jsem z toho podrážděný/á."
), 1:7)




recode_map_total <- c(recode_map_aut, recode_map_bmpn, recode_map_dass, recode_map_dem, recode_map_ders, recode_map_hi)
codebook_items <- list(as.data.frame(recode_map_aut), as.data.frame(recode_map_bmpn),as.data.frame(recode_map_dass),
                       as.data.frame(recode_map_dem),as.data.frame(recode_map_ders),as.data.frame(recode_map_hi))
names(codebook_items) <- c("aut", "bmpn", "dass", "dem", "ders", "HI")




#FINAL recoding of btm answers
data[] <- lapply(data, function(col) {
  if (is.character(col)) {
    for (key in names(recode_map_total)) {
      col <- gsub(key, recode_map_total[key], col, fixed = TRUE)
    }
  }
  return(col)
})

patterns_replacements <- list(
  "K\\s*lidem,\\s*se\\s*kterými\\s*trávím\\s*čas,\\s*zažívám\\s*silný\\s*pocit\\s*blízkosti\\." = '1',
  "Udělal/a\\s*jsem\\s*nějakou\\s*hloupost,\\s*po\\s*které\\s*jsem\\s*se\\s*cítil/a\\s*neschopně\\." = '2',
  "Dařilo\\s*se\\s*mi\\s*i\\s*v\\s*náročných/obtížných\\s*věcech\\." = '3',
  "Někteří\\s*lidé\\s*mi\\s*radili,\\s*co\\s*bych\\s*měl/a\\s*dělat\\." = '4',
  "Musel/a\\s*jsem\\s*dělat\\s*věci\\s*proti\\s*své\\s*vůli\\." = '5'
)

columns_to_replace <- c("bmpn_1_2_DO", "bmpn_1_3_DO", "bmpn_1_4_DO", "bmpn_1_5_DO", 
                        "bmpn_2_3_DO", "bmpn_2_4_DO", "bmpn_2_5_DO", "bmpn_3_4_DO", "bmpn_3_5_DO")

# Apply the patterns to the specified columns
for (pattern in names(patterns_replacements)) {
  for (col in columns_to_replace) {
    data[[col]] <- str_replace_all(data[[col]], regex(pattern, ignore_case = TRUE), patterns_replacements[[pattern]])
  }
}

# Remove unwanted text from specified columns
columns_to_clean <- c('ls_aut_1', 'ls_aut_2','ls_aut_3', 'ls_aut_4','ls_aut_5', 'ls_aut_6', 'ls_aut_7')
for (col in columns_to_clean) {
  data[[col]] <- gsub(' <style=\"vertical-align: middle;> \n\n', '', data[[col]])
}

### recoding of likert scales
# 4 point Likert
recode_4LS <- c("Silněnesouhlasím" = '1',
                "Nesouhlasím" = '2',
                "Souhlasím" = '3',
                "Silněsouhlasím" = '4'
)

data <- data %>%
  mutate(across(c(248:254, 260:264, 270:276, 282:289, 295:301, 307:314), ~str_replace_all(., recode_4LS)))


### DATASETS ####


dem <- data[320:323] # demographics

table(dem$gender) # checking participants sex
dem$gender <- factor(dem$gender,
                     levels = c("Muž", "Žena"),
                     labels = c("male", "female"))

# creating supra datasets with PC and LS
aut <- as.data.frame(c(data[c(14, 20:61, 248:259, 333:334, 350)], dem))
bmpn <- as.data.frame(c(data[c(14, 63:82, 260:269, 335:336, 351)], dem))
dass <- as.data.frame(c(data[c(14, 84:125, 270:281, 337:338, 352)], dem))
democr <- as.data.frame(c(data[c(14, 127:146, 282:294, 339:340, 353)], dem))
ders <- as.data.frame(c(data[c(14, 148:189, 295:306, 341:342, 354)], dem))
height_inv <- as.data.frame(c(data[c(14, 191:246, 307:319, 343:344, 355)], dem))

# saving these snippets
write.csv(aut, "snippets/aut-snip.csv", row.names = FALSE)
write.csv(bmpn, "snippets/bmpn-snip.csv", row.names = FALSE)
write.csv(dass, "snippets/dass-snip.csv", row.names = FALSE)
write.csv(democr, "snippets/democr-snip.csv", row.names = FALSE)
write.csv(ders, "snippets/ders-snip.csv", row.names = FALSE)
write.csv(height_inv, "snippets/height_inv-snip.csv", row.names = FALSE)


### DEMOGRAPHY ####

summary(as.factor(data$gender))
prop.table(table(as.factor(data$gender)))
sd(as.numeric(data$age))
data$age <- as.numeric(data$age)
data$height <- as.numeric(data$height)

a <- data %>% 
  select(gender, age, edu, height) %>% 
  group_by(gender) %>% 
  nest() %>% 
  mutate(mean_age = map_dbl(data, ~ mean(.x$age, na.rm = TRUE)),
         sd_age = map_dbl(data, ~ sd(.x$age, na.rm = TRUE)),
         n = map_dbl(data, ~ nrow(.x)),
         hist = map(data, ~ hist(.x$age)),
         heigt = map_dbl(data, ~ mean(.x$height, na.rm = TRUE)),
         heigt_sd = map_dbl(data, ~ sd(.x$height, na.rm = TRUE)),
         height_hist = map(data, ~ hist(.x$height)))

summary(as.factor(data$edu))
view(a)
t.test(age ~ gender, data = data)


