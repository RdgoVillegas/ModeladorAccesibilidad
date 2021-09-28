# Script para obtener máximo histórico de matrícula
library(tidyverse); library(glue); library(sf)
# Carpetas de trabajo
rootdir <- "C:/Users/CEDEUS 27/"
tesidir <- glue(rootdir, "Google Drive/Magister Desarrollo Urbano/Tesis/Data/")
datadir <- glue(tesidir, "rbd/Resumen de matricula/")
# Lista de archivos a leer
filelist <- list.files(datadir, full.names = T)

data <- data.frame()

for (i in 1:length(filelist)) {
  tempdata <- read.csv2(filelist[i]) %>%
    rename_all(tolower) %>%
    select(matches("agno"), rbd, mat_ens_2, mat_ens_5, mat_ens_7) %>%
    rename("agno" = matches("agno"))
  data <- rbind(data, tempdata)
}

matriculaMaxima <- data %>%
  mutate(rbd = rbd %>% as.character()) %>%
  group_by(rbd) %>%
  summarise(basica_cap = max(mat_ens_2, na.rm = T),
            mediahc_cap = max(mat_ens_5, na.rm = T),
            mediatp_cap = max(mat_ens_7, na.rm = T))

save(matriculaMaxima, file = "matriculaMaxima.RDS")

