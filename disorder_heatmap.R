###to make disorder score heatmaps for mRNA 3' end processing proteins (color-blind accessible)
##################can clean later.

install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

library(readxl)
library(ggplot2)
library(dplyr)

data<-read_excel("iupred_update_polyA.xlsx")
scores <- "iupred_update_polyA.xlsx"
sheets <- excel_sheets(scores)
df_list <- list()

for (sheet in sheets) {
  data <- read_excel(scores, sheet = sheet)
  colnames(data) <- trimws(colnames(data))
  if (ncol(data) < 3) {
    warning(paste("skipping", sheet, "- not enough columns"))
    next
  }

  temp <- data %>%
    select(Residue = 1, DisorderScore = 3) %>%
    mutate(Residue = as.numeric(Residue),
           DisorderScore = as.numeric(DisorderScore),
           Protein = sheet)
  
  df_list[[sheet]] <- temp
}

combined_df <- bind_rows(df_list)

combined_df$Protein <- factor(combined_df$Protein, levels = sheets)

real_proteins <- unique(as.character(combined_df$Protein))

spaced_proteins <- unlist(
  lapply(seq_along(real_proteins), function(i) c(real_proteins[i], paste0("spacer_", i)))
)

combined_df$Protein <- factor(combined_df$Protein, levels = spaced_proteins)

####need to create some blank rows..
spacer_df <- bind_rows(
  lapply(seq_along(real_proteins), function(i) {
    tibble(
      Residue = NA,
      DisorderScore = NA,
      Protein = paste0("spacer_", i)
    )
  })
)

#combine
combined_with_gaps <- bind_rows(combined_df, spacer_df)

#make sure protein is a factor with correct levels
combined_with_gaps$Protein <- factor(combined_with_gaps$Protein, levels = spaced_proteins)

p<-ggplot(combined_with_gaps, aes(x = Residue, y = Protein, fill = DisorderScore)) +
  geom_tile(na.rm = TRUE) +
  scale_fill_gradient2(
    low = "#E69F00", mid = "#F0F0F0", high = "#0072B2",
    midpoint = 0.5, limits = c(0, 1),
    name = "Disorder Score"
  ) +
  labs(
    x = "Residue",
  ) +
  scale_y_discrete(labels = function(x) ifelse(grepl("spacer_", x), "", x)) +  # hides spacer labels
  theme_minimal() +
  theme(
    panel.grid = element_blank(),         #removes both major and minor grid lines
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 90, size = 18, color="black"),
    axis.text.y = element_text(size = 18, color="black"),
    axis.title.x = element_text(size = 16, color="black", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, color="black"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 14, margin = margin(b = 10)),
    legend.text = element_text(size = 12)
  )

ggsave("disorder_heatmap_polyA.png", plot = p, width = 12, height = 0.4 * length(unique(combined_df$Protein)) + 2, dpi = 300)

print(p)




#E#E#EEEEEEEEEEEEEEEEE#E#E#E













