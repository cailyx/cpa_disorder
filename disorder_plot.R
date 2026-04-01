####use this to adjust figures plotting AIUPred scores for mRNA 3' end processing proteins.



#get packages for reading Excel files + making plots
install.packages("readxl")
install.packages("ggplot2")

#if already installed start run here (load pkgs):
library(readxl)
library(ggplot2)

#read file
data<-read_excel("iupred_update.xlsx")

#gather all sheets into vector
sheets<-excel_sheets("iupred_update.xlsx")

#create list to store plots (empty for now)
plot_list<-list()

##make numeric (for reference, is already numeric)
data$`# Pos`<-as.numeric(data$`# Pos`)
data$`Residue AIUpred`<-as.numeric(data$`Residue AIUpred`)

#make + store plots
for(sheet in sheets) { #loop thru each sheet name in sheets
  data<-read_excel("iupred_update.xlsx",sheet=sheet)    #read data in sheet
  p<-ggplot(data,aes(x=`# Pos`,y=`Residue AIUpred`)) +   #make plot using data, define which columns to use for x,y
    geom_line(color="steelblue",linewidth=0.9) + #add line thru data
    geom_hline(color="grey",yintercept=0.5,linetype="dashed",linewidth=0.6) +
    scale_y_continuous(
      limits=c(0,1),
      breaks=c(0.25,0.5,0.75,1)
    ) +
    labs(title=paste(sheet),  #paste sheet name as title of plot
      x="Residue",  #title x,y axes
      y="Score") +
    theme_minimal() +
    theme(panel.grid=element_blank(), #get rid of grid lines
          panel.background=element_blank(), #get rid of background
          axis.line=element_line(color="black"), #add back in axis lines
          axis.text.x=element_text(size=14),
          axis.text.y=element_text(size=14),
          plot.margin=margin(t = 10, r = 30, b = 20, l = 10)
          )

  plot_list[[sheet]]<-p  #(?)store each plot into plot_list using sheet name as key

  #TO SAVE AS PNG:
    #specify where to save if needed, eg setwd("C::/Users/CailyxQuan/Documents")
  ggsave(filename=paste0(sheet,".png"),plot=p,width = 6, height = 2, dpi = 300)
  }

#see plots
for(plot in plot_list){
  print(plot)
}




##if want to see one plot at a time in window
for (plot in plot_list) {
  print(plot)
  Sys.sleep(1)  # Pause for 1 second between each plot (optional)
}


