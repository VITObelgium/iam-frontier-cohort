library("gplots")
library("RColorBrewer")
library(ComplexHeatmap)
library(ggbiplot)


##################################################
### figure 3
#load("./data/mean_wide_weekly.RData")
mean.wide<-as.matrix(mean.wide)

col_fun1 = colorRampPalette(brewer.pal(9, "Reds"))(256)
png("figure3.png", width=1300, height=450, units="px")
par(oma=c(1,4,0,0))
lmat = rbind(c(0,3,4),c(2,1,0))
lwid = c(1.5,4)
lhei = c(1.5,4,1)
lwid=c(0.1,4,0.1); lhei=c(0.1,3)
#For reordering based on clusters of complains: something still goes wrong
#mean.wide <- mean.wide[match(vec2, row.names(mean.wide)), ]
#Offsetrow and cexCol paramateres can be adjusted to play with label position and size
heatmap.2(mean.wide, Colv=FALSE, Rowv=FALSE, dendrogram="none", margins = c(5, 1), scale="none", labRow = rownames(mean.wide),  col=col_fun1, symbreaks=FALSE, symkey=FALSE, trace="none", 
          colsep=grep("13", colnames(mean.wide)),rowsep=seq(3,33,by=3),sepcolor="black", sepwidth=c(0.05,0.05), 
          offsetCol=0, offsetRow=-160, key=TRUE, cexRow=1.7, cexCol=0.57,
          lwid=c(0.1,1), lhei=c(0.1,4))
dev.off()

##################################################
### figure 4
#load("./data/pct_out_clinical.RData")
data=as.matrix(d.out_mean2)
col_fun1 = colorRamp2(c(0, 0.3, 0.6), c("seagreen", "white", "darkred"))
#pdf("cosine_clinical.pdf", height=10, width=10)
Heatmap(data, name = "Outliers percentage",show_row_names = TRUE, row_names_gp = gpar(fontsize = 6),
        show_column_names = TRUE,column_names_gp = gpar(fontsize = 6),
        row_order = order(rownames(data)), column_order = order(colnames(data)), col = col_fun1) 

##################################################
### figure 5
# by person id
color.subj = c(IAM01 = 'brown',IAM02 = 'forestgreen', IAM03 = 'red2',
               IAM04 = 'orange', IAM05 =  'cornflowerblue', IAM06 = 'magenta',
               IAM07 = 'darkolivegreen4', IAM08 =  'indianred1', IAM09 =  'tan4',
               IAM10 =  'darkblue', IAM11 = 'mediumorchid1', IAM12 = 'firebrick4',
               IAM13 =  'yellowgreen', IAM14 = 'lightsalmon', IAM15 = 'tan3',
               IAM16 = "tan1", IAM17 = 'darkgray', IAM18 = 'black',
               IAM19 =  '#DDAD4B', IAM20 = 'chartreuse', IAM21 = 'seagreen1',
               IAM22 = 'moccasin', IAM23 = 'mediumvioletred', IAM24 = 'forestgreen',
               IAM25 = 'cadetblue1', IAM26 = "darkolivegreen1" , IAM27 = "tan2" ,
               IAM28 =   "tomato3" , IAM29 = "#7CE3D8" ,IAM30 = "gainsboro")

#dt2<-read.csv("./data/clinical_pca.csv", sep=";", header = T, row.names = 1)
#load("./data/clin_pca_list.RData")

dt2$person_id<-as.factor(dt2$person_id)
plots_age <- vector("list", length = 3)
k=1
for (i in 1:3) {
  for (j in 1:3) {
    if(i<j){
      plots_age[[k]] <- ggbiplot(d.clin.pca, var.axes = F, ellipse = F, groups = dt2$person_id, 
                                 choices = c(i,j),  scale = 0.5) +
        scale_color_manual(name="Subjects", values= color.subj)+
        ylim(-5, 6)+
        xlim(-5, 6)+
        theme_bw() +
        theme(legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              axis.text.x = element_text(size = 13),
              axis.text.y = element_text(size = 13),
              axis.title.x = element_text(size=15),
              axis.title.y = element_text(size=15))
      
      k=k+1
    }
  }
}
