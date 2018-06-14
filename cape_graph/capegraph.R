#
# generates plot for CAPE (Shiller) PEs
# to prepare the data, go to https://www.researchaffiliates.com/assetallocation/Pages/Equities.aspx
# in the section for Shiller PE pick the 8 markets you want to display
# then click "export data" and store the csv file in the same folder with this r script
# creates a pdf called "shillerplot.pdf" with the graph in it
#

require("lubridate")
require("ggplot2")
capeval=read.csv("global_shiller_pe_values.csv")
#nr=which(""==capeval[,1])[1]-1
capeval=capeval[1:8,]
capedf=data.frame(market=factor(),shillerpe=vector(),highlight=logical())
for (i in 1:8) {
  tempcape=rep(as.numeric(capeval[i,c(7,9,10)]),50)
  tempcape=c(tempcape,rep(as.numeric(capeval[i,c(6,8,5)]),1))
  tempmarket=rep(capeval[i,2],153)
  temphigh=rep(c(FALSE,TRUE),c(152,1))
  capedf=rbind(capedf,data.frame(market=tempmarket,shillerpe=tempcape,highlight=temphigh))
}
pdf("shillerplot.pdf",width=8,height=5)
ggplot(capedf,aes(x=market,y=shillerpe))+geom_boxplot(outlier.size=.5,color='blue')+
  geom_point(data=subset(capedf,highlight),aes(x=market,y=shillerpe),color='red',size=2)+
  ylab("Shiller/CAPE PE")+xlab("")+
  ggtitle(paste0("Comparative Shiller/CAPE PEs\n(red dot is value at ",capeval[1,1],")"))
dev.off()


