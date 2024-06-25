# Create data
# set.seed(112)
# data <- matrix(sample(1:30,15) , nrow=3)
# colnames(data) <- c("A","B","C","D","E")
# rownames(data) <- c("var1","var2","var3")
# 
# 
# # Get the stacked barplot
# barplot(data, 
#         col=colors()[c(23,89,12)] , 
#         border="white", 
#         space=0.04, 
#         font.axis=2, 
#         xlab="group")
        
        # table 1: taxon group by IUCN 
#d2=readRDS('/Users/ctg/Dropbox/Projects/2023_Exposure/ExposureXIUCNXECO_t2m-q099_99p.rds')
d2=readRDS(paste0('/Users/ctg/Dropbox/Projects/2023_Exposure/ExposureXIUCNXECO_q99_p99_v2.rds'))
# aggregate threars
(d3=d2 %>% filter(per.expYear>=.2) %>% group_by(group) %>% mutate(threat='Threatened') %>% mutate(threat=replace(threat,redlistCategory=='Least_Concern','Least_Concern')) %>% mutate(threat=replace(threat,redlistCategory=='Data_Deficient','Data_Deficient')))
# yes, there's double counting here...
(d31=d3 %>% dplyr::select(spName,group,threat) %>% unique %>% count(threat) %>% arrange(desc(n))  %>% rename(Group=group,Status=threat,NSpecies=n))
# wide format
d4=pivot_wider(d31,names_from=Status,values_from=NSpecies)
names=d4$Group
d5=as.matrix(d4[-1])
rownames(d5)=names

cols=c("red1", "steelblue1","gold",'steelblue4')
plot.f1=paste0(plotDir,'/IUCNMisses_v6.pdf')
pdf(plot.f1,w=8,h=8)
par(oma=c(3,3,3,3))
	barplot(d5, col=cols ,  border="white", space=0.04, font.axis=2, xlab="Redlist Category",ylab='Number of Species',las=1,cex.lab=1.5)
	legend('topright',legend=rownames(d5),pch=15,col=cols,bty='n',cex=2)
dev.off()
system(paste0('open ', plot.f1))

# publish the IUCN list 
write.csv(d3 %>% dplyr::select(spName,group,threat) %>% unique %>% arrange(group,threat,spName),file='/Users/ctg/Dropbox/Projects/2023_Exposure/IUCNxExposure_v2.csv',row.names=F)












