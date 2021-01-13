#' Calculate the bidimentional map for genetic information
#'
#'
#' @export
bimap = function(dados,NComp_Var,Figu_name){
  dist.samples <- vegdist(dados[colnames(dados)%in%(NComp_Var)])  #colunas interesse log.ratio
  MDS.sammon <- sammon(dist.samples,k=3)
  mod <- MDS.sammon$points%>%as.data.frame()
  mod$Site <- dados$Site
  mod$Sp <- dados$sp
  mod$Area <-  substr(dados$Site,1,1)
  mod <- mod%>%`rownames<-`(c(paste0(dados$Site,"_",dados$sp,dados$especime)))

  sam <- mod%>%ggplot()+
    # xlim(c(-0.6,0.6))+
    # ylim(c(-0.75,0.75))+
    geom_point(data=mod,
               aes(x=V1,y=V2,shape= Site,color=Sp),alpha=0.4,size=4)+
    scale_shape_manual(values=c(15:18))+
    geom_text_repel(data=mod,
                    aes(x=V1,y=V2,label=rownames(mod),color=Sp),
                    size=2,fontface="bold")+
    geom_hline(yintercept = 0,linetype=2,color="grey50")+
    geom_vline(xintercept = 0,linetype=2,color="grey50")+
    theme_bw()+
    theme(axis.text.x = element_text(size=10,angle=90,hjust = 0.9,vjust = 0.5),
          axis.text.y = element_text(size=12),
          axis.title = element_text(size=10),
          axis.text.y.left = element_text(size=10),
          strip.text =  element_text(size=12))+
    labs(x="MDS 1",y="MDS 2",#x="MDS 1 (63%)",y="MDS 2 (23%)",
         subtitle = paste0("Stress:",round(MDS.sammon$stress,2)))
  return(sam)
  ggsave(filename = paste(Figu_name,".pdf",sep = ""),
         plot = sam,device = "pdf",width = 12,height = 10)
}
