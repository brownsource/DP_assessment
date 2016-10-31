pale<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
fine = 100
V(network_all)$color <- pale(fine)[as.numeric(cut(V(network_all)$centrality_out,breaks=fine))]