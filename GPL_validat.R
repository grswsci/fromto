library(fromto)
gplid = "GPL97"
i_gpl = readRDS(paste0("C:/fromto/data/",gplid,".RDS"))
u_gpl = read_fromto(paste0("/fromto/",gplid,"_noParents.an.txt"),row_names = FALSE)
colnames(u_gpl) = u_gpl[1,];u_gpl = u_gpl[-1,];u_gpl_check = u_gpl[which(u_gpl[,2] != ""),]
gpl_new = merge_col_add(data1 = i_gpl,
                        data2 = u_gpl,
                        data1_var = colnames(i_gpl)[1],
                        data2_var_same_data1 = colnames(u_gpl)[1],
                        data2_var_add_data1 = colnames(u_gpl)[2])
gpl_new[,4] = strsplit_fromto(gpl_new[,2]," /// ",1)
gpl_finally = gpl_new[,c(1,ncol(gpl_new))]
colnames(gpl_finally) = c("ID","Gene Symbol")
saveRDS(gpl_finally,paste0("C:/fromto/data/GPL/",gplid,".RDS"))
rm(list = ls())
