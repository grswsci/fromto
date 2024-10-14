#' @title scmarkers
#' @description scmarkers in publications
#' @param label "main_lable" "fine_lable_t" "fine_lable_b" "fine_lable_dc" "fine_lable_myeloid" "fine_lable_fibroblast" "fine_lable_endothelial" "fine_lable_epithelial"
#' @return scmarkers
#' @examples
#' # examples
#' scmarkers(label = "main_lable")
#' scmarkers(label = "fine_lable_t")
#' scmarkers(label = "fine_lable_b")
#' scmarkers(label = "fine_lable_dc")
#' scmarkers(label = "fine_lable_myeloid")
#' scmarkers(label = "fine_lable_fibroblast")
#' scmarkers(label = "fine_lable_endothelial")
#' scmarkers(label = "fine_lable_epithelial")
scmarkers = function(label = "main_lable"){
  if(label == "main_lable"){
  message(paste0(
    "Epithelial_cells = c('EPCAM','KRT19','CDH1','KRT18') #PMID: 28474673/31067475\n",
    "T_lymphocytes = c('CD3D','CD3E','CD3G','TRAC') #PMID: 28475900/31209336\n",
    "B_lymphocytes = c('CD79A','IGHM','IGHG3','IGHA2') #PMID: 31712411/30523328\n",
    "Myeloid_cells = c('CD68',' MARCO','FCGR3A','LYZ') #PMID: 28475900/29967419\n",
    "NK_cells = c('NCAM1','NKG7','GNLY','KLRD1') #PMID: 28475900/31477722\n",
    "Mast_cells = c('KIT','MS4A2','GATA2') #PMID: 30979687\n",
    "Fibroblasts = c('DCN','COL1A1','COL1A2','THY1') #PMID: 31209336/29198524\n",
    "Endothelial_cells = c('PECAM1','CLDN5','FLT1','RAMP2') #PMID: 30674341/21460247/23355623\n",
    "Oligodendrocytes = c('OLIG1','OLIG2','MOG','CLDN11') #PMID: 29615592/26628089\n")
    )
  }else if(label == "fine_lable_t"){
    message(paste0(
      "NK = c('XCL1','FCGR3A','KLRD1','KLRF1') #PMID: 31477722\n",
      "T_lymphocytes = c('CD3D','CD3E','CD3G','TRAC') #PMID: 28475900/31209336\n",
      "CD4T = c('Il7R','CD4') #PMID: 14662907/28475900\n",
      "CD8T = c('CD8A','CD8B') #PMID: 28475900\n",
      "Naïve = c('TCF7','SELL','LEF1','CCR7') #PMID: 29942094\n",
      "Treg = c('IL2RA','FOXP3','IKZF2','TGFB1','TGFB3','TGFBI','TGFBR1') #PMID: 29942094/28474673\n",
      "Tex = c('LAG3','TIGIT','PDCD1','HAVCR2') #PMID: 29942094\n",
      "Teff = c('IL2','GZMA','GNLY','PRF1','GZMB','GZMK','IFNG','NKG7') #PMID: 29942094\n",
      "Th1 = c('STAT4','IFNG','IL12RB2') #PMID: 24987392/21685955\n",
      "Th2 = c('GATA3', 'STAT6','IL4') #PMID: 24987392\n",
      "Th17 = c('IRF4','CREM','NR4A2') #PMID: 21381156/27680869/23437182\n",
      "Tgd = c('TRDC','TRGC2','TRGC1') #PMID: 31118283\n",
      "Tfh = c('MAF','CXCR5','PDCD1','CXCL13') #PMID: 28265271/28570278\n")
    )
  }else if(label == "fine_lable_b"){
    message(paste0(
      "B_lymphocytes = c('CD79A','IGHM','IGHG3','IGHA2') ##PMID: 31712411/30523328\n",
      "B_GC_DZ = c('STMN1','AICDA','MKI67','BIRC5') #PMID: 30104629\n",
      "B_GC_LZ = c('LMO2','BCL2A1') #PMID: 30104629\n",
      "GrB_secreting = c('GZMB') #PMID: 21808264\n",
      "B_follicular = c('MS4A1','HLA-DRA') #PMID: 29988129\n",
      "B_MALT = c('JCHAIN','IGHA1') #PMID: 29988129\n",
      "Plasma = c('IGHG1') #PMID: 29988129\n"
    )
    )
  }else if(label == "fine_lable_dc"){
    message(paste0(
    "DCs = c('CLEC10A','CD1C','CLEC4C','PTCRA','CCR7','LAMP3') #PMID: 28475900\n",
    "DCs_CD1c = c('CD1C','ITGAX') #PMID: 24744755\n",
    "DCs_CD141 = c('CLEC9A','XCR1') #PMID: 24744755\n",
    "DCs_CD207_CD1a = c('CD207','CD1A') #PMID: 24744755\n",
    "DCs_Activated = c('CCR7','LAMP3') #PMID: 17312119\n",
    "pDCs = c('IL3RA','CLEC4C') #PMID: 28428369\n",
    "DCs_CD163_CD14 = c('CD14','CD163') #PMID: 31474513\n"))
  }else if(label == "fine_lable_myeloid"){
    message(paste0(
      "Monocyte = c('CTSS','FCN1','S100A8','S100A9','LYZ','VCAN') #PMID: 29967419\n",
      "Macrophage = c('LGMN','CTSB','CD14','FCGR3A') #PMID: 29967419\n",
      "mo_lineage = c('MAFB','MAF','CX3CR1','ITGAM','CSF1R') #PMID: 28257233\n",
      "Alveolar_Mac = c('MARCO','FABP4','MCEMP1') #PMID: 28257233\n",
      "Anti_inflammatory = c('CD163','APOE','SEPP1','C1QA','C1QB','C1QC') #PMID: 27381735/21350196/26053663/22523386\n",
      "Pro_inflammatory = c('CXCL8', 'IL1B') #PMID: 25339958\n",
      "Cycling = c('STMN1','MKI67','TOP2A','CDK1') #PMID: 29967419\n",
      "DC = c('CLEC10A','CD1C', 'CLEC4C','PTCRA','CCR7','LAMP3') #PMID: 28475900\n"
    ))
  }else if(label == "fine_lable_fibroblast"){
    message(paste0(
    "Fibroblasts = c('DCN','COL1A1','COL1A2','THY1') #PMID: 31209336/29198524\n",
    "FBs_COL13A1 = c('COL13A1','TCF21','ITGAB','CXCL14','NPNT') #PMID: 29590628\n",
    "FBs_COL14A1 = c('COL14A1','GSN', 'PI16','CYGB','PRRX1') #PMID: 29590628\n",
    "Myofibroblasts = c('ACTA2','MYH11','TAGLN', 'ACTG2','MYLK') #PMID: 29590628\n",
    "SMCs = c('CNN1','SYNPO2','CRYAB','DES') #PMID: 28564607\n",
    "Mesothelial_cells = c('UPK3B','MSLN','CALB2','WT1') #PMID: 29590628\n",
    "Pericytes = c('RGS5','CSPG4','ABCC9','KCNJ8') #PMID: 28564607\n",
    "Perivascular_FBs = c('CYP1B1','APOD') #PMID: 29443965\n",
    "Lipofibroblast = c('FABP4','FABP5','PPARG') #PMID: 29590628\n"))
  }else if(label == "fine_lable_endothelial"){
    message(paste0(
      "Endothelial_cells = c('PECAM1','CLDN5','FLT1','RAMP2') #PMID: 30674341/21460247/23355623\n",
      "Tip_like_ECs = c('RAMP3','RGCC','ADM') #PMID: 29449267\n",
      "Stalk_like_ECs = c('SELP', 'ACKR1') #PMID: 29449267\n",
      "Lymphatic_ECs = c('CCL21', 'LYVE1') #PMID: 29449267\n",
      "EPCs = c('TYROBP','C1QB') #PMID: 29449267\n",
      "Tumor_ECs = c('HSPG2','INSR','VWA1') #PMID: 29988129/30559346/10629090\n"
    ))
  }else if(label == "fine_lable_epithelial"){
    message(paste0(
    "Epithelial_cells = c('EPCAM','KRT19','CDH1','KRT18') #PMID: 28474673/31067475\n",
    "AT1 = c('AGER') #PMID: 30554520\n",
    "AT2 = c('SFTPC','LAMP3') #PMID: 30554520\n",
    "Club = c('SCGB1A1') #PMID: 30554520\n",
    "Ciliated = c('FOXJ1','RFX2') #PMID: 30554520\n"
    ))
  }
}


