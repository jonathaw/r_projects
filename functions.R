### functions:
require(plyr)
DDG=-20
PACK=0.6
SASA=1300 
SHAPE=0.5

what1_and_what2_single_df <- function(DF, what1, what2, name){
  plot(DF[, which(names(DF) == what1)], DF[, which(names(DF) == what2)], xlab = what1, ylab = what2, main = paste0(what1, ' Vs. ', what2, ' \n', name), ylim = c(min(DF[, which(names(DF) == what2)], na.rm=TRUE), max(DF[, which(names(DF) == what2)], na.rm=TRUE)), xlim = c(min(DF[, which(names(DF) == what1)])-1, max(DF[, which(names(DF) == what1)], na.rm=T)))
}

choose_best <- function(DF, rms_cutoff_over_min = 5){
  rms_cutoff <- min(DF$a_rms, na.rm=T) + rms_cutoff_over_min
  print(paste0('will use ', rms_cutoff, ' as cutoff, with rms min of ', min(DF$a_rms, na.rm=T)))
  new_DF <- DF[DF$a_rms <= rms_cutoff, ]
  new_DF
}

summarize_filters <-function(DF, filters_f){
  for (filter in filters_f){
    ind <- which(names(DF) == filter)
    print (paste0('summarizing ', name, ' for ', filter))
    print(summary(DF[, ind]))
  }
}

count_by_filters_single_df <- function(DF, ddg=-20, pack=0.6, sasa=1300, shape=0.5){
  subset <- subset(DF, a_ddg < ddg & a_packstat > pack & a_sasa > sasa & a_shape > shape)
  length(subset[[1]])
}

color_by_filters_single_df <- function(DF, what1, what2, ddg=-20, pack=0.6, sasa=1300, shape=0.5, colour = 'purple'){
  subset <- subset(DF, a_ddg < ddg & a_packstat > pack & a_sasa > sasa & a_shape > shape)
  points(subset[, which(names(DF) == what1)], subset[, which(names(DF) == what2)], col = colour, pch=18)
}

bb_barplots <- function(DF, bb, ddg=-20, pack=0.6, sasa=1300, shape=0.5){
  subset <- subset(DF, a_ddg < ddg & a_packstat > pack & a_sasa > sasa & a_shape > shape)
  if (bb == 'coh'){
    bb_temp <- gsub('.*\\_on\\_([[:alnum:]]{4})\\_A\\_.*', "\\1", subset$description)
  } else { 
    bb_temp <- gsub('.*\\_on\\_([[:alnum:]]{4})\\_A\\_([[:alnum:]]{4})\\_.*', "\\2", subset$description) 
  }
  if (dim(subset)[1] != 0){
    bb_table <- table(bb_temp)
    barplot(bb_table/sum(bb_table), main = paste(name, bb, 'BB table\nwith ', sum(bb_table), 'structures', ' '), las = 2)
  }
}

squeeze_df <- function(DF){
  good.cols <- c(which(names(DF) == 'a_ddg'), which(names(DF) == 'a_sasa'), which(names(DF) == 'a_packstat'), which(names(DF) == 'a_shape'),which(names(DF) == 'coh_temp'), which(names(DF) == 'doc_temp'), which(names(DF) == 'description'))
  new.df <- DF[, good.cols]
  new.df
}

squeeze_df_subset <- function(DF, bb, ddg=DDG, pack=PACK, sasa=SASA, shape=SHAPE){
  subset <- subset(DF, a_ddg < ddg & a_packstat > pack & a_sasa > sasa & a_shape > shape)
  temp = squeeze_df(subset)
  temp
}

plot_identities <- function(DF, name, ddg=-20, pack=0.6, sasa=1300, shape=0.5){
  desc.parts <- ldply(strsplit(DF$description, '_'))
  colnames(desc.parts) <- c('coh_query_sp', 'coh_query', 'doc_query_sp','doc_query', 'on', 'coh_temp', 'A','doc_temp', 'num', 'num2')  
  DF$coh_temp <- desc.parts$coh_temp
  DF$doc_temp <- desc.parts$doc_temp
  DF$coh_query <- paste0(desc.parts$coh_query_sp, desc.parts$coh_query)
  DF$doc_query <- paste0(desc.parts$doc_query_sp, desc.parts$doc_query)
  DF$doc_iden <- DF$coh_iden <- NA
  for (row in 1:length(rownames(DF))){
    DF$doc_iden[row] <- doc.iden.mtrx[ which(rownames(doc.iden.mtrx) == DF$doc_temp[row]), which(rownames(doc.iden.mtrx) == DF$doc_query[row])]
    DF$coh_iden[row] <- coh.iden.mtrx[ which(rownames(coh.iden.mtrx) == DF$coh_temp[row]), which(rownames(coh.iden.mtrx) == DF$coh_query[row])]
  }
  plot(DF$coh_iden, DF$doc_iden, xlim=c(0, 100), ylim=c(0,100), main = name)
  color_by_filters_single_df(DF, what1 = 'coh_iden', what2 = 'doc_iden', ddg = ddg, pack = pack, sasa = sasa, shape = shape)
}

templates_by_filters <- function(DF, ddg=-20, pack=0.6, sasa=1300, shape=0.5){
  best <- subset(DF, a_ddg<-20 & a_packstat>0.6 & a_sasa>1300 & a_shape > 0.5)
  desc.parts <- ldply(strsplit(best$description, '_'))
  colnames(desc.parts) <- c('coh_query_sp', 'coh_query', 'doc_query_sp','doc_query', 'on', 'coh_temp', 'A','doc_temp', 'num', 'num2')  
  best$coh_temp <- desc.parts$coh_temp
  best$doc_temp <- desc.parts$doc_temp
  table = table(best$coh_temp)
  
  
}

filter_result <- function(num_purples_pass, ddg=DDG, pack=PACK, sasa=SASA, shape=SHAPE){
  coh_query_names <- unique(master.df$coh_query)
  doc_query_names <- unique(master.df$doc_query)
  result <- data.frame(coh_query=character(), doc_query=character(), simulation=numeric(), binder=numeric())
  for (coh_i in coh_query_names){
    for (doc_i in doc_query_names){
      temp_binder <- master.df[which( master.df$coh_query == coh_i & master.df$doc_query == doc_i)[1], 36]
      passes <- subset(master.df, coh_query == coh_i & doc_query == doc_i & a_ddg < ddg & a_packstat > pack & a_sasa > sasa & a_shape > shape)
      temp <- data.frame(coh_query=coh_i, doc_query=doc_i, simulation=length(rownames(passes)), binder=temp_binder)
      result <- rbind(result, temp)
    }
  }
  true.pos <- length(rownames(subset(result, binder == 1 & simulation > num_purples_pass)))
  true.neg <- length(rownames(subset(result, binder == 0 & simulation <= num_purples_pass)))
  false.pos <- length(rownames(subset(result, binder == 0 & simulation > num_purples_pass)))
  false.neg <- length(rownames(subset(result, binder == 1 & simulation <= num_purples_pass)))
  print(c('ddg', ddg, 'pack', pack, 'sasa', sasa, 'shape', shape, 'TP', true.pos, 'TN', true.neg, 'FP', false.pos, 'FN', false.neg))
  result
}

print_pdb_names <- function(pdbs){
  msg <- ''
  for (i in pdbs){
    msg <- paste(msg, paste0(i, '\n'))
  }
  msg
}

read_n_clean_scores <- function(PATH_DATA_i, file_i){
  file.df <- data.frame(read.csv2(paste0(PATH_DATA_i, file_i), header = T, sep = '', stringsAsFactors = F))
  file.df <- file.df[ file.df$description != 'description', ]
  file.df <- file.df[ file.df$SCORE. != 'SEQUENCE:', ]
  file.df$a_ddg <- as.numeric(file.df$a_ddg)
  if ('total_score' %in% names(file.df)){
    file.df$total_score <- as.numeric(file.df$total_score)
  } else {
    file.df$total_score <- as.numeric(file.df$score)
  }
  file.df$a_sasa <- as.numeric(file.df$a_sasa)
  file.df$a_stability_score_full <- as.numeric(file.df$a_stability_score_full)
  file.df$a_packstat <- as.numeric(file.df$a_packstat)
  file.df <- file.df[ file.df$a_sasa > 0, ]
  file.df
}

ReadFasta<-function(file) {
  # adopted from http://stackoverflow.com/questions/26843995/r-read-fasta-files-into-data-frame-using-base-r-not-biostrings-and-the-like
  # Read the file line by line
  fasta<-readLines(file)
  # Identify header lines
  ind<-grep(">", fasta)
  # Identify the sequence lines
  s<-data.frame(ind=ind, from=ind+1, to=c((ind-1)[-1], length(fasta)))
  # Process sequence lines
  seqs<-rep(NA, length(ind))
  for(i in 1:length(ind)) {
    seqs[i]<-paste(fasta[s$from[i]:s$to[i]], collapse="")
  }
  # Create a data frame 
  DF<-data.frame(name=gsub(">", "", fasta[ind]), sequence=seqs, stringsAsFactors = F)
  # Return the data frame as a result object from the function
  return(DF)
}

FastasToIdentity <- function(file){
  fastas <- ReadFasta(file)
  iden.mtrx <- matrix(nrow=length(fastas$name), ncol=length(fastas$name))
  colnames(iden.mtrx) <- rownames(iden.mtrx) <- fastas$name
  for(col in colnames(iden.mtrx)){
    for(row in rownames(iden.mtrx)){
      if (which(colnames(iden.mtrx) == col) >= which(rownames(iden.mtrx) == row)){
        iden = pid(pairwiseAlignment(fastas[which(fastas$name == row), 2], fastas[which(fastas$name == col), 2], substitutionMatrix = "BLOSUM62"))
        #       print(c(col, row, iden))
        iden.mtrx[which(fastas$name == row), which(fastas$name == col)] = iden
      }
    }
  }
  iden.mtrx
}