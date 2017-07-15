#' Processing the data by removing zeros in 80% of samples and and log2 transform + 1
#' processDataAfterTPM(infile,perc)
#' @export
#' 
#' 

processDataAfterTPM <- function(infile,perc){
  if(missing(infile)){stop("Please give the input file in .txt format.\n")}
  if(missing(perc)){stop("Please give the percentage value so that those percentage across columns having zeros will be removed.\n")}

############################  
filename = basename(infile)
fname = strsplit(filename,".txt")

per = perc
percentage = as.numeric(per)/100

cat("Reading your expression dataset \n\n")
data = read.delim(infile,header = TRUE,row.names=1); dim(data)  

no_zero_in_data=data[rowSums(data != 0)/ncol(data) >= percentage, ]
dat = log2(no_zero_in_data + 1)
dd=data.frame(rownames(dat),dat)
colnames(dd)<-c("Genes",colnames(dd)[-1])
log2_add_1_no_zero_in_dataFile = paste0(fname,"_with_no_zero_",perc,"_log2_plus_1_processed.txt",sep="")
write.table(dd,log2_add_1_no_zero_in_dataFile,sep="\t",row.names = FALSE)
cat(" Your processd TPM data is in your working directory \n\n")
}
