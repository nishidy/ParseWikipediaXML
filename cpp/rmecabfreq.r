library(RMeCab)

args <- commandArgs()
tmpfile <- args[6]
title <- args[7]
# [1] "C:\\Program Files\\R\\R-2.8.1\\bin\\Rterm.exe"
# [2] "--slave"
# [3] "--no-restore"
# [4] "--file=file_names.R"
# [5] "--args"

e<-try(Data<-RMeCabFreq(tmpfile), silent=FALSE)
if(class(e)=="try-error"){
	message("[Error: RMeCabFreq failed. tmpfile is ",tmpfile," and title is ",title,".]")
}

Data <- Data[ ( Data$Info1 == "動詞" & Data$Info2 == "自立" ) |
		Data$Info1 == "名詞" & ( Data$Info2 == "一般" |
					 Data$Info2 == "固有名詞" |
					 Data$Info2 == "形容動詞語幹" ) |
					 #Data$Info2 == "サ変接続" ) |
		Data$Info1 == "形容詞", ]

# Data$Info2 == "サ変接続" may include sequences of only symbols while it certainly includes important words...
# That's my motivation to implement check_all_symbols().

Data <- Data[,-2:-3]

data.list <- function(d)
{
        cat(paste(t(d), c(rep(" ", ncol(d)-1), "\n")), sep="")
}

if(is.data.frame(Data)){
	e<-try(data.list(Data), silent=FALSE)
	if(class(e)=="try-error"){
		message("[Error: data.list(Data) failed. Data is ",Data,", tmpfile is ",tmpfile," and title is ",title,".]")
	}
}else{
	message("[Warning: This is not data frame. Data is \"",Data,"\", tmpfile is ",tmpfile," and title is ",title,".]")
}

quit()

