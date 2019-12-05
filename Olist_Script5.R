rm(list=ls(all=T))
setwd ("C:/Users/matilde.pessoa/Documents/ANALISES_OLIST") 

#arquivo olist_orders_dataset
#...........................................................................................................
dados_orders <- read.table("orders.csv",na.string=" ",header = T,sep=",")
dados_orders<-dados_orders[order(dados_orders$customer_id),]
dados_orders1<-subset(dados_orders, select=c(order_id, order_status, 
                                             order_purchase_timestamp,order_delivered_customer_date))
'tempo de entrega em dias'
dados_orders1$hora_data_compra <- as.Date(dados_orders1$order_purchase_timestamp,"%Y-%m-%d %H:%M:%S")
dados_orders1$hora_data_entrega <- as.Date(dados_orders1$order_delivered_customer_date,"%Y-%m-%d %H:%M:%S")

dados_orders1$data_compra <-strptime(dados_orders1$order_purchase_timestamp,"%Y-%m-%d")
dados_orders1$ano_compra<-format(dados_orders1$data_compra, format = "%Y")

dados_orders1$temp_entrega<-as.numeric(dados_orders1$hora_data_entrega - dados_orders1$hora_data_compra)
summary(dados_orders1$temp_entrega)
#...........................................................................................................

#arquivo olist_reviews_dataset
#...........................................................................................................
dados_order_reviews <- readLines("order_reviews.csv")
#regexpr(",,,,,,$",dados_order_reviews[14:15])
table(as.numeric(regexpr(",,,,,,$",dados_order_reviews))==-1)

order_reviews <- dados_order_reviews
n <- length(order_reviews)
i <- 1
while(i<=n){
  print(cat(paste0("####   ",i,"   ###\n")))
  if(as.numeric(regexpr(",,,$" ,order_reviews[i]))==-1){
    order_reviews[i] <- paste0(order_reviews[i],order_reviews[i+1])
    order_reviews <- order_reviews[-(i+1)]
    n <- n - 1
    #i <-  i - 1
  }else{
    i <- i + 1
  }
  }


table(as.numeric(regexpr(",,,,,,$",order_reviews))!=-1)==length(order_reviews)


table(as.numeric(regexpr(",,,,,,$",order_reviews))!=-1)
fim <- nchar(order_reviews)
table(substr(order_reviews,fim-5,fim))

order_reviews2 <- sub(",,,,,,$","",order_reviews)
order_reviews2 <- sub(",,,,,$","",order_reviews2)
order_reviews2 <- sub(",,,,$","",order_reviews2)
order_reviews2 <- sub(",,,$","",order_reviews2)
fim2 <- nchar(order_reviews2)
table(substr(order_reviews2,fim2,fim2))

gregexpr(",",order_reviews2[6])

dados <- data.frame(review_id=rep(NA,length(order_reviews2)-1),order_id=rep(NA,length(order_reviews2)-1),
                    review_score=rep(NA,length(order_reviews2)-1),
                    review_creation_date=rep(NA,length(order_reviews2)-1),
                    review_answer_timestamp=rep(NA,length(order_reviews2)-1),comentario=rep(NA,length(order_reviews2)-1))

for(j in 2:length(order_reviews2)){
  cat(paste0("####    ",j,"    ####\n"))
  virgula <- unlist(gregexpr(",",order_reviews2[j]))
  dados$review_id[j-1] <- substr(order_reviews2[j],1,virgula[1]-1)
  dados$order_id[j-1] <- substr(order_reviews2[j],virgula[1]+1,virgula[2]-1)
  dados$review_score[j-1] <- substr(order_reviews2[j],virgula[2]+1,virgula[3]-1)
  dados$review_answer_timestamp[j-1] <- substr(order_reviews2[j],virgula[length(virgula)]+1,nchar(order_reviews2[j]))
  dados$review_creation_date[j-1] <- substr(order_reviews2[j],virgula[length(virgula)-1]+1,virgula[length(virgula)]-1)
  dados$comentario[j-1] <- substr(order_reviews2[j],virgula[3]+1,virgula[length(virgula)-1]-1)
}
#..........................................................................................................................
#JUNTANDO AQRUIVOS 
orders_review<-merge(dados_orders1,dados,by="order_id",all.y=T)
#..........................................................................................................................
orders_review$n=1
orders_review$review_score <- gsub(" ","",orders_review$review_score)
scor_ano<-aggregate(n~review_score+ano_compra,orders_review,sum)

orders_review$ano_mes_compra <- format(orders_review$hora_data_compra,"%Y%m")
scor_ano_mes <-aggregate(n~review_score+ano_mes_compra,orders_review,sum)

write.table(scor_ano_mes,"C:/Users/matilde.pessoa/Documents/ANALISES_OLIST/ARQUIVOS/scores.txt",row.names=T,col.names=T)#excel'

d_cor <- subset(orders_review,!is.na(orders_review$temp_entrega))
cor(as.numeric(d_cor$review_score),d_cor$temp_entrega)
'............................................................................................................................'

'cancelados'
cancelados<- subset(orders_review, orders_review$order_status=="canceled")






