rm(list=ls(all=T))
setwd ("C:/Users/matilde.pessoa/Documents/ANALISES_OLIST") 

#LEITURA DOS ARQUIVOS DE DADOS
#********************************************************************************************************
#arquivo olist_payments_dataset
dados_payments <- read.table("order_payments.csv",na.string=" ",header = T,sep=",")
dados_payments<-dados_payments[order(dados_payments$order_id,dados_payments$payment_sequential),]
dados_payments2<-dados_payments
#*********************************************************************************************************
'numero de pagamentos realizados por mais de uma forma'
dados_payments2$n=1
pay_types <- reshape(dados_payments2[,c("order_id","payment_type","n")],timevar="payment_type",idvar="order_id",direction="wide")
pay_types[is.na(pay_types)] <- 0
pay_types$s <- apply(pay_types[,2:5],1,sum)
table(pay_types$s)
round(prop.table(table(pay_types$s)),5)

dados_payments2$ind<-paste0(dados_payments2$payment_sequential,"-",dados_payments2$payment_type)
table(dados_payments2$ind)
payments<-aggregate(n~payment_sequential+payment_type,dados_payments2,sum)
