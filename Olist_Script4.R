rm(list=ls(all=T))
setwd ("C:/Users/matilde.pessoa/Documents/ANALISES_OLIST") 

#LEITURA DOS ARQUIVOS DE DADOS
#********************************************************************************************************
#arquivo olist_sellers_dataset e base_order_itens e products
dados_sellers <- read.table("sellers.csv",na.string=" ",header = T,sep=",")
dados_order_itens<- read.table("base_order_itens.txt",na.string=" ",header = T,sep=" ")
dados_produtos <- read.table("products.csv",na.string=" ",header = T,sep=",")

#dados_sellers<-dados_sellers[!duplicated(dados_sellers$seller_id),]
#dados_sellers<-dados_sellers[order(dados_sellers$seller_id),]
n_lojistas<-dados_sellers

#CRIANDO REGIOES - LOjISTAS 
'regioes - lojistas'
n_lojistas$regiao_loja<-""
n_lojistas$regiao_loja<-ifelse(n_lojistas$seller_state%in%
                             c("MG","SP","RJ","ES"),"SUDESTE",n_lojistas$regiao_loja)

n_lojistas$regiao_loja<-ifelse(n_lojistas$seller_state%in%
                             c("PR","SC","RS"),"SUL",n_lojistas$regiao_loja)

n_lojistas$regiao_loja<-ifelse(n_lojistas$seller_state%in%
                             c("GO","MT","MS", "DF"),"CENTRO-OESTE",n_lojistas$regiao_loja)

n_lojistas$regiao_loja<-ifelse(n_lojistas$seller_state%in%
                             c("AL","BA","CE","PB","PE","PI","RN","SE","MA"),"NORDESTE",n_lojistas$regiao_loja)

n_lojistas$regiao_loja<-ifelse(n_lojistas$seller_state%in%
                             c("AM","AP","PA","RO","RR","TO", "AC"),"NORTE",n_lojistas$regiao_loja)

#*******************************************************************************************************
order_sellers<-merge(n_lojistas,dados_order_itens,by="seller_id",all.y=T)
order_sellers_products<-merge(order_sellers,dados_produtos,by="product_id",all.x=T)

order_sellers<-order_sellers_products
#*******************************************************************************************************

'lojistas por ano'
order_sellers$ind<-paste0(order_sellers$seller_id,"-",order_sellers$ano_compra)
order_sellers<-order_sellers[order(order_sellers$ind),]

order_sellers2<-order_sellers[!duplicated(order_sellers$ind),]
order_sellers2$n=1
lojistas_ano<-aggregate(n~ano_compra,order_sellers2,sum)

'lojistas por região'
order_sellers$ind1<-paste0(order_sellers$seller_id,"-",order_sellers$regiao_loja)
order_sellers<-order_sellers[order(order_sellers$ind1),]
order_sellers3<-order_sellers[!duplicated(order_sellers$ind1),]
order_sellers3$n=1
lojistas_regiao<-aggregate(n~regiao_loja,order_sellers3,sum)
lojistas_regiao$FreqRelativa<-(lojistas_regiao$n/(sum(lojistas_regiao$n)))*100

' lojista de maior número de vendas e faturamento'
order_sellers$n=1
vendas_lojista<-aggregate(cbind(n,price)~seller_id,order_sellers,sum)
vendas_lojista<-vendas_lojista[order(-vendas_lojista$n),]#maior número de itens vendidos
vendas_lojista<-vendas_lojista[order(-vendas_lojista$price),]#maior faturamento

' lojista de maior faturamento por categoria'
vendas_lojista_cat<-aggregate(cbind(n,price)~seller_id+product_category_name,order_sellers,sum)
vendas_lojista_cat<-vendas_lojista_cat[order(-vendas_lojista_cat$price),]#maior número de itens vendidos
vendas_lojista_cat<-subset(vendas_lojista_cat,vendas_lojista_cat$product_category_name!="")
'......................................................................................................'


#vendas_lojista_cat<-subset(vendas_lojista_cat,!is.na(vendas_lojista_cat$product_category_name))





#*******************************************************************************************************************