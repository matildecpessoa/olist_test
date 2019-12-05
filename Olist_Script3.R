rm(list=ls(all=T))
setwd ("C:/Users/matilde.pessoa/Documents/ANALISES_OLIST")

'************************************************************************************************************
LEITURA E MANIPULAÇÃO DOS ARQUIVOS base_order_itens e olist_products_dataset
*************************************************************************************************************'
dados_order_itens<- read.table("base_order_itens.txt",na.string=" ",header = T,sep=" ")
dados_products <- read.table("products.csv",na.string=" ",header = T,sep=",")
order_itens_products<-merge(dados_order_itens,dados_products,by="product_id",all.x=T)
#************************************************************************************************************
items_products<-order_itens_products
#************************************************************************************************************

'número de produtos vendidos por ano' - 'produtos mais vendidos por ano'
items_products$n<-1
Top_2016<-subset(items_products,items_products$ano_compra=="2016")
Top_prod_2016<-aggregate(cbind(n,price)~product_id+product_category_name,Top_2016,sum)
Top_prod_2016<-Top_prod_2016[order(-Top_prod_2016$n),]
n_16<-sum(Top_prod_2016$n)

Top_2017<-subset(items_products,items_products$ano_compra=="2017")
Top_prod_2017<-aggregate(cbind(n,price)~product_id+product_category_name,Top_2017,sum)
Top_prod_2017<-Top_prod_2017[order(-Top_prod_2017$n),]
n_17<-sum(Top_prod_2017$n)

Top_2018<-subset(items_products,items_products$ano_compra=="2018")
Top_prod_2018<-aggregate(cbind(n,price)~product_id+product_category_name,Top_2018,sum)
Top_prod_2018<-Top_prod_2018[order(-Top_prod_2018$n),]
n_18<-sum(Top_prod_2018$n)

'faturamento por ano / produtos' - 'o mais vendido pode não ser o de maior faturamento'
Top_prod_2016<-Top_prod_2016[order(-Top_prod_2016$price),]
Top_prod_2017<-Top_prod_2017[order(-Top_prod_2017$price),]
Top_prod_2018<-Top_prod_2018[order(-Top_prod_2018$price),]
'...............................................................................................................'
'.................................................................................................................'
'categorias mais vendidas'
n_categorias<- data.frame(table(dados_products$product_category_name))
n_categorias<-n_categorias[order(-n_categorias$Freq),]
write.table(n_categorias,"C:/Users/matilde.pessoa/Documents/ANALISES_OLIST/ARQUIVOS/categorias.txt",quote=F,row.names=F,col.names=T)

Top_cat_2016<-aggregate(cbind(n,price)~product_category_name,Top_prod_2016,sum)
Top_cat_2016<-Top_cat_2016[order(-Top_cat_2016$n),]

Top_cat_2017<-aggregate(cbind(n,price)~product_category_name,Top_prod_2017,sum)
Top_cat_2017<-Top_cat_2017[order(-Top_cat_2017$n),]

Top_cat_2018<-aggregate(cbind(n,price)~product_category_name,Top_prod_2018,sum)
Top_cat_2018<-Top_cat_2018[order(-Top_cat_2018$n),]

'faturamento por ano/categorias'
Top_cat_2016<-Top_cat_2016[order(-Top_cat_2016$price),]
Top_cat_2017<-Top_cat_2017[order(-Top_cat_2017$price),]
Top_cat_2018<-Top_cat_2018[order(-Top_cat_2018$price),]

write.table(Top_cat_2016,"C:/Users/matilde.pessoa/Documents/ANALISES_OLIST/ARQUIVOS/Top_cat_2016.txt",row.names=T,col.names=T)#excel'
write.table(Top_cat_2017,"C:/Users/matilde.pessoa/Documents/ANALISES_OLIST/ARQUIVOS/Top_cat_2017.txt",row.names=T,col.names=T)#excel'
write.table(Top_cat_2018,"C:/Users/matilde.pessoa/Documents/ANALISES_OLIST/ARQUIVOS/Top_cat_2018.txt",row.names=T,col.names=T)#excel'
'.....................................................................................................................................'




