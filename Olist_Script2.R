rm(list=ls(all=T))
setwd ("C:/Users/matilde.pessoa/Documents/ANALISES_OLIST")

'************************************************************************************************************
LEITURA E MANIPULAÇÃO DOS ARQUIVOS base_order_itens e olist_customers_dataset
*************************************************************************************************************'
dados_order_itens<- read.table("base_order_itens.txt",na.string=" ",header = T,sep=" ")
dados_customers <- read.table("customers.csv",na.string=" ",header = T,sep=",")
dados_produtos <- read.table("products.csv",na.string=" ",header = T,sep=",")
order_itens_customers<-merge(dados_order_itens,dados_customers,by="customer_id",all.x=T)
order_itens_customers_products<-merge(order_itens_customers,dados_produtos,by="product_id",all.x=T)
#************************************************************************************************************
n_clientes<- order_itens_customers_products
#*************************************************************************************************************

'criando regioes - clientes'
table(n_clientes$customer_state)
n_clientes$regiao<-""
n_clientes$regiao<-ifelse(n_clientes$customer_state%in%
                             c("MG","SP","RJ","ES"),"SUDESTE",n_clientes$regiao)

n_clientes$regiao<-ifelse(n_clientes$customer_state%in%
                             c("PR","SC","RS"),"SUL",n_clientes$regiao)

n_clientes$regiao<-ifelse(n_clientes$customer_state%in%
                             c("GO","MT","MS", "DF"),"CENTRO-OESTE",n_clientes$regiao)

n_clientes$regiao<-ifelse(n_clientes$customer_state%in%
                             c("AL","BA","CE","PB","PE","PI","RN","SE","MA"),"NORDESTE",n_clientes$regiao)

n_clientes$regiao<-ifelse(n_clientes$customer_state%in%
                             c("AM","AP","PA","RO","RR","TO", "AC"),"NORTE",n_clientes$regiao)
#número de clientes/região
table(n_clientes$regiao)

#*******************************************************************************************************
'PRODUTOS E FATURAMENTOS ANUAIS/REGIAO E POR CATEGORIA'
n_clientes$n=1

P_16<-subset(n_clientes,n_clientes$ano_compra=="2016")
P_17<-subset(n_clientes,n_clientes$ano_compra=="2017")
P_18<-subset(n_clientes,n_clientes$ano_compra=="2018")

'vendas/regiao/ano'
p_reg_16<-aggregate(cbind(n,price)~regiao,P_16,sum)
p_reg_16<-p_reg_16[order(p_reg_16$regiao,-p_reg_16$n),]
faturamento_16<-sum(p_reg_16$price)

p_reg_17<-aggregate(cbind(n,price)~regiao,P_17,sum)
p_reg_17<-p_reg_17[order(p_reg_17$regiao,-p_reg_17$n),]
faturamento_17<-sum(p_reg_17$price)

p_reg_18<-aggregate(cbind(n,price)~regiao,P_18,sum)
p_reg_18<-p_reg_18[order(p_reg_18$regiao,-p_reg_18$n),]
faturamento_18<-sum(p_reg_18$price)

'faturamento por região/ano'
fat_reg_16<-p_reg_16[order(-p_reg_16$price),]
fat_reg_17<-p_reg_17[order(-p_reg_17$price),]
fat_reg_18<-p_reg_18[order(-p_reg_18$price),]
'............................................................................................................'
#..........................................................................................................
'vendas anuais/regiao/categoria'
p_reg_cat_16<-aggregate(cbind(n,price)~regiao+product_category_name,P_16,sum)
p_reg_cat_16<-p_reg_cat_16[order(p_reg_cat_16$regiao,-p_reg_cat_16$n),]

p_reg_cat_17<-aggregate(cbind(n,price)~regiao+product_category_name,P_17,sum)
p_reg_cat_17<-p_reg_cat_17[order(p_reg_cat_17$regiao,-p_reg_cat_17$n),]

p_reg_cat_18<-aggregate(cbind(n,price)~regiao+product_category_name,P_18,sum)
p_reg_cat_18<-p_reg_cat_18[order(p_reg_cat_18$regiao,-p_reg_cat_18$n),]

'faturamento por região/categoria/ano'
fat_reg_cat_16<-p_reg_cat_16[order(p_reg_cat_16$regiao,-p_reg_cat_16$price),]
fat_reg_cat_17<-p_reg_cat_17[order(p_reg_cat_17$regiao,-p_reg_cat_17$price),]
fat_reg_cat_18<-p_reg_cat_18[order(p_reg_cat_18$regiao,-p_reg_cat_18$price),]
#...............................................................................................
'categoria/região'
cat_reg<-aggregate(n~regiao+product_category_name,n_clientes,sum)
cat_reg<-cat_reg[order(cat_reg$regiao, -cat_reg$n),]
#gráficos power bi
write.table(cat_reg,"C:/Users/matilde.pessoa/Documents/ANALISES_OLIST/ARQUIVOS/categ_regiao.txt",row.names=T,col.names=T)#excel'


















