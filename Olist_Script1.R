rm(list=ls(all=T))
setwd ("C:/Users/matilde.pessoa/Documents/ANALISES_OLIST")

'************************************************************************************************************
LEITURA E MANIPULAÇÃO DOS ARQUIVOS olist_orders_dataset E olist_order_items_dataset
*************************************************************************************************************'
dados_orders <- read.table("orders.csv",na.string=" ",header = T,sep=",")
dados_order_items <- read.table("order_items.csv",na.string=" ",header = T,sep=",")

orders_itens<-merge(dados_order_items,dados_orders,by="order_id",all=T)
orders_itens<-subset(orders_itens,!is.na(orders_itens$product_id))#775 pedidos sem identificação do produto
#************************************************************************************************************
n_orders<-orders_itens
#************************************************************************************************************
'DATAS'
'compra'
n_orders$data_compra <-strptime(n_orders$order_purchase_timestamp,"%Y-%m-%d")
n_orders$ano_compra<-format(n_orders$data_compra, format = "%Y")
n_orders$ano_mes_compra<-format(n_orders$data_compra, format = "%Y%m")
'entrega'
n_orders$data_entrega <-strptime(n_orders$order_delivered_customer_date,"%Y-%m-%d")
n_orders$ano_entrega<-format(n_orders$data_entrega, format = "%Y")
#***********************************************************************************************************

'descrção do número de itens/pedido'
n_orders<-n_orders[order(n_orders$order_id, -n_orders$order_item_id),]
n_itens<-n_orders[!duplicated(n_orders$order_id),]
table(n_itens$order_item_id)

'descrição n pedidos e entregas / ano'
n_pedidos<-n_orders[!duplicated(n_orders$order_id),]
table(n_pedidos$ano_compra)

n_entregas<-subset(n_pedidos,n_pedidos$order_status=="delivered")
table(n_entregas$ano_compra)

'descrição do status do pedido'
status<-data.frame(table(n_pedidos$order_status))
status$Freq_relativa<-(status$Freq/(sum(status$Freq)))*100

'vendas e faturamento por mes/ano'
n_orders$n=1
vendas_faturamento_ano_mes<-aggregate(cbind(n,price)~ano_mes_compra,n_orders,sum)
'arquivo gráficos em excel'
write.table(vendas_faturamento_ano_mes,"C:/Users/matilde.pessoa/Documents/ANALISES_OLIST/ARQUIVOS/vendas_ano_mes.txt",quote=F,row.names=F,col.names=T)#excel'

'..............................................................................................................'

'tempos entre compra e entrega em dias dos pedidos'
n_pedidos$hora_data_compra <- as.Date(n_pedidos$order_purchase_timestamp,"%Y-%m-%d %H:%M:%S")
n_pedidos$data_compra <- as.Date(n_pedidos$order_purchase_timestamp,"%Y-%m-%d")
n_pedidos$hora_data_aprovacao <- as.Date(n_pedidos$order_approved_at,"%Y-%m-%d %H:%M:%S")
n_pedidos$hora_data_distrib <- as.Date(n_pedidos$order_delivered_carrier_date,"%Y-%m-%d %H:%M:%S")
n_pedidos$hora_data_cliente <- as.Date(n_pedidos$order_delivered_customer_date,"%Y-%m-%d %H:%M:%S")
n_pedidos$hora_data_estimada <- as.Date(n_pedidos$order_estimated_delivery_date,"%Y-%m-%d %H:%M:%S")

'dacompra a aprovação'
n_pedidos$diff1<-as.numeric(n_pedidos$hora_data_aprovacao - n_pedidos$hora_data_compra)
summary(n_pedidos$diff1)
'da compra a distribuidora'
n_pedidos$diff2<-as.numeric(n_pedidos$hora_data_distrib - n_pedidos$hora_data_compra)
summary(n_pedidos$diff2)#valores negativos - inconcistência
'da distribuidora ao cliente'
n_pedidos$diff3<-as.numeric(n_pedidos$hora_data_cliente - n_pedidos$hora_data_distrib)
summary(n_pedidos$diff3)#valores negativos - inconcistência
'da compra a entrega ao cliente'
n_pedidos$diff4<-as.numeric(n_pedidos$hora_data_cliente - n_pedidos$hora_data_compra)
summary(n_pedidos$diff4)#valores negativos - inconcistência
'data real de entrega x data estimada'
n_pedidos$diff5<-as.numeric(n_pedidos$hora_data_cliente - n_pedidos$hora_data_estimada)
table(n_pedidos$diff5==0)
table(n_pedidos$diff5>0)
table(n_pedidos$diff5<0)
'.............................................................................................................'

base_order_itens<-subset(n_orders, select = c(order_id, order_item_id,product_id,seller_id,customer_id,
                                              ano_compra,ano_mes_compra,price,freight_value,customer_id, 
                                              order_status, data_compra))

write.table(base_order_itens,"C:/Users/matilde.pessoa/Documents/ANALISES_OLIST/base_order_itens.txt",quote=T,row.names=F,col.names=T)
