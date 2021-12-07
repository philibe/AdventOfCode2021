suppressMessages(library (readr))
suppressMessages(library(dplyr))
suppressMessages(library(DescTools))

datas <- suppressMessages(dplyr::as_tibble(readr::read_csv("../Day01/input.txt")))
colnames(datas)<-c("col0")

func1 <-function (df, colparam,  rownparam ) {
    df  %>% ungroup() %>% mutate(
            {{colparam}}:=if_else(if_else(-1+ceiling (.data[[rownparam]]%%4)==0L |.data[[rownparam]]==1,1L,0L)==1L,rown1,0L),
           {{colparam}}:=if_else(lag(.data[[colparam]],n=1,default=0L)!=0L,lag(.data[[colparam]],n=1,default=0L),.data[[colparam]]),
           {{colparam}}:=if_else(lag(.data[[colparam]],n=1,default=0L)!=0L,lag(.data[[colparam]],n=1,default=0L),.data[[colparam]]),
    )
}

answer<-(      
  (datas
   %>% mutate (
     rown1=row_number(),
     rown2=if_else(rown1-1L<0 ,0L,rown1-1L),
     rown3=if_else(rown2-1L<0 ,0L,rown2-1L),
     rown4=if_else(rown3-1L<0 ,0L,rown3-1L),
   ) 
  )
  %>% func1 (.,"col1","rown1")
  %>% func1 (.,"col2","rown2")
  %>% func1 (.,"col3","rown3")
  %>% func1 (.,"col4","rown4")
  %>% select (-rown2,-rown3,-rown4)  
)


df_max_val_col<-(answer 
  %>% filter(rown1>max(answer$rown1)-3)
 %>% filter(col1==max(answer$col1)  |col2==max(answer$col2) | col3==max(answer$col3) |col4==max(answer$col4)  )  
  %>% mutate (
      col1=if_else(col1==sum(col1)/3,col1,0L),
       col2=if_else(col2==sum(col2)/3,col2,0L),     
       col3=if_else(col3==sum(col3)/3,col3,0L),    
       col4=if_else(col4==sum(col4)/3,col4,0L),    
      max_val_col=max(col1,col2,col3,col4)
   )
) 

prev=0 
howmany=0
for (numligne in 1:unique(df_max_val_col$max_val_col))  {
  rep=0  
  rep=rep+DescTools::Coalesce((answer %>% filter (UQ(rlang::sym("col1") )==numligne)  %>% group_by(UQ(rlang::sym("col1") )) %>% summarise(rep=sum(col0),.groups="drop_last"))$rep,0)
  rep=rep+DescTools::Coalesce((answer %>% filter (UQ(rlang::sym("col2") )==numligne)  %>% group_by(UQ(rlang::sym("col2") )) %>% summarise(rep=sum(col0),.groups="drop_last"))$rep,0)
  rep=rep+DescTools::Coalesce((answer %>% filter (UQ(rlang::sym("col3") )==numligne)  %>% group_by(UQ(rlang::sym("col3") )) %>% summarise(rep=sum(col0),.groups="drop_last"))$rep,0)
  rep=rep+DescTools::Coalesce((answer %>% filter (UQ(rlang::sym("col4") )==numligne)  %>% group_by(UQ(rlang::sym("col4") )) %>% summarise(rep=sum(col0),.groups="drop_last"))$rep,0)  
  if (rep >prev) howmany=howmany+1
  prev=rep
} 

print(howmany)