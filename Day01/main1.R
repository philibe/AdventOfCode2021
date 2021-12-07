suppressMessages(library (readr))
suppressMessages(library(dplyr))
suppressMessages(library(DescTools))

datas <- suppressMessages(dplyr::as_tibble(readr::read_csv("input.txt")))
colnames(datas)<-c("col0")


answer<-(datas
         %>% mutate (
           delta=lag(col0,n=1,  default = 0L),
           sup=if_else(col0>delta,1L,0L)
         )
)
# answer

answer<-(answer
         %>% summarise(
           nb_sup=sum(sup, na.rm= TRUE)
         )
)


print(answer)

