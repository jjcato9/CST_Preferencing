job_filter <- function(data,input){
  df <- data
  if('ALL' %in% input){
    df <- df
  }
  
  if('ENT' %in% input){
    df <- df %>% filter(ENT == 1)
  }
  
  if('Breast' %in% input){
    df <- df %>% filter(BRE == 1)
  }
  
  if('Cardiothoracics' %in% input){
    df <- df %>% filter(CTX == 1)
  }
  
  if('Oral & Maxillofacial Surgery' %in% input){
    df <- df %>% filter(OMFS == 1)
  }
  
  if('Trauma & Orthopaedics' %in% input){
    df <- df %>% filter(TO == 1)
  }
  
  if('General Surgery' %in% input){
    df <- df %>% filter(GSX == 1)
  }
  
  if('Plastic Surgery' %in% input){
    df <- df %>% filter(PLA == 1)
  }
  
  if('Paediatric Surgery' %in% input){
    df <- df %>% filter(PAED == 1)
  }
  
  if('Urology' %in% input){
    df <- df %>% filter(URO == 1)
  }
  
  if('Vascular' %in% input){
    df <- df %>% filter(VAS == 1)
  }
  
  if('ITU' %in% input){
    df <- df %>% filter(ICM == 1)
  }
  
  return(df)
}
