#| SECTOR        RGB                                 HEX
#| All           (0,   114, 182, maxColorValue=255)  #0072B6
#| Basic_needs   (199,  94,  34, maxColorValue=255)  #C75E22
#| Education     (169,  19,  20, maxColorValue=255)  #A91314
#| Food_security ( 47, 110,  31, maxColorValue=255)  #2F6E1F
#| Health        (194,  74, 121, maxColorValue=255)  #C24A79
#| Livelihoods   NA
#| Protection    (  0, 130, 185, maxColorValue=255)  #0082B9
#| Shelter       (246, 160,   0, maxColorValue=255)  #F6A000
#| WASH          (120,  76,  74, maxColorValue=255)  #784C4A

load_sector_colors <- function(){

  sectors <-  c("All","Basic_needs","Education","Food_security","Health","Livelihoods","Protection","Shelter","WASH")
  
  rgb_code <- c("(0,   114, 182, maxColorValue=255)","(199,  94,  34, maxColorValue=255)","(169,  19,  20, maxColorValue=255)",
                "( 47, 110,  31, maxColorValue=255)","(194,  74, 121, maxColorValue=255)",NA,
                "(  0, 130, 185, maxColorValue=255)","(246, 160,   0, maxColorValue=255)","(120,  76,  74, maxColorValue=255)")
  
  hex_code <- c("#0072B6","#C75E22","#A91314","#2F6E1F","#C24A79","","#0082B9","#F6A000","#784C4A")
  
  sector_colors <- data.frame(sectors, rgb_code, hex_code)
}


