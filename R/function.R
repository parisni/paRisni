trim <- function(txt){
gsub("^[[:blank:]]|[[:blank:]]$","",txt)
}

strExtractAll <- function(pattern, text, capture=F) {
if(capture){
extract <- function(x,y){
      substring(y, attr(x, "capture.start"), attr(x, "capture.start") + attr(x, "capture.length") - 1)
}
}else{
extract <- function(x,y){
	substring(y,x,x+attr(x,"match.length")-1)
}
}
mapply(function(x, y) {
      if (is.na(x) || length(x) == 1 && x == -1) {
      NA_character_
      } else {
      extract(x,y)
      }
    }, gregexpr(pattern, text, perl=T), text, SIMPLIFY = FALSE)
}

rmRGEX <- function(txt){
gsub("([$?.*+^()|]|[[]|[]])","\\\\\\1",txt)
}


