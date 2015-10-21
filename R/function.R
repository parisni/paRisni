# removes first/lasts spaces of any kind
trim <- function(txt){
    gsub("^[[:blank:]]+|[[:blank:]]+$","",txt)
}

# removes first/lasts spaces of any kind
trimAll <- function(txt){
    gsub("^[[:cntrl:][:blank:]]+|[[:cntrl:][:blank:]]+$","",txt)
}

# fast regex extraction pure c/c++ functions
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

# safes string to be included in regex
rmRGEX <- function(txt){
    gsub("([$?.*+^()|]|[[]|[]])","\\\\\\1",txt)
}

#returns the path from where the script file is
get_script_path <- function() {
        getPath<-function(txt){gsub("[^/\\]+$","",txt)}
    cmdArgs = commandArgs(trailingOnly = FALSE)
        needle = "--file="
        match = grep(needle, cmdArgs)
            if (length(match) > 0) {
                        # Rscript
                        return(getPath(normalizePath(sub(needle, "", cmdArgs[match]))))
            } else {
                        ls_vars = ls(sys.frames()[[1]])
                    if ("fileName" %in% ls_vars) {
                                    # Source'd via RStudio
                                    return(getPath(normalizePath(sys.frames()[[1]]$fileName)))
                            } else {
                                            # Source'd via R console
                                            return(getPath(normalizePath(sys.frames()[[1]]$ofile)))
                                    }
                        }
}

#tests if vector is empty
empty <- function(vect){
        is.na( vect ) | vect == ""
}

#reads txt flat file
read.txt <- function(file)
{
    readChar(file,file.info(file)$size)
}

#removes latex trash
latexClearFile<-function(rep){
    fich<-list.files(path=rep,pattern=".*aux$|.*log$|.*toc$|.*lot$|.*mtc0$|.*out$|.*maf$|.*sw.$")
    res<-sapply(file.path(rep,fich),file.remove)
}

#
#NPS : remplace les  caractères accentués
accentRemove<-function(string){
            chartr("àáâãäçèéêëìíîïñòóôõöùúûüýÿÀÁÂÃÄÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ"
                                          ,"aaaaaceeeeiiiinooooouuuuyyAAAAACEEEEIIIINOOOOOUUUUY"
                                                                     ,string)
}

# ajoute des 0 devant
zeroPrefix<-function(n,vect){
    sprintf( paste0("%0",n,"d"), vect)
}
