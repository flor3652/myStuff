### txtme Version 0.9 (with setting to and from) ----
# note that current settings are made to send from a g-mail account. If sending from another host, these will need to be changed. Additionally, this is currently set to sent to a t-mobile carrier. Check out what ending you need for your carrier here: http://www.emailtextmessages.com/

#' @title Text Me
#' 
#' @description 
#' This function is designed to send a user a text message (currently, settings are designed to do this from a G-Mail account) to a designated phone number (current carrier is set to T-Mobile). It will store the password for a set amount of time (defaulting to 10 minutes) before requiring it to be re-entered
#' 
#' @param txt The message to text
#' @param sbj The subject of the message
#' @param resetfrom Logical. If true, resets "from"
#' @param resetto Logical. If true, resets "to"
#' @param resetpw Logical. If true, resets the password
#' @param resetall Logical. If true, reset "to", "from", and password
#' @param defcar This the ending for the carrier you want to text
#' @param keeptime Numeric (with character option). Specify how long you would like your password to be stored (in minutes). Note that, when reset, your password will need to be reset. Default is 10 minutes. Entry of text string "forever" will keep password with no time-limit.
#' 
#' @details This fuunction uses the memoise package to store passwords as variables in the global environment. This may or may not be desireable, but was deemed most appropriate of potential options. Please see the memoise documentation for details on this storage (read: due to my ignorance, I really don't know). Note that, due to trial and error, passwords should be saved even if console is closed (likely saved in workspace). Again, this may be undesireable if \code{keeptime} is set to "forever", so it is recommended to build in password resets into chunks using this function if using code{keeptime="forever"}. 
#' 
#' The texting is truly done through sending an e-mail to a cell-phone's e-mail address: number@carrierinfo.blah... For those looking to change the carrier, please visit the following website to find your carrier's information: http://www.emailtextmessages.com/
#' 
#' @seealso \code{\link[memoise]{memoise}}, \code{mailR}
#' 
#' @author Michael Floren
#' 

txtme <- function(txt="", sbj="", resetfrom=FALSE, resetto=FALSE, resetpw=FALSE, resetall=FALSE, def.car="@tmomail.net", keeptime=NULL){
  
  #defaults not defined as parameters:
  def.keeptime <- 10 #default keeptime in minutes
  def.from <- "@gmail.com" #default "from" ending: needs to be G-mail for now. Can be moved to parameter if/when multiple hosts are supported
  
  # downloading and loading packages #for me
  
#   packages <- c("gWidgets", "mailR", "devtools")
#   
#   for(i in unique(packages)){
#     if(!require(i, character.only = TRUE)) install.packages(i, dependencies=TRUE, repos='http://cran.us.r-project.org')
#     if(!(paste0("package:",i) %in% search())) library(i, character.only = TRUE)
#   }
#   
#   if(!require(memoise)) install_github("hadley/memoise") #if you don't have memoise, grab it
#   if(packageVersion("memoise")<1) install_github("hadley/memoise") #if you have an old version, grab the newer one
#   library(memoise)
  
  #allow the user to reset all elements at once
  if(resetall){
    resetto=TRUE
    resetfrom=TRUE
    resetpw=TRUE
  }
  
  
  
  #creating the memoised to grab function
  if(!exists("totxtme")){ #run this if it doesn't exist
    getto <- function(){
      paste0(ginput("Enter the number to text:"), def.car)
    }
    
    totxtme <<- memoise(getto, 
                        envir= .GlobalEnv)
  }
  
  if(resetto)
    forget(totxtme)
  
  
  
  #creating the memoised from grab function
  if(!exists("fromtxtme")){ #run this if it doesn't exist
    getfrom <- function(){
      paste(ginput("Enter the Gmail address you are sending from:", text=def.from))
    }
    
    fromtxtme <<- memoise(getfrom, 
                          envir= .GlobalEnv)
  }
  
  if(resetfrom)
    forget(fromtxtme)
  
  
  
  #creating the memoised password grab function
  if(!exists("pwtxtme") & is.null(keeptime)){ #run this if it doesn't exist and user hasn't specified keeptime
    getpw <- function(){
      paste(ginput("Enter your password:"))
    }
    
    #think I have to store globally to keep it
    #consider creating a parameter for keeptime (difficulties here resetting keep time without resetting the password, consider using a ginput with a memoise to change it (perhaps 2 ginputs?)). Otherwise, we'll just store it here (minutes)
    pwtxtme <<- memoise(getpw, 
                        envir= .GlobalEnv,
                        ~timeout(def.keeptime*60))
    
  } else if(!is.null(keeptime)){ #if user specifies keeptime, run this... Otherwise, do nothing (user just sending a message)
    
    getpw <- function(){
      paste(ginput("Enter your password:"))
    }
    
    if(mode(keeptime) == "numeric"){ #if user specifies numeric keeptime:
      pwtxtme <<- memoise(getpw,
                          envir= .GlobalEnv,
                          ~timeout(keeptime*60))
      
    } else if(mode(keeptime) == "character"){ #if keeptime is character...
      if(keeptime=="forever"){ #check if it is appropriate keyword
        pwtxtme <<- memoise(getpw,
                            envir= .GlobalEnv)
        
      } else{ #keeptime is character but not keyword:
        stop("keeptime must be numeric or text string 'forever'")
      }
      
    } else { #keeptime isn't numeric or character
      stop("keeptime must be numeric or text string 'forever'")
    }
  }
  
  if(resetpw)
    forget(pwtxtme)
  
  
  
  send.mail(from=fromtxtme(), 
            to=totxtme(), 
            subject=sbj, 
            body=txt,
            authenticate=TRUE,
            smtp=list(host.name="smtp.gmail.com", 
                      port=465, 
                      user.name=fromtxtme(), 
                      passwd=pwtxtme(),
                      ssl=TRUE))
}





#### testing ----
# 
# txtme("test")
# 
# txtme("test2", keeptime=.5)
# txtme("test21")
# Sys.sleep(30)
# txtme("test22") #should need the password again... And it does!
# 
# #txtme("test3", keeptime="forever") #runs, so no errors...

