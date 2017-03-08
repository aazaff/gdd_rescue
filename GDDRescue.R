# Custom functions are camelCase. Arrays, parameters, and arguments are PascalCase
# Dependency functions are not embedded in master functions, and are marked with the flag dependency in the documentation
# []-notation is used wherever possible, and $-notation is avoided.
# R packages are flagged when called for documentation purposes.

######################################### Load Required Libraries ###########################################
# Install libraries if necessary and load them into the environment
if (suppressWarnings(require("RCurl"))==FALSE) {
    install.packages("RCurl",repos="http://cran.cnr.berkeley.edu/");
    library("RCurl");
    }

if (suppressWarnings(require("RJSONIO"))==FALSE) {
    install.packages("RJSONIO",repos="http://cran.cnr.berkeley.edu/");
    library("RJSONIO");
    }

if (suppressWarnings(require("stringdist"))==FALSE) {
    install.packages("stringdist",repos="http://cran.cnr.berkeley.edu/");
    library("stringdist");
    }

# Currently mac only
if (suppressWarnings(require("doParallel"))==FALSE) {
    install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
    library("doParallel");
    }

if (suppressWarnings(require("plyr"))==FALSE) {
    install.packages("plyr",repos="http://cran.cnr.berkeley.edu/");
    library("plyr");
    }

# Start a cluster for multicore, 3 by default 
# Can make it higher if passed as a command line argument through terminal - e.g., RScript GDDRescue.R 7
CommandArgument<-commandArgs(TRUE)
if (length(CommandArgument)==0) {
    Cluster<-makeCluster(3)
    } else {
    Cluster<-makeCluster(as.numeric(CommandArgument[1]))
    }
    
# Establish the range of packages to be downloaded, 1 to 50 by default
# Can make it higher if passed as a command line argument through terminal - e.g., RScript GDDRescue.R 7 51 100
if (length(CommandArgument)!=3) {
    Start<-1;
    End<-50;
    } else {
    Start<-as.numeric(CommandArgument[2])
    End<-as.numeric(CommandArgument[3])
    }  

#############################################################################################################
########################################### DOWNLOAD DATA, RESCUE ###########################################
#############################################################################################################
# No functions at this time.

############################################ Download Datasets from API  ####################################
# Increase the timeout option to allow for larger data downloads
options(timeout=600)

# Download the initial list of data rescue projects
DRDataList<-RJSONIO::fromJSON("http://demo.ckan.org/api/3/action/package_list")[[3]] # There seems to be a lot of garbage/test datasets in this list? Maybe those could be removed?

# Extract only the "packages" to be analyzed in this run
DRDataList<-DRDataList[Start:End]

#############################################################################################################
############################################# PARSE DATA, RESCUE ############################################
#############################################################################################################
# Currently breaks because the attribute fields of the JSON stream are inconsistent
# Download the data associated with a package, and extract relevant fields
singlePackage<-function(PackageName) {
      Query<-paste("http://demo.ckan.org/api/3/action/package_show?id=",PackageName,sep="")
      PackageInfo<-RJSONIO::fromJSON(Query)[[3]]
      # Format chosen data into a tidy data.frame
      PackageFrame<-as.data.frame(array(NA,dim=4,dimnames=list(c("author","author_email","organization","title")))) # hardcoded currently, probably should be passed as a function argument
      colnames(PackageFrame)<-PackageName
      # Extract the information into the data.frame
      for (Name in rownames(PackageFrame)) {
          if (is.null(PackageInfo[[Name]])) {next;}
          PackageFrame[Name,1]<-PackageInfo[[Name]]
          }
      return(t(PackageFrame))
      }

# Macro to loop through the entire DRData subset
pullDRData<-function(DRDataList) {
      FinalList<-vector("list",length=length(DRDataList))
      for (i in 1:length(DRDataList)) {
          FinalList[[i]]<-suppressWarnings(singlePackage(DRDataList[[i]])) # ignore the stupid organization warning
          }
      FinalList<-do.call(rbind,FinalList)
      return(FinalList)
      }

############################################ Download Datasets from API  ####################################
