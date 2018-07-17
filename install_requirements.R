# This script installs all the required R packages
packages <- read.table('requirements.txt', stringsAsFactors = F)[,1]

for(x in packages){
    if(!is.element(x, installed.packages()[,1]))
    {install.packages(x, repos="http://cran.fhcrc.org")
    } else {print(paste(x, " library already installed"))}
}