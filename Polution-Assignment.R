  # Part 1 
  
  # Write a function named 'pollutantmean' that calculates the mean of a pollutant 
  # (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' 
  # takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID 
  # numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory 
  # specified in the 'directory' argument and returns the mean of the pollutant across all of 
  # the monitors, ignoring any missing values coded as NA.
  
  
      pollutantmean <- function(directory, pollutant, id = 1:332) 
      {  
        
          #listing files
          
          file_list <- list.files(directory, full.names=TRUE, pattern="*.csv") 
          
        
          #data frame
        
          dat <- data.frame()
          
          
            #loop for each file
          
            for (i in id)
            {
              # #Read the file
              dat <- rbind(dat, read.csv(file_list[i]))
              
            }
            #Calculate mean
            mean_data <- mean(dat[,pollutant], na.rm = TRUE)
            return(mean_data)
      }
      
      
      ### tests
      
      # >   pollutantmean("specdata", "sulfate", 1:10)
      # [1] 4.064128
      # >   
      #   >   pollutantmean("specdata", "nitrate", 70:72)
      # [1] 1.706047
      # >   
      #   >   pollutantmean("specdata", "nitrate", 23)
      # [1] 1.280833
  
  
  # Part 2
  # 
  # Write a function that reads a directory full of files and reports the number 
  # of completely observed cases in each data file. The function should return a 
  # data frame where the first column is the name of the file and the second column 
  # is the number of complete cases.
  
  
      complete <- function(directory, id = 1:332)
        
      {
        
        #listing files
        
        file_list <- list.files(directory, full.names=TRUE) 
        
        
        #data frame
        
        dat <- data.frame()
        
        
            for (i in id)
            {
              # Read files
              temp <- read.csv(file_list[i])
              # sum of all complete cases
              nobs <-sum(complete.cases(temp))
              # bind to data
              dat <-rbind(dat, data.frame(i, nobs))
              
            }
        colnames(dat) <- c("id", "nobs")
        return(dat)
      }
  
      ### tests
      
      # >       complete("specdata", 1)
      # id nobs
      # 1  1  117
      # >       
      #   >       complete("specdata", c(2, 4, 8, 10, 12))
      # id nobs
      # 1  2 1041
      # 2  4  474
      # 3  8  192
      # 4 10  148
      # 5 12   96
      # >       
      #   >       complete("specdata", 30:25)
      # id nobs
      # 1 30  932
      # 2 29  711
      # 3 28  475
      # 4 27  338
      # 5 26  586
      # 6 25  463
      # >       
      #   >       complete("specdata", 3)
      # id nobs
      # 1  3  243
      # > 
      
      
      
    # Part 3
      
    # Write a function that takes a directory of data files and a threshold 
    # for complete cases and calculates the correlation between sulfate and 
    # nitrate for monitor locations where the number of completely observed 
    # cases (on all variables) is greater than the threshold. The function 
    # should return a vector of correlations for the monitors that meet the 
    # threshold requirement. If no monitors meet the threshold requirement, 
    # then the function should return a numeric vector of length 0. 
      
      
    corr <- function(directory, threshold = 0)
    {
      
      #listing files
      
      file_list <- list.files(directory, full.names=TRUE) 
      
      
      #data set
      
      dat <- vector(mode = "numeric", length = 0)
        for(i in 1:length(file_list))
          {
            # Read File
            tmp <- read.csv(file_list[i])
            
            #Calculate csum    
            csum <- sum((!is.na(tmp$sulfate)) & (!is.na(tmp$nitrate)))
            if (csum > threshold)
            {
              #Extract data of niteate and sulfate and calculate correlation between them
              sul <- tmp[which(!is.na(tmp$sulfate)), ]
              nit <- sul[which(!is.na(sul$nitrate)), ]
              dat <- c(dat, cor(nit$sulfate, nit$nitrate))
            }
          }
      
      dat 
    }
    
    
    ### tests
    
    # >     cr <- corr("specdata", 150)
    # >     head(cr)
    # [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
    # >     
    #   >     summary(cr)
    # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    # -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 
    # >     
    #   >     cr <- corr("specdata", 400)
    # >     head(cr)
    # [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
    # >     
    #   >     summary(cr)
    # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    # -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313 
    # >     
    #   >     
    #   >     cr <- corr("specdata", 5000)
    # >     summary(cr)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 
    # >     
    #   >     length(cr)
    # [1] 0
    # >     
    #   >     cr <- corr("specdata")
    # >     summary(cr)
    # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    # -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 
    # >     
    #   >     length(cr)
    # [1] 323
