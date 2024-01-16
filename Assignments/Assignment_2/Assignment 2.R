

#2 Assignment 2 (C. Stringham)

# Task 4:Find the csv files and save as an object
csv_files <- list.files(path = "Data",pattern = ".csv",recursive = TRUE)

# Task 5: HOw many fiels match?
length(csv_files)

# Task 6:Open the wingspan_vs_mass.csv file and store the contents as an R object named “df” using the read.csv() function
list.files(pattern = "wingspan_vs_mass.csv",recursive = TRUE)
df <- read.csv(list.files(pattern="wingspan_vs_mass.csv",recursive = TRUE))

# Task 7:Inspect the first 5 lines of this data set using the head() function
head(df,n=5)

# Task 8:Find any files (recursively) in the Data/ directory that begin with the letter “b” (lowercase)
list.files(path = "Data",pattern = "^b",recursive=TRUE)

# Task 9:Write a command that displays the first line of each of those “b” files (this is tricky… use a for-loop)
bfiles <- list.files(path = "Data",pattern = "^b",recursive=TRUE,full.names = TRUE)
readLines(bfiles[1],n=1)
readLines(bfiles[2],n=1)
readLines(bfiles[3],n=1)

# for-loop
for(i in bfiles){
print(readLines(i,n=1))
}

# Task 10:Do the same thing for all files that end in “.csv”
list.files(path="Data",pattern=".csv$",recursive = TRUE)
files.list <- list.files(path = "Data",pattern=".csv$",recursive=TRUE,full.names = TRUE)

for(files in files.list){
print(readLines(files,n=1))
}
