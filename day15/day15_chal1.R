data <- readLines("inputs/day15_input.txt") |>
  stringr::str_extract_all(pattern = "-*\\d+") |>
  unlist() |>
  as.numeric() |>
  matrix(ncol = 4, byrow = T)

df <- as.data.frame(data)
names(df) <- c("sx","sy","bx","by")

y <- 2000000
df$md <- abs(data[,1]-data[,3])+abs(data[,2]-data[,4])
df$ovl <- df$md - abs(y-df$sy)

indx <- c()

for(i in 1:nrow(df)){
  if(df$ovl[i] >= 0){
    indx = unique(c(indx, (df$sx[i]-df$ovl[i]):(df$sx[i]+df$ovl[i])))
  }
}

print(sum(indx!=df$bx[df$by==y]))