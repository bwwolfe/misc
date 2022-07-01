

#load in the data that was in the facebook post

dat <- "X.       x        y hdgÂ.  ofs1    z1  ofs2    z2  ofs3    z3  ofs4    z4  ofs5    z5  ofs6    z6  ofs7    z7  ofs8    z8
  1  20   804718. 6193564.  273.  -100  19.7   -99  19.7   -98  19.7   -97  19.8   -96  19.7   -95  19.7   -94  19.7   -93  19.7
2  20.0 804717. 6193564.  273.  -100  19.7   -99  19.7   -98  19.8   -97  19.8   -96  19.7   -95  19.7   -94  19.7   -93  19.6
3  20.0 804716. 6193564.  273.  -100  19.7   -99  19.8   -98  19.7   -97  19.7   -96  19.7   -95  19.6   -94  19.6   -93  19.6
4  20.0 804715. 6193564.  273.  -100  19.7   -99  19.7   -98  19.7   -97  19.7   -96  19.6   -95  19.6   -94  19.7   -93  19.7
5  20.0 804714. 6193564.  273.   -86  19.7   -82  19.7   -81  19.7   -80  19.7   -50  19.7   -78  19.7   -77  19.7   -76  19.7
6  20.0 804713. 6193564.  273.   -82  19.6   -81  19.7   -80  19.7   -79  19.6   -78  19.7   -77  19.7   -76  19.7   -75  19.7
7  20.0 804712. 6193564.  273.   -82  19.6   -81  19.6   -80  19.6   -79  19.6   -78  19.6   -77  19.6   -76  19.7   -75  19.7
8  20.0 804711. 6193565.  273.   -47  19.7   -46  19.7   -45  19.7   -44  19.7   -43  19.7   -42  19.7   -41  19.7   -40  19.7
9  20.0 804710. 6193565.  273.   -47  19.7   -46  19.7   -45  19.7   -44  19.7   -43  19.7   -42  19.7   -41  19.7   -40  19.7"
temp <- tempfile()
writeLines(dat, temp)
df <- read.table(temp)
offset columns
off_cols <- grep("ofs",names(df))

#z columns, which should be one greater than offset columns
z_cols <- grep("z",names(df))
all.equal(off_cols + 1, z_cols)

#columns you want to keep
keep <- c(-100, -50, 0, 50, 100)

# matrix of offsets
off_mat <- as.matrix(df[,off_cols])

#rows and columns of offsets of interest
ind <- arrayInd(which(off_mat %in% keep, arr.ind = TRUE ), dim(off_mat))

#now for each set of coordinates
#we want all the columns before the first offset, 
#we want a column with each offset of interest and one of each corresponding z value

#matrix of z values
z_mat <- as.matrix(df[,z_cols])

result <- do.call('rbind',
        apply(ind, 1, function(x) {
  row <- x[1]
  col <- x[2]
  data.frame(df[row, 1:(off_cols[1]-1)], ofs=off_mat[row,col], z=z_mat[row,col])    
}
))

result

#concise version:

extract_offsets <- function(df, keep) {
  do.call('rbind',
          apply(arrayInd(
            which(as.matrix(df[, grep("ofs", names(df))]) %in% keep, arr.ind = TRUE),
            dim(as.matrix(df[, grep("ofs", names(df))]))
          ),
          1,
          function(x) {
            data.frame(df[x[1], 1:(grep("ofs", names(df))[1]-1)],
                       ofs = df[x[1], grep("ofs", names(df))[x[2]]],
                       z = df[x[1], (grep("ofs", names(df)) + 1)[x[2]]])
          }))
}

extract_offsets(df=df, keep=keep)
