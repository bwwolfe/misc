example shape area 2
================
barrett wolfe
2022-07-02

``` r
library(imager)
library(scales)
library(Momocs)

img <- load.image("areas2.jpg")
img.g <- grayscale(img)
area.list <- split_connected(threshold(img.g, adjust = 1))
#120 pixel cut-off
real.areas <- area.list[which(sapply(area.list, function(x) sum(x)> 120))]
real.areas <- real.areas[order(sapply(real.areas, sum), decreasing = TRUE)]

centroids <-
  round(coo_centpos(Out(lapply(real.areas, function(x) {
    which(apply(as.matrix(x),1,rev), arr.ind = TRUE)
    #   which(as.matrix(x), arr.ind = TRUE)
  }))),
  0)

label_pos <-
  round(coo_centpos(Out(lapply(real.areas, function(x) {
    which(apply(apply(as.matrix(x),1,rev),2,rev), arr.ind = TRUE)
    #   which(as.matrix(x), arr.ind = TRUE)
  }))),0)

real.inv <- as.imlist(lapply(real.areas, \(x)!x))

#this mapply prints the index of the shape for troubleshooting
#error is likely due to the centroid not being inside the shape if it is tiny or
#concave -
#import_Conte needs a starting point inside the shape
outlines <- mapply(\(x,y) {

  x1 <- as.matrix(as.cimg(x))
  #if outlines are flipped incorrectly, remove the following line
  x1 <- apply(x1, 1, rev)
  tryCatch(import_Conte(x1,centroids[y,]), finally = print(y))
}, x = real.inv,  y= 1:nrow(centroids))
```
I reckon only things changed from the original are the adjust parameter and pixel cut-off for areas of interest

``` r
plot(img)
sapply(outlines, \(x) {
  lines(x[,1],x[,2],col = alpha("red3", 0.5), lwd = 1.5)
})
```

``` r
text(label_pos[-1,2], label_pos[-1,1],
     labels = 2:length(real.areas),
     col = "blue4", cex = 1.4)
```

![](example-map-shape-area-2_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
shape_area <- data.frame(Region = 1:length(real.areas),
                       Area_in_pixels = sapply(real.areas, function(x) sum(x))
)

shape_area <- rbind(shape_area,
                    data.frame(Region = "Total",
                               Area_in_pixels = sum(shape_area$Area_in_pixels)))

shape_area$Prop_of_total <- 
  round(shape_area[,2]/shape_area[nrow(shape_area),2],4)

```

``` r
#loading this library causes an error earlier, detach it if code is suddenly 
#not working when you rerun
library(spatstat.geom)
#needed for plot.im function though
plot.im(as.im(t(as.matrix(real.inv[[1]]))),
        xlim = c(0,450), ylim = c(600,0),
        col = c(alpha("black",0.75), "white"),
        ribbon = FALSE, main = "blank pixels = not counted in total area")
for(i in 2:length(real.inv)) {
  plot.im(as.im(t(as.matrix(real.inv[[i]]))),
     add = TRUE, col = c(alpha("red4",0.3),NA))
}
```

![](example-map-shape-area-2_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
detach("package:spatstat.geom", character.only = TRUE)
knitr::kable(shape_area)
```

| Region | Area_in_pixels | Prop_of_total |
|:-------|---------------:|--------------:|
| 1      |         138856 |        0.8335 |
| 2      |           2182 |        0.0131 |
| 3      |           2006 |        0.0120 |
| 4      |           1688 |        0.0101 |
| 5      |           1109 |        0.0067 |
| 6      |            993 |        0.0060 |
| 7      |            813 |        0.0049 |
| 8      |            684 |        0.0041 |
| 9      |            648 |        0.0039 |
| 10     |            641 |        0.0038 |
| 11     |            631 |        0.0038 |
| 12     |            546 |        0.0033 |
| 13     |            515 |        0.0031 |
| 14     |            508 |        0.0030 |
| 15     |            464 |        0.0028 |
| 16     |            418 |        0.0025 |
| 17     |            413 |        0.0025 |
| 18     |            405 |        0.0024 |
| 19     |            400 |        0.0024 |
| 20     |            395 |        0.0024 |
| 21     |            358 |        0.0021 |
| 22     |            332 |        0.0020 |
| 23     |            327 |        0.0020 |
| 24     |            326 |        0.0020 |
| 25     |            315 |        0.0019 |
| 26     |            308 |        0.0018 |
| 27     |            305 |        0.0018 |
| 28     |            296 |        0.0018 |
| 29     |            296 |        0.0018 |
| 30     |            296 |        0.0018 |
| 31     |            294 |        0.0018 |
| 32     |            282 |        0.0017 |
| 33     |            280 |        0.0017 |
| 34     |            279 |        0.0017 |
| 35     |            277 |        0.0017 |
| 36     |            266 |        0.0016 |
| 37     |            259 |        0.0016 |
| 38     |            259 |        0.0016 |
| 39     |            253 |        0.0015 |
| 40     |            252 |        0.0015 |
| 41     |            250 |        0.0015 |
| 42     |            245 |        0.0015 |
| 43     |            240 |        0.0014 |
| 44     |            233 |        0.0014 |
| 45     |            230 |        0.0014 |
| 46     |            229 |        0.0014 |
| 47     |            219 |        0.0013 |
| 48     |            214 |        0.0013 |
| 49     |            214 |        0.0013 |
| 50     |            212 |        0.0013 |
| 51     |            210 |        0.0013 |
| 52     |            205 |        0.0012 |
| 53     |            201 |        0.0012 |
| 54     |            180 |        0.0011 |
| 55     |            179 |        0.0011 |
| 56     |            172 |        0.0010 |
| 57     |            171 |        0.0010 |
| 58     |            170 |        0.0010 |
| 59     |            167 |        0.0010 |
| 60     |            166 |        0.0010 |
| 61     |            165 |        0.0010 |
| 62     |            163 |        0.0010 |
| 63     |            161 |        0.0010 |
| 64     |            160 |        0.0010 |
| 65     |            157 |        0.0009 |
| 66     |            156 |        0.0009 |
| 67     |            151 |        0.0009 |
| 68     |            151 |        0.0009 |
| 69     |            146 |        0.0009 |
| 70     |            141 |        0.0008 |
| 71     |            139 |        0.0008 |
| 72     |            132 |        0.0008 |
| 73     |            129 |        0.0008 |
| 74     |            124 |        0.0007 |
| 75     |            123 |        0.0007 |
| 76     |            122 |        0.0007 |
| Total  |         166602 |        1.0000 |
