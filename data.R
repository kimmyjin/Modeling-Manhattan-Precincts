a=seq(40.796769,40.800543, length.out = 500)
dataset1=matrix(a,nrow=100,ncol=5)
b=seq(-73.972990,-73.958237, length.out = 1500)
dataset2=matrix(b,nrow=100,ncol=15)
s=5000
res = list() 
for(i in 1:s)
{
  res[[i]] = data_frame(address = "Central Park", 
                        precinct ="22",
                        x = sample(dataset1, 1, replace=replace),
                        y = sample(dataset2, 1, replace=replace)
  )
}

res_data = as.data.frame(bind_rows(res))