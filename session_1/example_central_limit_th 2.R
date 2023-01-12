


data_dist = rnorm(100000,0,1)
hist(data_dist,main="Initial Distribution",color ="blue")

size_samples = 100
number_samples = 200
lst_samples = numeric(number_samples)

for(index in number_samples){
  sample = sample(data_dist,size_samples)
  mean_sample = mean(sample)
  lst_samples[index] = mean_sample
}

print(lst_samples)

