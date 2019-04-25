data = read.table("gfa_data_2.csv", header = TRUE, sep = ",")
GfaPredictor = data[, -ncol(data)]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

# Create centralized data
GfaPredictor_cent = GfaPredictor - rep.row(apply(GfaPredictor, 2, mean),nrow(GfaPredictor))
dev = sqrt(apply(GfaPredictor, 2, var))
GfaPredictor_cent_scaled = as.matrix(t(apply(GfaPredictor_cent, 1, function(x) x/dev)))

# Find Eigenvectors and Eigenvalues of covariance matrix of centralized data
cov_gfa_centra = 1/((nrow(GfaPredictor_cent_scaled))-1) * t(GfaPredictor_cent_scaled) %*% GfaPredictor_cent_scaled
results = eigen(cov_gfa_centra)
pca_eigen_value = results$values
sum_information = sum(results$values)
accumulative = 0
for (i in 1:length(results$values)){
  accumulative = accumulative + results$values[i]
  accu_write = paste(i,"\t",accumulative/sum_information)
  write(accu_write,"accumulate_information.txt", append = TRUE)
}

# Plot the new data using top 3 priciple components
top_3_eigv <- results$vectors[, 1:3]
top_3_coord <- as.matrix(GfaPredictor_cent_scaled) %*% top_3_eigv
color_code <- rep("red", nrow(top_3_coord))

# Change glass former to "green"
color_code[grep("1", data$Class)] = "green"

# 3D plot using top 3 priciple components

car::scatter3d(x = top_3_coord[, 1], y = top_3_coord[, 2], z = top_3_coord[, 3],
               point.col = color_code,sphere.size=3,
               grid = FALSE, surface = FALSE)

# 2D plot using top 2 priciple components
top_2_coord <- as.matrix(GfaPredictor_cent_scaled) %*% results$vectors[, 1:2]
plot(x = top_2_coord[, 1], y = top_2_coord[, 2], col = color_code)