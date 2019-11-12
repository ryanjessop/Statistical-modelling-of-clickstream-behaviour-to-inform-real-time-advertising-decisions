# Comparison to PCA

rfmp_sample

# Our variable selection 
our_vars_rfmp_sample <- rfmp_sample %>% 
  select( c('day_count', 
            'avg_hits',
            'recent_date',
            'avg_inter_visit', 
            'max_inter_visit')) %>%
  rename(DayCount = day_count) %>%
  rename(AverageHits = avg_hits) %>%
  rename(RecentDate = recent_date) %>%
  rename(AverageInterVisit = avg_inter_visit) %>%
  rename(MaximumInterVisit = max_inter_visit)

pairs(our_vars_rfmp_sample)

res <- cor(our_vars_rfmp_sample)
round(res, 2)
corrplot(res, method="color",
         type="lower", number.cex=1.2,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="red", tl.srt=60, tl.cex = 1.1,
         diag=FALSE, is.corr = F 
)


# Variability left over in X-1 after accounting for x1

var_left <- function(X,x,Y){
  tr(var(X) - cov(X,x)%*%(solve(var(x)))%*%cov(x,X)) / tr(var(Y))
}

var_left_no_Y <- function(X,x){
  tr(var(X) - cov(X,x)%*%(solve(var(x)))%*%cov(x,X)) / tr(var(X))
}

# Frequency 
var_left(frequency_vars_rfmp_sample[,-1], frequency_vars_rfmp_sample[,1], frequency_vars_rfmp_sample)
var_left_no_Y(frequency_vars_rfmp_sample[,-1], frequency_vars_rfmp_sample[,1])

# Intensity 
var_left(intensity_vars_rfmp_sample[,-1], intensity_vars_rfmp_sample[,1], intensity_vars_rfmp_sample)
var_left_no_Y(intensity_vars_rfmp_sample[,-1], intensity_vars_rfmp_sample[,1])

# Recency
var_left(recency_vars_rfmp_sample[,-5], recency_vars_rfmp_sample[,5], recency_vars_rfmp_sample)
var_left_no_Y(recency_vars_rfmp_sample[,-5], recency_vars_rfmp_sample[,5])

# Variability
var_left(variability_vars_rfmp_sample[,-c(1,3)], variability_vars_rfmp_sample[,c(1,3)], variability_vars_rfmp_sample)
var_left_no_Y(variability_vars_rfmp_sample[,-c(1,3)], variability_vars_rfmp_sample[,c(1,3)])


aaa <- variability_vars_rfmp_sample %>% 
  filter(RangeInterVisit > 0)

var_left(aaa[,c(1,3)], aaa[,-c(1,3)], aaa)



# PCA on each attribute 

freq_pca <- prcomp(frequency_vars_rfmp_sample, scale. = FALSE)
round(freq_pca$rotation, 4)
summary(freq_pca)

int_pca <- prcomp(intensity_vars_rfmp_sample, scale. = FALSE)
round(int_pca$rotation, 4)
summary(int_pca)

rec_pca <- prcomp(recency_vars_rfmp_sample, scale. = FALSE)
round(rec_pca$rotation, 4)
summary(rec_pca)

var_pca <- prcomp(variability_vars_rfmp_sample, scale. = FALSE)
round(var_pca$rotation, 4)
summary(var_pca)

all_pca <- prcomp(cbind(frequency_vars_rfmp_sample,
                        intensity_vars_rfmp_sample,
                        recency_vars_rfmp_sample,
                        variability_vars_rfmp_sample), scale. = TRUE)
round(all_pca$rotation, 2)
summary(all_pca)

library(factoextra)
fviz_eig(all_pca)

full_pca_matrix <- round(all_pca$rotation, 2)
colnames(full_pca_matrix)
rownames(full_pca_matrix)

plot_ly(x = colnames(full_pca_matrix), 
        y = rownames(full_pca_matrix),
        z = full_pca_matrix, 
        type = "heatmap") %>%
  layout(xaxis = list(title='Observed States'),
         yaxis = list(title='Hidden States'),
         title = "6 hidden state Markov model",
         annotations=legendtitle)



# compute a correlation matrix
loadings <- round(all_pca$rotation, 2)[,c(1:12)]
nms <- rownames(loadings)
cnms <- colnames(loadings)

colorlength <- 100

null_value <- (0 - min(loadings)) / (max(loadings) - min(loadings))        
border <- as.integer(null_value * colorlength)
colorscale <- as.list(1:colorlength)

#colorscale below zero
s <- scales::seq_gradient_pal("blue", "white", "Lab")(seq(0,1,length.out=border))
for (i in 1:border) {
  colorscale[[i]] <- c((i - 1) / colorlength, s[i])
}

#colorscale above zero
s <- scales::seq_gradient_pal("white", "red", "Lab")(seq(0,1,length.out=colorlength - border))
for (i in 1:(colorlength - border)) {
  colorscale[[i + border]] <- c((i + border) / colorlength, s[i])
}

plot_ly(x = cnms, y = nms, z = loadings,
        type = "heatmap", color = ~loadings,
        colorscale = colorscale,
        colorbar = list(len=1)) %>%
  layout(xaxis = list(title='Principal Components'),
         yaxis = list(title='Statistical variables'),
         title = "PCA")



# Method 2 
library(randomForest)
library(dotchart2)

frequency_vars_rfmp_sample_de_dupe <- frequency_vars_rfmp_sample %>% distinct()
intensity_vars_rfmp_sample_de_dupe <- intensity_vars_rfmp_sample %>% distinct()
recency_vars_rfmp_sample_de_dupe <- recency_vars_rfmp_sample %>% distinct()
variability_vars_rfmp_sample_de_dupe <- variability_vars_rfmp_sample %>% distinct()

freq_forest <- randomForest(frequency_vars_rfmp_sample_de_dupe)
importance(freq_forest)
varImpPlot(freq_forest, main='Frequency feature importance', pt.cex=2.5, bg='black', color='black')
segments(x0=-2, y0=3, x1=54.25535, y1=3, lwd=6)
segments(x0=-2, y0=2, x1=47.48493, y1=2, lwd=6)
segments(x0=-2, y0=1, x1=30.69297, y1=1, lwd=6)

int_forest <- randomForest(intensity_vars_rfmp_sample_de_dupe)
importance(int_forest)
varImpPlot(int_forest, main='Intensity feature importance', pt.cex=2.5, bg='black')
segments(x0=-8, y0=5, x1=221.65538, y1=5, lwd=6)
segments(x0=-8, y0=4, x1=201.32104, y1=4, lwd=6)
segments(x0=-8, y0=3, x1=169.57621, y1=3, lwd=6)
segments(x0=-8, y0=2, x1=155.74450, y1=2, lwd=6)
segments(x0=-8, y0=1, x1=87.35259, y1=1, lwd=6)

rec_forest <- randomForest(recency_vars_rfmp_sample_de_dupe)
importance(rec_forest)
varImpPlot(rec_forest, main='Recency feature importance', pt.cex=2.5, bg='black')
segments(x0=-90, y0=5, x1=2439.851, y1=5, lwd=6)
segments(x0=-90, y0=4, x1=1860.208, y1=4, lwd=6)
segments(x0=-90, y0=3, x1=1329.517, y1=3, lwd=6)
segments(x0=-90, y0=2, x1=1285.791, y1=2, lwd=6)
segments(x0=-90, y0=1, x1=1079.711, y1=1, lwd=6)

var_forest <- randomForest(variability_vars_rfmp_sample_de_dupe)
importance(var_forest)
varImpPlot(var_forest, main='Variability feature importance', pt.cex=2.5, bg='black')
segments(x0=-20, y0=6, x1=539.5960, y1=6, lwd=6)
segments(x0=-20, y0=5, x1=437.9875, y1=5, lwd=6)
segments(x0=-20, y0=4, x1=397.5995, y1=4, lwd=6)
segments(x0=-20, y0=3, x1=388.5850, y1=3, lwd=6)
segments(x0=-20, y0=2, x1=244.7610, y1=2, lwd=6)
segments(x0=-20, y0=1, x1=165.8224, y1=1, lwd=6)


# Method 3
library(Rtsne)

freq_tsne <- Rtsne(frequency_vars_rfmp_sample_de_dupe, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
plot(freq_tsne$Y, main="tsne")

int_tsne <- Rtsne(intensity_vars_rfmp_sample_de_dupe, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
plot(int_tsne$Y, main="tsne")

rec_tsne <- Rtsne(recency_vars_rfmp_sample_de_dupe, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
plot(rec_tsne$Y, main="tsne")

var_tsne <- Rtsne(variability_vars_rfmp_sample_de_dupe, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
plot(var_tsne$Y, main="tsne")


?Rtsne


# Method 4

var(frequency_vars_rfmp_sample)
var(intensity_vars_rfmp_sample)
var(recency_vars_rfmp_sample)
var(variability_vars_rfmp_sample)
