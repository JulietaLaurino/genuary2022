## Credit to Rasmus Bååth's blog (https://www.sumsar.net/blog/2019/01/image-dithering-in-r/)

library(imager)

img <- load.image("./Day02/chalten.jpeg")

img_gray <- grayscale( rm.alpha(img) )

# rep_mat takes a matrix (mat) and tiles it so that the resulting
# matrix has size nrow_out x ncol_out.
# It's basically a 2d version of rep()
rep_mat <- function(mat, nrow_out, ncol_out) {
  mat[rep(seq_len(nrow(mat)), length.out = nrow_out),
      rep(seq_len(ncol(mat)), length.out = ncol_out)]
}
 

# Calculates a non-normalized Bayer pattern matrix of size 2^n
recursive_bayer_pattern <- function(n) {
  if(n <= 0) {
    return(matrix(0))
  }
  m <- recursive_bayer_pattern(n - 1)
  rbind(
    cbind(4 * m + 0, 4 * m + 2),
    cbind(4 * m + 3, 4 * m + 1))
}

# Returns a Bayer pattern of size 2^n normalized so all values
# are between 0.0 and 1.0.
normalized_bayer_pattern <- function(n) {
  pattern <- recursive_bayer_pattern(n)
  (1 + pattern) / ( 1 + length(pattern) )
}

bayer_matrix <- rep_mat(normalized_bayer_pattern(2),
                        nrow(img_gray), ncol(img_gray))
bayer_cimg <- as.cimg(bayer_matrix)
img_bayer <- img_gray > bayer_cimg
plot(img_bayer)


# With color
img_bayer_color <- img
for(rgb_i in 1:3) {
  color_channel <- img_bayer_color[ , , 1, rgb_i, drop = FALSE]
  img_bayer_color[ , , 1, rgb_i] <- color_channel > bayer_cimg
}

img_plot <- plot(img_bayer_color, axes =FALSE)

save.image(img_plot, './Day02/chalten_dithered.png')


