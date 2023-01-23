#' Return Jaccard distance for two vectors
#'
#' @param vec1
#' @param vec2
#'
#' @return
#' @export
#'
#' @examples
jacc_distance = function(vec1, vec2){
  jac_dis=1-length(intersect(vec1, vec2))/length(union(vec1, vec2))
  return(jac_dis)
}


#' Return matrix of jaccard distances of allele frequency matrix.
#' Samples in columns
#'
#' @param gene_AF_mat
#'
#' @return
#' @export
#'
#' @examples
jac_distance_mat = function(gene_AF_mat){
  mat = matrix(NA, nrow = ncol(gene_AF_mat), ncol = ncol(gene_AF_mat))
  colnames(mat) = colnames(gene_AF_mat)
  rownames(mat) = colnames(gene_AF_mat)
  for (i in seq_along(colnames(gene_AF_mat))){
    for(j in seq_along(colnames(gene_AF_mat))){
      mat[i,j] = jacc_distance(gene_AF_mat[,i], gene_AF_mat[,j])
    }
  }
  return(mat)
}
