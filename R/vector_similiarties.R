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


#' Calculates overlap coefficient between two vectors
#'
#' @param vec1
#' @param vec2
#'
#' @return
#' @export
#'
#' @examples
overlap_distance = function(vec1, vec2){
  vec1 = vec1[!is.na(vec1)]
  vec2 = vec2[!is.na(vec2)]
  vec1 = names(vec1)
  vec2 = names(vec2)
  inter_sect = length(intersect(vec1, vec2))
  smaller_set = min(length(vec1), length(vec2))
  overlap_dis = 1-inter_sect/smaller_set
  return(overlap_dis)
}


#' Overlap coefficient matrix
#'
#' @param gene_AF_mat
#'
#' @return
#' @export
#'
#' @examples
overlap_coef_mat = function(gene_AF_mat){
  mat = matrix(NA, nrow = ncol(gene_AF_mat), ncol = ncol(gene_AF_mat))
  colnames(mat) = colnames(gene_AF_mat)
  rownames(mat) = colnames(gene_AF_mat)
  for (i in seq_along(colnames(gene_AF_mat))){
    for(j in seq_along(colnames(gene_AF_mat))){
      mat[i,j] = overlap_distance(gene_AF_mat[,i], gene_AF_mat[,j])
    }
  }
  return(mat)
}


