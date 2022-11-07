#' Algorithm Summary UI Component
#'
#' @param title The title of the r function/algorithm
#' @param description description content
#'
#' @return html tags
#' @export
#'
#' @examples
algorithm_description <- function(title, description) {
  htmltools::div(
    htmltools::h4(
      class = "algo-title",
      "Algorithm Summary"
    ),

    htmltools::div(
      class = "algo-content",

      htmltools::h6(
        class = "algo-fun-title",
        title
      ),

      htmltools::p(
        class = "algo-summary-content",
        description
      )
    )
  )
}




#' Mode Center Of Tendency
#'
#' @description Get the most occurring value/values in a object.
#'
#' @param x A R object
#' @param multi_mode bool: TRUE for multiple modes.
#'
#' @return most frequent value(s).
#' @export
#'
#' @examples
mode_center <- function(x, multi_mode = FALSE) {
  unique_x <- unique(x)

  max_tbl <- match(x, unique_x) |> tabulate()

  max_value_len <- max_tbl[which(max_tbl == max(max_tbl))] |> length()

  if (max_value_len > 1) {
    if (isFALSE(multi_mode)) {
      unique_modes <- unique_x[which(max_tbl == max(max_tbl))]

      index <- which.max(max_tbl)

      message(
        paste("There are", max_value_len, "modes in this object",
              paste0("(", paste(unique_modes, collapse = ", "), "),"),
              "if `multi_mode` = FALSE the first mode will be returned")
      )
    } else {
      index <- which(max_tbl == max(max_tbl))
    }
  } else {
    index <- which.max(max_tbl)
  }
  output <- unique_x[index]
  return(output)
}




#' Data Suitability For Cluster Analysis.
#'
#' @description  Check if the data is suitable to run a cluster analysis.
#'
#' @param df  data.frame.
#' @param scale logical: Whether to standardize the data before the analysis.
#'
#' @return print suitability result.
#' @export
#'
#' @examples
data_suitability <- function(df, scale = FALSE) {
  clust_str <- parameters::check_clusterstructure(x = df,
                                                  standardize = scale)$H

  if (clust_str < 0.5) {
    print("The dataset is suitable for clustering")
  } else {
    print("The dataset is not suitable for clustering")
  }
}




#' Get Optimal Number Of Centers.
#'
#' @param df data.frame with only numeric variables.
#' @param max_n numeric: The maximum number of optimized centers to search.
#' @param search_method character: method to use either 'kmeans' or 'average'.
#' @param type character: The type of output to return, either "n_cluster" for
#' consensus number of clusters or 'summary' all optimal clusters.
#'
#' @return optimal number of centers.
#' @import mode_center
#' @export
#'
#' @examples
optimal_center <- function(df,
                           max_n = 10,
                           search_method = "kmeans",
                           type = "n_cluster") {
  type <- match.arg(type, c("n_cluster", "summary"))

  check_vars <- sapply(df, \(.x) length(unique(.x))) |>
    lapply(\(.x) .x != 2) |>
    unlist()

  if (all(as.vector(check_vars) == FALSE)) {
    c_index <- c("duda", "pseudot2", "beale", "ball", "db", "ratkowsky",
                 "sdindex")
    f_tbl <- df

  } else if (all(as.vector(check_vars) == TRUE)) {
    c_index <- c("duda", "pseudot2", "beale", "ball", "db",
                 "ratkowsky", "sdindex", "friedman", "ccc",
                 "rubin", "scott", "marriot", "trcovw", "tracew")
    f_tbl <- df

  } else if (any(as.vector(check_vars) != FALSE) && any(as.vector(check_vars) != TRUE)) {
    c_index <- c("duda", "pseudot2", "beale", "ball", "db", "trcovw",
                 "ratkowsky", "sdindex", "friedman", "ccc",
                 "rubin", "scott", "marriot", "trcovw", "tracew")

    f_tbl <- df[check_vars]

    if (ncol(f_tbl) == 1) {
      shiny::validate("Only 1 useable numeric variable is avaliable.")
    }
  }

  opt_cluster_rec <- tryCatch(
    lapply(c_index, function(.x) {
      NbClust::NbClust(data = f_tbl,
                       distance = "euclidean",
                       min.nc = 2,
                       max.nc = max_n,
                       method = search_method,
                       index  = .x)$Best.nc
    }),

    error = function(e) {
      e

      if (dim(df)[1] > 2000) {
        number_k <- fpc::pamk(
          data = df,
          krange  = 1:max_n,
          scaling = FALSE,
          criterion = "multiasw",
          usepam    = FALSE,
          medoids.x = FALSE,
          keep.data = FALSE
        )$nc

      } else {
        number_k <- fpc::pamk(
          data = df,
          krange  = 1:max_n,
          scaling = FALSE,
          criterion = "asw",
          usepam    = TRUE,
          keep.data = FALSE,
          keep.diss = FALSE
        )$nc
      }

      if (number_k == 1) {
        number_k <- number_k + 1
      }
      return(number_k)
    }
  )

  if (is.list(opt_cluster_rec)) {
    get_all_n_center <- lapply(opt_cluster_rec, as.vector) |>
      lapply(function(.x) {
        f_val <- as.vector(.x)
        f_val[1]
      })

    if (type == "n_cluster") {
      unlist(get_all_n_center) |> mode_center()

    } else if (type == "summary") {
      names(get_all_n_center) <- c_index
      unlist(get_all_n_center)
    }

  } else {
    opt_cluster_rec
  }
}




# NOT USED YET

#' Optimal Cluster Plot.
#'
#' @param df data.frame
#' @param scale logical: Whether to standardize the data before the analysis.
#' @param plot_method character: either 'wss', 'silhouette' or 'consensus'.
#' @param k_max numeric: The maximum number of optimized centers to search.
#'
#' @return An elbow, a silhouette or consensus plot.
#' @export
#'
#' @examples
optimal_center_plot <- function(df,
                                scale  = TRUE,
                                plot_method,
                                k_max = 6) {
  plot_method <- match.arg(plot_method, c("wss", "silhouette", "consensus"))

  if (plot_method %in% c("wss", "silhouette")) {
    if (scale) f_tbl <- as.data.frame(scale(df)) else f_tbl <- df
    sub_tl <- setNames(c("Elbow Plot","Silhouette Plot"), c("wss","silhouette"))

    factoextra::fviz_nbclust(f_tbl,
                             FUNcluster = kmeans,
                             method = plot_method,
                             k.max  = k_max) +
      ggplot2::labs(subtitle = sub_tl[[plot_method]]) +
      ggplot2::theme_light()

  } else if (plot_method == "consensus") {
    parameters::n_clusters(df,
                           package = "NbClust",
                           include_factors = TRUE,
                           nbclust_method = search_method,
                           standardize = TRUE,
                           n_max = k_max) |>
      plot(n_clust) +
      ggplot2::labs(subtitle = "Consensus") +
      ggplot2::theme_light()
  }
}




#' Run a cluster Analysis Using Partitioning Algorithms.
#'
#' @param df  data.frame
#' @param use_algo character: The type of partitioning algorithm. Either 'kmeans'
#' 'pam' or 'clara'
#' @param scale logical: Whether to standardize the data or not.
#' @param k_center numeric: Number of K.
#' @param iter.max numeric: The maximum number of iterations allowed.
#' @param n_starts numeric: Number of random set to choose.
#' @param samples  numeric: N number of samples to be drawn from the dataset.
#'
#' @return Depending on the type of algorithm 'use_algo' supplied, either an
#' object class kmeans, clara or pam.
#'
#' @details see stats::kmeans, cluster::clara, cluster::pam.
#' @export
#'
#' @examples
k_center_model <- function(df,
                           use_algo = "kmeans",
                           scale = FALSE,
                           k_center,
                           iter.max = 10,
                           n_starts = 10,
                           samples) {

  use_algo <- match.arg(use_algo, c("kmeans", "pam", "clara"))

  if (use_algo == "kmeans") {
    s_df <- as.data.frame(scale(df))
    mdl <- kmeans(x = s_df,
                  centers = k_center,
                  nstart  = n_starts,
                  iter.max = iter.max)

  } else if (use_algo == "pam") {
    mdl <- cluster::pam(x = df,
                        k = k_center,
                        stand  = scale,
                        variant = "faster",
                        # nstart = n_starts,
                        cluster.only = TRUE)

  } else if (use_algo == "clara") {
    samples <- ifelse(missing(samples), nrow(df)*0.1, samples)

    mdl <- cluster::clara(x = df,
                          k = k_center,
                          stand   = scale,
                          # rngR = TRUE,
                          samples = samples,
                          cluster.only = TRUE)
  }

  return(mdl)
}




#' Tidy Cluster Analysis.
#'
#' @param df data.frame: The original data.
#' @param algo character: The algorithm used in creating the cluster.
#' @param mdl model/data.frame: For kmeans the model, for hclust the model data.
#' @param use_fun character: For kmeans the type of algorithm used.
#'
#' @return A data.frame/tibble with a cluster variables.
#' @export
#'
#' @examples
tidy_cluster <- function(df, algo, mdl, use_fun) {

  algo <- match.arg(algo, c("kmeans", "hclust"))

  if (algo == "kmeans") {
    use_fun <- match.arg(use_fun, c("kmeans", "pam", "clara"))

    if (use_fun == "kmeans") {
      f_tbl <- broom::augment(mdl, df)

      if (".rownames" %in% names(f_tbl)) {
        f_tbl <- dplyr::rename(f_tbl, row_id = .rownames)
      }
    }
    if (use_fun %in% c("pam", "clara")) {
      if (tibble::has_rownames(df)) {
        f_tbl <- tibble::as_tibble(df, rownames = "row_id")
      } else {
        f_tbl <- tibble::as_tibble(df)
      }
      f_tbl$.cluster <- mdl
      f_tbl$.cluster <- as.factor(f_tbl$.cluster)
    }

    return(f_tbl)

  } else if (algo == "hclust") {
    tibble::add_column(df, .cluster = dplyr::pull(mdl, .cluster)) |>
      tibble::as_tibble(rownames = "row_id")
  }
}




#' Hierarchical Clustering Analysis.
#'
#' @param df data.frame
#' @param use_fun character: The type of Hierarchical method. Either
#' Agglomerative or Divisive.
#' @param scale logical: Whether to standardize the data or not.
#' @param method character: the agglomeration method to use. see ?stats::hclust
#' for more details.
#' @param cut_k numeric: The desired number of groups.
#'
#' @return An object class hclust, diana or agnes.
#' @export
#'
#' @examples
h_cluster <- function(df,
                      use_fun,
                      scale = FALSE,
                      method = "median", # average ++++++++++++++
                      cut_k) {
  use_fun <- match.arg(use_fun, c("hclust", "agnes", "diana"))

  if (use_fun == "hclust") {
    f_mdl <- hclust(cluster::daisy(df), method = method)

  } else if (use_fun == "agnes") {
    f_mdl <- cluster::agnes(df,
                            metric = "euclidean",
                            stand = scale,
                            method = method,
                            keep.data = FALSE)

  } else if (use_fun == "diana") {
    f_mdl <- cluster::diana(df,
                            metric = "euclidean",
                            stand = scale,
                            keep.data = FALSE)
  }

  df$.cluster <- cutree(f_mdl, cut_k)
  df$.cluster <- as.character(df$.cluster)

  df <- tibble::as_tibble(df)
  return(df)
}




#' Available Cluster Algorithm.
#'
#' @description Re-code cluster algorithm and number of clusters.
#'
#' @param algorithm character: The selected type of cluster algorithm.
#' @param number_centers numeric: The number of centers.
#'
#' @return
#' @export
#'
#' @examples
clust_alg_value <- function(algorithm, number_centers) {

  if (!missing(algorithm) && missing(number_centers)) {
    if (algorithm == "K-partition clustering (kmeans)") {
      "kmeans"

    } else if (algorithm == "Partitioning Around Medoids (pam)") {
      "pam"

    } else if (algorithm == "Clustering Large Applications") {
      "clara"

    } else if (algorithm == "Hierarchical clustering") {
      "hclust"

    } else if (algorithm == "Agglomerative Hierarchical Clustering (agnes)") {
      "agnes"

    } else if (algorithm == "Divisive Hierarchical Clustering") {
      "diana"

    } else {
      stop("Algorithm is not recognised, choose from the avaliable algorithms")
    }

  } else if (missing(algorithm) && !missing(number_centers)) {
    if (is.numeric(number_centers)) {
      number_centers
    } else if (number_centers == 0) {
      2
    } else {
      stop("argument `number_centers` returned a non numeric value")
    }
  }
}




#' Wrapper for Cluster Algorithms.
#'
#' @param mdl_df data.frame: A data data have been cleaned and preprocessed for
#' cluster analysis.
#' @param org_df data.frame: The original data frame with out any preprocessing.
#' @param algorithm_type character: The type of cluster algorithm to use.
#' @param h_method character: hlust method. see stats::hclust.
#' @param number_k_centers numeric: number of centers to use.
#'
#' @return A data frame with a .cluster variable.
#' @import clust_alg_value, k_center_model, tidy_cluster, h_cluster.
#' @export
#'
#' @examples
run_cluster_algo <- function(mdl_df,
                             org_df,
                             algorithm_type,
                             h_method = "median", # average
                             number_k_centers) {

  algorithm_type <- clust_alg_value(algorithm = algorithm_type)

  cluster_algorithm <- match.arg(algorithm_type,
                                 c("kmeans", "pam", "clara",
                                   "hclust", "agnes", "diana"))

  if (cluster_algorithm %in% c("kmeans", "pam", "clara")) {
    f_mdl <- k_center_model(df = mdl_df,
                            use_algo  = cluster_algorithm,
                            scale    = FALSE,
                            k_center = number_k_centers,
                            iter.max = 10,
                            n_starts = 10)

    tidy_cluster(df   = org_df,
                 algo = "kmeans",
                 mdl  = f_mdl,
                 use_fun = cluster_algorithm)

  } else if (cluster_algorithm %in% c("hclust", "agnes", "diana")) {
    d_mdl <- h_cluster(df = mdl_df,
                       use_fun = cluster_algorithm,
                       scale   = FALSE,
                       method  = h_method,
                       cut_k   = number_k_centers)

    tidy_cluster(df = org_df, algo = "hclust", mdl = d_mdl)
  }
}





#' Algorithm Description.
#'
#' @param algorithm character: The type of algorithm summary to return.
#'
#' @return An html div with an algorithm description
#' @export
#'
#' @examples
algorithm_summary <- function(algorithm) {
  if (algorithm == "K-partition clustering (kmeans)") {

    list(
      title = "K-means Algorithm",
      content = " Kmeans aims to partition the data supplied into k groups.
      i.e. items with similarity are grouped together such that the sum of
      squares from each data points to the assigned cluster centers is
      minimized. This operation is done using the 'Hartigan-Wong' algorithm
      from the kmeans function in the stats library in R."
    )

  } else if (algorithm == "Partitioning Around Medoids (pam)") {

    list(
      title = "Partitioning Around Medoids (pam)",
      content = "Similar to kmeans algorithm, pam-algorithm search for k medoids among
      the observations in the dataset, After finding a set of k medoids,
      k clusters are constructed by assigning each observation to the
      nearest medoid. The goal is to find k representative objects which
      minimize the sum of the dissimilarities of the observations to their
      closest representative object. This operation is done using the pam
      function from the cluster package in R."
    )

  } else if (algorithm == "Clustering Large Applications") {

    list(
      title = "Clustering Large Applications",
      content = "This can deal with much larger datasets compared to other petitioning
      algorithms This is done by taking sub set of the datasets
      of fixed size then each sub-dataset is partitioned into k clusters.
      This operation is performed using the clara function from the cluster
      package in R."
    )

  } else if (algorithm == "Hierarchical clustering") {

    list(
      title = "Hierarchical clustering",
      content = "Hierarchical cluster algorithm uses a set of dissimilarities of the
      dataset observations. Initially, each object is assigned to its own
      cluster and then the algorithm proceeds iteratively, at each stage
      joining the two most similar clusters continuing until there is just
      a single cluster. This operation is performed using The 'average'
      method from the hclust function in the stats library in R."
    )

  } else if (algorithm == "Agglomerative Hierarchical Clustering (agnes)") {

    list(
      title = "Agglomerative Hierarchical clustering Strategy",
      content = "Agglomerative method constructs a hierarchy of clusterings, initially
      each observation is a small cluster by itself. Clusters are merged
      until only one large cluster remains which contains all the
      observations. At each stage the two nearest clusters are combined
      to form one larger cluster. This operation is done using the agnes
      function from the cluster package in R."
    )

  } else if (algorithm == "Divisive Hierarchical Clustering") {

    list(
      title = "Divisive Hierarchical clustering Strategy",
      content = "Divisive method constructs a hierarchy of clusterings starting with
      large cluster containing all n observations. Clusters are
      divided until each cluster contains only a single observation. At
      each stage, the cluster with the largest diameter is selected.
      This operation is done using the diana function from the cluster
      package in R."
    )
  } else {
    stop("Algorithm is not recognised, choose from the avaliable algorithms")
  }
}
