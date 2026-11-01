// Neighbourhood memberships for the fuzzy map comparisons in validation-metrics.r.
//
// For a categorical map on a regular grid (cell order row-major, as in terra; NA_INTEGER
// outside the domain) and a weight matrix centred on each cell, the membership of cell i in
// category c is the largest weight among the cells of category c in i's window:
//   m[i, c] = max_{j in window(i), map[j] == c} w(j - i),
// 0 if there is none. This is the "distance-decayed nearest neighbour" membership of Hagen
// (2003) and Hagen-Zanker (2009) with a crisp category similarity matrix, and the
// `.change_membership()` of evoland's calc_transition_similarity(), computed for all categories
// in one pass over the window.
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix neighbourhood_membership(IntegerVector map, int nrow, int ncol, int n_categories,
                                       NumericMatrix weights) {
  if (map.size() != (R_xlen_t)nrow * ncol) stop("map must have nrow * ncol cells");
  const int wr = weights.nrow(), wc = weights.ncol();
  if (wr % 2 != 1 || wc % 2 != 1) stop("weights must have odd dimensions");
  const int hr = wr / 2, hc = wc / 2;
  const R_xlen_t n = map.size();
  NumericMatrix out(n, n_categories);
  for (int r = 0; r < nrow; ++r) {
    for (int c = 0; c < ncol; ++c) {
      const R_xlen_t i = (R_xlen_t)r * ncol + c;
      if (map[i] == NA_INTEGER) {
        for (int k = 0; k < n_categories; ++k) out(i, k) = NA_REAL;
        continue;
      }
      for (int dr = -hr; dr <= hr; ++dr) {
        const int rr = r + dr;
        if (rr < 0 || rr >= nrow) continue;
        for (int dc = -hc; dc <= hc; ++dc) {
          const int cc = c + dc;
          if (cc < 0 || cc >= ncol) continue;
          const double w = weights(dr + hr, dc + hc);
          if (w <= 0) continue;
          const int k = map[(R_xlen_t)rr * ncol + cc];
          if (k == NA_INTEGER) continue;
          if (k < 1 || k > n_categories) stop("category codes must be in 1..n_categories");
          if (w > out(i, k - 1)) out(i, k - 1) = w;
        }
      }
    }
  }
  return out;
}
