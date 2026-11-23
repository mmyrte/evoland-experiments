// Efficient kernel density estimation (eKDE; Mazy & Longaretti) with the box kernel, and the
// volume of a hypercube clipped by hyperplanes (Cho & Kim 2020) for its boundary correction.
// Compiled by ekde.r with Rcpp::sourceCpp(); see README.md in this directory.
//
// SPDX-License-Identifier: GPL-3.0-or-later
//
// A derived work, ported from the reference implementation (mmyrte/clumpy at e9f73b9):
// - the binning and the neighbour search of ekde (ekde/cython/ekdefunc.pyx, ekde/ekde/base.py),
//   Copyright (c) François-Rémi Mazy, BSD-3-Clause;
// - the clipped volume of hyperclip (hyperclip/cython/hyperfunc.pyx), Copyright (c)
//   François-Rémi Mazy, GPL-3.0.
// As it contains GPL-3.0 code, this file as a whole is under the GPL-3.0 (or later); the BSD
// notice of ekde is kept: redistributions must retain that copyright notice, its list of
// conditions and its disclaimer (ekde/LICENSE.txt of the reference).
//
// The arithmetic follows the reference operation by operation (including its order of
// summation), so that results agree with it to rounding. Nothing here draws random numbers or
// depends on the order of the input rows; the optional threads of the box-kernel sums each
// compute whole cells, so the same input gives the same output with any number of threads.

#include <Rcpp.h>
#include <algorithm>
#include <atomic>
#include <cmath>
#include <numeric>
#include <thread>
#include <vector>

using namespace Rcpp;

// --- binning ------------------------------------------------------------------------------------

// Cell index of each row, ((x - x_min) / dx) truncated toward zero as numpy's astype(int).
// Row-major n x d. Far-out points are clamped to +-2^30 (the reference would wrap at 2^31).
static std::vector<int> discretize(const NumericMatrix &X, const NumericVector &x_min, double dx) {
  const int n = X.nrow(), d = X.ncol();
  std::vector<int> Z((size_t)n * d);
  const double lim = 1073741824.0;
  for (int j = 0; j < d; ++j) {
    for (int i = 0; i < n; ++i) {
      double v = (X(i, j) - x_min[j]) / dx;
      if (std::isnan(v)) stop("missing or infinite value in the data");
      if (v > lim) v = lim;
      if (v < -lim) v = -lim;
      Z[(size_t)i * d + j] = (int)v; // truncation toward zero
    }
  }
  return Z;
}

// Row indices sorted lexicographically by their cells.
static std::vector<int> lex_order(const std::vector<int> &Z, int n, int d) {
  std::vector<int> idx(n);
  std::iota(idx.begin(), idx.end(), 0);
  std::stable_sort(idx.begin(), idx.end(), [&](int a, int b) {
    const int *za = &Z[(size_t)a * d], *zb = &Z[(size_t)b * d];
    for (int j = 0; j < d; ++j) {
      if (za[j] != zb[j]) return za[j] < zb[j];
    }
    return false;
  });
  return idx;
}

// Bins the (whitened) data: the occupied cells U, sorted lexicographically as pandas' groupby
// returns them, and the number of rows in each, nu.
// [[Rcpp::export(name = ".ekde_bin")]]
List ekde_bin(NumericMatrix X, NumericVector x_min, double dx) {
  const int n = X.nrow(), d = X.ncol();
  std::vector<int> Z = discretize(X, x_min, dx);
  std::vector<int> idx = lex_order(Z, n, d);
  std::vector<int> first;
  std::vector<double> nu;
  for (int k = 0; k < n; ++k) {
    bool same = k > 0;
    if (same) {
      const int *za = &Z[(size_t)idx[k] * d], *zb = &Z[(size_t)idx[k - 1] * d];
      for (int j = 0; j < d; ++j) {
        if (za[j] != zb[j]) {
          same = false;
          break;
        }
      }
    }
    if (same) {
      nu.back() += 1.0;
    } else {
      first.push_back(idx[k]);
      nu.push_back(1.0);
    }
  }
  const int nU = (int)first.size();
  IntegerMatrix U(nU, d);
  for (int i = 0; i < nU; ++i) {
    for (int j = 0; j < d; ++j) U(i, j) = Z[(size_t)first[i] * d + j];
  }
  return List::create(_["U"] = U, _["nu"] = NumericVector(nu.begin(), nu.end()));
}

// --- estimation -----------------------------------------------------------------------------------

namespace {

struct Grid {
  const int *U; // column-major nU x d
  int nU, d, margin;
  const double *nu;
  std::vector<std::vector<int>> next; // next[j][i]: first row after i whose cells 0..j differ

  int col(int i, int j) const { return U[(size_t)j * nU + i]; }

  // first row in [lo, hi) with column j >= x (rows of [lo, hi) are sorted on column j)
  int lower(int j, int lo, int hi, int x) const {
    while (lo < hi) {
      int m = lo + (hi - lo) / 2;
      if (col(m, j) < x) lo = m + 1; else hi = m;
    }
    return lo;
  }
  // first row in [lo, hi) with column j > x
  int upper(int j, int lo, int hi, int x) const {
    while (lo < hi) {
      int m = lo + (hi - lo) / 2;
      if (col(m, j) > x) hi = m; else lo = m + 1;
    }
    return lo;
  }

  // Sum of the weights of the occupied cells within `margin` cells of z in every coordinate
  // (the box kernel on the grid), visited in lexicographic order like the reference's merge().
  void explore(int j, int lo, int hi, const int *z, double &f) const {
    int a = lower(j, lo, hi, z[j] - margin);
    int b = upper(j, a, hi, z[j] + margin);
    if (j == d - 1) {
      for (int i = a; i < b; ++i) f = f + nu[i];
      return;
    }
    int i = a;
    while (i < b) {
      int e = next[j][i];
      explore(j + 1, i, e, z, f);
      i = e;
    }
  }
};

} // namespace

// Box-kernel sums g(z) = sum of nu over the occupied cells within (q - 1) / 2 cells of the cell
// of z, for each row of Xq (whitened). U must be as returned by .ekde_bin(). The caller divides
// by n (2h)^d and the whitening scale. With num_threads > 1 the distinct cells of the query rows
// are dealt out to threads in contiguous blocks; each cell's sum is computed by one thread in the
// same order, so the result does not depend on the number of threads.
// [[Rcpp::export(name = ".ekde_box_sums")]]
NumericVector ekde_box_sums(IntegerMatrix U, NumericVector nu, NumericMatrix Xq,
                            NumericVector x_min, double dx, int q, int num_threads = 1) {
  const int nU = U.nrow(), d = U.ncol(), n = Xq.nrow();
  if (Xq.ncol() != d) stop("dimension mismatch");
  NumericVector out(n);
  if (n == 0 || nU == 0) return out;
  Grid g;
  g.U = U.begin();
  g.nU = nU;
  g.d = d;
  g.margin = (q - 1) / 2;
  g.nu = nu.begin();
  g.next.assign(d > 1 ? d - 1 : 0, std::vector<int>(nU));
  for (int j = 0; j + 1 < d; ++j) {
    std::vector<int> &nx = g.next[j];
    nx[nU - 1] = nU;
    for (int i = nU - 2; i >= 0; --i) {
      bool same = true;
      for (int k = 0; k <= j; ++k) {
        if (g.col(i, k) != g.col(i + 1, k)) {
          same = false;
          break;
        }
      }
      nx[i] = same ? nx[i + 1] : i + 1;
    }
  }
  // estimate once per distinct cell of the query points
  std::vector<int> Z = discretize(Xq, x_min, dx);
  std::vector<int> idx = lex_order(Z, n, d);
  std::vector<int> first; // position in idx of the first row of each distinct cell
  for (int k = 0; k < n; ++k) {
    bool same = k > 0;
    if (same) {
      const int *z = &Z[(size_t)idx[k] * d], *zp = &Z[(size_t)idx[k - 1] * d];
      for (int j = 0; j < d; ++j) {
        if (z[j] != zp[j]) {
          same = false;
          break;
        }
      }
    }
    if (!same) first.push_back(k);
  }
  const int nz = (int)first.size();
  std::vector<double> sums(nz, 0.0);
  auto work = [&](int from, int to) {
    for (int c = from; c < to; ++c) {
      double f = 0.0;
      g.explore(0, 0, nU, &Z[(size_t)idx[first[c]] * d], f);
      sums[c] = f;
    }
  };
  const int block = 4096;
  if (num_threads <= 1) {
    for (int from = 0; from < nz; from += block) {
      work(from, std::min(nz, from + block));
      checkUserInterrupt();
    }
  } else {
    std::atomic<int> next_block(0);
    std::vector<std::thread> pool;
    for (int t = 0; t < num_threads; ++t) {
      pool.emplace_back([&]() {
        int from;
        while ((from = next_block.fetch_add(block)) < nz) work(from, std::min(nz, from + block));
      });
    }
    for (std::thread &t : pool) t.join();
  }
  for (int c = 0; c < nz; ++c) {
    const int to = c + 1 < nz ? first[c + 1] : n;
    for (int k = first[c]; k < to; ++k) out[idx[k]] = sums[c];
  }
  return out;
}

// --- hyperclip: volume of the unit hypercube clipped by half-spaces ------------------------------
//
// Port of hyperclip/cython/hyperfunc.pyx: clipping_condition_B_and_volume(), compute_vertex(),
// test_vertex(), gauss_AB(), determinant() (cofactor expansion) and volumes(). The half-spaces
// are a_k . x + r_k >= 0, a_k the columns of A (n x m). The formula is that of Cho & Kim (2020).
// The enumeration and the arithmetic are those of the reference, in the same order; only the
// memory handling differs (buffers allocated once instead of malloc per vertex).

namespace {

// All k-subsets of 0..n-1 in lexicographic order (itertools.combinations), cached.
class Combinations {
  std::vector<std::vector<std::vector<std::vector<int>>>> cache_; // [n][k] -> list
public:
  const std::vector<std::vector<int>> &get(int n, int k) {
    if ((int)cache_.size() <= n) cache_.resize(n + 1);
    if ((int)cache_[n].size() <= n + 1) cache_[n].resize(n + 2);
    static const std::vector<std::vector<int>> empty;
    if (k < 0 || k > n) return empty;
    std::vector<std::vector<int>> &out = cache_[n][k];
    if (!out.empty()) return out;
    std::vector<int> c(k);
    std::iota(c.begin(), c.end(), 0);
    while (true) {
      out.push_back(c);
      int i = k - 1;
      while (i >= 0 && c[i] == i + n - k) --i;
      if (i < 0) break;
      c[i] += 1;
      for (int j = i + 1; j < k; ++j) c[j] = c[j - 1] + 1;
    }
    return out;
  }
};

// sorted complement of the sorted set K (size cK) in 0..n-1, written to out; returns its size
inline int bar(int n, const int *K, int cK, int *out) {
  int o = 0, b = 0;
  for (int i = 0; i < n; ++i) {
    if (b < cK && K[b] == i) {
      ++b;
    } else {
      out[o++] = i;
    }
  }
  return o;
}

class Clipper {
  int n_; // dimension
  int stride_;
  std::vector<double> det_buf_; // one n x n block per recursion level
  std::vector<double> sub_;     // sub-matrix for det_sub
  std::vector<double> AB_;      // augmented system
  std::vector<double> v_, vK_;
  Combinations comb_;

  // determinant of the r x r matrix M (row stride `stride_`) by cofactor expansion along the
  // first row, as hyperfunc.pyx determinant()
  double determinant(const double *M, int r, int level) {
    if (r == 0) return 1.0;
    if (r == 1) return M[0];
    double D = 0;
    double *temp = &det_buf_[(size_t)level * stride_ * stride_];
    int sign = 1;
    for (int f = 0; f < r; ++f) {
      int ii = 0, jj = 0;
      for (int row = 0; row < r; ++row) {
        for (int col = 0; col < r; ++col) {
          if (row != 0 && col != f) {
            temp[ii * stride_ + jj] = M[row * stride_ + col];
            jj = jj + 1;
            if (jj == r - 1) {
              jj = 0;
              ii = ii + 1;
            }
          }
        }
      }
      D = D + sign * M[f] * determinant(temp, r - 1, level + 1);
      sign = -sign;
    }
    return D;
  }

  // det of A[I, J] with r = |I| rows and the first r of the columns J (|J| >= r)
  double det_sub(const NumericMatrix &A, const int *I, int r, const int *J, int cJ) {
    for (int a = 0; a < r; ++a) {
      for (int b = 0; b < stride_; ++b) sub_[a * stride_ + b] = b < cJ ? A(I[a], J[b]) : 0.0;
    }
    return determinant(sub_.data(), r, 1);
  }

  // Gaussian elimination with partial pivoting on the augmented cK x (cK + 1) system in AB_
  void gauss_AB(int n, double zero) {
    const int w = stride_ + 1;
    double *AB = AB_.data();
    for (int i = 0; i < n; ++i) {
      double maxEl = std::fabs(AB[i * w + i]);
      int maxRow = i;
      for (int k = i + 1; k < n; ++k) {
        if (std::fabs(AB[k * w + i]) > maxEl) {
          maxEl = std::fabs(AB[k * w + i]);
          maxRow = k;
        }
      }
      for (int k = i; k < n + 1; ++k) std::swap(AB[maxRow * w + k], AB[i * w + k]);
      for (int k = i + 1; k < n; ++k) {
        if (AB[i * w + i] == 0.0) AB[i * w + i] = zero;
        double c = -AB[k * w + i] / AB[i * w + i];
        for (int j = i; j < n + 1; ++j) {
          if (i == j) AB[k * w + j] = 0; else AB[k * w + j] += c * AB[i * w + j];
        }
      }
    }
    for (int i = n - 1; i >= 0; --i) {
      if (AB[i * w + i] == 0.0) AB[i * w + i] = zero;
      vK_[i] = AB[i * w + n] / AB[i * w + i];
      for (int k = i - 1; k >= 0; --k) AB[k * w + n] -= AB[k * w + i] * vK_[i];
    }
  }

public:
  explicit Clipper(int n) : n_(n), stride_(std::max(n, 1) + 1) {
    det_buf_.assign((size_t)(stride_ + 2) * stride_ * stride_, 0.0);
    sub_.assign((size_t)stride_ * stride_, 0.0);
    AB_.assign((size_t)stride_ * (stride_ + 1), 0.0);
    v_.assign(n, 0.0);
    vK_.assign(stride_, 0.0);
  }

  // Volume of {x in [0, 1]^n : A[, k] . x + R[k] >= 0, k < m}; 1 when the clipping condition
  // (B) of Cho & Kim fails (a vanishing denominator), as the reference returns.
  double volume(const NumericMatrix &A, const NumericVector &R, int m, double zero) {
    const int n = n_;
    const double tol = zero * 10; // test_vertex() is called with zero * 10
    const int w = stride_ + 1;
    double fact = 1;
    {
      int f = 1;
      for (int i = 1; i <= n; ++i) f = f * i;
      fact = (double)f;
    }
    std::vector<int> Kbar(n), J(n), v01(n), vstar(n), Iur(m + 1), Ium(m + 1), vsu(n + 1),
        Ibar(m);
    double vol = 0.0;
    for (int cI = 0; cI <= m - 1; ++cI) {
      const std::vector<std::vector<int>> &list_I = comb_.get(m - 1, cI);
      for (const std::vector<int> &I : list_I) {
        const int nIbar = bar(m, I.data(), cI, Ibar.data());
        const std::vector<std::vector<int>> &list_K = comb_.get(n, cI);
        for (const std::vector<int> &K : list_K) {
          bar(n, K.data(), cI, Kbar.data());
          for (int cJ = 0; cJ <= n - cI; ++cJ) {
            const std::vector<std::vector<int>> &list_J = comb_.get(n - cI, cJ);
            for (const std::vector<int> &Jidx : list_J) {
              for (int r = 0; r < cJ; ++r) J[r] = Kbar[Jidx[r]];
              // compute_vertex()
              for (int a = 0; a < cI; ++a) {
                for (int b = 0; b < cI; ++b) AB_[a * w + b] = A(K[b], I[a]);
                double rhs = -R[I[a]];
                for (int b = 0; b < cJ; ++b) rhs = rhs - A(J[b], I[a]);
                AB_[a * w + cI] = rhs;
              }
              gauss_AB(cI, zero);
              for (int i = 0; i < n; ++i) v_[i] = 0;
              for (int b = 0; b < cI; ++b) v_[K[b]] = vK_[b];
              for (int b = 0; b < cJ; ++b) v_[J[b]] = 1;
              // test_vertex(): on the hyperplanes I, on the positive side of the others, in the cube
              bool ok = true;
              for (int a = 0; a < cI && ok; ++a) {
                double g = R[I[a]];
                for (int i = 0; i < n; ++i) g = g + A(i, I[a]) * v_[i];
                if (std::fabs(g) > tol) ok = false;
              }
              for (int a = 0; a < nIbar && ok; ++a) {
                double g = R[Ibar[a]];
                for (int i = 0; i < n; ++i) g = g + A(i, Ibar[a]) * v_[i];
                if (g < 0) ok = false;
              }
              for (int i = 0; i < n && ok; ++i) {
                if (v_[i] < 0 || v_[i] > 1) ok = false;
              }
              if (!ok) continue;
              // the vertex contributes
              int n01 = 0, n0 = 0;
              for (int i = 0; i < n; ++i) {
                if (v_[i] == 0 || v_[i] == 1) v01[n01++] = i;
                if (v_[i] == 0) n0 += 1;
              }
              const int nstar = bar(n, v01.data(), n01, vstar.data());
              double denominator = 1.0;
              for (int id_t = 0; id_t < cI; ++id_t) {
                // union_remove(I, m - 1, t)
                int shift = 0;
                for (int i = 0; i < cI - 1; ++i) {
                  if (I[i] == I[id_t]) shift = 1;
                  Iur[i] = I[i + shift];
                }
                Iur[cI - 1] = m - 1;
                denominator = denominator * det_sub(A, vstar.data(), nstar, Iur.data(), cI);
              }
              for (int id_t = 0; id_t < n01; ++id_t) {
                // union(I, m - 1) and union(vstar, v01[id_t]), both sorted
                for (int i = 0; i < cI; ++i) Ium[i] = I[i];
                Ium[cI] = m - 1;
                int o = 0;
                bool put = false;
                for (int i = 0; i < nstar; ++i) {
                  if (!put && vstar[i] > v01[id_t]) {
                    vsu[o++] = v01[id_t];
                    put = true;
                  }
                  vsu[o++] = vstar[i];
                }
                if (!put) vsu[o++] = v01[id_t];
                denominator = denominator * det_sub(A, vsu.data(), nstar + 1, Ium.data(), cI + 1);
              }
              if (std::fabs(denominator) < zero) return 1.0;
              int vstar_sum = 0;
              for (int i = 0; i < nstar; ++i) vstar_sum = vstar_sum + vstar[i] + 1;
              double g_m_v = R[m - 1];
              for (int i = 0; i < n; ++i) g_m_v = g_m_v + A(i, m - 1) * v_[i];
              double AvI = det_sub(A, vstar.data(), nstar, I.data(), cI);
              double numerator = ((n0 + vstar_sum) % 2 == 0) ? 1.0 : -1.0;
              numerator = numerator * std::pow(g_m_v * AvI, (double)n);
              denominator = denominator * fact * std::fabs(AvI);
              vol = vol + numerator / denominator;
            }
          }
        }
      }
    }
    return vol;
  }
};

} // namespace

// Volume of the hypercube of side `side` centred at each row of C (n_C x d) that lies on the
// positive side of all half-spaces A[, k] . x + R[k] >= 0 (A is d x m), as a fraction of the
// hypercube: 1 for a hypercube no half-space reaches. hyperclip's volumes(A, R, C, h) with
// side = h; a half-space counts when the distance of the centre to it, in the norm of the
// reference (|a.x + r| / |a|_2^2 * |a|_inf), is at most side / 2.
// [[Rcpp::export(name = ".ekde_clipped_volumes")]]
NumericVector ekde_clipped_volumes(NumericMatrix A, NumericVector R, NumericMatrix C, double side,
                                   double zero) {
  const int N = C.nrow(), n = A.nrow(), m = A.ncol();
  if (C.ncol() != n || R.size() != m) stop("dimension mismatch");
  if (n > 30) stop("the clipped volume is limited to 30 dimensions");
  NumericVector vols(N, 1.0);
  if (m == 0) return vols;
  std::vector<double> norm_inf(m, 0.0), norm_2(m, 0.0);
  for (int k = 0; k < m; ++k) {
    double s = 0.0;
    for (int j = 0; j < n; ++j) {
      norm_inf[k] = std::max(norm_inf[k], std::fabs(A(j, k)));
      s += A(j, k) * A(j, k);
    }
    norm_2[k] = std::sqrt(s);
  }
  Clipper clipper(n);
  NumericMatrix Ax(n, m);
  NumericVector Rx(m);
  for (int i = 0; i < N; ++i) {
    int mx = 0;
    for (int k = 0; k < m; ++k) {
      double dist = 0.0;
      for (int j = 0; j < n; ++j) dist = dist + C(i, j) * A(j, k);
      dist = dist + R[k];
      dist = std::fabs(dist) / (norm_2[k] * norm_2[k]) * norm_inf[k];
      if (dist <= side / 2.0) {
        for (int j = 0; j < n; ++j) Ax(j, mx) = A(j, k);
        Rx[mx] = R[k];
        for (int j = 0; j < n; ++j) Rx[mx] = Rx[mx] - Ax(j, mx) * (-C(i, j) + side / 2);
        for (int j = 0; j < n; ++j) Ax(j, mx) = Ax(j, mx) * side;
        mx = mx + 1;
      }
    }
    if (mx > 0) vols[i] = clipper.volume(Ax, Rx, mx, zero);
    if ((i & 0x3FF) == 0) checkUserInterrupt();
  }
  return vols;
}

// The clipped volume of the unit hypercube itself (hyperclip's Hyperclip().set_A_R(A, R).volume(),
// without the clipping-condition (A) check), for tests.
// [[Rcpp::export(name = ".ekde_unit_clipped_volume")]]
double ekde_unit_clipped_volume(NumericMatrix A, NumericVector R, double zero) {
  if (A.nrow() > 30) stop("the clipped volume is limited to 30 dimensions");
  Clipper clipper(A.nrow());
  return clipper.volume(A, R, A.ncol(), zero);
}
