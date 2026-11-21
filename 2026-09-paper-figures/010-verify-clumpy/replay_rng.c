/*
 * A user-supplied uniform generator for R (see ?RNGkind, "User-supplied random number
 * generation") that replays a fixed stream of uniforms, e.g. the ones numpy drew in the
 * reference clumpy. With RNGkind("user-supplied"), every unif_rand() call in R or in compiled
 * code (R::unif_rand in Rcpp) returns the next value of the stream, so evoland's allocator can
 * be run on exactly the draws the reference used, without changing evoland.
 *
 * Build: R CMD SHLIB replay_rng.c; load with dyn.load() before RNGkind("user-supplied").
 */
#include <R.h>
#include <R_ext/Random.h>
#include <stdlib.h>

static double *stream = NULL;
static int stream_len = 0;
static int stream_pos = 0;
static int overrun = 0;
static double value;

double *user_unif_rand(void) {
  if (stream_pos < stream_len) {
    value = stream[stream_pos++];
  } else {
    /* past the end: count it and return a value that cannot select a transition */
    overrun++;
    value = 0.999999999;
  }
  return &value;
}

/* .C("replay_set", x, length(x)): replace the stream and rewind */
void replay_set(double *x, int *n) {
  free(stream);
  stream = (double *)malloc(sizeof(double) * (*n > 0 ? *n : 1));
  for (int i = 0; i < *n; i++) stream[i] = x[i];
  stream_len = *n;
  stream_pos = 0;
  overrun = 0;
}

/* .C("replay_state", pos = 0L, overrun = 0L): how many values were used, and how many calls
 * went past the end */
void replay_state(int *pos, int *over) {
  *pos = stream_pos;
  *over = overrun;
}
