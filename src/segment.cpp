#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

struct compare_by_rows {
  bool operator()(const NumericMatrix::Row& a, const NumericMatrix::Row& b) const {
    if (a.size() != b.size()) {
      return a.size() < b.size();
    }

    for (int i = 0; i < a.size(); i++) {
      if (a[i] != b[i]) {
        return a[i] < b[i];
      }
    }

    return false;
  }
};

typedef std::map<NumericMatrix::Row, int, compare_by_rows> row_comparison_map;

inline row_comparison_map row_frequency_table(NumericMatrix x) {
  row_comparison_map my_map;

  for (int i = 0; i < x.nrow(); i++) {
    NumericMatrix::Row row = x(i, _);
    my_map[row]++;
  }

  return my_map;
}

inline NumericVector extract_map_values(row_comparison_map& my_map) {
  std::vector<int> values;

  for (row_comparison_map::const_iterator it=my_map.begin(); it!=my_map.end(); ++it) {
    values.push_back( it->second );
  }

  NumericVector valuesVector(values.begin(), values.end());
  return valuesVector;
}

// [[Rcpp::export]]
double cpp_multivariate(NumericMatrix x) {
  row_comparison_map my_map = row_frequency_table(x);
  NumericVector values = extract_map_values(my_map);
  NumericVector partial_probs = values / sum(values);
  double loglik = sum(values * log(partial_probs));
  int df = values.size();
  return loglik;
}
