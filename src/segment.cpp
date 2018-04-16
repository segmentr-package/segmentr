#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

struct compareByRows {
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

typedef std::map<NumericMatrix::Row, int, compareByRows> rowComparisonMap;

// [[Rcpp::export]]
double cpp_multivariate(NumericMatrix x) {
  rowComparisonMap myMap;

  for (int i = 0; i < x.nrow(); i++) {
    NumericMatrix::Row row = x(i, _);
    myMap[row]++;
  }

  // Extract values from the frequencies map
  std::list<double> valueList;
  for (rowComparisonMap::const_iterator it=myMap.begin(); it!=myMap.end(); ++it) {
    valueList.push_back( it->second );
  }

  NumericVector values(valueList.begin(), valueList.end());
  NumericVector partial_probs = values / sum(values);
  double loglik = sum(values * log(partial_probs));
  int df = values.size();
  return loglik;
}
