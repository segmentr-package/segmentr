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
NumericVector multivariate(NumericMatrix x) {
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
  return NumericVector::create(loglik, df);
}

inline NumericVector call_loglikfun(std::string loglikmethod, Function r_loglikfun, NumericMatrix x) {
  if(loglikmethod == "multivariate") {
    return multivariate(x);
  }

  return r_loglikfun(x);
}

// [[Rcpp::export]]
std::vector<int> segment_base(NumericMatrix x, std::string loglikmethod, Function r_loglikfun) {
  int c1 = 1;
  int segmax = x.ncol();
	int m = x.ncol();
	int n = x.nrow();
	NumericMatrix z(segmax, m);
	NumericMatrix r(segmax - 1, m);

	for (int i = 0; i < m; i++) {
	  NumericVector w = call_loglikfun(loglikmethod, r_loglikfun, x(_, Range(0, i)));
	  z(0, i) = w[0] + 1.0 * c1 * sqrt(n);
	}

	for (int k = 1; k < segmax; k++) {
	  for (int i = k; i < m; i++) {
      std::vector<double> q;
	    for (int j = k - 1; j <= i - 1; j++) {
	      NumericVector w = call_loglikfun(loglikmethod, r_loglikfun, x(_, Range(j + 1, i)));
	      q.push_back(z(k - 1, j) + w[0] + 1.0 * c1 * sqrt(n));
	    }


	    int max_pos = std::distance(q.begin(), std::max_element(q.begin(), q.end()));
	    double max_q = *std::max_element(q.begin(), q.end());

	    z(k, i) = max_q;
	    r(k-1, i) = max_pos + k - 2;
	  }
	}

	NumericVector z_temp = z(_, m - 1);
	int segshat = std::distance(z_temp.begin(), std::max_element(z_temp.begin(), z_temp.end()));
	std::vector<int> segs;
	if (segshat > 0) {
	  segs.push_back(r(segshat - 1, m - 1));

		if (segshat > 1) {
		  for (int k = segshat - 1; k > 0; k--) {
		    int last = segs.back();
		    segs.push_back(r(k - 1, last));
		  }
		}

		std::reverse(segs.begin(), segs.end());
	}

	return segs;
}
