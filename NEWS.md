# segmentr 0.2.0

Iteration of the package, improving on the following:

- Rewrite vignette improving on the method of research
- Fix bug in the `exact` algorithm

# segmentr 0.1.1

Minor changes to documentation to solve release issues.

- Simplify equation in `auto_penalize` documentation
- Adopt canonical CRAN URL in `berlin` documentation
- Adopt correct format of DESCRIPTION and LICENSE

# segmentr 0.1.0

First version of the package. Implements a handful of algorithms to help
segment data according to a likelihood function.

- Implements generic `segment` function which works as a proxy making it easy to switch algorithms
- Includes `berlin` dataset build using the rdwd package. Execute `make_berlin.R` to build the dataset.
- Include `vignette('segmentr')` teaching how to use the package.
