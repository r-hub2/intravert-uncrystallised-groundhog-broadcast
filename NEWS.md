

# broadcast 0.1.5.1

* Reduced compiled size of rcpp_bc_b; this should fix the CRAN Error on `r-devel-linux-x86_64-fedora-clang`.


# broadcast 0.1.5

Update of first CRAN release.

**Argument Changes:**

The `recurse_classed` argument in the casting methods has been replaced with the `recurse_all` argument.  
Before, the argument `recurse_classed` controlled if the casting methods recurse through classed lists.  
Now, `recurse_all`, controls if the casting methods recurse through classed **and/or dimensional** lists.

Moreover, the S3 methods in this package now check for unknown arguments given through the ellipsis (`...`).

**Behavioural Changes:**

* The `as_*` functions now also preserve the `broadcaster` class attribute. 
* If both `x` and `y` in the `bc.b()` method are of type `raw`, `bc.b()` will return type of `raw`.

**New Methods:**

* Added the `hiernames2dimnames()` method, to make it easier to compose `dimnames` for the result of `cast_hier2dim()`.


**Documentation Improvements**:

* Added on-attach package start-up message.
* Fixed some spelling errors that went under the radar.
* Fixed some inconsistent usage of Title Case in the titles of the help pages.
* Shortened the main help page.
* Fixed some "See Also" sections in some of the help pages.
* Improved the "Examples" sections in some of the help pages.

**More tests:**

* Added and adapted the unit tests for the above changes.
* Re-ran the unit tests coverage report shown on the website.



# broadcast 0.1.3
* Continuation of Initial CRAN submission.
* Fixed the title case.


# broadcast 0.1.2
* Continuation of Initial CRAN submission.
* Shortened the title.
* Removed the LICENSE file, and its reference in the Description.


# broadcast 0.1.1
* Continuation of Initial CRAN submission.
* Replaced abs function with labs function when using long integers in src/rcpp_bc_int.


# broadcast 0.1
* Initial CRAN submission


# broadcast 0.0.0.9018
* Small performance improvements (re-ran the benchmarks again).
* Proof-read the documentation, and made some tweaks.
* Started preparations for CRAN release in the near future.


# broadcast 0.0.0.9000
* Initial GitHub Publication
