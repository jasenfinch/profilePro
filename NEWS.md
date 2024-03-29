# profilePro 0.8.2

* Add Bioconductor dependencies to the remote field in DESCRIPTION to ensure that these can be installed when installing the package using [`pak`](https://pak.r-lib.org/) or [`renv`](https://rstudio.github.io/renv/articles/renv.html).

* Fix for [`plotChromatogram()`](https://jasenfinch.github.io/profilePro/reference/plotChromatogram.html) when the specified sample classes using the `cls` argument is not a character vector.

* Fix for [`plotChromatogram()`](https://jasenfinch.github.io/profilePro/reference/plotChromatogram.html) with the now returned [`MChromatograms`](https://lgatto.github.io/MSnbase/reference/MChromatograms-class.html) class object from `xcms::chromatogram()`.

# profilePro 0.8.1

* Temporarily use `jasenfinch/BiocParallel.FutureParam` until the [pull request](https://github.com/HenrikBengtsson/BiocParallel.FutureParam/pull/8) for the package installation error due to the update of `BiocParallel` to version 1.30.0.

* Removed `readr` from the `Imports` field the package DESCRIPTION.

# profilePro 0.8.0

* Added missing import of `MSnbase::as.MSnExp.OnDiskMSnExp()`.

* Re-export [`plan()`](https://future.futureverse.org/reference/plan.html) from the [`future`](https://future.futureverse.org/) package.

* The parallel processing back-end for xcms processing is now specified using [`plan()`](https://future.futureverse.org/reference/plan.html) from [`future`](https://future.futureverse.org/) package.
This is enabled by the [`BiocParallel.FutureParam`](https://biocparallel.futureparam.futureverse.org/) package.

* Removed the `nCores` argument from [`profileParameters()`](https://jasenfinch.github.io/profilePro/reference/profileParameters.html).

* Removed [`patchwork`](https://patchwork.data-imaginist.com/) as a dependency.

# profilePro 0.7.1

* Removed parallel arguments from [`erah`](https://github.com/xdomingoal/erah-devel) processing routine as these now support the use of the [`future`](https://future.futureverse.org/) package.

* Fixed LC-MS processing routine when on a single ionisation mode is present.

* Fixed [`plotChromatogram()`](https://jasenfinch.github.io/profilePro/reference/plotChromatogram.html) for single ionisation mode data.

* Added unit tests for GC-MS processing using [`xcms`](https://github.com/sneumann/xcms).

* Improved plotting aesthetics of [`plotChromatogram()`](https://jasenfinch.github.io/profilePro/reference/plotChromatogram.html) and [`plotTIC()`](https://jasenfinch.github.io/profilePro/reference/plotTIC.html).

# profilePro 0.7.0

* Added a `NEWS.md` file to track changes to the package.

* Added [`pkgdown`](https://pkgdown.r-lib.org/) site available at https://jasenfinch.github.io/profilePro/

* The [`magrittr::%>%`](https://magrittr.tidyverse.org/reference/pipe.html) now re-exported.

* Improved console output for [`profilePro::profileProcess()`](https://jasenfinch.github.io/profilePro/reference/profileProcess.html).

* Added full examples for [`profilePro::profileProcess()`](https://jasenfinch.github.io/profilePro/reference/profileProcess.html).

* The `nCores` argument can now be supplied [`profilePro::profileProcess()`](https://jasenfinch.github.io/profilePro/reference/profileProcess.html) to select the number of cores to use for parallel processing.

* Added [`profilePro::availableTechniques()`](https://jasenfinch.github.io/profilePro/reference/availableTechniques.html) to list available processing techniques in the package.

* [`MetaboProfile`](https://jasenfinch.github.io/profilePro/reference/MetaboProfile-class.html) S4 class now inherits from [`ProfileParameters`](https://jasenfinch.github.io/profilePro/reference/ProfileParameters-class.html) S4 class.

* Added [get and set methods](https://jasenfinch.github.io/profilePro/reference/processed.html) for the [`MetaboProfile`](https://jasenfinch.github.io/profilePro/reference/MetaboProfile-class.html) S4 class.

* Added [get and set methods](https://jasenfinch.github.io/profilePro/reference/parameters.html) for the [`ProfileParamters`](https://jasenfinch.github.io/profilePro/reference/ProfileParameters-class.html) S4 class.

* Added validators for [`MetaboProfile`](https://jasenfinch.github.io/profilePro/reference/MetaboProfile-class.html) S4 class to ensure that sample information contains the correct fields and that the file names in the specified paths match those in the sample information.

* Added unit tests
