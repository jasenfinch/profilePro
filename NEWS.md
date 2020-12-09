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
