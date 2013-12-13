# Code for Machine Learning for Hackers #

This repository contains all of the code examples for [Machine Learning for Hackers](http://shop.oreilly.com/product/0636920018483.do) (2012).  The code may not appear exactly as it does in the text, as additional comments and modifications may have been added since publication.


## Getting started ##

The code in this book relies on several R libraries.  Two libraries, `RCurl` and `XML` require additional software to be installed:

 - [http://curl.haxx.se/](curl) (for `RCurl`)
 - [http://xmlsoft.org/](libxml2-dev) (for `XML`)

To make sure you have all of the requite libraries run the `package_installer.R` script. In your R console type the following:

	> source("package_installer.R")

When running the installer inside R you will be asked where to save the library files.  If you'd like to install the libraries globally, you can run script at the command-line with admin privileges:

	$ sudo RScript package_installer.R



## Authors ##

 - Drew Conway, Department of Politics, New York University [http://drewconway.com/](http://drewconway.com/)
 - John Myles White, Data Science, Facebook [http://www.johnmyleswhite.com/](http://www.johnmyleswhite.com/)

## License ##

All source code is copyright (c) 2012, under the Simplified BSD License.  
For more information on FreeBSD see: [http://www.opensource.org/licenses/bsd-license.php](http://www.opensource.org/licenses/bsd-license.php)

All images and materials produced by this code are licensed under the Creative Commons 
Attribution-Share Alike 3.0 United States License: [http://creativecommons.org/licenses/by-sa/3.0/us/](http://creativecommons.org/licenses/by-sa/3.0/us/)
