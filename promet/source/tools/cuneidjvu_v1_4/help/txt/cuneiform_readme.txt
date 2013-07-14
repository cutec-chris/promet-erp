Cuneiform for Linux


Cuneiform is an multi-language OCR system originally developed
and open sourced by Cognitive Technologies. Cuneiform was
originally a Windows program, which was ported to Linux
by Jussi Pakkanen. 

This version of Cuneiform has been tested to work on the following
platforms.

Linux
FreeBSD
OS X
Windows using MSVC, MinGW, and Cygwin

The following people have sent patches or have otherwise helped the
project. If someone is missing, please let me know, so I can add them.

Keith Beaumont
Vincent Wagelaar
zanin
Alexander Schlegel
Alex Samorukov
yaleks
Serj Poltavskiy
Rene Rebe
Aleks Kuzemko
Tonal
Mike Ladwig
Dmitri Polevoy
Steven Van Ingelgem
Sven Eckelmann
Benjamin Kluck
raff
Julien
John A
Frik


Caveats

There are known limitations in this port. Among these are the following:

- it only works on x86 and amd64 processors
- there is no table recognition, because of
  https://bugs.launchpad.net/cuneiform-linux/+bug/260327

Patches to fix any of these issues are gladly accepted.


Compiling on unix

Extract the source and go to the root folder (the one this file is in).
Then type the following commands:

mkdir builddir
cd builddir
cmake -DCMAKE_BUILD_TYPE=debug ..
make
make install 

By default Cuneiform installs to /usr/local. You can specify a different prefix
by giving a command line switch "-DCMAKE_INSTALL_PREFIX=/what/ever/you/want"
to CMake.

Note that this does not use any optimizations. To enable them, replace
-DCMAKE_BUILD_TYPE=debug with -DCMAKE_BUILD_TYPE=release or with
-DCMAKE_BUILD_TYPE=relwithdebinfo. The latter builds with both optimization
and debug flags.


Compiling on OSX

Follow the instructions for unix above. You can also try the Xcode project
generator. It works but is not maintained.


Compiling on Windows

Run CMake. Point it to the directory you extracted the source to. 
Select a different directory for your build tree, You can not build inside
your source tree.

Select "Visual studio [the version you are using] project files", 
"MinGW makefiles", or "Cygwin makefiles" depending on your environment.
If you are using MinGW or Cygwin, set CMAKE_BUILD_TYPE to what you want
(probaly "debug" as above). Click "config", then "generate".

For Visual Studio, open the generated project files.
Then select "Build solution (F7)".

For MinGW and Cygwin, cd into your build tree and run "mingw32-make" or "make". 


Further info on configuration and running

If you have ImageMagick++ on your system, Cuneiform autodetects and builds
against it. Then Cuneiform can process any image that ImageMagick knows how
to open. Otherwise it can only read uncompressed BMP images.

If you want to run Cuneiform without installing it on your system, you
have to point the CF_DATADIR environment variable to a directory
containing the .dat files. These can be found in the "datafiles"
directory of the source package.

WINDOWS NOTE: Cuneiform tries to access its data files in the
directory that contains the DLLs. You can place Cuneiform
anywhere you want on your hard drive, just put the .dat files
in the same directory. This works also when using Cuneiform
as a library.


Running

After install you simply run.

cuneiform [-l language -o result_file -f [outputformat] extra_options ] <image_file>

Optional arguments are the following.

--dotmatrix uses a recognition mode optimised for text printed with a
dot matrix printer.

--fax uses a recognition mode optimised for text that has been faxed.

--singlecolumn disables page layout analysis and assumes that your
image consists of only one column of text.

If you do not define an output file with the -o switch, Cuneiform
writes the result to a file "cuneiform-out.[format]". The file extension
depends on your output format.

By default Cuneiform recognizes English text. To change the language use the
command line switch -l followed by your language string. To get a list of
supported languages type "cuneiform -l".

By default Cuneiform outputs plain text. There are several other output formats.
To get a list run the command "cuneiform -f".


Contact information

Project home page: https://launchpad.net/cuneiform-linux
Mailing list: https://launchpad.net/~cuneiform
