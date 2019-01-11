----------------------------------------------------------------------
Sigma16: Computer architecture and digital circuit
----------------------------------------------------------------------

See index.html

Contents of README.txt
~~~~~~~~~~~~~~~~~~~~~~

  Synopsis
  Author
  Overview
  License
  Quick start
  Compiling and installing
  Documentation
  Directory layout

Synopsis
~~~~~~~~

This software provides a complete environment for working with machine
language programs and digital circuits.  It is suitable for education
and research.  The software implements a 16-bit computer architecture,
Sigma16, which has a simple but realistic instruction set.

Author
~~~~~~

John T. O'Donnell
Copyright (c) 2017 John T. O'Donnell

* web:    www.dcs.gla.ac.uk/~jtod/
* email:  john.odonnell@glasgow.ac.uk
* email:  john.t.odonnell9@gmail.com

Patched by Enrico "erolm_a" Trombetta
* web:    erolm_a.gitlab.io
* email:  2396702t@student.gla.ac.uk

Overview
~~~~~~~~

Sigma16 is a 16-bit computer architecture, with a regular and simple
instruction set.  It is an academic design, not a commercial machine,
and it is intended for research and teaching in computer systems.
This software provides a complete environment for developing and
experimenting machine language and digital circuits.

It contains an assembler and emulator, along with an interactive
design environment.  The tools allow you to develop and run programs
in assembly or machine language, and a collection of example programs
is provided.  The emulator shows the effect of each instruction on the
registers, memory, and state of the machine.

Sigma16 is designed using Hydra, a functional computer hardware
description language for designing digital circuits.  Hydra provides
several levels of abstraction, from logic gates, combinational and
synchronous circuits, the register transfer level, datapath and
control, to processor instruction set architectures.

License
~~~~~~~

This is free and open source software and is distributed under the GPL
License, either version 3 or (at your option) any later version.  See
the LICENSE and LICENSE_GPL3 files in the src directory.  This program
is experimental software, and is under development.

Quick start
~~~~~~~~~~~

  * bash run (or ./run) Works in Linux, or on Windows emacs eshell.

  * bash runcygwin  (or ./cygwin)  Works with  Windows cygwin.

You can run Sigma16 from the source files using ghci.  This requires
that you have the Haskell compiler ghc installed.  The following
makefile command will launch it:

  $ make runghci    (launch ghci and point it to the application code)
  > main            (launch Sigma16)
  $ ...             (enter commands to Sigma16 text user interface)

Alternatively you can enter the full commands directly, without using
make.

  $ cd Sigma16-i.j.k (where i.j.k is the version number)
  $ ghci -isrc/srcHaskell
  > :load MainText
  > main
  $ ... (enter commands to Sigma16 text user interface)


Compiling and installing
~~~~~~~~~~~~~~~~~~~~~~~~

You can either obtain a pre-compiled binary installation, or you can
obtain the source code and compile it.  In either case, the
Installation section in the user manual explains how to install and
run the software; the Documentation section in this file explains
where to find it.  To compile the application you need a complete
installation of the ghc Haskell compiler, a number of standard
packages, and the gtk3 GUI toolkit.  All the required software is open
source.

N.B. On many Linux distros you may have a hard time in compiling.
For example, on Arch Linux all the libraries are dynamically loaded
and thus their interface objects are declared so. Unfortunately ghc
by defaults does not load those libraries dynamically.
Read https://wiki.archlinux.org/index.php/haskell#Problems_with_linking
for details and a possible workaround.
(Note added by Enrico Trombetta)

Documentation
~~~~~~~~~~~~~

The system documentation includes a tutorial to help you get started,
an explanation of the Sigma16 architecture, introduction to
programming techniques, and a reference manual.  There is also a set
of examples in the directory programs.

The documentation is in html, so it can be read in a browser.  The
html is generated from source written in markdown format.  In a clean
source distribution, the html will be missing, but the source is
readable and it's easy to generate the html (make doc).  Once you
launch the application, you can access the documentation using the
Help menu, which will launch the html file in your default web
browser.

*  Documentation html      datafiles/doc/html/index.txt
*  Documentation source    src/docsrc/index.txt


Directory layout
~~~~~~~~~~~~~~~~

The files annotated with (make ...) are not present in a clean source
directory, but are produced by make.  They should be present in a
binary distribution.

 directory layout
  Hydra
    current-version
      Hydra-i.j.k
      
    Hydra
      archive
    Sigma16
      archive

  Sigma16-development
      archive
      required-files
      ...
      Sigma16-i.j.k
         bin                executable code
         LICENSE.txt
         LICENSE_GPL3.txt 
         makefile
         README.txt         this file
         Sigma16
            Examples
            Library
         datafiles          contains data read by the application
   	    doc             documentation in html, generated (make doc)
            static          fixed datafiles
	 src
            cabal.config    options for cabal program
 	    dist            (make compile-win) object code
            Setup.hs        used by cabal to compile source
            Sigma16.cabal   metadata for the application
            Sigma16.config  options to be read by the application
	    srcDoc          documentation source in markdown format
	    srcHaskell      source code

