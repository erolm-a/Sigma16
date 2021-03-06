#---------------------------------------------------------------------
# makefile for Sigma16
# John T. O'Donnell.  See README.txt and LICENSE.txt
#---------------------------------------------------------------------

# Requires: ghc, pandoc

# Usage

#  make doc               same as make user-guide
#  make user-guide        create doc/html files from markdown source
#  make compile           build on Linux
#  make clean             delete object code and backup files
#  make very-clean        also delete generated doc/html files

# The following are for development only:
#    make archive-tarball   save a .tgz tarball in archive directory
#    make archive-copy      save a full copy in archive directory
#    make list-archive      list the archive directory

# See README.txt for file and directory layout.  The ArchiveLocation
# is relative to the parent directory of Sigma16-i.j.k

.DEFAULT_GOAL := compile
ArchiveLocation := ../../Sigma16/archive

#---------------------------------------------------------------------
# Calculate locations and times, don't need to edit these

# Generate filename for archive
PackagePath := $(shell pwd)
PackageName := $(shell basename $(PackagePath))
DateTimeStamp := $(shell date +%Y-%m-%d-at-%H-%M)
BuildDate != date +%d\ %b\ %Y
TarballName := $(PackageName)-on-$(DateTimeStamp).tgz
FullCopyName := $(PackageName)-on-$(DateTimeStamp)

# Provide version number in generated html documentation
Sigma16Version != grep < Sigma16.cabal ^version
VersionDate = $(Sigma16Version)

# make show-parameters -- print calculated parameters
.PHONY : show-parameters
show-parameters :
	echo PackagePath = $(PackagePath)
	echo PackageName = $(PackageName)
	echo DateTimeStamp = $(DateTimeStamp)
	echo BuildDate = $(BuildDate)
	echo TarballName = $(TarballName)
	echo FullCopyName = $(FullCopyName)
	echo ArchiveLocation = $(ArchiveLocation)
	echo Sigma16Version = $(Sigma16Version)
	echo VersionDate = $(VersionDate)

#---------------------------------------------------------------------
# Compilation

.PHONY : compile
compile :
	cabal configure
	make doc
	cabal build
	mv -f dist/build/Sigma16/Sigma16 .

# A console window will open; putStr output will appear on that, which
# is useful for debugging and development but distracting for users.
# Use the following to prevent the console window from opening:

  # cabal build --ghc-options=-optl-mwindows

#---------------------------------------------------------------------
# Generate documentation

# make doc -- generate the html documentation from markdown source
.PHONY : doc
doc :
	mkdir -p datafiles/doc/html
	cp -r src/docsrc/figures datafiles/doc/html
	cp src/docsrc/style.css datafiles/doc/html
	pandoc --standalone \
          --table-of-contents --toc-depth=3 \
          --variable=date:'$(VersionDate)' \
          --variable=css:style.css \
          -o datafiles/doc/html/index.html \
	  src/docsrc/index.txt

#---------------------------------------------------------------------
# Maintaining the directories

# make clean -- remove backup and object files, and temporary
# compilation files.  Leaves executable, documentation, and source in
# place.

.PHONY : clean
clean :
	rm -rf dist      # object files for this package
	find . \( -name '*~' -o -name '*.hi' \
	  -o -name '*.bak' \) -delete
	runhaskell Setup clean

# make very-clean -- remove html documentation files generated by
# pandoc, leaving a minimal source directory

.PHONY : very-clean
very-clean :
	make clean
	rm -rf datafiles/doc
	rm -f Sigma16*

# make archive-tarball -- create a tgz file of the entire
# Sigma16-i.j.k directory and place it in the archive directory.  See
# README.txt for directory layout.
.PHONY : archive-tarball
archive-tarball :
	cd .. ; \
	  pwd ; \
	  tar -czf $(TarballName) $(PackageName) ; \
	  mv $(TarballName) $(ArchiveLocation)

# make archive-copy -- create a full copy of the entire Sigma16-i.j.k
# directory and place it in the archive directory.
.PHONY : archive-copy
archive-copy :
	cd ..; \
	  pwd ; \
	  cp -rp $(PackagePath) $(ArchiveLocation)/$(FullCopyName)

# make list-archive -- list contents of the archive directory
.PHONY : list-archive
list-archive :
	ls -lctr ../$(ArchiveLocation)

#----------------------------------------------------------------------
# Currently unsupported, deprecated or out of date...

#----------------------------------------------------------------------
# Still need to enable launching by clicking application icon...

# Usage: copy-gtk-auxiliary-files.  There are a number of files and
# directories that are needed by gtk at runtime.  This operation
# copies them into the Sigma16-i.j.k directory.

.PHONY : copy-gtk-auxiliary-files
copy-gtk-auxiliary-files :
	cp --update -r $(RequiredFiles)/etc .
	cp --update -r $(RequiredFiles)/share .

# Usage: copy-dlls.  This is only for Windows; copy the required dlls
# into the Sigma16-i.j.k directory.
.PHONY : copy-dlls
copy-dlls :
	cp --update $(RequiredFiles)/dll-gtk3/* .

#  make compile-win       compile and create Windows bin

RequiredFiles := ../required-files
# A directory containing a number of files (xml, dll, etc.) that are
# needed by gtk or Windows

# make clean, with gtk
# 	rm -rf src/etc       # temporary files for gtk
# 	rm -rf src/share     # contains xml file for file chooser

# Usage: make build-windows.  Build windows binary from clean source.
# The directory can be copied to another location, disk, or machine,
# and ./Sigma16 should run there.
.PHONY : build-windows
build-windows :
	date
	make recompile-win
	make doc
	make copy-gtk-auxiliary-files
	make copy-dlls
	make copy-s16-files
	date

# Usage: make copy-s16-files.  Copy some important public text files
# into the top level directory for a Windows binary installation; they
# are swamped in lots of other files in the actual Sigma16 directory.
.PHONY : copy-s16-files
copy-s16-files :
	cp -f README.txt ../..
	cp -f LICENSE*.txt ../..

# New compilation procedure, Oct 21 2015
# using gtk3 and ghc-7.10.2
# cabal isn't working but this seems ok
# I got gtk3 installed successfully for my user account,
# don't need sandbox
# Nov 7 2015: got it working with cabal again

.PHONY : userunhaskell
 userunhaskell :
	runhaskell Setup configure --user
	runhaskell Setup build
	runhaskell Setup haddock --css=doc/style.css
	mv dist/build/Sigma16/Sigma16.exe .
	cp --update ../needed-for-windows/dll-gtk3/* .
	cp --update -r ../needed-for-windows/etc .

# ------------------------------------------------------------
# sandbox

# With Msys2, the sandbox doesn't work and neither do the following:

  # make build-windows        compile for Windows from clean source
  # make sandbox-setup        prepare to build packages locally
  # make clean-sandbox        remove the sandbox, which is huge

# make sandbox-setup : prepare a sandbox for local compilation.  The
# purpose of this is to avoid problems where dependencies need to be
# compiled but they would risk breaking unrelated programs
.PHONY : sandbox-setup
sandbox-setup :
	date
	cabal sandbox init
#	cabal sandbox add-source $(HydraLocation)
	cabal install --only-dependencies
	cabal configure
	date

# Usage: make clean-sandbox.  Remove other Haskell packages in the
# local sandbox.  This saves a lot of space but it takes time to
# reconstruct, and there is a risk that the gtk packages won't build
# if anything is missing from the environment.
.PHONY : clean-sandbox
clean-sandbox :
	/bin/rm -rf .cabal-sandbox
	/bin/rm -f cabal.sandbox.config
#	cabal sandbox delete # sandbox for external packages Doesn't
#	work?
