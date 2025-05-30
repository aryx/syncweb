           Syncweb, Literate Programming meets Unison


Introduction
----------------

Syncweb is a command-line tool enabling programmers to use the
literate programming[1] development methodology, using the noweb[2]
tool, while still being able to modify the generated files
from the literate document. syncweb provides a way to
"synchronize" the possibly modified original document with its
possibly modified views with an interface similar to unison[3]. In
addition, syncweb synchronizes data at a fine grained level by
computing and storing md5sum of the different chunks.

Note that literate programming is different from using javadoc, or
doxygen, or ocamlweb. Noweb, and syncweb, do not provide the same kind
of services. Literate programming allows programmers to explain their
code in the order they think the code will be better understood, and
allows among other things to explain code piece by piece with the
possibility to present a high-level view first of the code. Moreover,
because noweb is essentially a macro-processing language, one can also
"program" at the noweb level, which can sometimes overcome some of the
limitations or the language of the documented program. For instance,
for OCaml programs, using noweb frees the programmer to declare the types both
in the .mli and .ml file, avoiding tedious copy-paste and maintenance
problems. One can also do some forms of Aspect-oriented Programming
at the noweb level.

Syncweb continues the long tradition of tools like cweb and noweb of
using a very confusing name in the Web-era for a program that has
nothing to do with the Web. Syncweb does not synchronize the Web.

Example of use
----------------

To install syncweb, see install.txt. Once installed, you can
test it by applying it to some of the files in the demos/ directory.
For instance, to generate for the first time the .mli and .ml file 
mentionned in demos/demo.ml.nw do:

  $ cd demos
  $ syncweb demo.ml.nw    demo.ml
  $ syncweb demo.ml.nw    demo.mli

To generate the latex document demo.tex from the literate document,
and combine it with another demo_main.tex file containing extra 
documentation do:

  $ noweblatex demo.ml.nw > demo.tex
  $ pdflatex demo_main.tex     # demo_main.tex contains a \input{demo}
  
You can then modify either the literate document demo.ml.nw, or the ml
files and propagate the modifications to each other using again
syncweb. For instance, after modifying the beginning of a chunk in
demo.ml.nw, and the end of a chunk in demo.ml, one can synchronize by
doing:

  $ syncweb demo.ml.nw demo.ml

The output of the tool, with some of the input given by the user indicated
by a !!! mark, should look like this:

  DIFF
  <<<<<<< orig <<<<<<<<
  type x = int
  ====================
  type x = int
  type y = float
  >>>>>>> view >>>>>>>>
  orig          view
          <---- changed ? (y/n) !!!y!!!
  DIFF
  <<<<<<< orig <<<<<<<<
  <<type>>
  let foo x = 1
  
  let bar y = 2
  
  ====================
  <<type>>
  let foo x = 1
  
  >>>>>>> view >>>>>>>>
  orig          view
  changed  ---->        ? (y/n) !!!y!!!
  orig has been updated
  view has been regenerated

syncweb can automatically infer the direction the change
should go (---> or <---- in the preceding output) by storing
somewhere the md5sum of the original chunks; it uses
the same technique than unison.

Marks
-------------------

To make it possible to backpropagate modifications from a view 
(e.g. a .ml file), to its original document (a .nw file), 
syncweb generates in the view file some special comment marks
like in demos/demo.mli:
 
   (* nw_s: type |2b70b211995152060feb826763f38330*)
   type x = int
   type y = float
   (* nw_e: type *)

update: by using the -md5sum_in_auxfile and -less_marks command
line flags, syncweb can generate less verbose marks. In particular
the md5sum code can be stored in an auxillary file (a .md5sum_xxx file),
which makes it even closer to what unison does (with its ~/.unison/
directory). The previous view demos/demo.mli then looks like

   (* s: type *)
   type x = int
   type y = float
   (* e: type *)

Limitations
-------------------

One must not modify the marks and if one move some code,
one must take care to move both the start and ending mark
correspondingly. An easier way to move some code is of
course to modify the literate document.

The current version of syncweb does not handle well files using tabs
instead of spaces. This can usually be easily overcomed by configuring
your editors, for instance with Emacs by adding the command
(setq-default indent-tabs-mode nil) in your .emacs file. You can also
use the M-x untabify Emacs macro to convert files. Nevertheless, 
for certain files like Makefiles, which relies on the tab symbol,
syncweb can not be used. 


Options
-------------------

By default syncweb generates marks suited for OCaml programs. You
can change this behavior by using the -lang option, for instance
with: 

  $ syncweb -lang C demo.nw foo.c

The -md5sum_in_auxfile and -less_marks options have been discussed
previously.

Makefile example
-------------------

If you want to integrate syncweb and literate programming in your
development cycle, just add a 'sync' and 'pdf' targets 
where you list all the files involved in literate documents
as in:

  SYNCWEB=syncweb
  NOWEB=noweblatex
  
  sync:
  	$(SYNCWEB) demo.ml.nw demo.mli
  	$(SYNCWEB) demo.ml.nw demo.ml
  
  pdf:
  	$(NOWEB) demo.ml.nw > demo.tex
  	pdflatex demo.tex
  	pdflatex demo.tex
    

Extra information
-------------------

The emacs/ directory contains the noweb-mode.el emacs mode to edit
noweb files allowing among other things the correct fontification of
both the latex and program fragments. This directory contains also
some hacks to change the default behavior of noweb-mode.el that you
may find useful, as well as hacks to use when editing the view file.
For instance one can easily hide the marks in the view files.


References
---------------

[1] http://en.wikipedia.org/wiki/Literate_programming
[2] http://www.cs.tufts.edu/~nr/noweb/
[3] http://www.seas.upenn.edu/~bcpierce/unison/

