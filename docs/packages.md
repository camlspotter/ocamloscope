Package handling
================================

OCaml has two major package systems. OCamlFind(findlib) and OPAM:

* OCamlFind(findlib) packages helps the use of libraries. It automatically resolves library dependencies and gives complilation and link flags of libraries you want to use.
* OPAM packages are to help installing libraries from the source. It automatically downloads, compiles and installs library source code.

Although both of them are pretty important in today's OCaml programming, as far as I know, their connections are weak: 
OPAM packages themselves have no information what OCamlFind packages they actually install. 
I think it is a natural design: 
it is hard to tell which OCamlFind packages are installed by an OPAM package, 
and sometimes it is statically impossible, since OPAM does not assume 
the specific build tool for OCaml softwares and they may install different
number of OCamlFind packages depending on the installation enviornment. 

However, this lack of information between two package worlds causes
a problem for package users: "With OCamlOScope, I found this function of 
this OCamlFind package useful. Which OPAM package should I install for it?"

To help this situation, OCamlOScope scans the OCaml library installation 
and the OPAM build directory, then create a database which OCamlFind packages
are provided by which OPAM package.
This is done by finding out where installed `cmi` files of OCamlFind pakcages
come from in the OPAM build directory.
(Therefore you have to keep the build source files with object files.)
You can see the result here: http://ocamloscope.herokuapp.com/packages

So far this works pretty fine and enough fast with the help of some heuristics
to save the number of scanned `cmi` files, but with the following limitations:

* It extracts information valid for the specific environment where the scraping is performed. It may be wrong in other environments.
* Ambiguity: some packages are hard to tell where they are from: for example, OPAM packages `deriving` and `deriving-ocsigen` install almost the same set of `cmi` files.

I hope the latter point can be improved by scanning more files such as `META`.
