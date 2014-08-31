Installation
===========================

Missing cmxs files
---------------------------

* `ocamlcommon.cmxs` and `odoc_info.cmxs` are missing in OCaml 4.01.0. You have to build them using `0fix_ocamlcommon.cmxs`.
* `ounit.cmxs` of OUnit 2.0.0 misses `oUnit2.cmx`. You have to fix it using `0fix_ounit_cmxs`

Required packages
---------------------------

* spotlib.2.4.1
* meta_conv.1.1.4
* orakuda.1.2.1
* treeprint.1.0.3
* ocaml_levenshtein.1.0.0
* pa_ounit.109.53.02
* ounit.2.0.0

How to build
---------------------------

```shell
$ omake
```

How to build DB
---------------------------

What does the scraper need:

* First of all, `cmt` files are required. OCaml modules must be compiled with `OCAMLPARAM=_,bin-annot=1`, and the compiled source tree with `cmt` files must be kept.
* For softwares installed by OPAM, `OPAMKEEPBUILDDIR=yes` is required to keep ths source files
* OPAM 1.1.0 has a bug and cannot keep the compiler source.

```shell
$ ./oco -d -c <ocaml compiler source dir>
```
