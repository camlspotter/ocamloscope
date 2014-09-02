Installation
===========================

Required softwares
---------------------------

* OCaml 4.01.0 (Code is highly dependent on the specific compiler version. Do not try compiling with other versions.)
* OPAM, findlib and OMake to build.
* spotlib.2.4.1
* meta_conv.1.1.4
* orakuda.1.2.1
* treeprint.1.0.3
* levenshtein.1.0.0
* pa_ounit.109.53.02
* ounit.2.0.0
* eliom.3.0.3

These packages should be available from OPAM. Windows is not supported.

Missing `cmxs` files
---------------------------

Some of the above packages lack `cmxs` files and you have to build them manually:

* `ocamlcommon.cmxs` and `odoc_info.cmxs` are missing in OCaml 4.01.0. You have to build them using `0fix_ocamlcommon.cmxs`.
* `ounit.cmxs` of OUnit 2.0.0 misses `oUnit2.cmx`. You have to fix it using `0fix_ounit_cmxs`

How to build
---------------------------

```shell
$ omake
```

How to build DB
---------------------------

What does scraping need:

* `cmt` files. OCaml modules must be compiled with `OCAMLPARAM=_,bin-annot=1`, and the compiled source tree with `cmt` files must be kept.
* For softwares installed by OPAM, `OPAMKEEPBUILDDIR=yes` is required to keep ths source files
* OPAM 1.1.0 has a bug and cannot keep the compiler source even with `OPAMKEEPBUILDDIR=yes`.

The following command should scrape the OCaml compiler source code and all the installed OCamlFind libraries and their corresponding OPAM build directorires, and stores the result under `data/` directory.

```shell
$ ./oco -d -c <ocaml compiler source dir>
```

After scraping, `oco` goes to its console query mode.

How to start Eliom server
---------------------------------

Change directory to `eliom/`, copy `eliom/oco.conf.in` to `oco.conf` and change `@PORT@`, `@OCAMLLIB@` and `@OCAMLSRC@`, then:

```shell
$ ./starteliom.sh
```
