Installation
===========================

Required softwares
---------------------------

* OCaml 4.02.1 (Code is highly dependent on the specific compiler version. Do not try compiling with other versions.)
* OPAM.1.2.0, findlib and OMake to build.
* spotlib.2.5.2
* ppx_meta_conv.2.0.0
* ppx_orakuda.2.0.0 (OPAM name is orakuda.2.0.0)
* treeprint.2.0.0
* levenshtein.1.1.0
* ppx_test.1.1.0
* ocsigenserver.2.5
* eliom.4.1.0

These packages should be available soon in the OPAM repository. 
Windows is not supported.

OPAM package for OCamlOScope is not yet available... since it requires the following mending of external softwares.

Missing `cmxs` files
---------------------------

Some of the above packages lack `cmxs` files and you have to build them manually to run OCamlOScope on Ocsigen:

### `ocamlcommon.cmxs` is missing in OCaml 4.02.1.

This is really tricky, since `ocamlcommon.cmxa` depends on `prims.o` and `libocamlrun.a` (and `-lcurses` if curses is linked) but their info is not inside `cmxa`:

First, create `ocamlcommon2.cmxa` as follows, `ocamlcommon.cmxa` with prims.o, libcamlrun.a and -lcurses:

```shell
$ cd ~/.opam/4.02.1/build/ocaml
$ boot/ocamlrun ./ocamlopt -nostdlib -I stdlib -I otherlibs/dynlink -a -linkall -o compilerlibs/ocamlcommon2.cmxa byterun/prims.o byterun/libcamlrun.a -ccopt -lcurses utils/misc.cmx utils/tbl.cmx utils/config.cmx utils/clflags.cmx utils/terminfo.cmx utils/ccomp.cmx utils/warnings.cmx utils/consistbl.cmx parsing/location.cmx parsing/longident.cmx parsing/ast_helper.cmx parsing/syntaxerr.cmx parsing/parser.cmx parsing/lexer.cmx parsing/parse.cmx parsing/printast.cmx parsing/pprintast.cmx parsing/ast_mapper.cmx typing/ident.cmx typing/path.cmx typing/primitive.cmx typing/types.cmx typing/btype.cmx typing/oprint.cmx typing/subst.cmx typing/predef.cmx typing/datarepr.cmx typing/cmi_format.cmx typing/env.cmx typing/typedtree.cmx typing/printtyped.cmx typing/ctype.cmx typing/printtyp.cmx typing/includeclass.cmx typing/mtype.cmx typing/envaux.cmx typing/includecore.cmx typing/typedtreeIter.cmx typing/typedtreeMap.cmx typing/cmt_format.cmx typing/includemod.cmx typing/typetexp.cmx typing/parmatch.cmx typing/stypes.cmx typing/typecore.cmx typing/typedecl.cmx typing/typeclass.cmx typing/typemod.cmx bytecomp/lambda.cmx bytecomp/printlambda.cmx bytecomp/typeopt.cmx bytecomp/switch.cmx bytecomp/matching.cmx bytecomp/translobj.cmx bytecomp/translcore.cmx bytecomp/translclass.cmx bytecomp/translmod.cmx bytecomp/simplif.cmx bytecomp/runtimedef.cmx driver/pparse.cmx driver/main_args.cmx driver/compenv.cmx driver/compmisc.cmx
```

Then, create `ocamlcommon.cmxs` from `ocamlcommon2.cmxa`:

```shell
$ ocamlopt -linkall -shared compilerlibs/ocamlcommon2.cmxa -o ~/.opam/4.02.1/lib/ocaml/compiler-libs/ocamlcommon.cmxs
```

### `odoc_info.cmxs` is missing in OCaml 4.02.1.

The following command should create `odoc_info.cmxs`:

```shell
$ ocamlopt -linkall -shared ocamldoc/odoc_info.cmxa -o ocamldoc/odoc_info.cmxs
```

How to build
---------------------------

```shell
$ cp OMakeroot.in OMakeroot
$ omake
```

How to build DB
---------------------------

What does scraping need:

* `cmt` files. OCaml modules must be compiled with `OCAMLPARAM=_,bin-annot=1`, and the compiled source tree with `cmt` files must be kept.
* For softwares installed by OPAM, `OPAMKEEPBUILDDIR=yes` is required to keep ths source files

The following command should scrape the OCaml compiler source code and all the installed OCamlFind libraries and their corresponding OPAM build directorires, and stores the result under `data/` directory.

```shell
$ ./oco -d -c <ocaml compiler source dir>
```

How to query locally
---------------------------

After scraping, `oco` goes to its console query mode against the dumped data file.

How to start Eliom server
---------------------------------

Change directory to `eliom/`, copy `eliom/oco.conf.in` to `oco.conf` and change `@PORT@`, `@OCAMLLIB@` and `@OCAMLSRC@`, then:

```shell
$ ./starteliom.sh
```
