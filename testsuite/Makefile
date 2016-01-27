### Makefile -- Test suite

# Lemonade (https://github.com/michipili/lemonade)
# This file is part of Lemonade
#
# Copyright © 2013–2015 Michael Grünewald
#
# This file must be used under the terms of the CeCILL-B.
# This source file is licensed as described in the file COPYING, which
# you should have received as part of this distribution. The terms
# are also available at
# http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt

PROGRAM=		unit-testing

.sinclude "${SRCDIR}/Makefile.config"

SRCS=			testList.ml
SRCS+=			testMaybe.ml

.if!empty(ENABLE_PPX_REWRITER:Myes)
SRCS+=			testPPX.ml
.endif

SRCS+=			main.ml

OCAMLLFLAGS+=		-linkall
.if!empty(ENABLE_PPX_REWRITER:Myes)
OCAMLCFLAGS+=		-ppx ${.OBJDIR}/../ppx/ppx_lemonade
.endif


do-test: build
	./unit-testing

do-install:
	${NOP}

.include "ocaml.prog.mk"

### End of file `Makefile'
