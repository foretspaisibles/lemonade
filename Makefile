### Makefile -- Project Lemonade

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

PACKAGE=		lemonade
VERSION=		0.5.0-releng
OFFICER=		michipili@gmail.com

.sinclude "Makefile.config"

MODULE=			ocaml.lib:src

.if!empty(ENABLE_PPX_REWRITER:Myes)
MODULE+=		ocaml.prog:ppx
.endif

MODULE+=		ocaml.meta:meta
MODULE+=		ocaml.manual:manual

SUBDIR=			testsuite

EXTERNAL=		ocaml.findlib:broken
EXTERNAL+=		ocaml.findlib:mixture

.if!empty(ENABLE_PPX_REWRITER:Myes)
EXTERNAL+=		ocaml.findlib:ppx_tools.metaquot
.endif

CONFIGURE+=		Makefile.config.in

.include "generic.project.mk"

### End of file `Makefile'
