# Makefile for class project
#
# COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
# All rights reserved.
#
# CMSC 22600 --- Compilers for Computer Languages
# Spring 2024
# University of Chicago
#
# targets:
#	make mlc	-- Build MiniML compiler and copy heap file to bin
#			   directory
#

SHELL =         /bin/sh
OS =            $(shell uname -s)

# we assume that sml (and thus ml-build is in the path)
#
ML_BUILD =	ml-build

HEAP_SUFFIX =	$(shell sml @SMLsuffix)

BINDIR =	../bin
LIBDIR =	../lib

CM_FILES =	$(wildcard */sources.cm)

COMMON_SRCS =	ast/ast.sml \
		ast/basis.sml \
		ast/data-con.sml \
		ast/dump-ast.sml \
		ast/exp.sml \
		ast/meta-var.sml \
		ast/ty-con.sml \
		ast/ty-var.sml \
		ast/type-rep.sml \
		ast/type.sml \
		ast/var.sml \
		bind-checker/binding.sml \
		bind-checker/context.sml \
		bind-tree/bind-basis.sml \
		bind-tree/bind-tree.sml \
		bind-tree/dump-bind-tree.sml \
		bind-tree/identifier-fn.sml \
		cfg/cfg-frag.sml \
		cfg/cfg-funct.sml \
		cfg/cfg-label.sml \
		cfg/cfg-rep.sml \
		cfg/cfg-util.sml \
		cfg/cfg-var.sml \
		cfg/cfg.sml \
		cfg/check-cfg.sml \
		cfg/dump-cfg.sml \
		cfg/print-cfg.sml \
		closure/closure.sml \
		closure/dump-free-vars.sml \
		closure/free-var-analysis.sml \
		codegen/arith-gen.sml \
		codegen/code-gen-info.sml \
		codegen/code-gen.sml \
		codegen/env.sml \
		codegen/mml-runtime.sml \
		codegen/util.sml \
		common/dump-util.sml \
		common/error.sml \
		common/id-gen-fn.sml \
		common/options.sml \
		common/prog-pt.sml \
		common/stamp.sml \
		convert/case-split.sml \
		convert/convert.sml \
		convert/env.sml \
		convert/live-var-analysis.sml \
		driver/main.sml \
		driver/unix-util.sml \
		llvm/llvm-block.sml \
		llvm/llvm-func.sml \
		llvm/llvm-global.sml \
		llvm/llvm-label.sml \
		llvm/llvm-module.sml \
		llvm/llvm-reg.sml \
		llvm/llvm-rep.sml \
		llvm/llvm-type.sml \
		llvm/llvm-var.sml \
		parse-tree/dump-parse-tree.sml \
		parse-tree/op-names.sml \
		parse-tree/parse-tree.sml \
		parser/parser.sml \
		parser/mml.grm \
		parser/mml.lex \
		prim/prim-cond.sml \
		prim/prim-type.sml \
		prim/prim.sml \
		prim/runtime.sml \
		simple-ast/case-contract.sml \
		simple-ast/case-info.sml \
		simple-ast/census.sml \
		simple-ast/check-simple.sml \
		simple-ast/dump-simple-ast.sml \
		simple-ast/let-float.sml \
		simple-ast/print-simple.sml \
		simple-ast/simple-ast.sml \
		simple-ast/simple-basis.sml \
		simple-ast/simple-data-con.sml \
		simple-ast/simple-var.sml \
		simple-opt/contract.sml \
		simple-opt/simple-opt.sml \
		simplify/normalize-ast.sml \
		simplify/simplify-type.sml \
		simplify/simplify.sml \
		type-checker/chk-exp.sml \
		type-checker/chk-pat.sml \
		type-checker/chk-ty.sml \
		type-checker/context.sml \
		type-checker/coverage.sml \
		type-checker/id-props.sml \
		type-checker/type-checker.sml \
		type-checker/type-util.sml \
		type-checker/unify.sml

MLC_SRCS =	$(COMMON_SRCS)

.PHONY:		install
install:	all

.PHONY:		all
all:		mlc runtime

.PHONY:		mlc
mlc:		mlc.$(HEAP_SUFFIX)
	mv mlc.$(HEAP_SUFFIX) $(BINDIR)

# build rule for compiler
mlc.$(HEAP_SUFFIX):	$(CM_FILES) $(MLC_SRCS)
	$(ML_BUILD) driver/sources.cm Main.main mlc

# build rule for runtime-system library
.PHONY:		runtime
runtime:
	(cd runtime; $(MAKE) install)

.PHONY:		clean
clean:
		rm -rf bin/*.$(HEAP_SUFFIX)
		rm -rf lib/mml-rt.o
		rm -rf .cm */.cm
		(cd runtime; make clean)
