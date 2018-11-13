set(LLVM_LINK_COMPONENTS
  MC
  MCParser
  Support
  )

link_directories(/disk1/koudai/.pyenv/versions/3.6.5/lib)
add_clang_library(clangParse
  ParseAST.cpp
  ParseCXXInlineMethods.cpp
  ParseDecl.cpp
  ParseDeclCXX.cpp
  ParseExpr.cpp
  ParseExprCXX.cpp
  ParseInit.cpp
  ParseObjc.cpp
  ParseOpenMP.cpp
  ParsePragma.cpp
  ParseStmt.cpp
  ParseStmtAsm.cpp
  ParseTemplate.cpp
  ParseTentative.cpp
  Parser.cpp
  ExtendParser.cpp
  PybindTest.cpp

  LINK_LIBS
  clangAST
  clangBasic
  clangLex
  clangSema
  rt
  pthread
  dl
  util
  python3.6m
#  pybind11::embed
  )

pybind11_add_module(PybindTest PybindTest.cpp)
#target_link_libraries(clangParse PRIVATE pybind11::embed rt pthread dl util)

