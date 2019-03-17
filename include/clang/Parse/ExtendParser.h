#ifndef LLVM_CLANG_PARSE_EXTEND_PARSER_H
#define LLVM_CLANG_PARSE_EXTEND_PARSER_H

#include "clang/Parse/Parser.h"

namespace clang {

class ExtendParser : public Parser {
public:
  ExtendParser(Preprocessor &PP, Sema &Actions, bool SkipFunctionBodies);
  ~ExtendParser() override;

  StmtResult
  ParseStatementOrDeclaration(StmtVector &Stmts, AllowedConstructsKind Allowed,
                              SourceLocation *TrailingElseLoc = nullptr) override;

  ExprResult ParseAssignmentExpression(TypeCastState isTypeCast = NotTypeCast) override;

  ExprResult
  ParseExpression(TypeCastState isTypeCast = NotTypeCast) override;

private:
  bool isExpandReflection;

  void ParseDeclarationSpecifiers(
      DeclSpec &DS,
      const ParsedTemplateInfo &TemplateInfo = ParsedTemplateInfo(),
      AccessSpecifier AS = AS_none,
      DeclSpecContext DSC = DeclSpecContext::DSC_normal,
      LateParsedAttrList *LateAttrs = nullptr,
      Expr* MetaCall = nullptr) override;
};

}

#endif
