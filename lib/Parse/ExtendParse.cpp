#include "clang/Parse/ExtendParser.h"

using namespace clang;

ExtendParser::ExtendParser(Preprocessor &PP, Sema &Actions, bool SkipFunctionBodies)
  : Parser(PP, Actions, SkipFunctionBodies) {}

StmtResult
ExtendParser::ParseStatementOrDeclaration(StmtVector &Stmts, AllowedConstructsKind Allowed,
                                           SourceLocation *TrailingElseLoc)
{
  return Parser::ParseStatementOrDeclaration(Stmts, Allowed, TrailingElseLoc);
}

ExprResult
ExtendParser::ParseExpression(TypeCastState isTypeCast)
{
  return Parser::ParseExpression(isTypeCast);
}

bool
ExtendParser::ParseTopLevelDecl(DeclGroupPtrTy &Result)
{
  return Parser::ParseTopLevelDecl(Result);
}

