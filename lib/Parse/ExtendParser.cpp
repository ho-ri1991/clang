#include "clang/Parse/ExtendParser.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/DebugMessage.h"

using namespace clang;

//IdentifierInfo* TestII = nullptr;

static Token GenerateToken(tok::TokenKind Kind, SourceLocation Loc = SourceLocation{})
{
  Token Tok;
  Tok.startToken();
  Tok.setKind(Kind);
  Tok.setLocation(std::move(Loc));
  return Tok;
}

static Token GenerateIdentifierToken(IdentifierInfo* II, SourceLocation Loc = SourceLocation{})
{
  Token Tok;
  Tok.startToken();
  Tok.setKind(tok::identifier);
  Tok.setIdentifierInfo(II);
  Tok.setLocation(std::move(Loc));
  return Tok;
}

static Token GenerateIdentifierToken(Preprocessor& PP, const char* Name, SourceLocation Loc = SourceLocation{})
{
  auto& table = PP.getIdentifierTable();
  auto II = &table.get(Name);
  return GenerateIdentifierToken(II, std::move(Loc));
}

ExtendParser::ExtendParser(Preprocessor &PP, Sema &Actions, bool SkipFunctionBodies)
  : Parser(PP, Actions, SkipFunctionBodies) {}

ExtendParser::~ExtendParser() {}

StmtResult
ExtendParser::ParseStatementOrDeclaration(StmtVector &Stmts, AllowedConstructsKind Allowed,
                                           SourceLocation *TrailingElseLoc)
{
  DebugMessage d("ParseStatementOrDeclaration");
  PP.DumpToken(Tok);
  std::cout << std::endl;
  return Parser::ParseStatementOrDeclaration(Stmts, Allowed, TrailingElseLoc);
}

ExprResult
ExtendParser::ParseExpression(TypeCastState isTypeCast)
{
  DebugMessage d("ParseExpression");
  PP.DumpToken(Tok);
  std::cout << std::endl;
  return Parser::ParseExpression(isTypeCast);
}

bool
ExtendParser::ParseTopLevelDecl(DeclGroupPtrTy &Result)
{
  DebugMessage d("ParseTopLevelDecl");
  PP.DumpToken(Tok);
  std::cout << std::endl;
//  if (Tok.is(tok::identifier) && std::string(Tok.getIdentifierInfo()->getNameStart()) == "Test")
//    TestII = Tok.getIdentifierInfo();
  if (Tok.is(tok::identifier) && std::string(Tok.getIdentifierInfo()->getNameStart()) == "hoge")
  {
//    assert(TestII);
    std::cout<< "identifier hoge!" << std::endl;
    auto loc = Tok.getLocation();
    ConsumeAnyToken();
    CachedTokens toks;
    toks.push_back(GenerateToken(tok::kw_void));
    toks.push_back(GenerateIdentifierToken(PP, (new std::string("Test"))->c_str(), loc));
//    toks.push_back(GenerateIdentifierToken(TestII, loc));
    toks.push_back(GenerateToken(tok::coloncolon));
    toks.push_back(GenerateIdentifierToken(PP, (new std::string("print"))->c_str(), loc));
    toks.push_back(GenerateToken(tok::l_paren));
    toks.push_back(GenerateToken(tok::r_paren));
    toks.push_back(GenerateToken(tok::l_brace));
    toks.push_back(GenerateToken(tok::r_brace));
    toks.push_back(Tok);
    PP.EnterTokenStream(toks, true);
    ConsumeAnyToken();
  }
  return Parser::ParseTopLevelDecl(Result);
}

