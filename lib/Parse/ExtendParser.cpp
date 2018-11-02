#include "clang/Parse/ExtendParser.h"
#include "clang/Lex/Preprocessor.h"

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
//  if (Tok.is(tok::identifier) && std::string(Tok.getIdentifierInfo()->getNameStart()) == "Test")
//    TestII = Tok.getIdentifierInfo();
  if (Tok.is(tok::identifier) && std::string(Tok.getIdentifierInfo()->getNameStart()) == "hoge")
  {
//    assert(TestII);
    auto loc = Tok.getLocation();
    ConsumeAnyToken();
    auto ptoks = new CachedTokens();
    auto& toks = *ptoks;
    toks.push_back(GenerateToken(tok::kw_void, loc));
    toks.push_back(GenerateIdentifierToken(PP, (new std::string("Test"))->c_str(), loc));
    toks.push_back(GenerateToken(tok::coloncolon, loc));
    toks.push_back(GenerateIdentifierToken(PP, (new std::string("print"))->c_str(), loc));
    toks.push_back(GenerateToken(tok::l_paren, loc));
    toks.push_back(GenerateToken(tok::r_paren, loc));
    toks.push_back(GenerateToken(tok::l_brace, loc));
    toks.push_back(GenerateToken(tok::r_brace, loc));
    toks.push_back(Tok);
    PP.EnterTokenStream(toks, true);
    ConsumeAnyToken();
  }
  return Parser::ParseTopLevelDecl(Result);
}

Parser::DeclGroupPtrTy ExtendParser::ParseCXXClassMemberDeclaration(
    AccessSpecifier AS, AttributeList *Attr,
    const ParsedTemplateInfo &TemplateInfo,
    ParsingDeclRAIIObject *DiagsFromTParams)
{
  if (Tok.is(tok::l_square))
  {
    ConsumeAnyToken();
    CachedTokens toks;
    ConsumeAndStoreUntil(tok::r_square, toks, /*StopAtSemi=*/true, /*ConsumeFinalToken=*/false);
    if (Tok.is(tok::semi))
      return DeclGroupPtrTy{};
    ConsumeAnyToken();
    if (Tok.isNot(tok::identifier))
      return DeclGroupPtrTy{};
    auto name = Tok.getIdentifierInfo()->getNameStart();
    auto loc = ConsumeToken();
    if (Tok.isNot(tok::l_brace))
      return DeclGroupPtrTy{};
    ConsumeAnyToken();
    auto pGeneratedToks = new CachedTokens();
    auto& generatedToks = *pGeneratedToks;
    while(Tok.isNot(tok::r_brace))
    {
      if (Tok.is(tok::identifier))
      {
        if (Tok.getIdentifierInfo()->getNameStart() == std::string("get"))
        {
          ConsumeToken();
          if (Tok.is(tok::l_brace))
          {
            for (auto tok: toks)
              generatedToks.push_back(tok);
            generatedToks.push_back(GenerateToken(tok::amp, loc));
            generatedToks.push_back(GenerateIdentifierToken(PP, (new std::string(name + std::string("__")))->c_str(), loc));
            generatedToks.push_back(GenerateToken(tok::l_paren, Tok.getLocation()));
            generatedToks.push_back(GenerateToken(tok::r_paren, Tok.getLocation()));
            generatedToks.push_back(Tok);
            ConsumeAnyToken();
            ConsumeAndStoreUntil(tok::r_brace, generatedToks, false, true);
          }
          else if(Tok.is(tok::kw_const))
          {
            for (auto tok: toks)
              generatedToks.push_back(tok);
            generatedToks.push_back(GenerateToken(tok::kw_const, loc));
            generatedToks.push_back(GenerateToken(tok::amp, loc));
            generatedToks.push_back(GenerateIdentifierToken(PP, (new std::string(name + std::string("__")))->c_str(), loc));
            generatedToks.push_back(GenerateToken(tok::l_paren, Tok.getLocation()));
            generatedToks.push_back(GenerateToken(tok::r_paren, Tok.getLocation()));
            generatedToks.push_back(Tok);
            ConsumeAnyToken();
            if (Tok.isNot(tok::l_brace))
              return DeclGroupPtrTy{};
            generatedToks.push_back(Tok);
            ConsumeAnyToken();
            ConsumeAndStoreUntil(tok::r_brace, generatedToks, false, true);
          }
        }
        else if (Tok.getIdentifierInfo()->getNameStart() == std::string("set"))
        {
          ConsumeToken();
          for (auto tok: toks)
            generatedToks.push_back(tok);
          generatedToks.push_back(GenerateToken(tok::amp, loc));
          generatedToks.push_back(GenerateIdentifierToken(PP, (new std::string(name + std::string("__")))->c_str(), loc));
          if (Tok.isNot(tok::l_paren))
            return DeclGroupPtrTy{};
          generatedToks.push_back(Tok);
          ConsumeAnyToken();
          ConsumeAndStoreUntil(tok::r_paren, generatedToks, false, true);
          if (Tok.isNot(tok::l_brace))
            return DeclGroupPtrTy{};
          generatedToks.push_back(Tok);
          ConsumeAnyToken();
          ConsumeAndStoreUntil(tok::r_brace, generatedToks, false, true);
        }
      }
      else
      {
        return DeclGroupPtrTy{};
      }
    }
    PP.EnterTokenStream(generatedToks, true);
    ConsumeAnyToken();
  }
  return Parser::ParseCXXClassMemberDeclaration(AS, Attr, TemplateInfo, DiagsFromTParams);
}

