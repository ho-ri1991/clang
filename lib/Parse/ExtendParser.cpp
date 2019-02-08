#include "clang/Parse/ExtendParser.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include <iostream>
#include <cassert>

using namespace clang;

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

CachedTokens MetaLateTokenizer(Parser* P, const char* Chars)
{
  llvm::SmallVector<char, 0> buf;
  auto Len = std::strlen(Chars);
  buf.append(Chars, Chars + Len + 1);
  buf.set_size(Len);
  auto mem_buf = llvm::make_unique<llvm::SmallVectorMemoryBuffer>(std::move(buf));
  auto mem_buf_ptr = mem_buf.get();
  auto& PP = P->getPreprocessor();
  auto& sourceMgr = PP.getSourceManager();
  SourceLocation loc{};
  auto fileId = sourceMgr.createFileID(std::move(mem_buf), SrcMgr::C_User, 0, 0, loc);
  CachedTokens Result;
  Lexer TheLexer(fileId, mem_buf_ptr, sourceMgr, PP.getLangOpts());
  TheLexer.SetCommentRetentionState(true);
  Token tok;
  while(!TheLexer.LexFromRawLexer(tok))
  {
    if (tok.is(tok::raw_identifier))
    {
      Result.push_back(GenerateIdentifierToken(PP, tok.getRawIdentifier().str().c_str(), loc));
    }
    else
    {
      Result.push_back(tok);
    }
  }
  if (tok.is(tok::raw_identifier))
  {
    Result.push_back(GenerateIdentifierToken(PP, tok.getRawIdentifier().str().c_str(), loc));
  }
  else
  {
    Result.push_back(tok);
  }
  return Result;
}

Decl* MetaLateParser(Parser* P, CachedTokens& Tokens)
{
  Tokens.push_back(P->getCurToken());
  P->getPreprocessor().EnterTokenStream(Tokens, true);
  P->ConsumeAnyToken();
  auto Result = P->ParseCXXClassMemberDeclaration(AS_public, nullptr);
  return Result.get().getSingleDecl();
}

ExtendParser::ExtendParser(Preprocessor &PP, Sema &Actions, bool SkipFunctionBodies)
  : Parser(PP, Actions, SkipFunctionBodies) {}

ExtendParser::~ExtendParser() {}

StmtResult
ExtendParser::ParseStatementOrDeclaration(StmtVector &Stmts, AllowedConstructsKind Allowed,
                                           SourceLocation *TrailingElseLoc)
{
  if (Tok.is(tok::cash) && NextToken().is(tok::identifier))
  {
    ConsumeToken();
    const char* name = Tok.getIdentifierInfo()->getNameStart();
    if (std::strcmp(name, "append") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      auto DeclRef = Parser::ParseExpression();
      if (Tok.is(tok::semi))
        ConsumeToken(); // semi
      CachedTokens Toks;
      {
        BalancedDelimiterTracker BDT(*this, tok::l_brace);
        BDT.consumeOpen();
        ConsumeAndStoreUntil(tok::r_brace, Toks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
        BDT.consumeClose();
      }
      BDT.consumeClose();
      std::vector<ASTMetaToken> MetaTokens;
      int state = 0; //0: normal, 1: cashcash, 2: meta-func, 3: l_paren, 4: arg for meta-func, 5: r_paren, 6: eval (experimental)
      CachedTokens EvalTokens;
      std::size_t EvalBraceCnt = 0;
      for (auto&& tok: Toks)
      {
        switch(state)
        {
        case 0:
          if (tok.is(tok::cashcash))
            state = 1;
          MetaTokens.push_back(ASTMetaToken{tok, nullptr});
          break;
        case 1:
          if (tok.is(tok::identifier))
          {
            if (std::strcmp(tok.getIdentifierInfo()->getNameStart(), "eval") == 0)
            {
              MetaTokens.pop_back();
              state = 6;
            }
            else
            {
              MetaTokens.push_back(ASTMetaToken{tok, nullptr});
              state = 2;
            }
          }
          else
            MetaTokens.push_back(ASTMetaToken{tok, nullptr});
          break;
        case 2:
          if (tok.is(tok::l_paren))
            state = 3;
          MetaTokens.push_back(ASTMetaToken{tok, nullptr});
          break;
        case 3:
          if (tok.is(tok::identifier))
          {
            UnqualifiedId Id;
            Id.setIdentifier(tok.getIdentifierInfo(), tok.getLocation());
            CXXScopeSpec ScopeSpec;
            auto IdExpr = Actions.ActOnIdExpression(getCurScope(), ScopeSpec, SourceLocation{}, Id, false, false);
            auto DeclRef = cast<DeclRefExpr>(IdExpr.get());
            MetaTokens.push_back(ASTMetaToken{tok, ImplicitCastExpr::Create(Actions.getASTContext(), DeclRef->getDecl()->getType(), CK_LValueToRValue, DeclRef, nullptr, VK_RValue)});

//            CachedTokens TmpToks;
//            TmpToks.push_back(tok);
//            TmpToks.push_back(GenerateToken(tok::semi));
//            TmpToks.push_back(Tok);
//            PP.EnterTokenStream(TmpToks, true);
//            ConsumeAnyToken();
//            auto TmpDeclRef = ParseExpression();
//            if (Tok.is(tok::semi))
//              ConsumeToken(); // semi
//            auto Type = TmpDeclRef.getAs<DeclRefExpr>()->getDecl()->getType();
//            auto expr = ImplicitCastExpr::Create(Actions.getASTContext(), Type, CK_LValueToRValue, TmpDeclRef.getAs<DeclRefExpr>(), nullptr, VK_RValue);
//            MetaTokens.push_back(ASTMetaToken{tok, expr});
            break;

//            DeclarationName DeclName(tok.getIdentifierInfo());
//            DeclarationNameInfo DeclNameInfo(DeclName, tok.getLocation());
//            LookupResult Result(Actions, DeclNameInfo, LookupOrdinaryName);
//            Actions.LookupName(Result, getCurScope());
          }
          else if (tok.is(tok::r_paren))
          {
            state = 0;
            MetaTokens.push_back(ASTMetaToken{tok, nullptr});
          }
          else
            MetaTokens.push_back(ASTMetaToken{tok, nullptr});
          break;
        case 6: {
          assert(tok.is(tok::l_brace));
          ++EvalBraceCnt;
          EvalTokens.push_back(tok);
          state = 7;
          break;
        }
        case 7:
          if (tok.is(tok::l_brace))
            ++EvalBraceCnt;
          else if (tok.is(tok::r_brace))
            --EvalBraceCnt;
          EvalTokens.push_back(tok);
          if (EvalBraceCnt == 0)
          {
            EvalTokens.push_back(Tok);
            PP.EnterTokenStream(EvalTokens, true);
            ConsumeAnyToken();
            auto EvalAST = ParseCompoundStatement(false);
            EvalTokens.clear();
            state = 0;
          }
          break;
        default:
            MetaTokens.push_back(ASTMetaToken{tok, nullptr});
          break;
        }
      }
      return Actions.ActOnASTMemberAppendExpr(DeclRef, std::move(MetaTokens), this, MetaLateParser, MetaLateTokenizer).get();
    }
    else if (std::strcmp(name, "inject") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      CachedTokens Toks;
      {
        BalancedDelimiterTracker BDT(*this, tok::l_brace);
        BDT.consumeOpen();
        ConsumeAndStoreUntil(tok::r_brace, Toks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
        BDT.consumeClose();
      }
      BDT.consumeClose();
      std::vector<ASTMetaToken> MetaTokens;
      int state = 0; //0: normal, 1: cashcash, 2: meta-func, 3: l_paren, 4: arg for meta-func, 5: r_paren, 6: eval (experimental)
      CachedTokens EvalTokens;
      std::size_t EvalBraceCnt = 0;
      for (auto&& tok: Toks)
      {
        switch(state)
        {
        case 0:
          if (tok.is(tok::cashcash))
            state = 1;
          MetaTokens.push_back(ASTMetaToken{tok, nullptr, nullptr});
          break;
        case 1:
          if (tok.is(tok::identifier))
          {
            if (std::strcmp(tok.getIdentifierInfo()->getNameStart(), "eval") == 0)
            {
              MetaTokens.pop_back();
              state = 6;
            }
            else
            {
              MetaTokens.push_back(ASTMetaToken{tok, nullptr, nullptr});
              state = 2;
            }
          }
          else
            MetaTokens.push_back(ASTMetaToken{tok, nullptr, nullptr});
          break;
        case 2:
          if (tok.is(tok::l_paren))
            state = 3;
          MetaTokens.push_back(ASTMetaToken{tok, nullptr, nullptr});
          break;
        case 3:
          if (tok.is(tok::identifier))
          {
            UnqualifiedId Id;
            Id.setIdentifier(tok.getIdentifierInfo(), tok.getLocation());
            CXXScopeSpec ScopeSpec;
            auto IdExpr = Actions.ActOnIdExpression(getCurScope(), ScopeSpec, SourceLocation{}, Id, false, false);
            auto DeclRef = cast<DeclRefExpr>(IdExpr.get());
            MetaTokens.push_back(ASTMetaToken{tok, ImplicitCastExpr::Create(Actions.getASTContext(), DeclRef->getDecl()->getType(), CK_LValueToRValue, DeclRef, nullptr, VK_RValue), nullptr});

//            CachedTokens TmpToks;
//            TmpToks.push_back(tok);
//            TmpToks.push_back(GenerateToken(tok::semi));
//            TmpToks.push_back(Tok);
//            PP.EnterTokenStream(TmpToks, true);
//            ConsumeAnyToken();
//            auto TmpDeclRef = ParseExpression();
//            if (Tok.is(tok::semi))
//              ConsumeToken(); // semi
//            auto Type = TmpDeclRef.getAs<DeclRefExpr>()->getDecl()->getType();
//            auto expr = ImplicitCastExpr::Create(Actions.getASTContext(), Type, CK_LValueToRValue, TmpDeclRef.getAs<DeclRefExpr>(), nullptr, VK_RValue);
//            MetaTokens.push_back(ASTMetaToken{tok, expr});
            break;

//            DeclarationName DeclName(tok.getIdentifierInfo());
//            DeclarationNameInfo DeclNameInfo(DeclName, tok.getLocation());
//            LookupResult Result(Actions, DeclNameInfo, LookupOrdinaryName);
//            Actions.LookupName(Result, getCurScope());
          }
          else if (tok.is(tok::r_paren))
          {
            state = 0;
            MetaTokens.push_back(ASTMetaToken{tok, nullptr, nullptr});
          }
          else
            MetaTokens.push_back(ASTMetaToken{tok, nullptr, nullptr});
          break;
        case 6: {
          assert(tok.is(tok::l_brace));
          ++EvalBraceCnt;
          EvalTokens.push_back(tok);
          state = 7;
          break;
        }
        case 7:
          if (tok.is(tok::l_brace))
            ++EvalBraceCnt;
          else if (tok.is(tok::r_brace))
            --EvalBraceCnt;
          EvalTokens.push_back(tok);
          if (EvalBraceCnt == 0)
          {
            EvalTokens.push_back(Tok);
            PP.EnterTokenStream(EvalTokens, true);
            ConsumeAnyToken();
            auto EvalAST = ParseCompoundStatement(false);
            MetaTokens.push_back(ASTMetaToken{Token{}, nullptr, EvalAST.get()});
            EvalTokens.clear();
            state = 0;
          }
          break;
        default:
            MetaTokens.push_back(ASTMetaToken{tok, nullptr, nullptr});
          break;
        }
      }
      return Actions.ActOnASTInjectExpr(InjectTokenBuffer, std::move(MetaTokens), this, MetaLateTokenizer).get();
    }
  }
  return Parser::ParseStatementOrDeclaration(Stmts, Allowed, TrailingElseLoc);
}

ExprResult
ExtendParser::ParseExpression(TypeCastState isTypeCast)
{
  return Parser::ParseExpression(isTypeCast);
}

ExprResult 
ExtendParser::ParseAssignmentExpression(TypeCastState isTypeCast)
{
  if (Tok.is(tok::cash) && NextToken().is(tok::identifier))
  {
    ConsumeToken(); //cash
    const char* name = Tok.getIdentifierInfo()->getNameStart();
    if (std::strcmp(name, "var_size") ==0 )
    {
      ConsumeToken(); // var_size
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      auto DeclRef = ParseExpression();
      if (Tok.is(tok::semi))
        ConsumeToken();
      BDT.consumeClose();
      return Actions.ActOnASTMemberVariableSizeExpr(DeclRef);
    }
    else if (std::strcmp(name, "var") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      auto DeclRefs = ParseExpression();
      if (Tok.is(tok::semi))
        ConsumeToken();
      BDT.consumeClose();
      return Actions.ActOnASTMemberVariableExpr(DeclRefs.getAs<BinaryOperator>()->getLHS(), DeclRefs.getAs<BinaryOperator>()->getRHS());
    }
    else if (std::strcmp(name, "var_name") == 0)
    {
      ConsumeToken(); // var_name
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      auto DeclRef = ParseExpression();
      BDT.consumeClose();
      return Actions.ActOnASTMemberVariableNameExpr(DeclRef);
    }
  }
  return Parser::ParseAssignmentExpression(isTypeCast);
}

bool
ExtendParser::ParseTopLevelDecl(DeclGroupPtrTy &Result)
{
  return Parser::ParseTopLevelDecl(Result);
}

Parser::DeclGroupPtrTy ExtendParser::ParseCXXClassMemberDeclaration(
    AccessSpecifier AS, AttributeList *Attr,
    const ParsedTemplateInfo &TemplateInfo,
    ParsingDeclRAIIObject *DiagsFromTParams)
{
  return Parser::ParseCXXClassMemberDeclaration(AS, Attr, TemplateInfo, DiagsFromTParams);
}

void ExtendParser::ParseDeclarationSpecifiers(
      DeclSpec &DS,
      const ParsedTemplateInfo &TemplateInfo,
      AccessSpecifier AS,
      DeclSpecContext DSC,
      LateParsedAttrList *LateAttrs,
      Expr* MetaCall)
{
  if (Tok.isOneOf(tok::kw_class, tok::kw_struct) && NextToken().is(tok::l_paren))
  {
    InjectTokenBuffer.clear();
    auto ClassTok = Tok;
    ConsumeToken(); // consume tok::kw_class
    std::string MetaFunctionCallExpr;
    {
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      CachedTokens Toks;
      BDT.consumeOpen();
      ConsumeAndStoreUntil(tok::r_paren, Toks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
      BDT.consumeClose();
      for (auto& tok: Toks)
      {
        auto begin = PP.getSourceManager().getCharacterData(tok.getLocation());
        auto end = begin + tok.getLength(); 
        for (auto itr = begin; itr != end; ++itr)
          MetaFunctionCallExpr.push_back(*itr);
      }
      MetaFunctionCallExpr += "(0LL);";
    }
    auto ClassNameTok = Tok;
    llvm::SmallVector<char, 0> buf;
    buf.append(MetaFunctionCallExpr.c_str(), MetaFunctionCallExpr.c_str() + MetaFunctionCallExpr.size() + 1);
    buf.set_size(MetaFunctionCallExpr.size());
    auto mem_buf = llvm::make_unique<llvm::SmallVectorMemoryBuffer>(std::move(buf));
    auto fileId = PP.getSourceManager().createFileID(std::move(mem_buf), SrcMgr::C_User, 0, 0, Tok.getLocation());
    PP.EnterSourceFile(fileId, nullptr, Tok.getLocation());
    ConsumeToken(); // consume class name
    ExprResult metaFuncCallExpr = ParseAssignmentExpression();
    if (Tok.is(tok::semi))
      ConsumeToken();
    if (metaFuncCallExpr.isInvalid())
      return;
    CachedTokens Toks;
    Toks.push_back(ClassTok);
    Toks.push_back(ClassNameTok);
    Toks.push_back(Tok);
    PP.EnterTokenStream(Toks, true);
    ConsumeAnyToken();
    return Parser::ParseDeclarationSpecifiers(DS, TemplateInfo, AS, DSC, LateAttrs, metaFuncCallExpr.get());
  }
  return Parser::ParseDeclarationSpecifiers(DS, TemplateInfo, AS, DSC, LateAttrs);
}

