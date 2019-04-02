#include "clang/Parse/ExtendParser.h"
#include "clang/Lex/Preprocessor.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/SemaDiagnostic.h"
#include <iostream>
#include <cassert>

using namespace clang;

static Token GenerateToken(tok::TokenKind kind, SourceLocation Loc = SourceLocation{})
{
  Token Tok;
  Tok.startToken();
  Tok.setKind(kind);
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

static Token GenerateLiteralToken(tok::TokenKind kind, const char* literalData, unsigned length, SourceLocation Loc = SourceLocation{})
{
  Token Tok;
  Tok.startToken();
  Tok.setKind(kind);
  Tok.setLocation(std::move(Loc));
  Tok.setLiteralData(literalData);
  Tok.setLength(length);
  return Tok;
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

ExtendParser::ExtendParser(Preprocessor &PP, Sema &Actions, bool SkipFunctionBodies)
  : Parser(PP, Actions, SkipFunctionBodies), isExpandReflection(true) {}

ExtendParser::~ExtendParser() {}

StmtResult
ExtendParser::ParseStatementOrDeclaration(StmtVector &Stmts, AllowedConstructsKind Allowed,
                                           SourceLocation *TrailingElseLoc)
{
  if (Tok.is(tok::cash) && NextToken().is(tok::identifier))
  {
    ConsumeToken();
    const char* name = Tok.getIdentifierInfo()->getNameStart();
    if (std::strcmp(name, "inject") == 0)
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
      return Actions.ActOnASTInjectExpr(InjectTokenBuffer, std::move(MetaTokens), this, MetaLateTokenizer, isExpandReflection).get();
    }
    else if (std::strcmp(name, "__constexpr") == 0)
    {
      ConsumeToken(); 
      CachedTokens Toks;
      Toks.push_back(GenerateToken(tok::l_square, Tok.getLocation()));
      Toks.push_back(GenerateToken(tok::r_square, Tok.getLocation()));
      Toks.push_back(GenerateToken(tok::l_paren, Tok.getLocation()));
      Toks.push_back(GenerateToken(tok::r_paren, Tok.getLocation()));
      Toks.push_back(Tok);
      BalancedDelimiterTracker BDT(*this, tok::l_brace);
      BDT.consumeOpen();
      ConsumeAndStoreUntil(tok::r_brace, Toks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
      Toks.push_back(GenerateToken(tok::kw_return, Tok.getLocation()));
      Toks.push_back(GenerateToken(tok::kw_nullptr, Tok.getLocation()));
      Toks.push_back(GenerateToken(tok::semi, Tok.getLocation()));
      Toks.push_back(Tok);
      BDT.consumeClose();
      Toks.push_back(GenerateToken(tok::l_paren, Tok.getLocation()));
      Toks.push_back(GenerateToken(tok::r_paren, Tok.getLocation()));
      Toks.push_back(GenerateToken(tok::semi, Tok.getLocation()));
      Toks.push_back(Tok);
      PP.EnterTokenStream(Toks, true);
      ConsumeAnyToken();
      auto LambdaCall = Parser::ParseExpression().get();
      Expr::EvalResult Eval;
      llvm::SmallVector<PartialDiagnosticAt, 0> Diags;
      Eval.Diag = &Diags;
      Expr::ConstExprUsage Usage = Expr::EvaluateForCodeGen;

      InjectTokenBuffer.clear();
      auto b = LambdaCall->EvaluateAsConstantExpr(Eval, Usage, Actions.Context);
      if (!Diags.empty())
      {
        if (Diags.size() == 1 &&
            Diags[0].second.getDiagID() == diag::note_invalid_subexpr_in_const_expr)
        {
          Actions.Diag(Diags[0].first, diag::err_expr_not_cce) << Expr::EvaluateForCodeGen;
        }
        else
        {
          Actions.Diag(LambdaCall->getLocStart(), diag::err_expr_not_cce)
            << Expr::EvaluateForCodeGen << LambdaCall->getSourceRange();
          for (unsigned I = 0; I < Diags.size(); ++I)
            Actions.Diag(Diags[I].first, Diags[I].second);
        }
        return StmtResult{};
      }
      InjectTokenBuffer.push_back(Tok);
      PP.EnterTokenStream(InjectTokenBuffer, true);
      return Parser::ParseStatementOrDeclaration(Stmts, Allowed, TrailingElseLoc);
    }
    else if (std::strcmp(name, "make_public") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberUpdateAccessSpecExpr(ArgExprs[0], AS_public, isExpandReflection).get();
    }
    else if (std::strcmp(name, "make_private") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberUpdateAccessSpecExpr(ArgExprs[0], AS_private, isExpandReflection).get();
    }
    else if (std::strcmp(name, "make_protected") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberUpdateAccessSpecExpr(ArgExprs[0], AS_protected, isExpandReflection).get();
    }
  }
  else if (Tok.is(tok::kw_for) && NextToken().is(tok::ellipsis))
  { // expansion statement
    auto ForLoc = ConsumeToken(); // consume kw_for
    ConsumeToken(); // consume ellipsis
    BalancedDelimiterTracker BDT(*this, tok::l_paren);
    BDT.consumeOpen();
    CachedTokens RangeDeclToks;
    ConsumeAndStoreUntil(tok::colon, RangeDeclToks, /*StopAtSemi=*/true, /*ConsumeFinalToken=*/false);
    ConsumeToken(); // consume colon
    ExprVector ArgExprs;
    CommaLocsTy CommaLocs;
    this->isExpandReflection = false;
    ParseExpressionList(ArgExprs, CommaLocs); // parse $enum_fields(E)/member_var_fields(X)/..
    assert(ArgExprs.size() == 1);
    BDT.consumeClose();

    StmtVector Stmts;
    {
      ParseScope CompoundScope(this, Scope::DeclScope | Scope::CompoundStmtScope);
      RangeDeclToks.push_back(GenerateToken(tok::equal, BDT.getCloseLocation()));
      RangeDeclToks.push_back(GenerateToken(tok::cash, RangeDeclToks.back().getLocation()));
      RangeDeclToks.push_back(GenerateIdentifierToken(PP, "reflexpr", RangeDeclToks.back().getLocation()));
      RangeDeclToks.push_back(GenerateToken(tok::l_paren, RangeDeclToks.back().getLocation()));
      RangeDeclToks.push_back(GenerateToken(tok::kw_int, RangeDeclToks.back().getLocation()));
      RangeDeclToks.push_back(GenerateToken(tok::r_paren, RangeDeclToks.back().getLocation()));
      RangeDeclToks.push_back(GenerateToken(tok::semi, RangeDeclToks.back().getLocation()));
      RangeDeclToks.push_back(Tok);
      PP.EnterTokenStream(RangeDeclToks, true);
      ConsumeAnyToken();
      auto R1 = ParseStatementOrDeclaration(Stmts, ACK_Any);
      Stmts.push_back(R1.get());
      auto R2 = ParseStatementOrDeclaration(Stmts, ACK_Any);
      Stmts.push_back(R2.get());
    }

    this->isExpandReflection = true;

    auto CompoundStmtResult =  Actions.ActOnCompoundStmt(BDT.getOpenLocation(), BDT.getCloseLocation(),
                                                         Stmts, /*isStmtExpr=*/false);
    return Actions.ActOnExpansionForStmt(ForLoc, BDT.getOpenLocation(), Stmts[0], ArgExprs[0], BDT.getCloseLocation(), CompoundStmtResult.get());

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
    if (std::strcmp(name, "reflexpr") == 0)
    {
      SourceLocation Loc = ConsumeToken(); // reflexpr
      bool isCastExpr = false;
      ParsedType CastTy;
      SourceRange CastRange;
      auto Operand = ParseExprAfterUnaryExprOrTypeTrait(GenerateToken(tok::kw_sizeof, Loc), isCastExpr, CastTy, CastRange);
      TypeSourceInfo *TInfo;
      (void) Sema::GetTypeFromParser(ParsedType::getFromOpaquePtr(CastTy.getAsOpaquePtr()), &TInfo);
      return Actions.ActOnReflexprExpr(Loc, TInfo, CastRange, isExpandReflection);
    }
    else if (std::strcmp(name, "var_size") == 0)
    {
      SourceLocation Loc = ConsumeToken(); // var_size
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      BDT.consumeClose();
      if (ArgExprs.size() != 1)
      {
        return ExprError();
      }
      return Actions.ActOnReflectionMemberVariableSizeExpr(ArgExprs[0], Loc, isExpandReflection);
    }
    else if (std::strcmp(name, "var") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 2);
      BDT.consumeClose();
      if (ArgExprs.size() != 2)
      {
        return ExprError();
      }
      return Actions.ActOnReflectionMemberVariableExpr(ArgExprs[0], ArgExprs[1], isExpandReflection);
    }
    else if (std::strcmp(name, "var_name") == 0)
    {
      ConsumeToken(); // var_name
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      BDT.consumeClose();
      if (ArgExprs.size() != 1)
      {
        return ExprError();
      }
      return Actions.ActOnReflectionMemberVariableNameExpr(ArgExprs[0], isExpandReflection);
    }
    else if (std::strcmp(name, "func_size") == 0)
    {
      ConsumeToken(); // func_size
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberFunctionSizeExpr(ArgExprs[0], isExpandReflection);
    }
    else if (std::strcmp(name, "func") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 2);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberFunctionExpr(ArgExprs[0], ArgExprs[1], isExpandReflection);
    }
    else if (std::strcmp(name, "func_name") == 0)
    {
      ConsumeToken(); // func_name
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberFunctionNameExpr(ArgExprs[0], isExpandReflection);
    }
    else if (std::strcmp(name, "is_public") == 0)
    {
      ConsumeToken(); 
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberCheckAccessSpecExpr(ArgExprs[0], AS_public, isExpandReflection);
    }
    else if (std::strcmp(name, "is_private") == 0)
    {
      ConsumeToken(); 
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberCheckAccessSpecExpr(ArgExprs[0], AS_private, isExpandReflection);
    }
    else if (std::strcmp(name, "is_protected") == 0)
    {
      ConsumeToken(); 
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionMemberCheckAccessSpecExpr(ArgExprs[0], AS_protected, isExpandReflection);
    }
    else if (std::strcmp(name, "enum_field") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 2);
      BDT.consumeClose();
      if (ArgExprs.size() != 2)
      {
        return ExprError();
      }
      SourceRange Range(BDT.getOpenLocation(), BDT.getCloseLocation());
      return Actions.ActOnReflectionEnumFieldExpr(ArgExprs[0], ArgExprs[1], Range, isExpandReflection);
    }
    else if (std::strcmp(name, "enum_value") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      llvm::APSInt Int(64);
      return Actions.ActOnReflectionEnumFieldValueExpr(ArgExprs[0], SourceRange(BDT.getOpenLocation(), BDT.getCloseLocation()), isExpandReflection);
    }
    else if (std::strcmp(name, "enum_name") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      BDT.consumeClose();
      if (ArgExprs.size() != 1)
      {
        return ExprError();
      }
      return Actions.ActOnReflectionEnumFieldNameExpr(ArgExprs[0], SourceRange(BDT.getOpenLocation(), BDT.getCloseLocation()), isExpandReflection);
    }
    else if (std::strcmp(name, "enum_fields") == 0)
    {
      ConsumeToken();
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      return Actions.ActOnReflectionEnumFieldsExpr(ArgExprs[0], SourceRange(BDT.getOpenLocation(), BDT.getCloseLocation()), isExpandReflection);
    }
  }
  return Parser::ParseAssignmentExpression(isTypeCast);
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

    BalancedDelimiterTracker BDT(*this, tok::l_paren);
    BDT.consumeOpen();
    // TODO: nested namespace
    UnqualifiedId Id;
    Id.setIdentifier(Tok.getIdentifierInfo(), Tok.getLocation());
    CXXScopeSpec ScopeSpec;
    auto IdExpr = Actions.ActOnIdExpression(getCurScope(), ScopeSpec, SourceLocation{}, Id, false, false);
    ConsumeToken(); // consume identifier
    BDT.consumeClose();

    llvm::APInt Int(64, 0);
    Expr* IntNode = IntegerLiteral::Create(Actions.getASTContext(), Int, Actions.getASTContext().getIntPtrType(), Tok.getLocation());
    MultiExprArg Args(IntNode);
    auto MetaFuncCallExpr = Actions.ActOnCallExpr(getCurScope(), IdExpr.get(), BDT.getOpenLocation(), Args, BDT.getCloseLocation());

    CachedTokens Toks;
    Toks.push_back(ClassTok);
    Toks.push_back(Tok);
    PP.EnterTokenStream(Toks, true);
    ConsumeAnyToken();
    return Parser::ParseDeclarationSpecifiers(DS, TemplateInfo, AS, DSC, LateAttrs, MetaFuncCallExpr.get());
  }
  return Parser::ParseDeclarationSpecifiers(DS, TemplateInfo, AS, DSC, LateAttrs);
}

