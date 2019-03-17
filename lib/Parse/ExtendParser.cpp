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
      return Actions.ActOnASTInjectExpr(InjectTokenBuffer, std::move(MetaTokens), this, MetaLateTokenizer).get();
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
      return Actions.ActOnASTMemberUpdateAccessSpecExpr(ArgExprs[0], AS_public).get();
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
      return Actions.ActOnASTMemberUpdateAccessSpecExpr(ArgExprs[0], AS_private).get();
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
      return Actions.ActOnASTMemberUpdateAccessSpecExpr(ArgExprs[0], AS_protected).get();
    }
  }
//  else if (Tok.is(tok::kw_for) && NextToken().is(tok::ellipsis))
//  { // expansion statement
//    BalancedDelimiterTracker BDT(*this, tok::l_paren);
//    BDT.consumeOpen();
//    CachedTokens RangeDeclToks;
//    ConsumeAndStoreUntil(tok::colon, RangeDeclToks, /*StopAtSemi=*/true, /*ConsumeFinalToken=*/false);
//    ConsumeToken(); // consume colon
//    assert(Tok.is(tok::cash));
//    ConsumeToken();
//    assert(Tok.is(tok::identifier));
//    const char* name = NextToken().getIdentifierInfo()->getNameStart();
//    ConsumeToken();
//    if (std::strcmp(name, "enum_fields") == 0)
//    {
//      BalancedDelimiterTracker FuncBDT(*this, tok::l_paren);
//      FuncBDT.consumeOpen();
//      ExprVector ArgExprs;
//      CommaLocsTy CommaLocs;
//      ParseExpressionList(ArgExprs, CommaLocs);
//      assert(ArgExprs.size() == 1);
//      FuncBDT.consumeClose();
//      BDT.consumeClose();
//      CachedTokens Toks;
//      if (Tok.is(tok::l_brace))
//      {
//        BalancedDelimiterTracker BodyBDT(*this, tok::l_brace);
//        Toks.push_back(Tok);
//        BodyBDT.consumeOpen();
//        for (tok: RangeDeclToks)
//          Toks.push_back(tok);
//        Toks.push_back(GenerateToken(tok::equal, Toks.back().getLocation()));
//        Toks.push_back(GenerateToken(tok::cash, Toks.back().getLocation()));
//        Toks.push_back(GenerateIdentifierToken(PP, "reflexpr", Toks.back().getLocation()));
//        Toks.push_back(GenerateToken(tok::l_paren, Toks.back().getLocation()));
//        Toks.push_back(GenerateToken(tok::kw_int, Toks.back().getLocation()));
//        Toks.push_back(GenerateToken(tok::r_paren, Toks.back().getLocation()));
//        Toks.push_back(GenerateToken(tok::semi, Toks.back().getLocation()));
//        ConsumeAndStoreUntil(tok::r_brace, Toks, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
//        Toks.push_back(Tok);
//        BodyBDT.consumeClose();
//        auto StmtRes = ParseCompoundStatement(false);
//      }
//      else
//      {
//      }
//    }
//    else if (std::strcmp(name, "member_var_fields") == 0)
//    {
//    }
//    else if (std::strcmp(name, "member_fun_fields") == 0)
//    {
//    }
//  }
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
      if (!isExpandReflection || TInfo->getType().getTypePtr()->isDependentType())
      {
        return Actions.ActOnReflexprExpr(Loc, TInfo, CastRange);
      }
      else
      {
        auto Record = static_cast<Decl*>(TInfo->getType().getTypePtr()->getAsTagDecl());
//      assert(Record);
        llvm::APInt Int(64, reinterpret_cast<uint64_t>(Record));
        return IntegerLiteral::Create(Actions.getASTContext(), Int, Actions.getASTContext().getIntPtrType(), Loc);
      }
    }
    else if (std::strcmp(name, "var_size") == 0)
    {
      SourceLocation Loc = ConsumeToken(); // var_size
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      if (!isExpandReflection || ArgExprs[0]->isValueDependent())
      {
        return Actions.ActOnASTMemberVariableSizeExpr(ArgExprs[0], Loc);
      }
      else
      {
        llvm::APSInt Int(64);
        bool res = ArgExprs[0]->EvaluateAsInt(Int, Actions.getASTContext());
        assert(res);
        auto Ast = reinterpret_cast<Decl*>(Int.getExtValue());
        auto ClassDecl = static_cast<CXXRecordDecl*>(Ast);
        auto MemberRange = ClassDecl->fields();
        uint64_t MemberNum = 0;
        for (auto itr = MemberRange.begin() ; itr != MemberRange.end(); ++itr)
          ++MemberNum;
        llvm::APInt Sz(64, MemberNum);
        return IntegerLiteral::Create(Actions.getASTContext(), Sz, Actions.getASTContext().getSizeType(), Loc);
      }
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
      if (!isExpandReflection || ArgExprs[0]->isValueDependent() || ArgExprs[1]->isValueDependent())
      {
        return Actions.ActOnASTMemberVariableExpr(ArgExprs[0], ArgExprs[1]);
      }
      else
      {
        llvm::APSInt Int1(64);
        llvm::APSInt Int2(64);
        bool res = ArgExprs[0]->EvaluateAsInt(Int1, Actions.getASTContext());
        assert(res);
        res = ArgExprs[1]->EvaluateAsInt(Int2, Actions.getASTContext());
        assert(res);
        auto Ast = reinterpret_cast<Decl*>(Int1.getExtValue());
        auto ClassDecl = static_cast<CXXRecordDecl*>(Ast);
        auto Index = Int2.getExtValue();
        auto MemberVarItr = ClassDecl->field_begin();
        while (Index--) { ++MemberVarItr; }
        llvm::APInt Ptr(64, reinterpret_cast<uint64_t>(*MemberVarItr));
        return IntegerLiteral::Create(Actions.getASTContext(), Ptr, Actions.getASTContext().getIntPtrType(), ArgExprs[0]->getExprLoc());
//        return Actions.ActOnIntegerConstant(ArgExprs[0]->getExprLoc(), reinterpret_cast<uint64_t>(*MemberVarItr));
//        ActOnIntegerConstant will use 32 bit width (strictly speaking, depends on the target int width)
      }
    }
    else if (std::strcmp(name, "var_name") == 0)
    {
      ConsumeToken(); // var_name
      BalancedDelimiterTracker BDT(*this, tok::l_paren);
      BDT.consumeOpen();
      ExprVector ArgExprs;
      CommaLocsTy CommaLocs;
      ParseExpressionList(ArgExprs, CommaLocs);
      assert(ArgExprs.size() == 1);
      BDT.consumeClose();
      if (!isExpandReflection || ArgExprs[0]->isValueDependent())
      {
        return Actions.ActOnASTMemberVariableNameExpr(ArgExprs[0]);
      }
      else
      {
        llvm::APSInt Int(64);
        bool res = ArgExprs[0]->EvaluateAsInt(Int, Actions.getASTContext());
        assert(res);
        auto Ast = reinterpret_cast<Decl*>(Int.getExtValue());
        auto FieldDeclPtr = static_cast<FieldDecl*>(Ast);

        SmallVector<SourceLocation, 4> StringTokLocs;
        StringTokLocs.push_back(ArgExprs[0]->getExprLoc());
        StringRef lit(FieldDeclPtr->getIdentifier()->getNameStart());
        QualType CharTy = Actions.getASTContext().CharTy;
        CharTy.addConst();
        CharTy = Actions.getASTContext().adjustStringLiteralBaseType(CharTy);
        QualType StrTy = Actions.getASTContext().getConstantArrayType(CharTy, llvm::APInt(32, lit.size() + 1), ArrayType::Normal, 0);
        return StringLiteral::Create(
            Actions.getASTContext(),
            lit,
            StringLiteral::Ascii,
            /*Pascal*/false, 
            StrTy,
            &StringTokLocs[0],
            /*NumConcatenated*/1);
      }
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
      return Actions.ActOnASTMemberFunctionSizeExpr(ArgExprs[0]);
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
      return Actions.ActOnASTMemberFunctionExpr(ArgExprs[0], ArgExprs[1]);
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
      return Actions.ActOnASTMemberFunctionNameExpr(ArgExprs[0]);
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
      return Actions.ActOnASTMemberCheckAccessSpecExpr(ArgExprs[0], AS_public);
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
      return Actions.ActOnASTMemberCheckAccessSpecExpr(ArgExprs[0], AS_private);
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
      return Actions.ActOnASTMemberCheckAccessSpecExpr(ArgExprs[0], AS_protected);
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
      if (ArgExprs[0]->isValueDependent() || ArgExprs[1]->isValueDependent())
      {
        return ExprError();
//        return Actions.ActOnASTMemberVariableExpr(ArgExprs[0], ArgExprs[1]);
      }
      else
      {
        llvm::APSInt Int1(64);
        llvm::APSInt Int2(64);
        bool res = ArgExprs[0]->EvaluateAsInt(Int1, Actions.getASTContext());
        assert(res);
        res = ArgExprs[1]->EvaluateAsInt(Int2, Actions.getASTContext());
        assert(res);
        auto Ast = reinterpret_cast<Decl*>(Int1.getExtValue());
        auto EnumDeclPtr = static_cast<EnumDecl*>(Ast);
        auto Index = Int2.getExtValue();
        auto EnumConstItr = EnumDeclPtr->enumerator_begin();
        while (Index--) { ++EnumConstItr; }
        llvm::APInt Ptr(64, reinterpret_cast<uint64_t>(*EnumConstItr));
        return IntegerLiteral::Create(Actions.getASTContext(), Ptr, Actions.getASTContext().getIntPtrType(), ArgExprs[0]->getExprLoc());
      }
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
      if (ArgExprs[0]->isValueDependent())
      {
        return ExprError();
//        return Actions.ActOnASTMemberVariableExpr(ArgExprs[0], ArgExprs[1]);
      }
      else
      {
        llvm::APSInt Int(64);
        bool res = ArgExprs[0]->EvaluateAsInt(Int, Actions.getASTContext());
        assert(res);
        auto Ast = reinterpret_cast<Decl*>(Int.getExtValue());
        auto EnumConstDeclPtr = static_cast<EnumConstantDecl*>(Ast);
        CXXScopeSpec ScopeSpec;
        return Actions.BuildDeclRefExpr(EnumConstDeclPtr, EnumConstDeclPtr->getType(), VK_RValue, Tok.getLocation(), &ScopeSpec);
      }
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

