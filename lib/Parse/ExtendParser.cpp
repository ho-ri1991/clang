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

static void AccessSpecifierToString(AccessSpecifier AS, std::string& Dest)
{
  switch (AS)
  {
  case AS_public:
    Dest += "meta::AccessSpecifier::Public";
    return;
  case AS_protected:
    Dest += "meta::AccessSpecifier::Protected";
    return;
  default:
    Dest += "meta::AccessSpecifier::Private";
    return;
  }
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
//  if (Tok.is(tok::identifier) && std::string(Tok.getIdentifierInfo()->getNameStart()) == "hoge")
//  {
//    std::string* str = new std::string(
////      "func(10, 4.2, -1);\n"
//      R"(generate(
//          meta::class_tokens(
//          boost::hana::make_tuple(
//            meta::member_variable(
//              boost::hana::make_tuple("int"_t),
//              "var1"_t,
//              boost::hana::make_tuple("{"_t, "}"_t, ";"_t)
//            ),
//            meta::member_variable(
//              boost::hana::make_tuple("double"_t),
//              "var2"_t,
//              boost::hana::make_tuple("{"_t, "}"_t, ";"_t)
//            )
//          ),
//          boost::hana::make_tuple(
//            meta::member_function(
//              boost::hana::make_tuple("int"_t),
//              "func"_t,
//              boost::hana::make_tuple(
//                meta::argument(
//                  boost::hana::make_tuple("int"_t),
//                  "x"_t,
//                  boost::hana::make_tuple()
//                ),
//                meta::argument(
//                  boost::hana::make_tuple("bool"_t),
//                  "b"_t,
//                  boost::hana::make_tuple("="_t, "false"_t)
//                )
//              ),
//              boost::hana::make_tuple("{"_t, "}"_t)
//            )
//          )
//        ));)"
//    );
//    llvm::SmallVector<char, 0> buf;
////    buf.append(str->begin(), str->end());
//    buf.append(str->c_str(), str->c_str() + str->size() + 1);
//    buf.set_size(str->size());
//    auto mem_buf = llvm::make_unique<llvm::SmallVectorMemoryBuffer>(std::move(buf));
//    auto mem_buf_ptr = mem_buf.get();
//    auto fileId = PP.getSourceManager().createFileID(std::move(mem_buf), SrcMgr::C_User, 0, 0, Tok.getLocation());
//    PP.EnterSourceFile(fileId, nullptr, Tok.getLocation());
//    auto loc = ConsumeAnyToken();
//    ExprResult Result = ParseAssignmentExpression();
//    assert(Tok.is(tok::semi));
//    ConsumeAnyToken();
//    Expr::EvalResult Eval;
//    Expr::ConstExprUsage Usage = Expr::EvaluateForCodeGen;
//    auto b = Result.get()->EvaluateAsConstantExpr(Eval, Usage, Actions.Context);
//    
//    auto gen_tok_str = [](APValue& token)
//    {
//      auto pstr = new std::string();
//      for (int i = 0; i < token.getStructField(0).getStructField(0).getArraySize() - 1; ++i)
//      {
//        pstr->push_back(static_cast<char>(*token.getStructField(0).getStructField(0).getArrayInitializedElt(i).getInt().getRawData()));
//      }
//      return pstr;
//    };
//    auto tuple_to_elem = [](APValue& t, int i)->decltype(t.getStructField(0).getStructBase(0).getStructBase(i).getStructField(0))
//    {
//      return t.getStructField(0).getStructBase(0).getStructBase(i).getStructField(0);
//    };
//    auto tuple_size = [](APValue& t)
//    {
//      return t.getStructNumFields() == 0 ? 0 : t.getStructField(0).getStructBase(0).getStructNumBases();
//    };
//    auto str_to_clang_token = [](Preprocessor& PP, std::string& str, SourceLocation loc)
//    {
//      llvm::SmallVector<char, 0> buf;
//      buf.append(str.c_str(), str.c_str() + str.size() + 1);
//      buf.set_size(str.size());
//      auto mem_buf = llvm::make_unique<llvm::SmallVectorMemoryBuffer>(std::move(buf));
//      auto mem_buf_ptr = mem_buf.get();
//      auto fileId = PP.getSourceManager().createFileID(std::move(mem_buf), SrcMgr::C_User, 0, 0, loc);
//      Token Result;
//      Lexer TheLexer(fileId, mem_buf_ptr, PP.getSourceManager(), PP.getLangOpts());
//      TheLexer.SetCommentRetentionState(true);
//      TheLexer.LexFromRawLexer(Result);
//      if (Result.is(tok::raw_identifier))
//        return GenerateIdentifierToken(PP, str.c_str(), loc);
//      else
//        return Result;
//      if (str == "::")
//        return GenerateToken(tok::coloncolon, loc);
//      else if (str == ";")
//        return GenerateToken(tok::semi, loc);
//      else if (str == "{")
//        return GenerateToken(tok::l_brace, loc);
//      else if (str == "}")
//        return GenerateToken(tok::r_brace, loc);
//      else if (str == "int")
//        return GenerateToken(tok::kw_int, loc);
//      else if (str == "double")
//        return GenerateToken(tok::kw_double, loc);
//      else if (str == "char")
//        return GenerateToken(tok::kw_char, loc);
//      else if (str == "bool")
//        return GenerateToken(tok::kw_bool, loc);
//      else if (str == "return")
//        return GenerateToken(tok::kw_return, loc);
//      else if (str == ",")
//        return GenerateToken(tok::comma, loc);
//      else if (str == ".")
//        return GenerateToken(tok::period, loc);
//      else if (str == "(")
//        return GenerateToken(tok::l_paren, loc);
//      else if (str == ")")
//        return GenerateToken(tok::r_paren, loc);
//      else if (str == "=")
//        return GenerateToken(tok::equal, loc);
//      else if (str == "==")
//        return GenerateToken(tok::equalequal, loc);
//      else
//        return GenerateIdentifierToken(PP, str.c_str(), loc);
//    };
//
//    auto ptoks = new CachedTokens();
//    auto& toks = *ptoks;
//    toks.push_back(GenerateToken(tok::kw_class, loc));
//    toks.push_back(GenerateIdentifierToken(PP, (new std::string("generated_class"))->c_str(), loc));
//    toks.push_back(GenerateToken(tok::l_brace, loc));
//    auto& member_variables = Eval.Val.getStructField(0).getStructField(0).getStructBase(0);
//    for(int i = 0; i < member_variables.getStructNumBases(); ++i)
//    {
//      auto& mem_var = member_variables.getStructBase(i).getStructField(0);
//      auto& mem_type_toks = mem_var.getStructField(0);
//      auto& mem_name_tok = mem_var.getStructField(1);
//      auto& mem_init_toks = mem_var.getStructField(2);
//      for (int j = 0; j < tuple_size(mem_type_toks); ++j)
//        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_type_toks, j)), loc));
//      toks.push_back(str_to_clang_token(PP, *gen_tok_str(mem_name_tok), loc));
//      for (int j = 0; j < tuple_size(mem_init_toks); ++j)
//        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_init_toks, j)), loc));
//    }
//    auto& member_functions = Eval.Val.getStructField(1).getStructField(0).getStructBase(0);
//    for (int i = 0; i < member_functions.getStructNumBases(); ++i)
//    {
//      auto& mem_func = member_functions.getStructBase(i).getStructField(0);
//      auto& mem_ret_type_toks = mem_func.getStructField(0);
//      auto& mem_name_tok = mem_func.getStructField(1);
//      auto& mem_args = mem_func.getStructField(2);
//      auto& mem_body_toks = mem_func.getStructField(3);
//      for (int j = 0; j < tuple_size(mem_ret_type_toks); ++j)
//        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_ret_type_toks, j)), loc));
//      toks.push_back(str_to_clang_token(PP, *gen_tok_str(mem_name_tok), loc));
//      toks.push_back(GenerateToken(tok::l_paren, loc));
//      for (int j = 0; j < tuple_size(mem_args); ++j)
//      {
//        auto& arg = tuple_to_elem(mem_args, j);
//        auto& type_toks = arg.getStructField(0);
//        auto& name_tok = arg.getStructField(1);
//        auto& default_arg_toks = arg.getStructField(2);
//        for (int k = 0; k < tuple_size(type_toks); ++k)
//          toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(type_toks, k)), loc));
//        toks.push_back(str_to_clang_token(PP, *gen_tok_str(name_tok), loc));
//        for (int k = 0; k < tuple_size(default_arg_toks); ++k)
//          toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(default_arg_toks, k)), loc));
//        if (j < tuple_size(mem_args) - 1)
//          toks.push_back(GenerateToken(tok::comma, loc));
//      }
//      toks.push_back(GenerateToken(tok::r_paren, loc));
//      for (int j = 0; j < tuple_size(mem_body_toks); ++j)
//        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_body_toks, j)), loc));
//    }
//    toks.push_back(GenerateToken(tok::r_brace, loc));
//    toks.push_back(GenerateToken(tok::semi, loc));
//    toks.push_back(Tok);
//    PP.EnterTokenStream(toks, true);
//    ConsumeAnyToken();
//  }
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

void ExtendParser::ParseDeclarationSpecifiers(
      DeclSpec &DS,
      const ParsedTemplateInfo &TemplateInfo,
      AccessSpecifier AS,
      DeclSpecContext DSC,
      LateParsedAttrList *LateAttrs)
{
  if (Tok.isOneOf(tok::kw_class, tok::kw_struct) && NextToken().is(tok::l_paren))
  {
    auto ptoks = new CachedTokens();
    auto& toks = *ptoks;
    AccessSpecifier AS = Tok.is(tok::kw_struct) ? AS_public : AS_private;
    toks.push_back(Tok); // push_back "class"
    auto loc = ConsumeAnyToken();
    CachedTokens qualifiedMetaFunction;
    BalancedDelimiterTracker BDT(*this, tok::l_paren);
    BDT.consumeOpen();
    ConsumeAndStoreUntil(tok::r_paren, qualifiedMetaFunction, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/false);
    BDT.consumeClose();
    ConsumeAndStoreUntil(tok::l_brace, toks, false, false);
    auto metaFuncCallExpr =  ParseClassMemberAndGenerateMetaFunctionCallExpr(qualifiedMetaFunction, AS);
    if (metaFuncCallExpr.isInvalid())
      return;
    Expr::EvalResult Eval;
    Expr::ConstExprUsage Usage = Expr::EvaluateForCodeGen;
    auto b = metaFuncCallExpr.get()->EvaluateAsConstantExpr(Eval, Usage, Actions.Context);
    
    if (!b)
      return;

    auto gen_tok_str = [](APValue& token)
    {
      auto pstr = new std::string();
      for (std::size_t i = 0; i < token.getStructField(0).getStructField(0).getArraySize() - 1; ++i)
      {
        pstr->push_back(static_cast<char>(*token.getStructField(0).getStructField(0).getArrayInitializedElt(i).getInt().getRawData()));
      }
      return pstr;
    };
    auto gen_tok_loc = [](APValue& token, SourceLocation fallback)
    {
      auto n = token.getStructField(1).getInt().getLimitedValue();
      return n == 0 ? fallback : SourceLocation::getFromRawEncoding(n);
    };
    auto tuple_to_elem = [](APValue& t, int i)->decltype(t.getStructField(0).getStructBase(0).getStructBase(i).getStructField(0))
    {
      return t.getStructField(0).getStructBase(0).getStructBase(i).getStructField(0);
    };
    auto tuple_size = [](APValue& t)
    {
      return t.getStructNumFields() == 0 ? 0 : t.getStructField(0).getStructBase(0).getStructNumBases();
    };
    auto str_to_clang_token = [](Preprocessor& PP, std::string& str, SourceLocation loc)
    {
      llvm::SmallVector<char, 0> buf;
      buf.append(str.c_str(), str.c_str() + str.size() + 1);
      buf.set_size(str.size());
      auto mem_buf = llvm::make_unique<llvm::SmallVectorMemoryBuffer>(std::move(buf));
      auto mem_buf_ptr = mem_buf.get();
      auto fileId = PP.getSourceManager().createFileID(std::move(mem_buf), SrcMgr::C_User, 0, 0, loc);
      Token Result;
      Lexer TheLexer(fileId, mem_buf_ptr, PP.getSourceManager(), PP.getLangOpts());
      TheLexer.SetCommentRetentionState(true);
      TheLexer.LexFromRawLexer(Result);
      if (Result.is(tok::raw_identifier))
        return GenerateIdentifierToken(PP, str.c_str(), loc);
      else
        return Result;
    };

    toks.push_back(GenerateToken(tok::l_brace, loc));
    auto& member_variables = Eval.Val.getStructField(0).getStructField(0).getStructBase(0);
    for(std::size_t i = 0; i < member_variables.getStructNumBases(); ++i)
    {
      auto& mem_var = member_variables.getStructBase(i).getStructField(0);
      auto& mem_type_toks = mem_var.getStructField(0);
      auto& mem_name_tok = mem_var.getStructField(1);
      auto& mem_init_toks = mem_var.getStructField(2);
      auto& mem_access_specifier = mem_var.getStructField(3);
      if (AS != mem_access_specifier.getInt().getLimitedValue())
      {
        AS = static_cast<AccessSpecifier>(mem_access_specifier.getInt().getLimitedValue());
        switch (AS)
        {
        case AS_public:
          toks.push_back(GenerateToken(tok::kw_public, loc));
          break;
        case AS_protected:
          toks.push_back(GenerateToken(tok::kw_protected, loc));
          break;
        case AS_private:
          toks.push_back(GenerateToken(tok::kw_private, loc));
          break;
        }
        toks.push_back(GenerateToken(tok::colon, loc));
      }
      for (std::size_t j = 0; j < tuple_size(mem_type_toks); ++j)
        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_type_toks, j)), gen_tok_loc(tuple_to_elem(mem_type_toks, j), loc)));
      toks.push_back(str_to_clang_token(PP, *gen_tok_str(mem_name_tok), gen_tok_loc(mem_name_tok, loc)));
      for (std::size_t j = 0; j < tuple_size(mem_init_toks); ++j)
        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_init_toks, j)), gen_tok_loc(tuple_to_elem(mem_init_toks, j), loc)));
    }
    auto& member_functions = Eval.Val.getStructField(1).getStructField(0).getStructBase(0);
    for (std::size_t i = 0; i < member_functions.getStructNumBases(); ++i)
    {
      auto& mem_func = member_functions.getStructBase(i).getStructField(0);
      auto& mem_ret_type_toks = mem_func.getStructField(0);
      auto& mem_name_tok = mem_func.getStructField(1);
      auto& mem_args = mem_func.getStructField(2);
      auto& mem_qualifier_toks = mem_func.getStructField(3);
      auto& mem_body_toks = mem_func.getStructField(4);
      auto& mem_access_specifier = mem_func.getStructField(5);
      if (AS != mem_access_specifier.getInt().getLimitedValue())
      {
        AS = static_cast<AccessSpecifier>(mem_access_specifier.getInt().getLimitedValue());
        switch (AS)
        {
        case AS_public:
          toks.push_back(GenerateToken(tok::kw_public, loc));
          break;
        case AS_protected:
          toks.push_back(GenerateToken(tok::kw_protected, loc));
          break;
        case AS_private:
          toks.push_back(GenerateToken(tok::kw_private, loc));
          break;
        }
        toks.push_back(GenerateToken(tok::colon, loc));
      }
      for (std::size_t j = 0; j < tuple_size(mem_ret_type_toks); ++j)
        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_ret_type_toks, j)), gen_tok_loc(tuple_to_elem(mem_ret_type_toks, j), loc)));
      toks.push_back(str_to_clang_token(PP, *gen_tok_str(mem_name_tok), gen_tok_loc(mem_name_tok, loc)));
      toks.push_back(GenerateToken(tok::l_paren, loc));
      for (std::size_t j = 0; j < tuple_size(mem_args); ++j)
      {
        auto& arg = tuple_to_elem(mem_args, j);
        auto& type_toks = arg.getStructField(0);
        auto& name_tok = arg.getStructField(1);
        auto& default_arg_toks = arg.getStructField(2);
        for (std::size_t k = 0; k < tuple_size(type_toks); ++k)
          toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(type_toks, k)), gen_tok_loc(tuple_to_elem(type_toks, k), loc)));
        toks.push_back(str_to_clang_token(PP, *gen_tok_str(name_tok), gen_tok_loc(name_tok, loc)));
        for (std::size_t k = 0; k < tuple_size(default_arg_toks); ++k)
          toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(default_arg_toks, k)), gen_tok_loc(tuple_to_elem(default_arg_toks, k), loc)));
        if (j < tuple_size(mem_args) - 1)
          toks.push_back(GenerateToken(tok::comma, loc));
      }
      toks.push_back(GenerateToken(tok::r_paren, loc));
      for (std::size_t j = 0; j < tuple_size(mem_qualifier_toks); ++j)
        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_qualifier_toks, j)), gen_tok_loc(tuple_to_elem(mem_qualifier_toks, j), loc)));
      for (std::size_t j = 0; j < tuple_size(mem_body_toks); ++j)
        toks.push_back(str_to_clang_token(PP, *gen_tok_str(tuple_to_elem(mem_body_toks, j)), gen_tok_loc(tuple_to_elem(mem_body_toks, j), loc)));
    }
    toks.push_back(GenerateToken(tok::r_brace, loc));
    toks.push_back(GenerateToken(tok::semi, loc));
    toks.push_back(Tok);
    PP.EnterTokenStream(toks, true);
    ConsumeAnyToken();

  }

  return Parser::ParseDeclarationSpecifiers(DS, TemplateInfo, AS, DSC, LateAttrs);
}

ExprResult ExtendParser::ParseClassMemberAndGenerateMetaFunctionCallExpr(const CachedTokens& qualifiedMetaFunction, AccessSpecifier AS)
{
  assert(Tok.is(tok::l_brace));
  BalancedDelimiterTracker BDT(*this, tok::l_brace);
  BDT.consumeOpen();
  std::vector<MemberVariableToken> MemberVariables;
  std::vector<MemberFunctionToken> MemberFunctions;
  while (Tok.isNot(tok::r_brace))
  {
    switch (Tok.getKind())
    {
    case tok::kw_public:
      AS = AS_public;
      ConsumeToken(); // consume access specifier
      ConsumeToken(); // consume colon
      break;
    case tok::kw_private:
      AS = AS_private;
      ConsumeToken(); // consume access specifier
      ConsumeToken(); // consume colon
      break;
    case tok::kw_protected:
      AS = AS_protected;
      ConsumeToken(); // consume access specifier
      ConsumeToken(); // consume colon
      break;
    case tok::kw_using:
    case tok::kw_typedef: //TODO support typedef/using
      SkipUntil(tok::semi);
      break;
    default: // member variable / function
      { // parse typename
        CachedTokens TypenameTokens;
        if (ConsumeAndStoreTypename(TypenameTokens))
        {
          SkipUntil(tok::r_brace, StopBeforeMatch);
          return true;
        }

        // consume variable/function name
        Token NameToken = Tok;
        ConsumeToken();

        if (Tok.is(tok::l_paren))
        { // the member is member function
          MemberFunctionToken MemberFunc{std::move(TypenameTokens), std::move(NameToken), {}, CachedTokens{}, CachedTokens{}, AS};
          // parse function paramter
          BalancedDelimiterTracker BDT(*this, tok::l_paren);
          BDT.consumeOpen();
          while(Tok.isNot(tok::r_paren))
          {
            FunctionParamToken Param{};
            if (ConsumeAndStoreTypename(Param.TypenameTokens))
            {
              SkipUntil(tok::r_paren, StopBeforeMatch);
              BDT.consumeClose();
              return true;
            }
            Param.NameToken = Tok;
            ConsumeToken();
            if (Tok.isOneOf(tok::identifier, tok::coloncolon))
            {
              if (TryAnnotateCXXScopeToken())
                return true;
            }
            if (Tok.is(tok::comma))
              ConsumeToken();
            MemberFunc.Params.push_back(std::move(Param));
          }
          BDT.consumeClose();
          if (Tok.is(tok::l_brace))
          {
            MemberFunc.BodyTokens.push_back(Tok);
            BalancedDelimiterTracker BDT(*this, tok::l_brace);
            BDT.consumeOpen();
            ConsumeAndStoreUntil(tok::r_brace, MemberFunc.BodyTokens, false, false);
            MemberFunc.BodyTokens.push_back(Tok);
            BDT.consumeClose();
          }
          else if (Tok.is(tok::semi))
            ConsumeToken();
          MemberFunctions.push_back(std::move(MemberFunc));
        }
        else
        { // the memebr is member variable
          // TODO support decl like int x = 1, y = 2, z = 3;
          MemberVariableToken MemberVar{std::move(TypenameTokens), std::move(NameToken), CachedTokens{}, AS};
          ConsumeAndStoreUntil(tok::semi, MemberVar.InitializerTokens, /*StopAtSemi=*/false, /*ConsumeFinalToken=*/true);
          MemberVariables.push_back(std::move(MemberVar));
        }
      }
    }
  }
  BDT.consumeClose();
  auto str = GenerateMetaFunctionCallExpr(MemberVariables, MemberFunctions, qualifiedMetaFunction);
	llvm::SmallVector<char, 0> buf;
	buf.append(str.c_str(), str.c_str() + str.size() + 1);
	buf.set_size(str.size());
	auto mem_buf = llvm::make_unique<llvm::SmallVectorMemoryBuffer>(std::move(buf));
	auto fileId = PP.getSourceManager().createFileID(std::move(mem_buf), SrcMgr::C_User, 0, 0, Tok.getLocation());
	PP.EnterSourceFile(fileId, nullptr, Tok.getLocation());
	auto loc = ConsumeAnyToken();
	ExprResult Result = ParseAssignmentExpression();
	assert(Tok.is(tok::semi));
	ConsumeAnyToken();
  return Result;
}

bool
ExtendParser::ConsumeAndStoreTypename(CachedTokens& Result)
{
  while (Tok.is(tok::kw_const) || Tok.is(tok::kw_volatile))
  { // consume cv-qualifier
    Result.push_back(Tok);
    ConsumeToken();
  }

  if (Tok.isOneOf(tok::identifier, tok::coloncolon))
  {
    if (TryAnnotateCXXScopeToken())
      return true;
  }

  if (Tok.is(tok::annot_cxxscope))
  { // in case of X::Y::Z
    Result.push_back(Tok);
    ConsumeAnyToken(); // X::Y::
    Result.push_back(Tok);
    ConsumeAnyToken(); // Z
  }
  else
  { // identifier only or primitive type
    Result.push_back(Tok);
    ConsumeToken();
  }

  while (Tok.is(tok::kw_const) || Tok.is(tok::kw_volatile) || Tok.is(tok::star) || Tok.is(tok::amp) || Tok.is(tok::ampamp))
  { // consume cv-qualifier and * and & and &&
    Result.push_back(Tok);
    ConsumeToken();
  }

  return false;
}

void
ExtendParser::AppendToken(const Token& Source, std::string& Dest)
{
  Dest += "meta::token(\"";
  AppendTokenStr(Source, Dest);
  Dest += "\"_str,";
  Dest += std::to_string(Source.getLocation().getRawEncoding());
  Dest += ")";
}

void 
ExtendParser::AppendTokenStr(const Token& Source, std::string& Dest)
{
  switch (Source.getKind())
  {
  case tok::annot_cxxscope:
    {
      auto begin = PP.getSourceManager().getCharacterData(Source.getLocation());
      // `X::Y::Z`
      // `X::Y::` is annot_cxxscope but EndLoc point to begining of the last `::`
      auto end = PP.getSourceManager().getCharacterData(Source.getEndLoc()) + 2;
      for (auto itr = begin; itr != end; ++itr)
        Dest.push_back(*itr);
      break;
    }
  case tok::annot_template_id:
  case tok::annot_typename:
    {
      auto begin = PP.getSourceManager().getCharacterData(Source.getLocation());
      // `X<Y>` is annot_template_id but EndLoc point to the last `>`
      auto end = PP.getSourceManager().getCharacterData(Source.getEndLoc()) + 1;
      for (auto itr = begin; itr != end; ++itr)
        Dest.push_back(*itr);
      break;
    }
  default:
    {
      auto begin = PP.getSourceManager().getCharacterData(Source.getLocation());
      auto end = begin + Source.getLength(); 
      for (auto itr = begin; itr != end; ++itr)
        Dest.push_back(*itr);
      break;
    }
  }
}

std::string
ExtendParser::GenerateMetaFunctionCallExpr(
    const std::vector<MemberVariableToken> MemberVariables,
    const std::vector<MemberFunctionToken> MemberFunctions,
    const CachedTokens& QualifiedMetaFunction)
{
  std::string Result;
	for (auto& tok: QualifiedMetaFunction)
    AppendTokenStr(tok, Result);
  Result += "(";
  {
    Result += "meta::class_tokens(";
    Result += "boost::hana::make_tuple(";
    for (auto& var: MemberVariables)
    {
      Result += "meta::member_variable(";
      {
        Result += "boost::hana::make_tuple(";
        for (auto& tok: var.TypenameTokens)
        {
          AppendToken(tok, Result);
          Result += ",";
        }
				if (Result.back() == ',')
					Result.pop_back();
        Result += "),";
        AppendToken(var.NameToken, Result);
        Result += ",";
        Result += "boost::hana::make_tuple(";
        for (auto& tok: var.InitializerTokens)
        {
          AppendToken(tok, Result);
          Result += ",";
        }
				if (Result.back() == ',')
					Result.pop_back();
        Result += "),";
        AccessSpecifierToString(var.AS, Result);
      }
      Result += "),";
    }
    if (Result.back() == ',')
      Result.pop_back();
    Result += "),";
    Result += "boost::hana::make_tuple(";
    for (auto& func: MemberFunctions)
    {
      Result += "meta::member_function(";
      {
        Result += "boost::hana::make_tuple(";
        for (auto& tok: func.TypenameTokens)
        {
          AppendToken(tok, Result);
          Result += ",";
        }
				if (Result.back() == ',')
					Result.pop_back();
        Result += "),";
        AppendToken(func.NameToken, Result);
        Result += ",";
        Result += "boost::hana::make_tuple(";
        for (auto& param: func.Params)
        {
          Result += "meta::argument(";
          Result += "boost::hana::make_tuple(";
          for (auto& tok: param.TypenameTokens)
          {
            AppendToken(tok, Result);
            Result += ",";
          }
          if (Result.back() == ',')
            Result.pop_back();
          Result += "),";
          AppendToken(param.NameToken, Result);
          Result += ",";
          Result += "boost::hana::make_tuple(";
          for (auto& tok: param.DefaultTokens)
          {
            AppendToken(tok, Result);
            Result += ",";
          }
          if (Result.back() == ',')
            Result.pop_back();
          Result += ")";
					Result += "),";
        }
				if (Result.back() == ',')
					Result.pop_back();
        Result += "),";
        Result += "boost::hana::make_tuple(";
        for (auto& tok: func.QualifierTokens)
        {
          AppendToken(tok, Result);
          Result += ",";
        }
				if (Result.back() == ',')
					Result.pop_back();
        Result += "),";
        Result += "boost::hana::make_tuple(";
        for (auto& tok: func.BodyTokens)
        {
          AppendToken(tok, Result);
          Result += ",";
        }
				if (Result.back() == ',')
					Result.pop_back();
        Result += "),";
        AccessSpecifierToString(func.AS, Result);
      }
      Result += "),";
    }
		if (Result.back() == ',')
			Result.pop_back();
    Result += ")";
    Result += ")";
  }
  Result += ");";
  return Result;
}

