#include "SemaTreeCopy.h"
#include "clang/Sema/Template.h"

namespace clang {
using namespace sema;

namespace {
VarDecl* CopyVarDecl(VarDecl* OldDecl, Expr* NewInit, Sema& SemaRef)
{
  auto NewDecl = VarDecl::Create(SemaRef.getASTContext(),
                                 OldDecl->getDeclContext(),
                                 OldDecl->getBeginLoc(),
                                 OldDecl->getLocation(),
                                 OldDecl->getIdentifier(),
                                 NewInit ? NewInit->getType() : OldDecl->getType(),
//                                 OldDecl->getType(),
                                 OldDecl->getTypeSourceInfo(),
                                 OldDecl->getStorageClass()); 
  if (NewInit)
    NewDecl->setInit(NewInit);
  NewDecl->setConstexpr(OldDecl->isConstexpr());
  if (OldDecl->isInlineSpecified())
    NewDecl->setInlineSpecified();
  NewDecl->setInitStyle(OldDecl->getInitStyle());
  NewDecl->setExceptionVariable(OldDecl->isExceptionVariable());
  NewDecl->setNRVOVariable(OldDecl->isNRVOVariable());
  NewDecl->setCXXForRangeDecl(OldDecl->isCXXForRangeDecl());
  NewDecl->setAccess(OldDecl->getAccess());
  if (OldDecl->hasAttrs())
    NewDecl->setAttrs(OldDecl->getAttrs());
  NewDecl->setInvalidDecl(OldDecl->isInvalidDecl());
  NewDecl->setImplicit(OldDecl->isImplicit());
  if (OldDecl->isUsed())
    NewDecl->setIsUsed();
  NewDecl->setReferenced(OldDecl->isReferenced() || OldDecl->isThisDeclarationReferenced());
  if (OldDecl->isLocalExternDecl())
    NewDecl->setLocalExternDecl();
  return NewDecl;
}
}

StmtResult ExpandForStmt(ExpansionForStmt* S, Sema& SemaRef)
{
  if (S->getInit()->isValueDependent() || S->getInit()->isTypeDependent())
    return S;
  return ExpandForStmt(S->getInit(), S->getBody(), SemaRef);
}

StmtResult ExpandForStmt(Expr* Init, Stmt* Body, Sema& SemaRef)
{
  auto TypedBody = cast_or_null<CompoundStmt>(Body);
  if (!TypedBody || TypedBody->body_empty())
    return StmtError();

  if (Init->isValueDependent() || Init->isTypeDependent())
  {
    return StmtError();
  }
  else
  {
    switch(Init->getStmtClass())
    {
      case Stmt::ReflectionEnumFieldsExprClass: {
        llvm::APSInt Int(64);
        auto EnumFieldsExpr = cast<ReflectionEnumFieldsExpr>(Init);
        if (!EnumFieldsExpr->getSubExpr()->EvaluateAsInt(Int, SemaRef.getASTContext()))
          return StmtError();
        Decl* DeclPtr = nullptr;
        auto T = EnumFieldsExpr->getSubExpr()->getType().getCanonicalType();
        T.removeLocalConst();
        if (T == SemaRef.getASTContext().getUIntPtrType())
        {
          auto TypePtr = reinterpret_cast<Type*>(Int.getExtValue());
          DeclPtr = TypePtr->getAsTagDecl();
        }
        else if (T == SemaRef.getASTContext().getIntPtrType())
        {
          DeclPtr = reinterpret_cast<Decl*>(Int.getExtValue());
        }
        else
        {
          return StmtError();
        }
        auto EnumDeclPtr = cast_or_null<EnumDecl>(DeclPtr);
        if (!EnumDeclPtr)
          return StmtError();
        SmallVector<Stmt*, 8> ExpandedBodies;
        for (auto Itr = EnumDeclPtr->enumerator_begin();
             Itr != EnumDeclPtr->enumerator_end(); 
             ++Itr)
        {
          auto DeclStmtPtr = cast_or_null<DeclStmt>(*TypedBody->body_begin());
          if (!DeclStmtPtr || !DeclStmtPtr->isSingleDecl())
            return StmtError();

          auto OldDecl = cast_or_null<VarDecl>(DeclStmtPtr->getSingleDecl());
          if (!OldDecl)
            return StmtError();

          auto NewDecl = CopyVarDecl(OldDecl, nullptr, SemaRef);
          auto OldInitExpr = OldDecl->getInit();
          llvm::APInt Int(64, reinterpret_cast<uint64_t>(*Itr));
          NewDecl->setInit(IntegerLiteral::Create(SemaRef.getASTContext(),
                                                  Int,
                                                  OldInitExpr->getType(),
                                                  OldInitExpr->getLocStart()));

//          if (SemaRef.CurrentInstantiationScope)
//            SemaRef.CurrentInstantiationScope->InstantiatedLocal(OldDecl, NewDecl);

          TreeCopy TreeCopyer(SemaRef, TypedBody->getLocStart());
          TreeCopyer.transformedLocalDecl(OldDecl, NewDecl);
          auto CopiedBody = TreeCopyer.TransformStmt(TypedBody);
          if (CopiedBody.isInvalid())
            return StmtError();
          ExpandedBodies.push_back(CopiedBody.get());
        }
        return CompoundStmt::Create(SemaRef.getASTContext(), ExpandedBodies, TypedBody->getLBracLoc(), TypedBody->getRBracLoc());
      }
      case Stmt::ReflectionDataMembersExprClass: {
        llvm::APSInt Int(64);
        auto DataMembersExpr = cast<ReflectionDataMembersExpr>(Init);
        if (!DataMembersExpr->getSubExpr()->EvaluateAsInt(Int, SemaRef.getASTContext()))
          return StmtError();
        Decl* DeclPtr = nullptr;
        auto T = DataMembersExpr->getSubExpr()->getType().getCanonicalType();
        T.removeLocalConst();
        if (T == SemaRef.getASTContext().getUIntPtrType())
        {
          auto TypePtr = reinterpret_cast<Type*>(Int.getExtValue());
          DeclPtr = TypePtr->getAsTagDecl();
        }
        else if (T == SemaRef.getASTContext().getIntPtrType())
        {
          DeclPtr = reinterpret_cast<Decl*>(Int.getExtValue());
        }
        else
        {
          return StmtError();
        }
        auto RecordDeclPtr = cast_or_null<RecordDecl>(DeclPtr);
        if (!RecordDeclPtr)
          return StmtError();
        SmallVector<Stmt*, 8> ExpandedBodies;
        for (auto Itr = RecordDeclPtr->field_begin();
             Itr != RecordDeclPtr->field_end(); 
             ++Itr)
        {
          auto DeclStmtPtr = cast_or_null<DeclStmt>(*TypedBody->body_begin());
          if (!DeclStmtPtr || !DeclStmtPtr->isSingleDecl())
            return StmtError();

          auto OldDecl = cast_or_null<VarDecl>(DeclStmtPtr->getSingleDecl());
          if (!OldDecl)
            return StmtError();

          auto NewDecl = CopyVarDecl(OldDecl, nullptr, SemaRef);
          auto OldInitExpr = OldDecl->getInit();
          llvm::APInt Int(64, reinterpret_cast<uint64_t>(*Itr));
          NewDecl->setInit(IntegerLiteral::Create(SemaRef.getASTContext(),
                                                  Int,
                                                  OldInitExpr->getType(),
                                                  OldInitExpr->getLocStart()));

          if (SemaRef.CurrentInstantiationScope)
            SemaRef.CurrentInstantiationScope->InstantiatedLocal(OldDecl, NewDecl);

          TreeCopy TreeCopyer(SemaRef, TypedBody->getLocStart());
          TreeCopyer.transformedLocalDecl(OldDecl, NewDecl);
          auto CopiedBody = TreeCopyer.TransformStmt(TypedBody);
          if (CopiedBody.isInvalid())
            return StmtError();
          ExpandedBodies.push_back(CopiedBody.get());
        }
        return CompoundStmt::Create(SemaRef.getASTContext(), ExpandedBodies, TypedBody->getLBracLoc(), TypedBody->getRBracLoc());
      }
      defalt:
        return StmtError();
    }
  }
  return StmtError();
}

TreeCopy::TreeCopy(Sema &SemaRef, SourceLocation Loc)
  : inherited(SemaRef), Loc(Loc) {}

ExprResult TreeCopy::TransformReflectionEnumFieldsExpr(ReflectionEnumFieldsExpr* E)
{
  ExprResult SubExpr = TransformExpr(E->getSubExpr());
  if (SubExpr.isInvalid())
    return ExprError();
  return new(getSema().getASTContext()) ReflectionEnumFieldsExpr(getSema().getASTContext(), SubExpr.get(), SourceRange(E->getLocStart(), E->getLocEnd()));
}

ExprResult TreeCopy::TransformReflectionDataMembersExpr(ReflectionDataMembersExpr* E)
{
  ExprResult SubExpr = TransformExpr(E->getSubExpr());
  if (SubExpr.isInvalid())
    return ExprError();
  return new(getSema().getASTContext()) ReflectionDataMembersExpr(getSema().getASTContext(), SubExpr.get(), SourceRange(E->getLocStart(), E->getLocEnd()));
}

StmtResult TreeCopy::TransformExpansionForStmt(ExpansionForStmt* S)
{
  ExprResult Init = TransformExpr(S->getInit());
  if (Init.isInvalid())
    return StmtError();
  StmtResult Body = TransformStmt(S->getBody());
  if (Body.isInvalid() || Body.get()->getStmtClass() != Stmt::CompoundStmtClass)
    return StmtError();
  auto CompoundStmtResult = cast<CompoundStmt>(Body.get());
  if (CompoundStmtResult->body_empty() ||
      (*CompoundStmtResult->body_begin())->getStmtClass() != Stmt::DeclStmtClass)
    return StmtError();
  auto VarDeclStmt = (*CompoundStmtResult->body_begin());

  if (Init.get()->isValueDependent() || Init.get()->isTypeDependent())
  {
    return getSema().ActOnExpansionForStmt(S->getForLoc(), S->getLParenLoc(), VarDeclStmt, Init.get(), S->getRParenLoc(), Body.get());
  }
  else
  {
    return ExpandForStmt(S, getSema());
  }
}

Decl* TreeCopy::TransformDefinition(SourceLocation Loc, Decl* D)
{
  auto TransformedDecl = TransformDecl(Loc, D);
  auto VarDeclPtr = cast_or_null<VarDecl>(TransformedDecl);
  if (VarDeclPtr && VarDeclPtr->getInit())
  {
    auto NewInit = TransformExpr(VarDeclPtr->getInit());
    if (NewInit.isInvalid())
      return D; // TODO: DeclError? like ExprError();
    auto NewDecl = CopyVarDecl(VarDeclPtr, NewInit.get(), getSema());
    transformedLocalDecl(D, NewDecl);
    return NewDecl;
  }
  return TransformedDecl;
}

}

