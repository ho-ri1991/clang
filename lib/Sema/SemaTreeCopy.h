#ifndef LLVM_CLANG_LIB_SEMA_TREECOPY_H
#define LLVM_CLANG_LIB_SEMA_TREECOPY_H

#include "TreeTransform.h"

namespace clang {
using namespace sema;

StmtResult ExpandForStmt(ExpansionForStmt* S, Sema& SemaRef);

StmtResult ExpandForStmt(Expr* Init, Stmt* Body, Sema& SemaRef);

class TreeCopy: public TreeTransform<TreeCopy> {
public:
  using inherited = TreeTransform<TreeCopy>;

  TreeCopy(Sema &SemaRef, SourceLocation Loc);

  SourceLocation getBaseLocation() const { return Loc; }
  // bool AlreadyTransformed(QualType T);
  // DeclarationName getBaseEntity();
  bool AlwaysRebuild() const { return true; }

  StmtResult TransformExpansionForStmt(ExpansionForStmt* S);

  ExprResult TransformReflectionEnumFieldsExpr(ReflectionEnumFieldsExpr* E);

  ExprResult TransformReflectionDataMembersExpr(ReflectionDataMembersExpr* E);

//  ExprResult TransformReflectionEnumFieldValueExpr(ReflectionEnumFieldValueExpr *E);

//  StmtResult RebuildDeclStmt(MutableArrayRef<Decl *> Decls, SourceLocation StartLoc, SourceLocation EndLoc);

  Decl* TransformDefinition(SourceLocation Loc, Decl* D);
private:
  SourceLocation Loc;
};

}

#endif

