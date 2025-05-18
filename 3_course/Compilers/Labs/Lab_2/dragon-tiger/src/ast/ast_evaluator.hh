#ifndef AST_EVALUATOR_HH
#define AST_EVALUATOR_HH

#include "../utils/errors.hh"

#include "nodes.hh"

namespace ast {
  
class ASTEvaluator : public ConstASTIntVisitor {
  int32_t visit(const class IntegerLiteral & literal) {
    return literal.value;
  }
  int32_t visit(const class BinaryOperator & op) {
    switch (op.op) {
      case o_plus: 
        return op.get_left().accept(*this) + op.get_right().accept(*this);
      case o_minus:
        return op.get_left().accept(*this) - op.get_right().accept(*this);
      case o_times:
        return op.get_left().accept(*this) * op.get_right().accept(*this);
      case o_divide:
        return op.get_left().accept(*this) / op.get_right().accept(*this);
      case o_eq:
        return op.get_left().accept(*this) == op.get_right().accept(*this);
      case o_neq:
        return op.get_left().accept(*this) != op.get_right().accept(*this);
      case o_lt:
        return op.get_left().accept(*this) < op.get_right().accept(*this);
      case o_le:
        return op.get_left().accept(*this) <= op.get_right().accept(*this);
      case o_gt:
        return op.get_left().accept(*this) > op.get_right().accept(*this);
      case o_ge:
        return op.get_left().accept(*this) >= op.get_right().accept(*this);
      default:
        utils::error("Wrong operator");
    }
  }
  int32_t visit(const class Sequence & seq) {
    if (seq.get_exprs().empty())
    {
      utils::error("Sequence empty");
    }
    const std::vector<Expr *> exprs = seq.get_exprs();
    int32_t result = 0;
    for (Expr* expr : exprs)
    {
      result = expr->accept(*this);
    }
    return result;
  }
  int32_t visit(const class IfThenElse & ite) {
    int32_t result = -1;
    if (ite.get_condition().accept(*this)) {
      result = ite.get_then_part().accept(*this);
    } else {
      result = ite.get_else_part().accept(*this);
    }
    return result;
  }
  int32_t visit(const class StringLiteral &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class Let &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class Identifier &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class VarDecl &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class FunDecl &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class FunCall &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class WhileLoop &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class ForLoop &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class Break &) {
    utils::error("Not implemented");
  }
  int32_t visit(const class Assign &) {
    utils::error("Not implemented");
  }
};

}

#endif