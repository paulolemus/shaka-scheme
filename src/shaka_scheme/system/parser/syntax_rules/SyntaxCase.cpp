#include "shaka_scheme/system/parser/syntax_rules/SyntaxCase.hpp"

namespace shaka {
namespace macro {


SyntaxCase::SyntaxCase(
    Symbol& macro_keyword,
    Symbol& ellipsis,
    std::set<Symbol>& literal_ids,
    NodePtr pattern,
    NodePtr templat,
    std::size_t scope
) : literal_ids(literal_ids),
    scope(scope) {
  this->macro_keyword = macro_keyword;
  this->ellipsis = ellipsis;
  this->pattern = pattern;
  this->templat = templat;
}


void SyntaxCase::generate() {
  return;
}


bool SyntaxCase::expand(NodePtr macro) {
  return this->expand_macro(macro);
}



std::ostream& operator<<(
    std::ostream& lhs,
    const SyntaxCase& rhs
) {
  lhs << '{';
  lhs << "macro-case: " << rhs.macro_keyword << "|";
  lhs << "ellipsis: " << rhs.ellipsis << "|";
  lhs << "literals: ";
  for(auto& it : rhs.literal_ids) {
    lhs << it << ", ";
  }
  lhs << "|";
  lhs << "pattern: " << *rhs.pattern << "|";
  lhs << "template: " << *rhs.templat << "|";
  lhs << "scope: " << rhs.scope << "}";
  return lhs;
}

} // namespace macro
} // namespace shaka