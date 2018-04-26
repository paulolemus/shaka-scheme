#include <ostream>

#include "shaka_scheme/system/parser/syntax_rules/SyntaxRule.hpp"


namespace shaka {
namespace macro {

SyntaxRule::SyntaxRule(
    Symbol& ellipsis,
    std::set<Symbol>& literal_ids,
    NodePtr pattern,
    NodePtr templat
) : ellipsis(ellipsis),
    literal_ids(literal_ids),
    pattern(pattern),
    templat(templat),
    is_built(false) {

  pattern_parser = [](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      std::set<Symbol>& literal_ids
  ) -> bool {
    return false;
  };

  template_builder = [](
      NodePtr macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis
  ) -> bool {
    return false;
  };
}


void SyntaxRule::build() {

  is_built = true;
}

bool SyntaxRule::match(NodePtr macro) {

  if(!is_built) {
    // throw error
  }
}

bool SyntaxRule::transform(NodePtr macro) {
  return false;
}




const Symbol& SyntaxRule::get_ellipsis() const {
  return this->ellipsis;
}
const std::set<Symbol>& SyntaxRule::get_literal_ids() const {
  return this->literal_ids;
}
const NodePtr& SyntaxRule::get_pattern() const {
  return this->pattern;
}
const NodePtr& SyntaxRule::get_templat() const {
  return this->templat;
}

std::ostream& operator<<(
    std::ostream& lhs,
    const SyntaxRule& rhs
) {
  lhs << "SyntaxRule {";
  lhs << "ellipsis: " << rhs.ellipsis << ", ";
  lhs << "literals: {";
  for(auto& it : rhs.literal_ids) {
    lhs << it << ", ";
  }
  lhs << "}, ";
  lhs << "pattern: " << *rhs.pattern << ", ";
  lhs << "templat: " << *rhs.templat << '}';

  return lhs;
}

}
}
