#include "shaka_scheme/system/parser/syntax_rules/SyntaxRulesMacro.hpp"

namespace shaka {
namespace macro {

SyntaxRulesMacro::SyntaxRulesMacro(
    Symbol& macro_keyword,
    std::vector<SyntaxCase>& syntax_cases
) : syntax_cases(syntax_cases) {
  this->macro_keyword = macro_keyword;
}

bool SyntaxRulesMacro::expand(NodePtr macro) {

  for(auto& syntax_case : syntax_cases) {
    if(syntax_case.expand(macro)) {
      return true;
    }
  }
  throw MacroExpansionException(
      60006,
      "Failed to parse " + this->macro_keyword.get_value() + " macro"
  );
}

std::ostream& operator<<(
    std::ostream& lhs,
    const SyntaxRulesMacro& rhs
) {
  lhs << "{";
  lhs << "macro: " << rhs.macro_keyword << "|";
  lhs << "cases: ";
  for(auto& syntax_case : rhs.syntax_cases) {
    lhs << syntax_case << ", ";
  }
  lhs << "}";
  return lhs;
}

} // namespace macro
} // namespace shaka