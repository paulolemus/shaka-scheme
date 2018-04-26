#include "shaka_scheme/system/parser/syntax_rules/SyntaxRulesMacro.hpp"
#include "shaka_scheme/system/exceptions/MacroExpansionException.hpp"


namespace shaka {
namespace macro {


SyntaxRulesMacro::SyntaxRulesMacro(
    Symbol& macro_keyword,
    std::vector<SyntaxRulePtr>& syntax_cases
) : macro_keyword(macro_keyword),
    syntax_rules(syntax_cases) {
  for(const auto& syntax_rule : syntax_rules) {
    std::cout << "rule: " << *syntax_rule << std::endl;
  }
}



bool SyntaxRulesMacro::transform(NodePtr macro) {

  for(const auto& syntax_case : syntax_rules) {
    if(syntax_case->transform(macro)) {
      return true;
    }
  }
  throw MacroExpansionException(
      4343,
      "Failed to parse " + this->macro_keyword.get_value() + " macro"
  );
}


std::ostream& operator<<(
    std::ostream& lhs,
    const SyntaxRulesMacro& rhs
) {
  lhs << "SyntaxRulesMacro print todo";
  return lhs;
}

}
}
