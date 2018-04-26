#ifndef SHAKA_SCHEME_SYNTAXRULESMACRO_HPP
#define SHAKA_SCHEME_SYNTAXRULESMACRO_HPP

#include <memory>
#include <vector>

#include "shaka_scheme/system/base/DataPair.hpp"
#include "shaka_scheme/system/base/Symbol.hpp"
#include "shaka_scheme/system/parser/syntax_rules/SyntaxRule.hpp"

namespace shaka {
namespace macro {

/**
 * @brief The type of a syntax-rules macro.
 *
 * It stores its pattern-matching and substitution functionality.
 *
 * This type is meant to operator on Scheme lists with an additional
 * MacroChecker context -- unlike R6RS, syntax objects are not directly needed
 * to support R7RS.
 */
class SyntaxRulesMacro {

public:
  /**
   * @brief The Macro class used to match and expand against a single
   * SyntaxRules Macro.
   * @param macro_keyword Identifier used to represent macro keyword
   * @param syntax_cases Vector of SyntaxRule instances used for case
   * matching and expanding.
   */
  SyntaxRulesMacro(
      Symbol& macro_keyword,
      std::vector<SyntaxRulePtr>& syntax_rules
  );

  /**
   * @brief Match to and expand a macro use. Used in macro expander
   * preprocessor to modify a the AST in-place.
   * Can potentially throw a MacroExpansionException.
   * @param macro The CDR of the macro expression.
   * @return true if successfully matched and replaced, false otherwise.
   */
  bool transform(NodePtr macro);

  /**
   * @brief Prints out a short representation of the SyntaxRulesMacro.
   * @param lhs The output stream.
   * @param rhs  The SyntaxRulesMacro.
   * @return the updated reference of the output stream.
   */
  friend std::ostream& operator<<(
      std::ostream& lhs,
      const SyntaxRulesMacro& rhs
  );

private:
  /**
   * @brief The identifier for this macro.
   * TODO: Consider if this is necessary.
   */
  const Symbol macro_keyword;

  /**
   * @brief Pointers to instances of SyntaxCases used for matching and
   * expansion of individual cases.
   */
  const std::vector<SyntaxRulePtr> syntax_rules;

};

/**
 * @brief The alias for the macro pointer type.
 */
using MacroPtr = std::shared_ptr<SyntaxRulesMacro>;

} // namespace macro
} // namespace shaka

#endif //SHAKA_SCHEME_SYNTAXRULESMACRO_HPP
