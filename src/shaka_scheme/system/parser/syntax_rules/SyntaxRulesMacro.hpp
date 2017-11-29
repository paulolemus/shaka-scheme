#ifndef SHAKA_SCHEME_SYNTAXRULESMACRO_HPP
#define SHAKA_SCHEME_SYNTAXRULESMACRO_HPP

#include "shaka_scheme/system/core/lists.hpp"

#include "shaka_scheme/system/exceptions/MacroExpansionException.hpp"
#include "shaka_scheme/system/parser/syntax_rules/SyntaxCase.hpp"

#include <memory>
#include <vector>

namespace shaka {
namespace macro {


/**
 * @brief Context used by macro to hygienically expand a macro use
 *        before reaching the compiler.
 */
class MacroContext;


/**
 * @brief The type of a syntax-rules macro.
 *
 * It stores its pattern-matching and substitution functionality for one
 * specific macro.
 *
 * This type is meant to operator on Scheme lists with an additional
 * MacroChecker context -- unlike R6RS, syntax objects are not directly needed
 * to support R7RS.
 */
struct SyntaxRulesMacro {

  /**
   * @brief The Macro class used to match and expand a single macro.
   * @param macro_keyword Identifier used to represent macro <keyword>.
   * @param syntax_cases Vector of SyntaxCase instances used for case
   * matching and expanding.
   *
   * TODO: There is a circular dependency with MacroContext. Figure out how
   * to include the class without breaking everything.
   */
  SyntaxRulesMacro(
      Symbol& macro_keyword,
      std::vector<SyntaxCasePtr>& syntax_cases
  );

  /**
   * @brief Match and expand a macro use.
   * @param macro The cdr of the macro expression.
   * @return true if successfully matched and replaced, false otherwise.
   */
  bool expand(NodePtr macro);


  /**
   * @brief The identifier of this macro.
   * TODO: Consider removing this, not entirely sure if needed.
   */
  Symbol macro_keyword;

  /**
   * @brief SyntaxCase instances used for matching.
   */
  std::vector<shaka::macro::SyntaxCasePtr> syntax_cases;


  /**
   * @brief Prints out a short representation of the SyntaxRulesMacro.
   * @param lhs The ouput stream.
   * @param rhs The SyntaxRulesMacro.
   * @return The updated reference of the output stream.
   */
  friend std::ostream& operator<<(
      std::ostream& lhs,
      const SyntaxRulesMacro& rhs
  );

};


/**
 * @brief The alias for the macro pointer type.
 */
using MacroPtr = std::shared_ptr<SyntaxRulesMacro>;

} // namespace macro
} // namespace shaka

#endif //SHAKA_SCHEME_SYNTAXRULESMACRO_HPP
