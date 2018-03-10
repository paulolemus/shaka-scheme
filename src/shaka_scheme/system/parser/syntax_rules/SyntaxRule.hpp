#ifndef SHAKA_SCHEME_SYNTAXRULE_HPP
#define SHAKA_SCHEME_SYNTAXRULE_HPP

#include <set>

#include "shaka_scheme/system/base/Data.hpp"
#include "shaka_scheme/system/base/Symbol.hpp"

namespace shaka {
namespace macro {

/**
 * @brief The class used for matching to and expanding a single <syntax-rule>
 * as defined in R7RS.
 *
 * How it works:
 * 1. build() is called. This builds the internal function that is used for
 * matching and extracting binding information from a macro use.
 * 2. expand(NodePtr macro). This binds all data from the macro pattern, and
 * then modifies the NodePtr location to expand transform it into the filled
 * template version.
 *
 * TODO: Create builder class for SyntaxRule.
 */
class SyntaxRule {

public:

  /**
   * @brief Create SyntaxRule. TODO: BUILDER CLASS
   * @param ellipsis Ellipsis identifier <ellipsis>.
   * @param literal_ids Set of special IDs that can be matched but not bound.
   * @param pattern NodePtr to <pattern> of <syntax rule>.
   * @param templat NodePtr to <template> of <syntax rule>.
   */
  SyntaxRule(
      Symbol& ellipsis,
      std::set<Symbol>& literal_ids,
      NodePtr pattern,
      NodePtr templat
  );

  /**
   * @brief Build the internal pattern matching and binding extracting functor.
   * Can potentially throw a MacroExpansionException.
   */
  void build();


  /**
   * @brief Match a macro use to this particular SyntaxRule pattern. On a
   * successful match, all bindings are extracted but not used.
   * @param macro NodePtr to the macro AST.
   * @return true on successful match, false otherwise.
   */
  bool match(NodePtr macro);

  /**
   * @brief Match and then expand a macro use if it matches the <pattern> of
   * this SyntaxRule. On failure to match, this function does nothing.
   * @param macro NodePtr to head of the macro use AST.
   * @return true on successful match and expansion, false otherwise.
   */
  bool transform(NodePtr macro);

  /**
   * @brief Prints a representation of SyntaxRule.
   * @param lhs The output stream.
   * @param rhs The SyntaxRule reference.
   * @return The updated reference of the output stream.
   */
  friend std::ostream& operator<<(
      std::ostream& lhs,
      const SyntaxRule& rhs
  );


  // Testing functions
  const Symbol& get_ellipsis() const;
  const std::set<Symbol>& get_literal_ids() const;
  const NodePtr& get_pattern() const;
  const NodePtr& get_templat() const;

private:

  const Symbol ellipsis;
  const std::set<Symbol> literal_ids;
  const NodePtr pattern;
  const NodePtr templat;

};

/**
 * @brief Alias for type.
 */
using SyntaxRulePtr = std::shared_ptr<SyntaxRule>;

}
}


#endif //SHAKA_SCHEME_SYNTAXRULE_HPP
