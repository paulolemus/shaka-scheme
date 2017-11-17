#ifndef SHAKA_SCHEME_SYNTAXCASE_HPP
#define SHAKA_SCHEME_SYNTAXCASE_HPP

#include "shaka_scheme/system/base/Data.hpp"

#include "shaka_scheme/system/core/lists.hpp"
#include "shaka_scheme/system/core/types.hpp"

#include "shaka_scheme/system/exceptions/MacroExpansionException.hpp"

#include <functional>
#include <memory>
#include <set>
#include <vector>


namespace shaka {
namespace macro {

/**
 * @brief The class used for matching to and expanding a single <syntax-rule>,
 * with:
 * <syntax-rule> ::= (<patter> <template>)
 */
struct SyntaxCase {

  /**
   * @brief Create SyntaxCase will everything it needs to match a NodePtr
   * to a pattern.
   * @param macro_keyword The identifier used to represent a macro <keyword>.
   * @param ellipsis Ellipsis identifier <ellipsis>.
   * @param literal_ids Set of special IDs to look during matching.
   * @param pattern NodePtr to the <pattern> list.
   * @param templat NodePtr to the <template> list.
   * @param scope Unique scope of this particular SyntaxCase.
   */
  SyntaxCase(
      Symbol& macro_keyword,
      Symbol& ellipsis,
      std::set<Symbol>& literal_ids,
      NodePtr pattern,
      NodePtr templat,
      std::size_t scope
  );

  /**
   * @brief Generates the function used to match a macro use.
   * Throws MacroExpansionException if the pattern is malformed.
   */
  void generate();

  /**
   * @brief Match and then expand a macro use. This function only expands the
   * macro if it was a match. On a failure, it does nothing. Note that this
   * function begins a match assuming the NodePtr points to the node *after*
   * the macro keyword at the start of the expression.
   * TODO: INTEGRATE WITH MacroContext
   * @param macro NodePtr to the Node *after* the macro keyword.
   * @return true on success, false on failure.
   */
  bool expand(NodePtr macro);


  /**
   * @brief The function that is generated to match and expand a macro.
   * Default state is as function that simply returns false.
   */
  std::function<bool(shaka::NodePtr)> expand_macro = [](NodePtr root) -> bool {
    return false;
  };

  /**
   * @brief The name of the macro this case belongs to.
   * TODO: Should be an <identifier>
   */
  Symbol macro_keyword;

  /**
   * @brief The ellipsis operator. Normally this is "...", however it can be
   * defined as something else in R7RS syntax-rules of the form:
   * <transformer spec> :==
   *    (syntax-rules <ellipsis> (<literal> ...) <syntax rule> ...)
   */
  Symbol ellipsis;

  /**
   * @brief All of the special identifiers found in the literal list in
   * syntax-rules. Used during matching.
   */
  std::set<Symbol> literal_ids;

  /**
   * @brief NodePtr to a pattern expression. Used to generate a pattern matcher.
   */
  NodePtr pattern;

  /**
   * @brief The template to expand a matched pattern into. Used during
   * expansion.
   */
  NodePtr templat;

  /**
   * @brief The unique scope to this particular syntax case.
   * TODO: I might not need this, consider removing.
   */
  std::size_t scope;


  /**
   * @brief Prints a representation of SyntaxCase.
   * @param lhs The output stream.
   * @param rhs The SyntaxCase.
   * @return The updated reference of the output stream.
   */
  friend std::ostream& operator<<(
      std::ostream& lhs,
      const SyntaxCase& rhs
  );

};


} // namespace macro
} // namespace shaka

#endif //SHAKA_SCHEME_SYNTAXCASE_HPP
