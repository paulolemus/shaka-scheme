#ifndef SHAKA_SCHEME_SYNTAXCASE_HPP
#define SHAKA_SCHEME_SYNTAXCASE_HPP

#include "shaka_scheme/system/base/Data.hpp"

#include "shaka_scheme/system/core/lists.hpp"
#include "shaka_scheme/system/core/types.hpp"

#include "shaka_scheme/system/exceptions/MacroExpansionException.hpp"

#include <functional>
#include <memory>
#include <map>
#include <set>
#include <vector>


namespace shaka {
namespace macro {

/**
 * @brief Type used for generating and saving pattern matchers.
 */
using SyntaxRule = std::function<bool()>;

/**
 * @brief Context that contains information on bindings and scope.
 *        Used during expansion time to expand case hygienically.
 */
struct MacroContext;

/**
 * @brief The class used for matching to and expanding a single <syntax-rule>,
 *        with:
 *        <syntax-rule> ::= (<patter> <template>)
 *        
 *        How it works:
 *        1. generate() is called. This generates the internal function that
 *           matches and extracts binding information from a macro use.
 *        2. match(NodePtr macro) is called. This 
 */
struct SyntaxCase {

  /**
   * @brief Create SyntaxCase will everything it needs to match a NodePtr
   *        to a pattern.
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
   * @return Reference to current instance.
   */
  void generate();

  /**
   * @brief Match a macro to this SyntaxCase's pattern. On a successful match,
   *        an internal state is set to allow for the expansion of the macro.
   * @param macro The NodePtr to the start of the macro use.
   * @return True if matching, false otherwise.
   */
  bool match(NodePtr macro);

  /**
   * @brief Expand a macro use. On a failure, it does nothing. Note that
   *        this function begins a match assuming the NodePtr points to the
   *        start of the macro.
   *        TODO: INTEGRATE WITH MacroContext
   * @param macro NodePtr to the Node *after* the macro keyword.
   * @return true on success, false on failure.
   * TODO: Pass this a reference to the Context.
   */
  void expand(NodePtr macro);

  /**
   * @brief Used internally during the expansion of a identifier in the
   * template.
   * @param curr The Data node with the current ientifier
   * @param next The rest of the list (cdr).
   * @return NodePtr of new expanded segment.
   */
  NodePtr transform_identifier(NodePtr curr, NodePtr next);

  /**
   * @brief The function that is generated to match and expand a macro.
   * Default state is as function that simply returns false.
   */
  SyntaxRule parse_macro_use = []() -> bool {
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
   * @brief Used for matching.
   */
  NodePtr it;

  /**
   * @brief used for transformation of macro use. Internal use.
   *        This keeps track of what to replace a pattern identifier with from
   *        the macro use.
   */
  std::map<Symbol, std::vector<NodePtr>> transform_bindings;

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


/**
 * @brief Alias for type.
 */
using SyntaxCasePtr = std::shared_ptr<SyntaxCase>;


} // namespace macro
} // namespace shaka

#endif //SHAKA_SCHEME_SYNTAXCASE_HPP
