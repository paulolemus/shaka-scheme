#include "shaka_scheme/system/parser/syntax_rules/SyntaxCase.hpp"

#include <stack>

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
  this->it = pattern;
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

/*
 * Assistive functions
 * Syntax Rule
 */
/**
 * @brief Create a function that matches to a special keyword.
 * @param syntax_case
 * @param root
 * @return
 */
SyntaxRule create_rule_keyword(
    SyntaxCase* syntax_case,
    const Symbol keyword
) {
  return [=]() -> bool {
    // Save pointer
    const NodePtr key = core::car(syntax_case->it);

    if(!core::is_symbol(key)) {
      return false;
    }

    if(keyword == key->get<Symbol>()) {
      syntax_case->it = core::cdr(syntax_case->it);
      return true;
    } else {
      return false;
    }

  };
}
/**
 * @brief Create new general identifier
 * @param syntax_case_ptr
 * @return
 */
SyntaxRule create_rule_identifier(
    SyntaxCase* syntax_case
) {
  return [=]() -> bool {

    if(core::is_null_list(syntax_case->it)) {
      return false;
    }

    // Identifier matches to almost anything
    if(!core::is_symbol(syntax_case->it) &&
       !core::is_proper_list(syntax_case->it) &&
       !core::is_boolean(syntax_case->it) &&
       !core::is_string(syntax_case->it)) {
      return false;
    }

    syntax_case->it = core::cdr(syntax_case->it);
    return true;
  };
}
/**
 * @brief Rule needs to pass at least one time
 * @param syntax_case
 * @return
 */
SyntaxRule create_rule_kleene_plus(
    SyntaxRule rule
) {
  return [=]() -> bool {
    bool found_one = rule();

    if(found_one) {
      while(rule());
    }
    return found_one;
  };
}

SyntaxRule create_rule_null_list(
    SyntaxCase* syntax_case
) {
  return [=]() -> bool {
    return core::is_null_list(syntax_case->it);
  };
}

SyntaxRule combine_rule_or(
    SyntaxRule lhs,
    SyntaxRule rhs
) {
  return [=]() -> bool {
    return lhs() || rhs();
  };
}

SyntaxRule combine_rule_and(
    SyntaxRule lhs,
    SyntaxRule rhs
) {
  return [=]() -> bool {
    return lhs() && rhs();
  };
}

SyntaxRule operator|(
    const SyntaxRule lhs,
    const SyntaxRule rhs
) {

  return combine_rule_or(lhs, rhs);

}
SyntaxRule operator&(
    const SyntaxRule lhs,
    const SyntaxRule rhs
) {
  return combine_rule_and(lhs, rhs);
}


/**
 * @brief Replace the expand_macro function with a valid matcher function.
 */
void SyntaxCase::generate() {
  SyntaxRule built_expander;
  it = pattern;
  NodePtr curr = it;
  NodePtr next = it;

  try {

    std::cout << "Entering SyntaxCase generation:" << std::endl;
    std::cout << *it << std::endl;

    // Check for initial macro symbol or underscore.
    curr = core::car(it);
    if(!core::is_symbol(curr)) {
      throw MacroExpansionException(
          60002,
          "No symbol at start of macro"
      );
    }
    auto str = curr->get<Symbol>().get_value();
    if(str != macro_keyword.get_value() && str != "_") {
      throw MacroExpansionException(
          60001,
          "macro keyword or underscore not found"
      );
    }
    // Create initial rule to match macro name.
    built_expander =
        create_rule_keyword(this, macro_keyword) |
        create_rule_keyword(this, Symbol("_"));


    // Loop through pattern building a expander.
    it = core::cdr(it);
    while(!core::is_null_list(it)) {
      std::cout << *it << std::endl;

      SyntaxRule new_rule;
      curr = core::car(it);
      it = core::cdr(it);
      next = it;

      if(!core::is_null_list(next)) {
        next = core::car(next);
      }

      // TODO: Currently assumes there are no lists, only symbols.
      if(!core::is_symbol(curr)) {
        throw MacroExpansionException(60001, "No symbol found");
      }

      // Check for special keywords
      if(literal_ids.find(curr->get<Symbol>()) != literal_ids.end()) {
        new_rule = create_rule_keyword(this, curr->get<Symbol>());
      } else {
        new_rule = create_rule_identifier(this);
      }

      // Check for ellipsis
      if(core::is_symbol(next) && next->get<Symbol>() == ellipsis) {
        new_rule = create_rule_kleene_plus(new_rule);
        it = core::cdr(it);
      }

      built_expander = built_expander & new_rule;
    }

    // Add null list to signify end of match
    built_expander = built_expander & create_rule_null_list(this);

  } catch(const MacroExpansionException& e) {
    throw e;
  } catch (...) {
    throw MacroExpansionException(
        60003,
        "Encountered error while matching syntax case "
            + macro_keyword.get_value()
    );
  }
  expand_macro = built_expander;
}

bool SyntaxCase::match(NodePtr macro) {
  it = macro;
  return expand_macro();
}

bool SyntaxCase::expand(NodePtr macro) {
  it = macro;
  return expand_macro();
}



} // namespace macro
} // namespace shaka