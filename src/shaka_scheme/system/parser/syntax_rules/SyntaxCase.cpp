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

const Symbol& SyntaxCase::get_macro_keyword() const {
  return this->macro_keyword;
}

const Symbol& SyntaxCase::get_ellipsis() const {
  return this->ellipsis;
}

const std::set<shaka::Symbol>& SyntaxCase::get_literal_ids() const {
  return this->literal_ids;
}

const NodePtr& SyntaxCase::get_pattern() const {
  return this->pattern;
}

const NodePtr& SyntaxCase::get_templat() const {
  return this->templat;
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
 * SyntaxRule assistive functions
 */

/**
 * @brief Create a function that matches to a special keyword.
 * @param syntax_case Contains expression iterator of pattern.
 * @param keyword The keyword to match to.
 * @return true for a match, false otherwise.
 */
SyntaxRule create_rule_keyword(
    const Symbol& keyword
) {
  return [=](NodePtr& it) -> bool {
    // Save pointer
    const NodePtr key = core::car(it);

    if(!core::is_symbol(key)) {
      return false;
    }

    if(keyword == key->get<Symbol>()) {
      it = core::cdr(it);
      return true;
    } else {
      return false;
    }
  };
}
/**
 * @brief Create new general identifier
 * @param syntax_case Instance of the current SyntaxCase.
 * @param symbol The pattern variable to bind to.
 * @return lambda that returns true on match, false otherwise.
 */
SyntaxRule create_rule_identifier(
    SyntaxCase* syntax_case,
    const Symbol& symbol
) {
  return [=](NodePtr& it) -> bool {

    // Guard against null list
    if(core::is_null_list(it)) {
      return false;
    }
    // Guard against invalid identifier types.
    if(!core::is_symbol(it)      &&
       !core::is_proper_list(it) &&
       !core::is_boolean(it)     &&
       !core::is_string(it)) {
      return false;
    }

    // Add NodePtr to transformation bindings.
    if(syntax_case->transform_bindings.count(symbol) < 1) {
      // No bindings currently exist, so add a new vector
      syntax_case->transform_bindings[symbol] =
          std::vector<NodePtr>{core::car(it)};
    } else {
      // A binding exists, push back on vector
      syntax_case->transform_bindings[symbol].push_back(core::car(it));
    }

    // Move the iterator up for any other matching functions that may be called.
    it = core::cdr(it);
    return true;
  };
}


/**
 * @brief Rule needs to pass at least one time
 * @param rule The rule to check for success one or more times.
 * @return lambda that returns true if rule matches one or more times, false
 * otherwise.
 */
SyntaxRule create_rule_kleene_plus(
    SyntaxRule& rule
) {
  return [=](NodePtr& it) -> bool {
    bool found_one = rule(it);

    if(found_one) {
      while(rule(it));
    }
    return found_one;
  };
}

SyntaxRule create_rule_null_list() {

  return [=](NodePtr& it) -> bool {
    return core::is_null_list(it);
  };
}

SyntaxRule create_list_rule(
    SyntaxRule rule
) {
  return [=](NodePtr& it) -> bool {
    if(core::is_null_list(it)) {
      return false;
    }
    NodePtr sub_it = core::car(it);
    it = core::cdr(it);
    return rule(sub_it);
  };
}

SyntaxRule combine_rule_or(
    SyntaxRule lhs,
    SyntaxRule rhs
) {
  return [=](NodePtr& it) -> bool {
    return lhs(it) || rhs(it);
  };
}

SyntaxRule combine_rule_and(
    SyntaxRule lhs,
    SyntaxRule rhs
) {
  return [=](NodePtr& it) -> bool {
    return lhs(it) && rhs(it);
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

SyntaxRule SyntaxCase::list_generator(NodePtr root) {

  SyntaxRule matcher = [=](NodePtr& it) -> bool { return true; };
  NodePtr it = root;
  NodePtr curr;
  NodePtr next;

  try {

    // Iterate through the pattern list. For each expression, generate a
    // function that will match to that pattern, then use that generated
    // pattern to build upon a single function that matches to the entire
    // pattern.
    while(!core::is_null_list(it)) {

      SyntaxRule new_rule;
      curr = core::car(it);
      it = core::cdr(it);
      next = it;

      // Use one look ahead token to check for ellipsis.
      if(!core::is_null_list(next)) {
        next = core::car(next);
      }

      switch(curr->get_type()) {

      case Data::Type::SYMBOL:
        // Generate rule for either a literal id or a pattern variable.
        if(literal_ids.count(curr->get<Symbol>()) > 0) {
          new_rule = create_rule_keyword(curr->get<Symbol>());
        } else {
          new_rule = create_rule_identifier(this, curr->get<Symbol>());
        }
        break;

      case Data::Type::DATA_PAIR:
        // Recursively call this function to parse the sub list.
        new_rule = create_list_rule(list_generator(curr));
        break;

      case Data::Type::NUMBER:
      case Data::Type::STRING:
      case Data::Type::PRIMITIVE_FORM:
      case Data::Type::BOOLEAN:
      case Data::Type::NULL_LIST:
      default:
        throw MacroExpansionException(
            60020,
            "list_generation: type unimplemented"
        );
      }

      // Repeat the previously generated pattern if the pattern is followed
      // by a variable.
      if(core::is_symbol(next) && next->get<Symbol>() == ellipsis) {
        new_rule = create_rule_kleene_plus(new_rule);
        it = core::cdr(it);
      }

      // Append new rule to pattern match, in the form of "and then".
      matcher = matcher & new_rule;
    }

    // Add null list to signify end of match
    matcher = matcher & create_rule_null_list();

  } catch(const MacroExpansionException& e) {
    throw e;
  } catch (...) {
    throw MacroExpansionException(
        60003,
        "Encountered error while matching syntax case "
            + macro_keyword.get_value()
    );
  }

  return matcher;
}


/**
 * @brief Replace the expand_macro function with a valid matcher function.
 */
void SyntaxCase::generate() {

  // Check for initial macro symbol or underscore.
  if(!core::is_pair(pattern) || !core::is_symbol(core::car(pattern))) {
    throw MacroExpansionException(
        60002,
        "No symbol at start of macro pattern list"
    );
  }
  auto str = core::car(pattern)->get<Symbol>().get_value();
  if(str != macro_keyword.get_value() && str != "_") {
    throw MacroExpansionException(
        60001,
        "macro keyword or underscore not found"
    );
  }

  // Create initial rule to match macro name.
  SyntaxRule matcher =
      create_rule_keyword(macro_keyword) |
      create_rule_keyword(Symbol("_"));

  // Build rule for the rest of the pattern and "and then" it with the
  // initial rule.
  parse_macro_use = matcher & list_generator(core::cdr(pattern));
}


bool SyntaxCase::match(NodePtr macro) {
  transform_bindings.clear();
  return parse_macro_use(macro);
}


/*
 * Assistive function
 * Macro transformation and expansion
 */
NodePtr copy_list(NodePtr root) {

  NodePtr built_list = core::list();
  NodePtr it;

  while(!core::is_null_list(root)) {
    it = core::car(root);
    root = core::cdr(root);

    built_list = core::append(
        built_list,
        core::list(create_node(*it))
    );
  }

  return built_list;
}


NodePtr SyntaxCase::transform_identifier(
    NodePtr curr,
    NodePtr next
) {
  const auto& c = create_node;
  using core::list;

  NodePtr segment = core::list();
  auto map_iter = transform_bindings.find(curr->get<Symbol>());
  bool followed_by_ellipse = false;

  // Check for leading ellipsis.
  if(next->get_type() == Data::Type::DATA_PAIR &&
     core::car(next)->get_type() == Data::Type::SYMBOL &&
     core::car(next)->get<Symbol>() == ellipsis) {
    followed_by_ellipse = true;
  }

  if(map_iter != transform_bindings.end()) {
    // Process a pattern variable.
    // If the vector contains a single NodePtr, the variable should NOT be
    // followed by ellipses.
    auto& bindings = map_iter->second;

    if(!followed_by_ellipse && bindings.size() > 1) {
      // There is a mismatch: cannot be more than 1 binding without ellipsis.
      throw MacroExpansionException(
          60001,
          "Found more than one binding without ellipsis repeat pattern"
      );
    }

    // Build expanded form.
    for(auto& node : bindings) {

      NodePtr sub_segment = core::list();

      if(core::is_symbol(node)) {
        sub_segment = core::append(
            sub_segment,
            list(c(Symbol(node->get<Symbol>().get_value())))
        );

      } else if(node->get_type() == Data::Type::PRIMITIVE_FORM) {
        sub_segment = core::append(
            sub_segment,
            list(c(PrimitiveFormMarker(node->get<PrimitiveFormMarker>().get())))
        );

      } else if(core::is_string(node)) {
        sub_segment = core::append(
            sub_segment,
            list(c(String(node->get<String>().get_string())))
        );

      } else if(core::is_boolean(node)) {
        sub_segment = core::append(
            sub_segment,
            list(c(Boolean(node->get<Boolean>().get_value())))
        );

      } else if(core::is_null_list(node)) {
        sub_segment = core::append(
            sub_segment,
            core::list(core::list())
        );

      } else if(core::is_pair(node)) {
        sub_segment = core::append(
            sub_segment,
            list(copy_list(node))
        );

      } else {
        throw MacroExpansionException(
            60020,
            "Unimplemented identifier expansion"
        );
      }

      segment = core::append(segment, sub_segment);
    }


  } else {
    // May be a free variable.
    segment = core::append(
        segment,
        core::list(shaka::create_node(Symbol(curr->get<Symbol>().get_value())))
    );
  }

  return segment;
}


NodePtr SyntaxCase::list_expander(NodePtr it) {
  using core::append;
  using core::list;
  using core::car;
  using core::cdr;
  const auto& c = shaka::create_node;

  NodePtr expanded_form = list();
  NodePtr expanded_it = expanded_form;
  NodePtr curr = it;
  NodePtr segment;

  // Iterate through the template, until the end is reached.
  // For each symbol, build the corresponding expanded form of the symbol and
  // push it into a list representing the complete expanded form.
  while(it->get_type() != Data::Type::NULL_LIST) {

    // Iterate one s-expression
    curr = car(it);
    it = cdr(it);

    // Skip past any ellipse, as they are handled in each function.
    if(core::is_symbol(curr) && curr->get<Symbol>() == ellipsis) {
      continue;
    }

    // The expression of curr needs to be evaluated differently depending on
    // the type of the list.
    switch(curr->get_type()) {
    case Data::Type::SYMBOL:
      if(curr->get<Symbol>() != ellipsis) {
        segment = transform_identifier(curr, it);
      }
      break;

    case Data::Type::PRIMITIVE_FORM:
      segment = list(
          c(PrimitiveFormMarker(curr->get<PrimitiveFormMarker>().get()))
      );
      break;

    case Data::Type::BOOLEAN:
      segment = list(c(Boolean(curr->get<Boolean>().get_value())));
      break;

    case Data::Type::STRING:
      segment = list(c(String(curr->get<String>().get_string())));
      break;

    case Data::Type::DATA_PAIR:
    std::cout << "ENTERED DATA PAIR PART" << std::endl;
      segment = list(list_expander(curr));
      break;

    default:
    std::cout << "ENTERED DEFAULT FOR THING" << std::endl;
      throw MacroExpansionException(
          60001,
          "NodePtr type is not implemented for macro expansion"
      );
    }

    // Append to expanded_form
    expanded_form = append(expanded_form, segment);
  }

  return expanded_form;
}


void SyntaxCase::expand(NodePtr macro) {
  using core::append;
  using core::list;
  using core::car;
  using core::cdr;

  NodePtr expanded_form = list();
  expanded_form = append(expanded_form, list_expander(templat));

  // After expansion is finished, replace the car and cons of the macro with
  // the expanded form. Then clear the bindings for the next usage.
  core::set_car(macro, car(expanded_form));
  core::set_cdr(macro, cdr(expanded_form));
  transform_bindings.clear();
}


} // namespace macro
} // namespace shaka