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
      if(literal_ids.count(curr->get<Symbol>()) > 0) {
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
  parse_macro_use = built_expander;
}

bool SyntaxCase::match(NodePtr macro) {
  transform_bindings.clear();
  it = macro;
  return parse_macro_use();
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
        create_node(*it)
    );
  }

  return built_list;
}

NodePtr SyntaxCase::transform_identifier(
    NodePtr curr,
    NodePtr next
) {
  const auto& c = create_node;

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
            c(Symbol(node->get<Symbol>().get_value()))
        );

      } else if(node->get_type() == Data::Type::PRIMITIVE_FORM) {
        sub_segment = core::append(
            sub_segment,
            c(PrimitiveFormMarker(node->get<PrimitiveFormMarker>().get()))
        );

      } else if(core::is_string(node)) {
        sub_segment = core::append(
            sub_segment,
            c(String(node->get<String>().get_string()))
        );

      } else if(core::is_boolean(node)) {
        sub_segment = core::append(
            sub_segment,
            c(Boolean(node->get<Boolean>().get_value()))
        );

      } else if(core::is_null_list(node)) {
        sub_segment = core::append(
            sub_segment,
            core::list(core::list())
        );

      } else if(core::is_pair(node)) {
        sub_segment = core::append(
            sub_segment,
            core::list(copy_list(node))
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
        segment, shaka::create_node(Symbol(curr->get<Symbol>().get_value()))
    );
  }

  return segment;
}


void SyntaxCase::expand(NodePtr macro) {
  const auto& c = shaka::create_node;

  NodePtr expanded_form = core::list();
  NodePtr expanded_it = expanded_form;
  NodePtr curr = templat;
  it = templat;
  NodePtr segment;

  // Begin generation from template
  while(it->get_type() != Data::Type::NULL_LIST) {

    curr = core::car(it);
    it = core::cdr(it);

    switch(curr->get_type()) {
    case Data::Type::SYMBOL:
      if(curr->get<Symbol>() != ellipsis) {
        segment = transform_identifier(curr, it);
      }
      break;

    case Data::Type::PRIMITIVE_FORM:
      segment = core::list(
          c(PrimitiveFormMarker(curr->get<PrimitiveFormMarker>().get()))
      );
      break;

    case Data::Type::BOOLEAN:
      segment = core::list(c(Boolean(curr->get<Boolean>().get_value())));
      break;

    case Data::Type::STRING:
      segment = core::list(c(String(curr->get<String>().get_string())));
      break;

    default:
      throw MacroExpansionException(
          60001,
          "NodePtr type is not implemented for macro expansion"
      );
    }

    // Append to expanded_form
    expanded_form = core::append(expanded_form, segment);
  }


  // After expansion is finished, replace original pointer
  core::set_car(macro, core::car(expanded_form));
  core::set_cdr(macro, core::cdr(expanded_form));
  transform_bindings.clear();
}



} // namespace macro
} // namespace shaka