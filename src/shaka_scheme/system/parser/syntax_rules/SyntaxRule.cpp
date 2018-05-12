#include <ostream>

#include "shaka_scheme/system/core/lists.hpp"
#include "shaka_scheme/system/exceptions/MacroExpansionException.hpp"
#include "shaka_scheme/system/parser/syntax_rules/SyntaxRule.hpp"
#include "shaka_scheme/system/parser/syntax_rules/pattern_lang_definitions.hpp"


namespace shaka {
namespace macro {

SyntaxRule::SyntaxRule(
    const Symbol& ellipsis,
    const std::set<Symbol>& literal_ids,
    const NodePtr pattern,
    const NodePtr templat
) : ellipsis(ellipsis),
    literal_ids(literal_ids),
    pattern(pattern),
    templat(templat),
    is_built(false) {

  pattern_parser = [](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literal_ids
  ) -> bool {
    return false;
  };
}


void SyntaxRule::build() {
  // TODO: wrap in try/catch for non MacroExpansionExceptions.

  // The initial pattern must be a proper or improper list.
  // It also must start with an identifier and is not involved in the matching.
  if(!core::is_proper_list(pattern) && !core::is_improper_list(pattern)) {
    throw MacroExpansionException(
        5439,
        "Syntax Error, pattern must be in list form."
    );
  }
  NodePtr head = core::car(pattern);
  if(!core::is_symbol(head)) {
    throw MacroExpansionException(
        4334,
        "Syntax Error, first item must be an identifier."
    );
  }

  NodePtr rest = core::cdr(pattern);
  pattern_parser = make_pattern_parser(
      rest,
      ellipsis,
      literal_ids
  );
  is_built = true;
}


bool SyntaxRule::match(NodePtr macro) {
  if(!is_built) {
    throw MacroExpansionException(
        5469,
        "Attempted to match before building SyntaxRule"
    );
  }

  // The initial pattern must be a proper or improper list.
  // It also must start with an identifier and is not involved in the matching.
  if(!core::is_proper_list(macro) && !core::is_improper_list(macro)) {
    throw MacroExpansionException(
        5439,
        "Syntax Error, pattern must be in list form."
    );
  }
  NodePtr head = core::car(macro);
  if(!core::is_symbol(head)) {
    throw MacroExpansionException(
        4334,
        "Syntax Error, first item must be an identifier."
    );
  }
  //std::cout <<"MATCH: before cdr\n";
  NodePtr rest = core::cdr(macro);
  //std::cout <<"MATCH: PASSING: " << *rest << std::endl;
  SyntaxRuleBindings bindings;
  return pattern_parser(rest, bindings, ellipsis, literal_ids);
}


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

static NodePtr transform_identifier(
    NodePtr curr,
    NodePtr next,
    const Symbol& ellipsis,
    const SyntaxRuleBindings& bindings
) {
  const auto& c = create_node;
  using core::list;

  NodePtr segment = core::list();
  auto map_iter = bindings.find(curr->get<Symbol>());
  bool followed_by_ellipse = false;

  // Check for leading ellipsis.
  if(next->get_type() == Data::Type::DATA_PAIR &&
      core::car(next)->get_type() == Data::Type::SYMBOL &&
      core::car(next)->get<Symbol>() == ellipsis) {
    followed_by_ellipse = true;
  }

  if(map_iter != bindings.end()) {
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

      } else if(node->get_type() == Data::Type::NUMBER) {
        sub_segment = core::append(
            sub_segment,
            list(c(Number(node->get<Number>())))
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

static NodePtr transform_list(
    NodePtr it,
    const Symbol& ellipsis,
    const SyntaxRuleBindings& bindings
) {

  using namespace core;
  const auto& c = shaka::create_node;

  NodePtr expanded_form = list();
  NodePtr expanded_it = expanded_form;
  NodePtr curr = it;
  NodePtr segment;

  std::cout << "SyntaxRule: transforming list: " << *it << std::endl;

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
        segment = transform_identifier(curr, it, ellipsis, bindings);
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

    case Data::Type::NUMBER:
      segment = list(c(Number(curr->get<Number>())));
      break;

    case Data::Type::DATA_PAIR:
      segment = list(transform_list(curr, ellipsis, bindings));
      break;

    case Data::Type::NULL_LIST:
      segment = list(list());
      break;

    default:
    std::cout << "ENUM TYPE: "
              << static_cast<std::underlying_type<Data::Type >::type>
                 (curr->get_type())
              << std::endl;
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


bool SyntaxRule::transform(NodePtr macro) {

  std::cout << "SyntaxRule: transform: " << *macro << std::endl;

  if(!is_built) {
    throw MacroExpansionException(
        5469,
        "Attempted to match before building SyntaxRule"
    );
  }

  // The initial pattern must be a proper or improper list.
  // It also must start with an identifier and is not involved in the matching.
  if(!core::is_proper_list(macro) && !core::is_improper_list(macro)) {
    throw MacroExpansionException(
        5439,
        "Syntax Error, pattern must be in list form."
    );
  }
  NodePtr head = core::car(macro);
  if(!core::is_symbol(head)) {
    throw MacroExpansionException(
        4334,
        "Syntax Error, first item must be an identifier."
    );
  }
  NodePtr rest = core::cdr(macro);

  SyntaxRuleBindings bindings;
  if(!pattern_parser(rest, bindings, ellipsis, literal_ids)) {
    std::cout << "SyntaxRule transform: failed to parse\n";
    return false;
  }

  for(auto& pair: bindings) {
    std::cout << "SyntaxRule bindings: " << pair.first.get_value() << ": ";
    for(auto& np : pair.second) {
      std::cout << *np << ", ";
    }
    std::cout << std::endl;
  }

  // Assuming that template is a list, I need to generate a new list with
  // expanded features and then replace the car and cdr of the NodePtr.
  using core::append;
  using core::list;

  try {
    NodePtr expanded_form = list();
    expanded_form = append(
        expanded_form,
        transform_list(templat, ellipsis, bindings)
    );

    if(core::is_null_list(expanded_form)) {
      expanded_form = list(list());
    }

    core::set_car(macro, core::car(expanded_form));
    core::set_cdr(macro, core::cdr(expanded_form));

  } catch(const MacroExpansionException& e) {
    std::cout << e.what() << std::endl;
    std::cout << "SyntaxRule.transform(): Failed" << std::endl;
    return false;
  }
  std::cout << "SyntaxRule.transform(): Success" << std::endl;
  return true;
}


const Symbol& SyntaxRule::get_ellipsis() const {
  return this->ellipsis;
}
const std::set<Symbol>& SyntaxRule::get_literal_ids() const {
  return this->literal_ids;
}
const NodePtr& SyntaxRule::get_pattern() const {
  return this->pattern;
}
const NodePtr& SyntaxRule::get_templat() const {
  return this->templat;
}

std::ostream& operator<<(
    std::ostream& lhs,
    const SyntaxRule& rhs
) {
  lhs << "SyntaxRule {";
  lhs << "ellipsis: " << rhs.ellipsis << ", ";
  lhs << "literals: {";
  for(auto& it : rhs.literal_ids) {
    lhs << it << ", ";
  }
  lhs << "}, ";
  lhs << "pattern: " << *rhs.pattern << ", ";
  lhs << "templat: " << *rhs.templat << '}';

  return lhs;
}

}
}
