#include <shaka_scheme/system/exceptions/MacroExpansionException.hpp>
#include "shaka_scheme/system/core/lists.hpp"
#include "shaka_scheme/system/parser/syntax_rules/pattern_lang_definitions.hpp"


namespace shaka {
namespace macro {

using namespace shaka::core;

//using PatternParser = std::function<bool(
//    NodePtr&, macro
//    SyntaxRuleBindings&, bindings
//    const Symbol&, ellipsis
//    const std::set<Symbol>& literals
//)>;


// Testing functions

bool is_literal(
    NodePtr& data,
    const Symbol& ellipsis,
    const std::set<Symbol>& literals
) {
  std::cout << "is_literal: ";
  // If it is any of the following types, it is a literal
  if(data->get_type() == Data::Type::BOOLEAN ||
      data->get_type() == Data::Type::NUMBER ||
      data->get_type() == Data::Type::STRING) {
    std::cout << "types - true\n";
    return true;
  }

  // If it is an ellipsis, it is not a literal.
  if(data->get_type() == Data::Type::SYMBOL &&
      data->get<Symbol>() == ellipsis) {
    std::cout << "ellipse - false\n";
    return false;
  }

  // If it is in literals, it is a literal.
  bool result = data->get_type() == Data::Type::SYMBOL &&
      literals.count(data->get<Symbol>()) > 0;

  if(result) {
    std::cout << "set - true\n";
    return true;
  } else {
    std::cout << "set - false\n";
    return false;
  }
}

bool is_pattern_var(
    NodePtr& data,
    const Symbol& ellipsis,
    const std::set<Symbol>& literals
) {
  return data->get_type() == Data::Type::SYMBOL &&
      literals.count(data->get<Symbol>()) < 1 &&
      data->get<Symbol>() != ellipsis;
}

bool is_ellipsis(
    NodePtr& data,
    const Symbol& ellipsis
) {
  return data->get_type() == Data::Type::SYMBOL &&
         data->get<Symbol>() == ellipsis;
}

bool is_pattern(
    NodePtr& data
) {
  return is_proper_list(data) || is_improper_list(data);
}



// General pattern matching functions


PatternParser make_parse_literal(NodePtr& literal) {

  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {

    std::cout << "parse_literal: " << *macro << std::endl;
    NodePtr literal_data;
    if(!is_null_list(macro)) {
      literal_data = car(macro);
    } else {
      return false;
    }
    bool found_literal = is_literal(literal_data, ellipsis, literals);

    if(found_literal) {
      macro = cdr(macro);
      std::cout << "parse_literal macro: " << *macro << std::endl;
      return true;
    } else {
      return false;
    }
  };
}

PatternParser make_parse_pattern_var(NodePtr& pattern_var) {

  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {
    std::cout << "parse_pattern_var: \n";

    // If pattern variable is previously used, throw syntax error.
    Symbol& pattern_binding = pattern_var->get<Symbol>();
    // TODO: UNCOMMENT
    //if(bindings.count(pattern_binding) > 0) {
    //  throw MacroExpansionException(
    //      3196,
    //      "PatternParser: reuse of pattern variable invalid."
    //  );
    //}

    // Push back if it is a pattern var.
    NodePtr pattern_bind;
    if(!is_null_list(macro)) {
      pattern_bind = car(macro);
    } else {
      return false;
    }

    if(bindings.count(pattern_binding) < 1) {
      // Add new vector
      bindings[pattern_binding] = std::vector<NodePtr>{pattern_bind};
    } else {
      // Already exists, push back.
      bindings[pattern_binding].push_back(pattern_bind);
    }
    macro = cdr(macro);
    return true;
  };
}

PatternParser make_parse_null() {

  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {
    std::cout << "parse_null: " << *macro << std::endl;
    if(is_null_list(macro)) {
      return true;
    } else {
      return false;
    }
  };
}

PatternParser make_parse_unit() {

  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {
    std::cout << "parse_unit \n";
    return true;
  };
}

PatternParser make_parse_kleene(PatternParser& func, size_t post_bind_count) {

  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {
    std::cout << "parse_kleene\n";

    int count = 0;
    int limit = (int)length(macro) - post_bind_count;

    // Limit guard
    if(limit < 0) {
      throw MacroExpansionException(
          3333,
          "Ellipsis Error, not enough arguments in list to satisfy bindings."
      );
    }

    while(count < limit && func(macro, bindings, ellipsis, literals)) {
      count++;
    }
    return true;
  };
}

PatternParser consume_ellipsis(
    NodePtr& lst,
    PatternParser& func,
    const Symbol& ellipsis,
    const std::set<Symbol>& literals
) {
  std::cout << "CONSUMING ELLIPSE\n";
  size_t post_binding_count = 0;
  NodePtr it = cdr(cdr(lst));

  while(!is_null_list(it)) {
    NodePtr data = car(it);
    if(is_pattern_var(data, ellipsis, literals)) {
      post_binding_count++;
    }
    it = cdr(it);
  }
  return make_parse_kleene(func, post_binding_count);
}

PatternParser make_or(PatternParser& left, PatternParser& right) {

  return [=](
      NodePtr macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {
    return left (macro, bindings, ellipsis, literals) ||
           right(macro, bindings, ellipsis, literals);
  };
}

PatternParser make_then(PatternParser& left, PatternParser& right) {

  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {
    return left (macro, bindings, ellipsis, literals) &&
           right(macro, bindings, ellipsis, literals);
  };
}


// <pattern> matching functions
PatternParser make_pattern_parser(
    NodePtr& pattern,
    const Symbol& ellipsis,
    const std::set<Symbol>& literals
) {

  std::cout << "MAKE_PTRN_PARSR: " << *pattern << std::endl;
  PatternParser parser_builder = make_parse_unit();
  PatternParser curr_parser = make_parse_unit();

  NodePtr curr = pattern;
  NodePtr next = pattern;

  if(!is_null_list(curr)) {
    next = cdr(curr);
  }

  bool found_ellipsis = false;
  bool ellipse_consumed = false;

  // Process a flat list.
  while(!is_null_list(curr) && !is_null_list(next)) {

    NodePtr curr_data = car(curr);
    NodePtr next_data = car(next);

    // ellipsis look ahead
    if(is_ellipsis(next_data, ellipsis)) {
      if(found_ellipsis) {
        throw MacroExpansionException(
            5565,
            "Syntax Error, ellipsis only valid once per pattern list."
        );
      }
      std::cout << "build: found ellipse\n";
      found_ellipsis = true;

    }

    // Process current node
    if(is_literal(curr_data, ellipsis, literals)) {
      std::cout << "build: adding literal\n";
      curr_parser = make_parse_literal(curr_data);

    } else if(is_pattern_var(curr_data, ellipsis, literals)) {
      std::cout << "build: adding pat var\n";
      curr_parser = make_parse_pattern_var(curr_data);

    } else if(is_pattern(curr_data)) {
      std::cout << "build: adding pat\n";
      curr_parser = make_pattern_parser(curr_data, ellipsis, literals);

    } else if(is_ellipsis(curr_data, ellipsis)) {
      std::cout << "build: adding ellipise\n";
      curr_parser = make_parse_unit();
    }

    // Process ellipsis if applicable
    if(found_ellipsis && !ellipse_consumed) {
      ellipse_consumed = true;
      curr_parser = consume_ellipsis(curr, curr_parser, ellipsis, literals);
    }

    parser_builder = make_then(parser_builder, curr_parser);
    curr = next;
    next = cdr(next);
  }


  if(!is_null_list(curr)) {
    std::cout << "build: entered outter loop\n";

    NodePtr curr_data = car(curr);

    if(is_literal(curr_data, ellipsis, literals)) {
      std::cout << "build: adding lit\n";
      curr_parser = make_parse_literal(curr_data);

    } else if(is_pattern_var(curr_data, ellipsis, literals)) {
      std::cout << "build: adding patrn var\n";
      curr_parser = make_parse_pattern_var(curr_data);

    } else if(is_pattern(curr_data)) {
      std::cout << "build: adding pattnr\n";
      curr_parser = make_pattern_parser(curr_data, ellipsis, literals);

    } else if(is_ellipsis(curr_data, ellipsis)) {
      std::cout << "build: adding ellipise\n";
      curr_parser = make_parse_unit();
    }
    parser_builder = make_then(parser_builder, curr_parser);
  }
  curr_parser = make_parse_null();
  return make_then(parser_builder, curr_parser);
}

}
}
