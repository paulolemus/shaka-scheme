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
  //std::cout << "is_literal: ";
  // If it is any of the following types, it is a literal
  if(data->get_type() == Data::Type::BOOLEAN ||
      data->get_type() == Data::Type::NUMBER ||
      data->get_type() == Data::Type::STRING) {
    //std::cout << "types - true\n";
    return true;
  }

  // If it is an ellipsis, it is not a literal.
  if(data->get_type() == Data::Type::SYMBOL &&
      data->get<Symbol>() == ellipsis) {
    //nstd::cout << "ellipse - false\n";
    return false;
  }

  // If it is in literals, it is a literal.
  bool result = data->get_type() == Data::Type::SYMBOL &&
      literals.count(data->get<Symbol>()) > 0;

  if(result) {
    //std::cout << "set - true\n";
    return true;
  } else {
    //std::cout << "set - false\n";
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

    //std::cout << "parse_literal: " << *macro << std::endl;
    NodePtr literal_data;
    if(!is_null_list(macro)) {
      literal_data = car(macro);
    } else {
      return false;
    }

    if(!is_literal(literal_data, ellipsis, literals)) {
      return false;
    }

    bool literal_matching = false;

    if(literal->get_type() == Data::Type::BOOLEAN &&
        literal_data->get_type() == Data::Type::BOOLEAN &&
        literal->get<Boolean>() == literal_data->get<Boolean>()) {
      literal_matching = true;
    }
    else if(literal->get_type() == Data::Type::NUMBER &&
        literal_data->get_type() == Data::Type::NUMBER &&
        literal->get<Number>() == literal_data->get<Number>()) {
      literal_matching = true;
    }
    else if(literal->get_type() == Data::Type::STRING &&
        literal_data->get_type() == Data::Type::STRING &&
        literal->get<String>() == literal_data->get<String>()) {
      literal_matching = true;
    }
    else if(literal->get_type() == Data::Type::SYMBOL &&
        literal_data->get_type() == Data::Type::SYMBOL &&
        literal->get<Symbol>() == literal_data->get<Symbol>()) {
      literal_matching = true;
    }

    if(literal_matching) {
      macro = cdr(macro);
      //std::cout << "parse_literal macro: " << *macro << std::endl;
      return true;
    } else {
      return false;
    }
  };
}

PatternParser make_parse_pattern_var(NodePtr& pattern_var) {

  //std::cout << "CREATING: parse_pattern_var: "
  //    + pattern_var->get<Symbol>().get_value()
  //          << std::endl;
  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {
    //std::cout << "parse_pattern_var: "
    //          + pattern_var->get<Symbol>().get_value()
    //          << std::endl;

    // The pattern symbol to bind to.
    Symbol& pattern_binding = pattern_var->get<Symbol>();
    if(bindings.count(pattern_binding) < 1) {
      bindings[pattern_binding] = std::vector<NodePtr>();
    }

    // Push back if it is a pattern var.
    NodePtr pattern_bind;
    if(!is_null_list(macro)) {
      pattern_bind = car(macro);
    } else {
      return false;
    }

    bindings[pattern_binding].push_back(pattern_bind);
    macro = cdr(macro);
    return true;
  };
}

PatternParser make_save_pattern_var(NodePtr& pattern_var) {

  //std::cout << "make_save_pattern_var: saving " + pattern_var->get<Symbol>()
  //    .get_value() << std::endl;
  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {

    // The pattern symbol to bind to.
    Symbol& pattern_binding = pattern_var->get<Symbol>();
    //std::cout << "make_save_pattern_var\n";
    if(bindings.count(pattern_binding) < 1) {
      //std::cout << "Saving " << pattern_binding.get_value() << std::endl;
      bindings[pattern_binding] = std::vector<NodePtr>();
    }
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
    //std::cout << "parse_null: " << *macro << std::endl;
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
    //std::cout << "parse_unit \n";
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
    //std::cout << "parse_kleene\n";

    int count = 0;
    int limit = (int)length(macro) - (int)post_bind_count;

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
  //std::cout << "CONSUMING ELLIPSE\n";
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

PatternParser make_list(PatternParser& pattern) {

  //std::cout << "parse: make_list\n";
  return [=](
      NodePtr& macro,
      SyntaxRuleBindings& bindings,
      const Symbol& ellipsis,
      const std::set<Symbol>& literals
  ) -> bool {

    //std::cout << "entered list: " << *macro << std::endl;
    if(!is_proper_list(macro) && !is_improper_list(macro)) {
      return false;
    }
    if(is_null_list(macro)) {
      return false;
    }

    NodePtr inner_ptr = car(macro);
    if(!is_proper_list(inner_ptr) && !is_improper_list(inner_ptr)) {
      return false;
    }
    //std::cout << "parsing list: " << *inner_ptr << std::endl;
    //std::cout << "passed on: " << *macro << std::endl;
    macro = cdr(macro);
    return pattern(inner_ptr, bindings, ellipsis, literals);
  };
}


// At the start of every pattern parser creation, I need to iterate through
// the current list and save all pattern variables.
// This is recursive, it should iterate the entire pattern.
PatternParser make_save_all_pattern_vars(
  NodePtr& pattern,
  const Symbol& ellipsis,
  const std::set<Symbol>& literals
  ) {
  //std::cout << "SAVING ALL PATTERN VARIABLES\n";
  NodePtr it = pattern;
  PatternParser builder = make_parse_unit();
  PatternParser curr_parser = make_parse_unit();

  while(!is_null_list(it)) {
    NodePtr curr = car(it);

    if(!is_literal(curr, ellipsis, literals) &&
        is_pattern_var(curr, ellipsis, literals)) {
      curr_parser = make_save_pattern_var(curr);

    } else if(is_proper_list(curr) || is_improper_list(curr)) {
      curr_parser = make_save_all_pattern_vars(curr, ellipsis, literals);
    }

    it = cdr(it);
    builder = make_then(builder, curr_parser);
  }

  return builder;
}


// <pattern> matching functions
PatternParser make_pattern_parser(
    NodePtr& pattern,
    const Symbol& ellipsis,
    const std::set<Symbol>& literals
) {

  //std::cout << "MAKE_PTRN_PARSR: " << *pattern << std::endl;
  PatternParser parser_builder = make_save_all_pattern_vars(
      pattern, ellipsis, literals
  );
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
      //std::cout << "build: found ellipse\n";
      found_ellipsis = true;

    }

    bool found_pattern_var = false;

    // Process current node
    if(is_literal(curr_data, ellipsis, literals)) {
      //std::cout << "build: adding literal\n";
      curr_parser = make_parse_literal(curr_data);

    } else if(is_pattern_var(curr_data, ellipsis, literals)) {
      //std::cout << "build: adding pat var\n";
      curr_parser = make_parse_pattern_var(curr_data);
      found_pattern_var = true;

    } else if(is_pattern(curr_data)) {
      //std::cout << "build: adding pat\n";
      curr_parser = make_pattern_parser(curr_data, ellipsis, literals);
      curr_parser = make_list(curr_parser);

    } else if(is_ellipsis(curr_data, ellipsis)) {
      //std::cout << "build: adding unit\n";
      curr_parser = make_parse_unit();

    }

    // Process ellipsis if applicable
    if(found_ellipsis && !ellipse_consumed) {
      ellipse_consumed = true;
      curr_parser = consume_ellipsis(curr, curr_parser, ellipsis, literals);
      if(found_pattern_var) {
        auto func = make_save_pattern_var(curr_data);
        curr_parser = make_then(func, curr_parser);
      }
    }

    parser_builder = make_then(parser_builder, curr_parser);
    curr = next;
    next = cdr(next);
  }


  if(!is_null_list(curr)) {
    //std::cout << "build: entered outer loop\n";

    NodePtr curr_data = car(curr);

    if(is_literal(curr_data, ellipsis, literals)) {
      //std::cout << "build: adding lit\n";
      curr_parser = make_parse_literal(curr_data);

    } else if(is_pattern_var(curr_data, ellipsis, literals)) {
      //std::cout << "build: adding patrn var\n";
      curr_parser = make_parse_pattern_var(curr_data);

    } else if(is_pattern(curr_data)) {
      //std::cout << "build: adding pattnr\n";
      curr_parser = make_pattern_parser(curr_data, ellipsis, literals);
      curr_parser = make_list(curr_parser);

    } else if(is_ellipsis(curr_data, ellipsis)) {
      //std::cout << "build: adding unit \n";
      curr_parser = make_parse_unit();

    }

    parser_builder = make_then(parser_builder, curr_parser);
  }
  curr_parser = make_parse_null();
  return make_then(parser_builder, curr_parser);
}

}
}
