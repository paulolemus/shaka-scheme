#ifndef SHAKA_SCHEME_MACRO_ENGINE_HPP
#define SHAKA_SCHEME_MACRO_ENGINE_HPP

#include "shaka_scheme/system/parser/parser_definitions.hpp"
#include "shaka_scheme/system/core/lists.hpp"
#include "shaka_scheme/system/core/types.hpp"

#include "shaka_scheme/system/exceptions/MacroExpansionException.hpp"

#include "shaka_scheme/system/parser/syntax_rules/SyntaxCase.hpp"
#include "shaka_scheme/system/parser/syntax_rules/MacroContext.hpp"

#include <map>
#include <stack>
#include <algorithm>

namespace shaka {
namespace macro {

bool is_primitive_set(Symbol symbol, MacroContext& context) {
  try {
    if (context.hvm.get_environment()->get_value(symbol)
        ->get<PrimitiveFormMarker>() == PrimitiveFormMarker("set!")) {
      return true;
    }
  } catch (shaka::InvalidInputException e) {
    return false;
  }
  return false;
};

MacroPtr get_macro(Symbol symbol, MacroContext& context) {
  IdentifierData current_binding(context.curr_scopes, nullptr);
  IdentifierData max_data(std::set<std::size_t>(), nullptr);
  for (auto it = context.get_bindings(symbol);
       it != context.identifier_bindings.end();
       ++it) {
    const auto scopes = it->second.scopes;
    std::vector<std::size_t> intersect;
    std::set_intersection(
        scopes.begin(),
        scopes.end(),
        current_binding.scopes.begin(),
        current_binding.scopes.end(),
        std::back_inserter(intersect)
    );
    if (intersect.size() > max_data.scopes.size()) {
      max_data = it->second;
    }
  }
  return max_data.macro;
};

bool is_primitive_syntax_rules(Symbol symbol, MacroContext& context) {
  try {
    if (context.hvm.get_environment()->get_value(symbol)
        ->get<PrimitiveFormMarker>()
        == PrimitiveFormMarker("syntax-rules")) {
      return true;
    }
  } catch (shaka::InvalidInputException e) {
    return false;
  }
  return false;
};

bool is_primitive_lambda(Symbol symbol, MacroContext& context) {
  try {
    if (context.hvm.get_environment()->get_value(symbol)
        ->get<PrimitiveFormMarker>() == PrimitiveFormMarker("lambda")) {
      return true;
    }
  } catch (shaka::InvalidInputException e) {
    return false;
  }
  return false;
};

bool is_primitive_define(Symbol symbol, MacroContext& context) {
  try {
    if (context.hvm.get_environment()->get_value(symbol)
        ->get<PrimitiveFormMarker>() == PrimitiveFormMarker("define")) {
      return true;
    }
  } catch (shaka::InvalidInputException e) {
    return false;
  }
  return false;
};

bool is_primitive_quote(Symbol symbol, MacroContext& context) {
  try {
    if (context.hvm.get_environment()->get_value(symbol)
        ->get<PrimitiveFormMarker>() == PrimitiveFormMarker("quote")) {
      return true;
    }
  } catch (shaka::InvalidInputException e) {
    return false;
  }
  return false;
};

bool is_primitive_define_syntax(Symbol symbol, MacroContext& context) {
  try {
    if (context.hvm.get_environment()->get_value(symbol)
        ->get<PrimitiveFormMarker>() ==
        PrimitiveFormMarker("define-syntax")) {
      return true;
    }
  } catch (shaka::InvalidInputException e) {
    return false;
  }
  return false;
};

bool is_primitive_let_syntax(Symbol symbol, MacroContext& context) {
  try {
    if (context.hvm.get_environment()->get_value(symbol)
        ->get<PrimitiveFormMarker>() ==
        PrimitiveFormMarker("let-syntax")) {
      return true;
    }
  } catch (shaka::InvalidInputException e) {
    return false;
  }
  return false;
};

// TODO: LOOK TO THIS FUNCTION FOR A DEMONSTRATION ON EXPANDING A LIST
// Ex: (define (foo f) (+ f 1)) -> (define foo (lambda (f) (+ f 1))
bool process_define_form(NodePtr& it, MacroContext& context) {
  if (!is_primitive_define(core::car(it)->get<Symbol>(), context)) {
    return false;
  }
  using namespace shaka::core;
  if (is_proper_list(car(cdr(it))) || is_improper_list(car(cdr(it)))) {
    auto lambda_form = list(
        create_node(Symbol("lambda")),
        cdr(car(cdr(it)))
    );
    set_cdr(cdr(lambda_form), cdr(cdr(it)));
    auto rewritten_form = list(
        create_node(Symbol("define")),
        car(car(cdr(it))),
        lambda_form
    );
    it = rewritten_form;
    //std::cout << "DEFINE: rewriting define procedure form: " << *it <<
    //          std::endl;
    return process_define_form(it, context);
  }
  if (is_symbol(car(cdr(it)))) {
    //std::cout << "mapping identifier: " << car(cdr(it))
    //    ->get<Symbol>() << std::endl;
    context.map_symbol(car(cdr(it))->get<Symbol>());

    it = core::cdr(it);
    it = core::cdr(it);
    return true;
  }
};

bool process_set_form(NodePtr& it, MacroContext& context) {
  if (!is_primitive_set(core::car(it)->get<Symbol>(), context)) {
    return false;
  }
  if (!core::is_symbol(core::car(core::cdr(it)))) {
    throw MacroExpansionException(60007, "(set!) must have an identifier "
        "as its second argument");
  }
  if (core::length(it) != 3) {
    throw MacroExpansionException(60008, "(set!) expression must be a "
        "list of 3 elements");
  }
  it = core::cdr(it);
  it = core::cdr(it);
  return true;
};

bool process_quote_form(NodePtr& it, MacroContext& context) {
  if (!is_primitive_quote(core::car(it)->get<Symbol>(), context)) {
    return false;
  }
  if (core::length(it) != 2) {
    throw MacroExpansionException(60004, "(quote) cannot have more than 1 "
        "argument");
  }
  it = core::cdr(it);
  return true;
};

bool process_lambda_form(NodePtr& it, MacroContext& context) {
  if (!is_primitive_lambda(core::car(it)->get<Symbol>(), context)) {
    return false;
  }
  if (core::length(it) <= 2) {
    throw MacroExpansionException(60001, "(lambda) form must contain at "
        "least the arguments and the body expression(s)");
  }
  auto args = core::car(core::cdr(it));
  std::vector<Symbol> identifiers;
  // If the lambda is a proper or improper list, we must figure out
  // if it consists of only identifiers.
  if (core::is_pair(args)) {
    auto jt = args;
    //std::cout << "jt: " << *jt << std::endl;
    for (;
        core::is_pair(jt);
        jt = core::cdr(jt)) {
      auto item = core::car(jt);
      //std::cout << "item: " << *item << std::endl;
      if (item->get_type() != Data::Type::SYMBOL) {
        throw MacroExpansionException(60000, "(lambda) arguments must "
            "contain only identifiers");
      }
      identifiers.push_back(item->get<Symbol>());
    }
    if (jt->get_type() == Data::Type::SYMBOL) {
      //std::cout << "(lambda) improper list last args: " <<
      //
      // jt/->get<Symbol>()
      //          << std::endl;
      identifiers.push_back(jt->get<Symbol>());
    } else if (jt->get_type() != Data::Type::NULL_LIST) {
      //std::cout << *jt << std::endl;
      throw MacroExpansionException(60006, "the last argument in a "
          "(lambda) arguments list must be a symbol or a null list");
    }
    // If we got through the loop, we need to mark the identifiers with
    // the current scope before we return true;
    it = core::cdr(it);
    it = core::cdr(it);
    context.push_scope();
    for (auto identifier : identifiers) {
      //std::cout << "mapping identifier: " << identifier << std::endl;
      context.map_symbol(identifier);
    }
    // The iterator is left at the body.
    return true;
  } else if (core::is_symbol(args)) {
    //std::cout << "mapping identifier: " << *args << std::endl;
    context.map_symbol(args->get<Symbol>());
    it = core::cdr(it);
    it = core::cdr(it);
    return true;
  } else {
    throw MacroExpansionException(60002, "(lambda) arguments cannot be "
        "non-symbol types");
  }
};

/**
 * @brief Build and save a macro to the MacroContext for when a macro is used.
 * @param root NodePtr to keyword after primitive define-syntax
 * @param context
 */
void build_macro(NodePtr root, MacroContext& context) {

  // TODO: Reached a define syntax keyword. PARSE THE DEFINITION HERE!
  // TODO: BUILD THE MACRO INSTANCE
  // TODO: I will need to use context.map_macro(Symbol, MacroPtr) after making macro
  // TODO: I need to keep track of scope for each case within the macro.
  // TODO: Expansions are NOT recursive within one expansion. Expansion is called on the root after expansion.
  // TODO: SyntaxCaseRules ONLY handles a MACRO. A macro is made up of: Macro symbol, list of SyntaxCases.
  // TODO: SyntaxCase object is contructed with the NodePtr to a case, a list of aux keywords, and the context.
  // TODO: SyntaxCase symbol in pattern or template can bind to outside symbol. This is why it is constructed with the context.
  //
  // SyntaxRulesMacro constructed with Symbol and list of SyntaxCases.
  // SyntaxCases is constructed with the macro symbol name, aux keywords, and context. It has functions to parse a case.
  // Move to syntax rules
  std::cout << "BUILDING MACRO" << std::endl;

  // Save Macro keyword
  Symbol macro_keyword(Symbol(core::car(root)->get<Symbol>().get_value()));
  std::cout << macro_keyword << std::endl;

  // Check for transformer spec (starts with primitive syntax-rules)
  root = core::car(core::cdr(root));
  Symbol syntax_rules_symbol = core::car(root)->get<Symbol>().get_value();
  if(!is_primitive_syntax_rules(syntax_rules_symbol, context)) {
    throw MacroExpansionException(60009, "primitive define-syntax "
        "must be followed by a keyword and a primitive syntax-rules");
  }

  // TODO: CHECK FOR ELLIPSIS REPLACEMENT
  root = core::cdr(root);
  Symbol ellipsis("...");

  // Make set of literal-IDs from literal-ID list
  NodePtr literal_id_list = core::car(root);
  std::set<Symbol> literal_ids;

  if(!core::is_proper_list(literal_id_list)) {
    throw MacroExpansionException(60008, "missing literal-id proper list");
  }
  // Add all literal-IDs in list to set
  while(literal_id_list->get_type() != Data::Type::NULL_LIST) {
    NodePtr current_id = core::car(literal_id_list);
    if(current_id->get_type() != Data::Type::SYMBOL) {
      throw MacroExpansionException(60007, "Invalid literal-id type");
    }
    literal_ids.insert(current_id->get<Symbol>());
    literal_id_list = core::cdr(literal_id_list);
  }

  root = core::cdr(root);

  // Here we have reached the syntax rule list
  // Important variables that have been created so far:
  // macro_keyword (Symbol): the special keyword of the macro
  // ellipsis (Symbol): string that represents ellipsis, currently set to "..."
  // literal_ids (set<Symbol>): all the literal-IDs that may be in patterns

  // Instantiate SyntaxCases
  std::vector<SyntaxCasePtr> syntax_cases;

  // Create each SyntaxCase instance
  while(root->get_type() != Data::Type::NULL_LIST) {

    NodePtr syntax_rule = core::car(root);
    NodePtr pattern = core::car(syntax_rule);
    NodePtr templat = core::car(core::cdr(syntax_rule));

    context.push_scope();

    std::cout << *syntax_rule << std::endl;
    std::cout << *pattern << std::endl;
    std::cout << *templat << std::endl;

    SyntaxCasePtr syntax_case = std::make_shared<SyntaxCase>(
        macro_keyword,
        ellipsis,
        literal_ids,
        pattern,
        templat,
        context.curr_scope
    );
    syntax_cases.push_back(syntax_case);

    context.pop_scope();
    std::cout << *syntax_cases.back() << std::endl;
    root = core::cdr(root);
  }

  // Generate the pattern matcher for each case
  for(auto& syntax_case : syntax_cases) {
    syntax_case->generate();
  }

  // Create Macro instance and bind to symbol in MacroContext
  MacroPtr macro = std::make_shared<SyntaxRulesMacro>(
      macro_keyword,
      syntax_cases
  );

  std::cout << *macro << std::endl;

  context.map_macro(
      macro_keyword,
      macro
  );
}

std::ostream& operator<<(
    std::ostream& lhs,
    const MacroContext& rhs) {
  lhs << "MacroContext(curr_scope: " << rhs.curr_scope << " | scope_stack: { ";
  for (auto it : rhs.curr_scope_stack) {
    lhs << it << " ";
  }
  lhs << "} | curr_scopes: { ";
  for (auto it : rhs.curr_scopes) {
    lhs << it << " ";
  }
  lhs << "})";
  return lhs;
}

void run_macro_expansion(
    NodePtr root,
    MacroContext& macro_context) {
  //std::cout << "\nBEFORE TRAVERSE TREE: " << *root << std::endl;
  //std::cout << "BEFORE TRAVERSE TREE CONTEXT: " << macro_context << std::endl;
  //for (
  //  auto binding :
  //    macro_context.identifier_bindings) {
  //  //std::cout << "BEFORE TRAVERSE TREE BINDINGS {" << binding.first << " | "
  //  //    "" << binding .second.scopes << std::endl;
  //}
  int count = 0;
  bool need_to_pop_scope = false;
  // Checks if the NodePtr is a list. It is assumed that input is NOT an inproper list.
  if (core::is_pair(root)) {
    NodePtr proc_name = core::car(root);
    if (core::is_symbol(proc_name)) {
      Symbol identifier = proc_name->get<Symbol>();
      auto macro = get_macro(identifier, macro_context);
      if (macro) {
        std::cout << "macro_engine: NEED TO EXPAND MACRO HERE!" << std::endl;
        // TODO: Consider only passing the cdr of root
        macro->expand(root);
      } else {
        if (process_define_form(root, macro_context)) {
          //std::cout << "PRIMITIVE: define" << std::endl;
        } else if (process_set_form(root, macro_context)) {
          //std::cout << "PRIMITIVE: set" << std::endl;
        } else if (process_lambda_form(root, macro_context)) {
          //std::cout << "PRIMITIVE: lambda" << std::endl;
          need_to_pop_scope = true;
        } else if (process_quote_form(root, macro_context)) {
          //std::cout << "PRIMITIVE: quote" << std::endl;
          return;
        } else if (is_primitive_define_syntax(identifier, macro_context)) {
          std::cout << "PRIMITIVE: define-syntax" << std::endl;
          need_to_pop_scope = true;
          macro_context.push_scope();
          build_macro(core::cdr(root), macro_context);

        } else if (is_primitive_let_syntax(identifier, macro_context)) {
          //std::cout << "PRIMITIVE: let-syntax" << std::endl;
          need_to_pop_scope = true;
          macro_context.push_scope();
        } else if (is_primitive_syntax_rules(identifier, macro_context)) {
          //std::cout << "PRIMITIVE: syntax-rules" << std::endl;
          need_to_pop_scope = true;
          macro_context.push_scope();
        } else {
          //std::cout << "NON-PRIMITIVE: " << *proc_name << std::endl;
        }
      }
    } else if (!core::is_pair(proc_name)) {
      throw MacroExpansionException(60010, "procedure name cannot be a "
          "non-identifier or non-procedure call");
    }
  }
  for (NodePtr it = root;
       core::is_pair(it);
       it = core::cdr(it), count++) {
    auto item = core::car(it);
    //std::cout << "#" << count << ": " << *it << " | " << *item << std::endl;
    if (core::is_proper_list(item)) {
      run_macro_expansion(item, macro_context);
    }
  }
  if (need_to_pop_scope) {
    macro_context.pop_scope();
  }
  //std::cout << "after TRAVERSE TREE: " << *root << std::endl;
  //std::cout << "after TRAVERSE TREE CONTEXT: " << macro_context << std::endl;
  //for (
  //  auto binding :
  //    macro_context.identifier_bindings) {
  //  //std::cout << "after TRAVERSE TREE BINDINGS {" << binding.first << " | "
  //  //    "" << binding .second.scopes << std::endl;
  //} //std::cout << std::endl;
}

} // namespace macro
} // namespace shaka

#endif //SHAKA_SCHEME_MACRO_ENGINE_HPP
