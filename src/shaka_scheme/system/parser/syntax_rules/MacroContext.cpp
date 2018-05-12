#include "shaka_scheme/system/parser/syntax_rules/MacroContext.hpp"
#include "shaka_scheme/system/base/DataPair.hpp"

namespace shaka {
namespace macro {

std::ostream& operator<<(std::ostream& left, const ScopeSet& right) {
  left << "ScopeMap({ ";
  for (auto it : right) {
    left << it << " ";
  }
  left << "})";
  return left;
}

MacroContext::MacroContext(HeapVirtualMachine& hvm) :
    hvm(hvm),
    curr_scope(0) {
  curr_scopes.insert(curr_scope);
  curr_scope_stack.push_back(curr_scope);
  curr_scope = next_scope;
  next_scope++;
  curr_scopes.insert(curr_scope);
  curr_scope_stack.push_back(curr_scope);
}

void MacroContext::push_scope() {
  curr_scope = next_scope;
  next_scope++;
  curr_scopes.insert(curr_scope);
  curr_scope_stack.push_back(curr_scope);
}

void MacroContext::pop_scope() {
  curr_scopes.erase(curr_scope);
  auto temp_scope = curr_scope_stack[curr_scope_stack.size() - 1];
  curr_scope_stack.pop_back();
  curr_scope = temp_scope;
}

void MacroContext::map_symbol(Symbol symbol) {
  IdentifierData id_data(curr_scopes, nullptr);
  identifier_bindings.insert({ symbol, std::move(id_data)});
}

void MacroContext::map_macro(Symbol symbol, MacroPtr macro) {
  IdentifierData id_data(curr_scopes, std::move(macro));
  identifier_bindings.insert({ symbol, std::move(id_data) });
}

void MacroContext::map_lexical_binding(Symbol symbol) {
  EnvPtr env = hvm.get_environment();
  if(env->is_defined(symbol)) {
    IdentifierData id_data(curr_scopes, nullptr, env->get_value(symbol));
    identifier_bindings.insert({symbol, std::move(id_data)});
  }
}

std::multimap<shaka::Symbol, IdentifierData>::const_iterator
MacroContext::get_bindings(Symbol symbol) {
  return identifier_bindings.find(symbol);
}

NodePtr MacroContext::get_lexical_binding(MacroPtr macro, Symbol symbol) {
  // Retrieve the scopeset of the macro.
  // Macro must be defined.
  // Use scopeset to find the proper IdentifierData from identifier_bindings.
  // If its not defined, return the literal symbol.
  bool found_macro_scope = false;
  ScopeSet binding_scopes;

  for(const auto& pair : identifier_bindings) {
    if(pair.second.macro == macro) {
      found_macro_scope = true;
      binding_scopes = pair.second.scopes;
    }
  }

  if(!found_macro_scope) {
    throw MacroExpansionException(4040, "Failed to find Macro binding");
  }

  for(const auto& pair : identifier_bindings) {
    if(pair.first == symbol && pair.second.scopes == binding_scopes) {
      return pair.second.data;
    }
  }
  return NodePtr(create_node(symbol));
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


} // namespace macro
} // namespace shaka
