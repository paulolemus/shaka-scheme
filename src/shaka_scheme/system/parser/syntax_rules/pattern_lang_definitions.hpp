#ifndef SHAKA_SCHEME_PATTERN_LANG_DEFINITIONS_HPP
#define SHAKA_SCHEME_PATTERN_LANG_DEFINITIONS_HPP


#include "shaka_scheme/system/base/Data.hpp"
#include "shaka_scheme/system/parser/syntax_rules/SyntaxRule.hpp"


namespace shaka {
namespace macro {

// General pattern matching functions

PatternParser make_parse_literal(NodePtr& literal); // No binding.Const/lits
PatternParser make_parse_pattern_var(NodePtr& patrn_var); // Match and bind.
PatternParser make_parse_null();       // Match empty list.
PatternParser make_parse_unit();       // Evaluates to true.

PatternParser make_or(PatternParser& left, PatternParser& right);
PatternParser make_then(PatternParser& left, PatternParser& right);


// <pattern> matching functions
PatternParser make_pattern_parser(
    NodePtr& pattern,
    const Symbol& ellipsis,
    const std::set<Symbol>& literals
);



}
}

#endif //SHAKA_SCHEME_PATTERN_LANG_DEFINITIONS_HPP
