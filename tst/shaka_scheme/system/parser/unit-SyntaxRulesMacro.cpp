#include <gmock/gmock.h>

#include "shaka_scheme/system/parser/parser_definitions.hpp"
#include "shaka_scheme/system/vm/HeapVirtualMachine.hpp"
#include "shaka_scheme/system/parser/syntax_rules/macro_engine.hpp"
#include "shaka_scheme/system/parser/syntax_rules/MacroContext.hpp"
#include "shaka_scheme/system/parser/syntax_rules/SyntaxRulesMacro.hpp"
#include "shaka_scheme/system/parser/syntax_rules/SyntaxCase.hpp"

using namespace shaka::core;
using namespace shaka::macro;
using namespace shaka;


TEST(SyntaxRulesMacroUnitTest, match_base) {

  auto root = list(create_node(Symbol("M")));
  std::cout << *root << std::endl;
  ASSERT_EQ(car(root)->get<Symbol>(), Symbol("M"));
}


TEST(SyntaxRulesMacroUnitTest, match_two) {
  auto root1 = list(create_node(Symbol("M")), create_node(String("testing")));
  auto root2 = list(create_node(Symbol("M")), list(create_node(Symbol("quote"))));

  std::cout << *root1 << std::endl;
  std::cout << *root2 << std::endl;

  ASSERT_EQ(length(root1), 2);
  ASSERT_EQ(length(root2), 2);
}

TEST(SyntaxRulesMacroUnitTest, make_sexp) {
  const auto& c = create_node;


  NodePtr expr =
      list(
          c(Symbol("define")),
          c(Symbol("x")),
          c(Symbol("\'helloworld"))
      );
}

TEST(SyntaxRulesMacroUnitTest, parse_string) {

  // HVM required for MacroContext
  HeapVirtualMachine hvm(
      create_node(String("Hello world")),
      core::list(create_node(Symbol("halt"))),
      std::make_shared<Environment>(nullptr),
      ValueRib(),
      nullptr
  );
  EnvPtr env = hvm.get_environment();
  env->set_value(Symbol("define"), create_node(PrimitiveFormMarker("define")));
  env->set_value(Symbol("set!"), create_node(PrimitiveFormMarker("set!")));
  env->set_value(Symbol("lambda"), create_node(PrimitiveFormMarker("lambda")));
  env->set_value(Symbol("quote"), create_node(PrimitiveFormMarker("quote")));
  env->set_value(Symbol("define-syntax"),
                 create_node(PrimitiveFormMarker("define-syntax")));
  env->set_value(Symbol("let-syntax"),
                 create_node(PrimitiveFormMarker("let-syntax")));
  env->set_value(Symbol("syntax-rules"),
                 create_node(PrimitiveFormMarker("syntax-rules")));

  ASSERT_EQ(PrimitiveFormMarker("define"),
            env->get_value(Symbol("define"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("set!"),
            env->get_value(Symbol("set!"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("lambda"),
            env->get_value(Symbol("lambda"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("quote"),
            env->get_value(Symbol("quote"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("define-syntax"),
            env->get_value(Symbol("define-syntax"))
                ->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("let-syntax"),
            env->get_value(Symbol("let-syntax"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("syntax-rules"),
            env->get_value(Symbol("syntax-rules"))->get<PrimitiveFormMarker>());

  // Given: an environment to hold bindings
  MacroContext context(hvm);

  // Parse a string
  std::string expr = "(define x \'hello)";
  parser::ParserInput input(expr);
  auto result = parser::parse_datum(input);

  std::cout << *result.it << std::endl;
  run_macro_expansion(result.it, context);
  std::cout << *result.it << std::endl;
}


TEST(SyntaxRulesMacroUnitTest, parse_syntax_rules) {

  // HVM required for MacroContext
  HeapVirtualMachine hvm(
      create_node(String("Hello world")),
      core::list(create_node(Symbol("halt"))),
      std::make_shared<Environment>(nullptr),
      ValueRib(),
      nullptr
  );
  EnvPtr env = hvm.get_environment();
  env->set_value(Symbol("define"), create_node(PrimitiveFormMarker("define")));
  env->set_value(Symbol("set!"), create_node(PrimitiveFormMarker("set!")));
  env->set_value(Symbol("lambda"), create_node(PrimitiveFormMarker("lambda")));
  env->set_value(Symbol("quote"), create_node(PrimitiveFormMarker("quote")));
  env->set_value(Symbol("define-syntax"),
                 create_node(PrimitiveFormMarker("define-syntax")));
  env->set_value(Symbol("let-syntax"),
                 create_node(PrimitiveFormMarker("let-syntax")));
  env->set_value(Symbol("syntax-rules"),
                 create_node(PrimitiveFormMarker("syntax-rules")));

  ASSERT_EQ(PrimitiveFormMarker("define"),
            env->get_value(Symbol("define"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("set!"),
            env->get_value(Symbol("set!"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("lambda"),
            env->get_value(Symbol("lambda"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("quote"),
            env->get_value(Symbol("quote"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("define-syntax"),
            env->get_value(Symbol("define-syntax"))
                ->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("let-syntax"),
            env->get_value(Symbol("let-syntax"))->get<PrimitiveFormMarker>());
  ASSERT_EQ(PrimitiveFormMarker("syntax-rules"),
            env->get_value(Symbol("syntax-rules"))->get<PrimitiveFormMarker>());

  // Given: an environment to hold bindings
  MacroContext context(hvm);

  // Define simple macro to parse
  std::string simple_macro =
      "(define-syntax one "
      "  (syntax-rules ()"
      "    ((one) ())))";

  parser::ParserInput input(simple_macro);
  auto result = parser::parse_datum(input);

  std::cout << simple_macro << std::endl;
  std::cout << context << std::endl;
  std::cout << *result.it << std::endl;

  run_macro_expansion(result.it, context);

  std::cout << context << std::endl;
  std::cout << *result.it << std::endl;

  // Insert macro, then run expansion on new form
  //std::string macro_use = "(one)";
  //parser::ParserInput macro_use_input(macro_use);
  //auto macro_result = parser::parse_datum(macro_use_input);
  //context.map_macro(Symbol("one"), std::make_shared<SyntaxRulesMacro>());

  //std::cout << macro_use << std::endl;
  //std::cout << context << std::endl;
  //std::cout << *macro_result.it << std::endl;

  //run_macro_expansion(macro_result.it, context);

  //std::cout << context << std::endl;
  //std::cout << *result.it << std::endl;

}
