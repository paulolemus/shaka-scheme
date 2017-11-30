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

TEST(SyntaxRulesMacroUnitTest, constructor) {
  Symbol macro_keyword("my-if");
  std::vector<SyntaxCasePtr > cases;

  SyntaxRulesMacro macro(macro_keyword, cases);
  ASSERT_EQ(macro.macro_keyword, macro_keyword);
  ASSERT_EQ(macro.syntax_cases.size(), 0);

  MacroPtr macro_ptr = std::make_shared<SyntaxRulesMacro>(macro_keyword, cases);
  ASSERT_EQ(macro_ptr->macro_keyword, macro_keyword);
  ASSERT_EQ(macro_ptr->syntax_cases.size(), 0);

  std::cout << macro << std::endl;
}

TEST(SyntaxRulesMacroUnitTest, match_simple) {
  const auto& c = create_node;

  // Build macro
  // (define-syntax test
  //   (syntax-rules ()
  //     [(test) (quote hello)]))
  Symbol macro_keyword("test");
  std::vector<SyntaxCasePtr> cases;

  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("test")));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("hello")));
  std::size_t scope = 3;

  SyntaxCasePtr syntax_case = std::make_shared<SyntaxCase>(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );
  syntax_case->generate();
  cases.push_back(syntax_case);

  SyntaxRulesMacro macro(macro_keyword, cases);
  std::cout << macro << std::endl;

  NodePtr expr1 = list(c(Symbol("test")));
  NodePtr expr2 = list(c(Symbol("test")));

  ASSERT_NO_THROW(syntax_case->expand(expr1));
  ASSERT_TRUE(macro.expand(expr2));
}

TEST(SyntaxRulesMacroUnitTest, match_multiple_cases) {
  const auto& c = create_node;

  // Build macro
  // (define-syntax test
  //   (syntax-rules ()
  //     [(test) (quote hello)]
  //     [(test a ...) (quote (a ...))]))
  Symbol macro_keyword("test");
  std::vector<SyntaxCasePtr> cases;

  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("test")));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("hello")));
  std::size_t scope = 3;

  SyntaxCasePtr syntax_case = std::make_shared<SyntaxCase>(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );
  syntax_case->generate();
  cases.push_back(syntax_case);

  NodePtr pattern2 = list(c(Symbol("test")), c(Symbol("a")), c(Symbol("...")));
  NodePtr templat2 = list(
      c(Symbol("quote")),
      list(
          c(Symbol("a'")),
          c(Symbol("..."))
      )
  );
  std::size_t scope2 = 4;
  SyntaxCasePtr syntax_case2 = std::make_shared<SyntaxCase>(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern2,
      templat2,
      scope2
  );
  syntax_case2->generate();
  cases.push_back(syntax_case2);

  SyntaxRulesMacro macro(macro_keyword, cases);
  std::cout << macro << std::endl;

  NodePtr expr1 = list(c(Symbol("test")));
  NodePtr expr2 = list(
      c(Symbol("test")),
      list(
          c(Symbol("quote")),
          c(Symbol("one-hundred"))
      ),
      c(Symbol("the-end"))
  );


  std::cout << "BEFORE: " << *expr1 << std::endl;
  ASSERT_TRUE(macro.expand(expr1));
  std::cout << "AFTER: " << *expr1 << std::endl;

  std::cout << "BEFORE: " << *expr2 << std::endl;
  ASSERT_TRUE(macro.expand(expr2));
  std::cout << "AFTER: " << *expr2 << std::endl;
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

  MacroPtr retrieved_macro = nullptr;
  for(auto it = context.get_bindings(Symbol("one"));
      it != context.identifier_bindings.end();
      ++it) {

    if(it->second.macro) {
      retrieved_macro = it->second.macro;
      break;
    }
  }

  if(retrieved_macro) {
    std::cout << "RETRIEVED MACRO" << std::endl;
    std::cout << *retrieved_macro << std::endl;

  } else {
    FAIL();
  }
}
