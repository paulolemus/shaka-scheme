#include <gmock/gmock.h>

#include "shaka_scheme/system/parser/parser_definitions.hpp"
#include "shaka_scheme/system/vm/HeapVirtualMachine.hpp"
#include "shaka_scheme/system/parser/syntax_rules/macro_engine.hpp"
#include "shaka_scheme/system/parser/syntax_rules/MacroContext.hpp"
#include "shaka_scheme/system/parser/syntax_rules/SyntaxCase.hpp"

using namespace shaka::core;
using namespace shaka::macro;
using namespace shaka;


/**
 * @brief Can I create a SyntaxCase instance that is property initialized?
 */
TEST(SyntaxCaseUnitTest, constructor) {
  const auto& c = create_node;

  // Testing macro:
  // (define-syntax test () (syntax-rules () [(test) (quote hello)]))
  Symbol macro_keyword("test");
  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("test")));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("hello")));
  std::size_t scope = 4;

  std::cout << macro_keyword << std::endl;
  std::cout << ellipsis << std::endl;
  for(auto& id : literal_ids) {
    std::cout << id << std::endl;
  }
  std::cout << *pattern << std::endl;
  std::cout << *templat << std::endl;

  SyntaxCase syntax_case(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );

  std::cout << syntax_case << std::endl;

  ASSERT_EQ(macro_keyword, syntax_case.macro_keyword);
  ASSERT_EQ(ellipsis, syntax_case.ellipsis);
  ASSERT_EQ(literal_ids, syntax_case.literal_ids);
  ASSERT_EQ(pattern, syntax_case.pattern);
  ASSERT_EQ(templat, syntax_case.templat);
}

/**
 * @brief Can I construct a SyntaxCase with a slightly more complex state?
 */
TEST(SyntaxCaseUnitTest, construction_with_complexion) {
  const auto& c = create_node;

  // Testing macro:
  // (define-syntax my-if
  //   (syntax-rules (then else)
  //     [(my-if true then x else y) (if true x y)]))
  Symbol macro_keyword("my-if");
  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  literal_ids.insert(Symbol("then"));
  literal_ids.insert(Symbol("else"));
  NodePtr pattern = list(
      c(Symbol("my-if")),
      c(Symbol("true")),
      c(Symbol("then")),
      c(Symbol("x")),
      c(Symbol("else")),
      c(Symbol("y"))
  );
  NodePtr templat = list(
      c(Symbol("if")),
      c(Symbol("true")),
      c(Symbol("x")),
      c(Symbol("y"))
  );
  std::size_t scope = 4;

  std::cout << macro_keyword << std::endl;
  std::cout << ellipsis << std::endl;
  for(auto& id : literal_ids) {
    std::cout << id << std::endl;
  }
  std::cout << *pattern << std::endl;
  std::cout << *templat << std::endl;

  SyntaxCase syntax_case(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );

  std::cout << syntax_case << std::endl;

  ASSERT_EQ(macro_keyword, syntax_case.macro_keyword);
  ASSERT_EQ(ellipsis, syntax_case.ellipsis);
  ASSERT_EQ(literal_ids, syntax_case.literal_ids);
  ASSERT_EQ(pattern, syntax_case.pattern);
  ASSERT_EQ(templat, syntax_case.templat);
}


/**
 * @brief Can I generate an internal matcher function given a valid initial
 * state?
 */
TEST(SyntaxCaseUnitTest, generate_simple) {
  const auto& c = create_node;

  // Testing macro:
  // (define-syntax test () (syntax-rules () [(test) (quote hello)]))
  Symbol macro_keyword("test");
  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("test")));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("hello")));
  std::size_t scope = 4;

  SyntaxCase syntax_case(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );

  std::cout << syntax_case << std::endl;

  ASSERT_EQ(macro_keyword, syntax_case.macro_keyword);
  ASSERT_EQ(ellipsis, syntax_case.ellipsis);
  ASSERT_EQ(literal_ids, syntax_case.literal_ids);
  ASSERT_EQ(pattern, syntax_case.pattern);
  ASSERT_EQ(templat, syntax_case.templat);
  ASSERT_NO_THROW(syntax_case.generate());
}


/**
 * @brief Given an invalid state, does matcher generate throw an exception?
 */
TEST(SyntaxCaseUnitTest, generate_exception) {
  const auto& c = create_node;

  // Testing macro:
  // (define-syntax test () (syntax-rules () [test (quote hello)]))
  Symbol macro_keyword("test");
  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  NodePtr pattern = c(Symbol("test"));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("hello")));
  std::size_t scope = 4;

  SyntaxCase syntax_case(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );
  std::cout << syntax_case << std::endl;

  ASSERT_EQ(macro_keyword, syntax_case.macro_keyword);
  ASSERT_EQ(ellipsis, syntax_case.ellipsis);
  ASSERT_EQ(literal_ids, syntax_case.literal_ids);
  ASSERT_EQ(pattern, syntax_case.pattern);
  ASSERT_EQ(templat, syntax_case.templat);
  ASSERT_THROW(syntax_case.generate(), MacroExpansionException);
}


/**
 * @brief After generation, can I match to a simple macro use?
 */
TEST(SyntaxCaseUnitTest, match_simples) {
  const auto& c = create_node;

  // Testing macro:
  // (define-syntax test () (syntax-rules () [(test a) (quote a)]))
  Symbol macro_keyword("test");
  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("test")), c(Symbol("a")));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("a")));
  std::size_t scope = 3;

  SyntaxCase syntax_case(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );
  std::cout << syntax_case << std::endl;

  ASSERT_EQ(macro_keyword, syntax_case.macro_keyword);
  ASSERT_EQ(ellipsis, syntax_case.ellipsis);
  ASSERT_EQ(literal_ids, syntax_case.literal_ids);
  ASSERT_EQ(pattern, syntax_case.pattern);
  ASSERT_EQ(templat, syntax_case.templat);
  try {
    syntax_case.generate();
  } catch(const std::exception& e) {
    std::cout << e.what() << std::endl;
    FAIL();
  }

  NodePtr valid_expr1 = list(
      c(Symbol("test")),
      c(Symbol("b"))
  );
  NodePtr valid_expr2 = list(
      c(Symbol("test")),
      list(
          c(Symbol("quote")),
          c(Symbol("hello"))
      )
  );

  ASSERT_TRUE(syntax_case.match(valid_expr1));
  ASSERT_TRUE(syntax_case.match(valid_expr2));
}


/**
 * @brief After generation, can I fail to match to a simple macro use?
 */
TEST(SyntaxCaseUnitTest, match_simples_fail) {
  const auto& c = create_node;

  // Testing macro:
  // (define-syntax test () (syntax-rules () [(test a) (quote a)]))
  Symbol macro_keyword("test");
  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("test")), c(Symbol("a")));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("a")));
  std::size_t scope = 3;

  SyntaxCase syntax_case(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );
  std::cout << syntax_case << std::endl;

  ASSERT_EQ(macro_keyword, syntax_case.macro_keyword);
  ASSERT_EQ(ellipsis, syntax_case.ellipsis);
  ASSERT_EQ(literal_ids, syntax_case.literal_ids);
  ASSERT_EQ(pattern, syntax_case.pattern);
  ASSERT_EQ(templat, syntax_case.templat);
  ASSERT_NO_THROW(syntax_case.generate());

  NodePtr valid_expr1 = list(c(Symbol("test")));
  NodePtr valid_expr2 = list(
      c(Symbol("test")),
      c(Symbol("hi")),
      c(Symbol("there"))
  );

  std::cout << *valid_expr1 << std::endl;
  std::cout << *valid_expr2 << std::endl;

  ASSERT_FALSE(syntax_case.match(valid_expr1));
  ASSERT_FALSE(syntax_case.match(valid_expr2));
}


/**
 * @brief After generation, can I match a use with ellipsis?
 */
TEST(SyntaxCaseUnitTest, match_with_ellipsis) {
  const auto& c = create_node;

  // Testing macro:
  // (define-syntax test () (syntax-rules () [(test a ...) (quote (a ...))]))
  Symbol macro_keyword("test");
  Symbol ellipsis("...");
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(
      c(Symbol("test")),
      c(Symbol("a")),
      c(Symbol("..."))
  );
  NodePtr templat = list(
      c(Symbol("quote")),
      list(
          c(Symbol("a")),
          c(Symbol("..."))
      )
  );
  std::size_t scope = 3;

  SyntaxCase syntax_case(
      macro_keyword,
      ellipsis,
      literal_ids,
      pattern,
      templat,
      scope
  );
  std::cout << syntax_case << std::endl;

  ASSERT_EQ(macro_keyword, syntax_case.macro_keyword);
  ASSERT_EQ(ellipsis, syntax_case.ellipsis);
  ASSERT_EQ(literal_ids, syntax_case.literal_ids);
  ASSERT_EQ(pattern, syntax_case.pattern);
  ASSERT_EQ(templat, syntax_case.templat);
  ASSERT_NO_THROW(syntax_case.generate());

  NodePtr valid_expr1 = list(
      c(Symbol("test")),
      c(Symbol("hi"))
  );
  NodePtr valid_expr2 = list(
      c(Symbol("test")),
      c(Symbol("hello")),
      c(Symbol("goodbye"))
  );
  NodePtr valid_expr3 = list(
      c(Symbol("test")),
      c(Symbol("hello")),
      c(Symbol("goodbye")),
      c(Symbol("wazzup"))
  );

  std::cout << *valid_expr1 << std::endl;
  std::cout << *valid_expr2 << std::endl;
  std::cout << *valid_expr3 << std::endl;

  ASSERT_TRUE(syntax_case.match(valid_expr1));
  ASSERT_TRUE(syntax_case.match(valid_expr2));
  ASSERT_TRUE(syntax_case.match(valid_expr3));
}
