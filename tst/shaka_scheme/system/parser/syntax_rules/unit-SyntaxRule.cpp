#include <gmock/gmock.h>

#include "shaka_scheme/system/core/lists.hpp"
#include "shaka_scheme/system/exceptions/MacroExpansionException.hpp"
#include "shaka_scheme/system/parser/syntax_rules/SyntaxRule.hpp"

using namespace shaka;
using namespace shaka::core;
using namespace shaka::macro;

const auto& c = create_node;
Symbol ellipsis("...");
std::set<Symbol> literal_id_none;

TEST(SyntaxRuleUnitTest, gtest) {
  ASSERT_EQ(1, 1);
}


TEST(SyntaxRuleUnitTest, constructor) {

  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("test")));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("hello")));

  SyntaxRule syntax_rule(ellipsis, literal_ids, pattern, templat);

  ASSERT_EQ(ellipsis, syntax_rule.get_ellipsis());
  ASSERT_EQ(literal_ids, syntax_rule.get_literal_ids());
  ASSERT_EQ(pattern, syntax_rule.get_pattern());
  ASSERT_EQ(templat, syntax_rule.get_templat());
}


TEST(SyntaxRuleUnitTest, complex_constructor) {

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

  SyntaxRule syntax_rule(ellipsis, literal_ids, pattern, templat);

  ASSERT_EQ(ellipsis, syntax_rule.get_ellipsis());
  ASSERT_EQ(literal_ids, syntax_rule.get_literal_ids());
  ASSERT_EQ(pattern, syntax_rule.get_pattern());
  ASSERT_EQ(templat, syntax_rule.get_templat());
}


TEST(SyntaxRuleUnitTest, build_simple) {

  // (define-syntax take-none (syntax-rules () ((take-none) (+ 1 2))))
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("take-none")));
  NodePtr templat = list(c(Symbol("+")), c(Number(1)), c(Number(2)));

  SyntaxRule syntax_rule(ellipsis, literal_ids, pattern, templat);

  ASSERT_NO_THROW(syntax_rule.build());
}


TEST(SyntaxRuleUnitTest, build_exception) {

  // (define-syntax test (syntax-rules () [test (quote hello)]))
  std::set<Symbol> literal_ids;
  NodePtr pattern = c(Symbol("test"));
  NodePtr templat = list(c(Symbol("quote")), c(Symbol("hello")));

  SyntaxRule syntax_rule(ellipsis, literal_ids, pattern, templat);

  ASSERT_THROW(syntax_rule.build(), MacroExpansionException);
}


TEST(SyntaxRuleUnitTest, match_simple) {

  // (define-syntax take-none (syntax-rules () ((take-none) (+ 1 2))))
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("take-none")));
  NodePtr templat = list(c(Symbol("+")), c(Number(1)), c(Number(2)));

  SyntaxRule syntax_rule(ellipsis, literal_ids, pattern, templat);

  NodePtr macro1 = list(c(Symbol("take-none")));
  NodePtr macro2 = list(c(Symbol("unused-identifier")));
  NodePtr fail1 = c(Symbol("take-none"));
  NodePtr fail2 = list(c(Symbol("take-none")), c(Symbol("hello")));

  ASSERT_NO_THROW(syntax_rule.build());
  std::cout << "ENDED BUILDING DAWG\n";
  ASSERT_TRUE(syntax_rule.match(macro1));
  ASSERT_TRUE(syntax_rule.match(macro2));
  ASSERT_THROW(syntax_rule.match(fail1), MacroExpansionException);
  ASSERT_FALSE(syntax_rule.match(fail2));
}


TEST(SyntaxRuleUnitTest, match_simple_literal) {

  // (define-syntax take-none (syntax-rules (lit) ((take-none) success)))
  // (define-syntax take-none (syntax-rules (lit) ((take-none lit) success)))
  std::set<Symbol> literal_ids;
  literal_ids.insert(Symbol("lit"));
  NodePtr pattern1 = list(c(Symbol("take-none")));
  NodePtr pattern2 = list(c(Symbol("take-none")), c(Symbol("lit")));
  NodePtr templat = c(Symbol("Success"));

  SyntaxRule syntax_rule1(ellipsis, literal_ids, pattern1, templat);
  SyntaxRule syntax_rule2(ellipsis, literal_ids, pattern2, templat);

  NodePtr macro1 = list(c(Symbol("take-none")));
  NodePtr macro2 = list(c(Symbol("take-none")), c(Symbol("lit")));

  ASSERT_NO_THROW(syntax_rule1.build());
  ASSERT_NO_THROW(syntax_rule2.build());
  ASSERT_TRUE(syntax_rule1.match(macro1));
  ASSERT_TRUE(syntax_rule2.match(macro2));
  ASSERT_FALSE(syntax_rule1.match(macro2));
  ASSERT_FALSE(syntax_rule2.match(macro1));
}


TEST(SyntaxRuleUnitTest, match_take_one) {

  // (define-syntax take-one (syntax-rules () ((take-one 1) item)))
  // (define-syntax take-one (syntax-rules () ((take-one item) item)))
  // (define-syntax take-one (syntax-rules (lit) ((take-one lit item) item)))
  // (define-syntax take-one (syntax-rules () ((take-one _) item)))
  std::set<Symbol> literal_ids;
  literal_ids.insert(Symbol("lit"));
  NodePtr pattern1 = list(c(Symbol("take-one")), c(Number(1)));
  NodePtr pattern2 = list(c(Symbol("take-one")), c(Symbol("item")));
  NodePtr pattern3 = list(
      c(Symbol("take-one")),
      c(Symbol("lit")),
      c(Symbol("item"))
  );
  NodePtr pattern4 = list(c(Symbol("take-one")), c(Symbol("_")));
  NodePtr templat = c(Symbol("item"));

  SyntaxRule syntax_rule1(ellipsis, literal_id_none, pattern1, templat);
  SyntaxRule syntax_rule2(ellipsis, literal_id_none, pattern2, templat);
  SyntaxRule syntax_rule3(ellipsis, literal_ids, pattern3, templat);
  SyntaxRule syntax_rule4(ellipsis, literal_id_none, pattern4, templat);

  NodePtr macro1 = list(c(Symbol("take-one")), c(Number(1)));
  NodePtr macro2 = list(c(Symbol("take-none")), c(Symbol("test")));
  NodePtr macro3 = list(
      c(Symbol("take-none")),
      c(Symbol("lit")),
      c(Symbol("hi"))
  );

  ASSERT_NO_THROW(syntax_rule1.build());
  ASSERT_NO_THROW(syntax_rule2.build());
  ASSERT_NO_THROW(syntax_rule3.build());
  ASSERT_NO_THROW(syntax_rule4.build());

  ASSERT_TRUE(syntax_rule1.match(macro1));
  ASSERT_TRUE(syntax_rule2.match(macro1));
  ASSERT_TRUE(syntax_rule2.match(macro2));
  ASSERT_TRUE(syntax_rule3.match(macro3));
  ASSERT_TRUE(syntax_rule4.match(macro1));
  ASSERT_TRUE(syntax_rule4.match(macro2));

  ASSERT_FALSE(syntax_rule1.match(macro2));
  ASSERT_FALSE(syntax_rule1.match(macro3));
  ASSERT_FALSE(syntax_rule2.match(macro3));
  ASSERT_FALSE(syntax_rule3.match(macro1));
  ASSERT_FALSE(syntax_rule3.match(macro2));
  ASSERT_FALSE(syntax_rule4.match(macro3));
}


TEST(SyntaxRuleUnitTest, match_ellipsis) {

  // (d/s take-multi (syntax-rules () ((take-multi a ...) 1)))
  // (d/s take-multi (syntax-rules () ((take-multi a ... b) 1)))
  // (d/s take-multi (syntax-rules () ((take-multi a b ... c) 1)))
  NodePtr pattern1 = list(
      c(Symbol("take-multi")),
      c(Symbol("a")),
      c(Symbol("..."))
  );
  NodePtr pattern2 = list(
      c(Symbol("take-multi")),
      c(Symbol("a")),
      c(Symbol("...")),
      c(Symbol("b"))
  );
  NodePtr pattern3 = list(
      c(Symbol("take-multi")),
      c(Symbol("a")),
      c(Symbol("b")),
      c(Symbol("...")),
      c(Symbol("c"))
  );
  NodePtr templat = c(Number(1));

  SyntaxRule syntax_rule1(ellipsis, literal_id_none, pattern1, templat);
  SyntaxRule syntax_rule2(ellipsis, literal_id_none, pattern2, templat);
  SyntaxRule syntax_rule3(ellipsis, literal_id_none, pattern3, templat);

  NodePtr macro1 = list(
      c(Symbol("take-multi"))
  );
  NodePtr macro2 = list(
      c(Symbol("take-multi")),
      c(Symbol("first"))
  );
  NodePtr macro3 = list(
      c(Symbol("take-multi")),
      c(Symbol("first")),
      c(Symbol("second"))
  );
  NodePtr macro4 = list(
      c(Symbol("take-multi")),
      c(Symbol("first")),
      c(Symbol("second")),
      c(Symbol("third"))
  );
  NodePtr macro5 = list(
      c(Symbol("take-multi")),
      c(Symbol("first")),
      c(Symbol("second")),
      c(Symbol("third")),
      c(Symbol("fourth"))
  );

  ASSERT_NO_THROW(syntax_rule1.build());
  ASSERT_NO_THROW(syntax_rule2.build());
  ASSERT_NO_THROW(syntax_rule3.build());

  ASSERT_TRUE(syntax_rule1.match(macro1));
  ASSERT_TRUE(syntax_rule1.match(macro2));
  ASSERT_TRUE(syntax_rule1.match(macro3));
  ASSERT_TRUE(syntax_rule1.match(macro4));
  ASSERT_TRUE(syntax_rule1.match(macro5));
  ASSERT_TRUE(syntax_rule2.match(macro2));
  ASSERT_TRUE(syntax_rule2.match(macro3));
  ASSERT_TRUE(syntax_rule2.match(macro4));
  ASSERT_TRUE(syntax_rule2.match(macro5));
  ASSERT_TRUE(syntax_rule3.match(macro3));
  ASSERT_TRUE(syntax_rule3.match(macro4));
  ASSERT_TRUE(syntax_rule3.match(macro5));

  ASSERT_THROW(syntax_rule2.match(macro1), MacroExpansionException);
  ASSERT_FALSE(syntax_rule3.match(macro1));
  ASSERT_THROW(syntax_rule3.match(macro2), MacroExpansionException);
}


TEST(SyntaxRuleUnitTest, transform_simple) {

  // (define-syntax take-none (syntax-rules () ((take-none) (+ 1 2))))
  std::set<Symbol> literal_ids;
  NodePtr pattern = list(c(Symbol("take-none")));
  NodePtr templat = list(c(Symbol("+")), c(Number(1)), c(Number(2)));

  SyntaxRule syntax_rule(ellipsis, literal_ids, pattern, templat);

  NodePtr macro1 = list(c(Symbol("take-none")));
  NodePtr macro2 = list(c(Symbol("unused-identifier")));
  NodePtr fail1 = c(Symbol("take-none"));
  NodePtr fail2 = list(c(Symbol("take-none")), c(Symbol("hello")));

  std::cout << "BEFORE: " << *macro1 << std::endl;
  ASSERT_NO_THROW(syntax_rule.build());
  ASSERT_TRUE(syntax_rule.transform(macro1));
  ASSERT_TRUE(syntax_rule.transform(macro2));
  ASSERT_THROW(syntax_rule.transform(fail1), MacroExpansionException);
  ASSERT_FALSE(syntax_rule.transform(fail2));

  std::cout << "AFTER: " << *macro1 << std::endl;

  ASSERT_EQ(
      car(macro1)->get<Symbol>(),
      car(templat)->get<Symbol>()
  );

  ASSERT_EQ(
      car(cdr(macro1))->get<Number>(),
      car(cdr(templat))->get<Number>()
  );

   ASSERT_EQ(
      car(cdr(cdr(macro1)))->get<Number>(),
      car(cdr(cdr(templat)))->get<Number>()
  );

}

TEST(SyntaxRuleUnitTest, transform_simple_literal) {

  // (define-syntax take-none (syntax-rules (lit) ((take-none) (success))))
  // (define-syntax take-none (syntax-rules (lit) ((take-none lit) (success))))
  std::set<Symbol> literal_ids;
  literal_ids.insert(Symbol("lit"));
  NodePtr pattern1 = list(c(Symbol("take-none")));
  NodePtr pattern2 = list(c(Symbol("take-none")), c(Symbol("lit")));
  NodePtr templat = list(c(Symbol("success")));

  SyntaxRule syntax_rule1(ellipsis, literal_ids, pattern1, templat);
  SyntaxRule syntax_rule2(ellipsis, literal_ids, pattern2, templat);

  NodePtr macro1 = list(c(Symbol("take-none")));
  NodePtr macro2 = list(c(Symbol("take-none")), c(Symbol("lit")));

  ASSERT_NO_THROW(syntax_rule1.build());
  ASSERT_NO_THROW(syntax_rule2.build());
  ASSERT_TRUE(syntax_rule1.transform(macro1));
  ASSERT_TRUE(syntax_rule2.transform(macro2));
  ASSERT_EQ(car(macro1)->get<Symbol>(), car(templat)->get<Symbol>());
  ASSERT_EQ(car(macro2)->get<Symbol>(), car(templat)->get<Symbol>());
}


TEST(SyntaxRuleUnitTest, transform_take_one) {

  // (define-syntax take-one (syntax-rules () ((take-one 1) item)))
  // (define-syntax take-one (syntax-rules () ((take-one item) item)))
  // (define-syntax take-one (syntax-rules (lit) ((take-one lit item) item)))
  // (define-syntax take-one (syntax-rules () ((take-one _) item)))
  std::set<Symbol> literal_ids;
  literal_ids.insert(Symbol("lit"));
  NodePtr pattern1 = list(c(Symbol("take-one")), c(Number(1)));
  NodePtr pattern2 = list(c(Symbol("take-one")), c(Symbol("item")));
  NodePtr pattern3 = list(
      c(Symbol("take-one")),
      c(Symbol("lit")),
      c(Symbol("item"))
  );
  NodePtr pattern4 = list(c(Symbol("take-one")), c(Symbol("_")));
  NodePtr templat = list(c(Symbol("item")));

  SyntaxRule syntax_rule1(ellipsis, literal_ids, pattern1, templat);
  SyntaxRule syntax_rule2(ellipsis, literal_ids, pattern2, templat);
  SyntaxRule syntax_rule3(ellipsis, literal_ids, pattern3, templat);
  SyntaxRule syntax_rule4(ellipsis, literal_ids, pattern4, templat);

  NodePtr macro1 = list(c(Symbol("take-one")), c(Number(1)));
  NodePtr macro2 = list(c(Symbol("take-none")), c(Symbol("test")));
  NodePtr macro3 = list(
      c(Symbol("take-none")),
      c(Symbol("lit")),
      c(Symbol("hi"))
  );
  NodePtr macro4 = list(c(Symbol("take-none")), c(Symbol("test")));

  ASSERT_NO_THROW(syntax_rule1.build());
  ASSERT_NO_THROW(syntax_rule2.build());
  ASSERT_NO_THROW(syntax_rule3.build());
  ASSERT_NO_THROW(syntax_rule4.build());
  std::cout << "BEFORE: " << *macro1 << std::endl;
  std::cout << "BEFORE: " << *macro2 << std::endl;
  std::cout << "BEFORE: " << *macro3 << std::endl;
  std::cout << "BEFORE: " << *macro4 << std::endl;
  ASSERT_TRUE(syntax_rule1.transform(macro1));
  ASSERT_TRUE(syntax_rule2.transform(macro2));
  ASSERT_TRUE(syntax_rule3.transform(macro3));
  ASSERT_TRUE(syntax_rule4.transform(macro4));
  std::cout << "AFTER: " << *macro1 << std::endl;
  std::cout << "AFTER: " << *macro2 << std::endl;
  std::cout << "AFTER: " << *macro3 << std::endl;
  std::cout << "AFTER: " << *macro4 << std::endl;
  // TODO: ADD EXPANSION TO TEMPLATE CHECK
}


TEST(SyntaxRuleUnitTest, transform_ellipsis) {

  // (d/s take-multi (syntax-rules () ((take-multi a ...) 1)))
  // (d/s take-multi (syntax-rules () ((take-multi a ... b) 1)))
  // (d/s take-multi (syntax-rules () ((take-multi a b ... c) 1)))
  NodePtr pattern1 = list(
      c(Symbol("take-multi")),
      c(Symbol("a")),
      c(Symbol("..."))
  );
  NodePtr pattern2 = list(
      c(Symbol("take-multi")),
      c(Symbol("a")),
      c(Symbol("...")),
      c(Symbol("b"))
  );
  NodePtr pattern3 = list(
      c(Symbol("take-multi")),
      c(Symbol("a")),
      c(Symbol("b")),
      c(Symbol("...")),
      c(Symbol("c"))
  );
  NodePtr templat = list(c(Symbol("a")), c(Symbol("...")));

  SyntaxRule syntax_rule1(ellipsis, literal_id_none, pattern1, templat);
  SyntaxRule syntax_rule2(ellipsis, literal_id_none, pattern2, templat);
  SyntaxRule syntax_rule3(ellipsis, literal_id_none, pattern3, templat);

  NodePtr macro1_1 = list(c(Symbol("take-multi")));
  NodePtr macro1_2 = list(c(Symbol("take-multi")), c(Symbol("first")));
  NodePtr macro2_1 = list(c(Symbol("take-multi")), c(Symbol("first")));
  NodePtr macro2_2 = list(
      c(Symbol("take-multi")),
      c(Symbol("first")),
      c(Symbol("second"))
  );
  NodePtr macro3_1 = list(
      c(Symbol("take-multi")),
      c(Symbol("first")),
      c(Symbol("second"))
  );
  NodePtr macro3_2 = list(
      c(Symbol("take-multi")),
      c(Symbol("first")),
      c(Symbol("second")),
      c(Symbol("third"))
  );
  NodePtr macro3_3 = list(
      c(Symbol("take-multi")),
      c(Symbol("first")),
      c(Symbol("second")),
      c(Symbol("third")),
      c(Symbol("fourth"))
  );

  ASSERT_NO_THROW(syntax_rule1.build());
  ASSERT_NO_THROW(syntax_rule2.build());
  ASSERT_NO_THROW(syntax_rule3.build());

  std::cout << "BEFORE: " << *macro1_1 << std::endl;
  std::cout << "BEFORE: " << *macro1_2 << std::endl;
  std::cout << "BEFORE: " << *macro2_1 << std::endl;
  std::cout << "BEFORE: " << *macro2_2 << std::endl;
  std::cout << "BEFORE: " << *macro3_1 << std::endl;
  std::cout << "BEFORE: " << *macro3_2 << std::endl;
  std::cout << "BEFORE: " << *macro3_3 << std::endl;
  ASSERT_TRUE(syntax_rule1.transform(macro1_1));
  ASSERT_TRUE(syntax_rule1.transform(macro1_2));
  ASSERT_TRUE(syntax_rule2.transform(macro2_1));
  ASSERT_TRUE(syntax_rule2.transform(macro2_2));
  ASSERT_TRUE(syntax_rule3.transform(macro3_1));
  ASSERT_TRUE(syntax_rule3.transform(macro3_2));
  ASSERT_TRUE(syntax_rule3.transform(macro3_3));
  std::cout << "AFTER: " << *macro1_1 << std::endl;
  std::cout << "AFTER: " << *macro1_2 << std::endl;
  std::cout << "AFTER: " << *macro2_1 << std::endl;
  std::cout << "AFTER: " << *macro2_2 << std::endl;
  std::cout << "AFTER: " << *macro3_1 << std::endl;
  std::cout << "AFTER: " << *macro3_2 << std::endl;
  std::cout << "AFTER: " << *macro3_3 << std::endl;
  // TODO: ASSERT EXPANSION VS TEMPLATE EQUIVALENCE
}

