#include "cppdecl/parsing/parse.h"

#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>

void Fail(std::string_view message)
{
    throw std::runtime_error("Test failed: " + std::string(message));
}

void CheckEq(std::string_view message, std::string_view a, std::string_view b)
{
    if (a != b)
    {
        std::cout << "Equality test failed: " << message << "\nA: `" << a << "`\nB: `" << b << "`\n";
        throw std::runtime_error("Test failed!");
    }
}

std::string ParseDeclToString(std::string_view view, cppdecl::ParseDeclFlags mode, std::size_t expected_junk_suffix_size = 0)
{
    const auto orig_view = view;
    auto ret = cppdecl::ParseDecl(view, mode);
    if (auto error = std::get_if<cppdecl::ParseError>(&ret))
    {
        std::cout << "Parse error at " << (view.data() - orig_view.data()) << ": " << error->message << '\n';
        std::cout << orig_view << '\n';
        std::cout << std::string(std::size_t(view.data() - orig_view.data()), ' ') << "^\n";
        Fail("Parse error.");
        return "";
    }

    if (expected_junk_suffix_size == 0)
    {
        // This branch just provides a better error message.
        if (!view.empty())
        {
            std::cout << "Unparsed junk after input: `" << view << "`\n";
            Fail("Unparsed junk after input.");
        }
    }
    else
    {
        if (view.size() != expected_junk_suffix_size)
        {
            std::cout << "Expected " << expected_junk_suffix_size << " junk characters at the end, but got " << view.size() << ".\n";
            Fail("Wrong amount of unparsed junk after input.");
        }
    }

    return std::get<cppdecl::MaybeAmbiguous<cppdecl::Decl>>(ret).ToDebugString(cppdecl::ToStringMode::debug);
}

void CheckParseSuccess(std::string_view view, cppdecl::ParseDeclFlags mode, std::string_view result)
{
    CheckEq("Wrong result of parsing.", ParseDeclToString(view, mode), result);
}
void CheckParseSuccessWithJunk(std::string_view view, cppdecl::ParseDeclFlags mode, std::size_t expected_junk_suffix_size, std::string_view result)
{
    CheckEq("Wrong result of parsing.", ParseDeclToString(view, mode, expected_junk_suffix_size), result);
}

void CheckParseFail(std::string_view view, cppdecl::ParseDeclFlags mode, std::size_t pos, std::string_view error_message)
{
    const auto orig_view = view;
    auto ret = cppdecl::ParseDecl(view, mode);
    if (auto error = std::get_if<cppdecl::ParseError>(&ret))
    {
        CheckEq("Wrong error message from parsing.", error->message, error_message);
        CheckEq("Wrong error offset", std::to_string(view.data() - orig_view.data()), std::to_string(pos));
        return;
    }

    Fail("Expected this parse to fail, but it parsed successfully to: " + std::get<cppdecl::MaybeAmbiguous<cppdecl::Decl>>(ret).ToDebugString(cppdecl::ToStringMode::debug));
}

int main()
{
    static constexpr auto m_type = cppdecl::ParseDeclFlags::accept_unnamed;
    static constexpr auto m_any = cppdecl::ParseDeclFlags::accept_everything;

    // Basic named variables.
    CheckParseSuccess("int foo",                               m_any, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("  int  foo  ",                          m_any, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("int*foo",                               m_any, R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("  int  *  foo  ",                       m_any, R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccessWithJunk("int*foo",                       m_type, 3, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccessWithJunk("  int  *  foo  ",               m_type, 5, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");

    // Basic templates and pointers.
    CheckParseSuccess("std::basic_string<char>",               m_type, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="std"},{name="basic_string",targs=[type{flags=[],quals=[],name={global_scope=false,parts=[{name="char"}]}}]}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  std  ::  basic_string  <  char  >  ", m_type, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="std"},{name="basic_string",targs=[type{flags=[],quals=[],name={global_scope=false,parts=[{name="char"}]}}]}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int*",                                  m_type, R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  *  ",                            m_type, R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int*const",                             m_type, R"({type="const pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  *  const  ",                     m_type, R"({type="const pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int*const volatile",                    m_type, R"({type="const volatile pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  *  const  volatile  ",           m_type, R"({type="const volatile pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int*__restrict",                        m_type, R"({type="restrict pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  *  __restrict  ",                m_type, R"({type="restrict pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int&",                                  m_type, R"({type="lvalue reference to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  &  ",                            m_type, R"({type="lvalue reference to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int&__restrict",                        m_type, R"({type="restrict lvalue reference to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  &  __restrict  ",                m_type, R"({type="restrict lvalue reference to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseFail("int&const",                                m_type, 4, "References can't be const-qualified.");
    CheckParseFail("  int  &  const  ",                        m_type, 10, "References can't be const-qualified.");

    // Ban pointers to references.
    CheckParseSuccess("int*&",                                 m_type, R"({type="lvalue reference to pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  *  &  ",                         m_type, R"({type="lvalue reference to pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseFail("int&*",                                    m_type, 3, "Reference can only apply at the top level.");
    CheckParseFail("  int  &  *  ",                            m_type, 7, "Reference can only apply at the top level.");

    // Parentheses and pointers.
    CheckParseSuccess("int(*foo)",                             m_any, R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("  int  (  *  foo  )  ",                 m_any, R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("int(*)",                                m_any, R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  (  *  )  ",                      m_any, R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int(*)",                                m_type,  R"({type="pointer to {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");

    // Arrays, arrays of pointers, pointers to arrays.
    CheckParseSuccess("int[]",                                 m_type,  R"({type="array of unknown bound of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  [  ]  ",                         m_type,  R"({type="array of unknown bound of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int[42]",                               m_type,  R"({type="array of size [num`42`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  [  42  ]  ",                     m_type,  R"({type="array of size [num`42`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int foo[42]",                           m_any, R"({type="array of size [num`42`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("  int  foo  [  42  ]  ",                m_any, R"({type="array of size [num`42`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("int (*)[42]",                           m_any, R"({type="pointer to array of size [num`42`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  int  (  *  )  [  42  ]  ",            m_any, R"({type="pointer to array of size [num`42`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int (*foo)[42]",                        m_any, R"({type="pointer to array of size [num`42`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("  int  (  *  foo  )  [  42  ]  ",       m_any, R"({type="pointer to array of size [num`42`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");

    // Multidimensional arrays.
    CheckParseSuccess("int foo[1][2]",                         m_any, R"({type="array of size [num`1`] of array of size [num`2`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");
    CheckParseSuccess("  int  foo  [  1  ]  [  2  ]  ",        m_any, R"({type="array of size [num`1`] of array of size [num`2`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="foo"}]}"})");

    // Combining weird qualifiers.
    CheckParseSuccess("long long",                             m_any, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="long long"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseFail("long long long",                           m_type, 10, "Can't add this keyword to the preceding type.");
    CheckParseFail("long long long",                           m_any, 10, "Can't add this keyword to the preceding type.");
    CheckParseFail("const const",                              m_any, 6, "Repeated `const`.");
    CheckParseFail("volatile volatile",                        m_any, 9, "Repeated `volatile`.");
    // A different error message here, for no particular reason.
    CheckParseFail("void *const const",                        m_any, 12, "Duplicate cv-qualifier.");
    CheckParseFail("void *volatile volatile",                  m_any, 15, "Duplicate cv-qualifier.");
    CheckParseFail("void *__restrict __restrict__",            m_any, 17, "Duplicate cv-qualifier.");
    // This parses as a type name right now, which is weird but whatever.
    CheckParseSuccess("__restrict",                            m_any, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="__restrict"}]}}",name="{global_scope=false,parts=[]}"})");
    // Signedness.
    CheckParseSuccess("unsigned long",                         m_any, R"({type="{flags=[unsigned],quals=[],name={global_scope=false,parts=[{name="long"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("signed long",                           m_any, R"({type="{flags=[explicitly_signed],quals=[],name={global_scope=false,parts=[{name="long"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("long unsigned",                         m_any, R"({type="{flags=[unsigned],quals=[],name={global_scope=false,parts=[{name="long"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("long signed",                           m_any, R"({type="{flags=[explicitly_signed],quals=[],name={global_scope=false,parts=[{name="long"}]}}",name="{global_scope=false,parts=[]}"})");
    // Reject combined signed/unsigned qualifiers.
    CheckParseFail("unsigned signed long",                     m_any, 9, R"(Both `unsigned` and `signed` on the same type.)");
    CheckParseFail("signed unsigned long",                     m_any, 7, R"(Both `signed` and `unsigned` on the same type.)");
    // Reject duplicate qualifiers.
    CheckParseFail("unsigned unsigned long",                   m_any, 9, R"(Repeated `unsigned`.)");
    CheckParseFail("signed signed long",                       m_any, 7, R"(Repeated `signed`.)");
    // Implicit `int` when signedness is specified.
    CheckParseSuccess("unsigned",                              m_any, R"({type="{flags=[unsigned],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("signed",                                m_any, R"({type="{flags=[explicitly_signed],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    // Empty decl-specifier-seq is an error.
    CheckParseFail("",                                         m_any, 0, "Expected a type.");
    // Explicit `int` flag.
    CheckParseSuccess("long int",                              m_any, R"({type="{flags=[redundant_int],quals=[],name={global_scope=false,parts=[{name="long"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int long",                              m_any, R"({type="{flags=[redundant_int],quals=[],name={global_scope=false,parts=[{name="long"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("long long int",                         m_any, R"({type="{flags=[redundant_int],quals=[],name={global_scope=false,parts=[{name="long long"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("long int long",                         m_any, R"({type="{flags=[redundant_int],quals=[],name={global_scope=false,parts=[{name="long long"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int long long",                         m_any, R"({type="{flags=[redundant_int],quals=[],name={global_scope=false,parts=[{name="long long"}]}}",name="{global_scope=false,parts=[]}"})");
    // long double
    CheckParseSuccess("long double",                           m_any, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="long double"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("double long",                           m_any, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="long double"}]}}",name="{global_scope=false,parts=[]}"})");

    // Function types.
    CheckParseSuccess("foo()",                                 m_any, R"({type="function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  )  ",                         m_any, R"({type="function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("foo()const",                            m_any, R"({type="const-qualified function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  )  const  ",                  m_any, R"({type="const-qualified function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("foo()const",                            m_any, R"({type="const-qualified function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("foo()volatile const __restrict noexcept", m_any, R"({type="const-volatile-restrict-qualified noexcept function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  )  volatile  const  __restrict  noexcept  ", m_any, R"({type="const-volatile-restrict-qualified noexcept function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("foo()volatile const __restrict&noexcept", m_any, R"({type="const-volatile-restrict-qualified lvalue-ref-qualified noexcept function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  )  volatile  const  __restrict  &  noexcept  ", m_any, R"({type="const-volatile-restrict-qualified lvalue-ref-qualified noexcept function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("foo()volatile const __restrict&&noexcept", m_any, R"({type="const-volatile-restrict-qualified rvalue-ref-qualified noexcept function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  )  volatile  const  __restrict  &&  noexcept  ", m_any, R"({type="const-volatile-restrict-qualified rvalue-ref-qualified noexcept function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccessWithJunk("  foo  (  )  volatile  const  __restrict  &  &  noexcept  ", m_any, 13, R"({type="const-volatile-restrict-qualified lvalue-ref-qualified function taking no parameters, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");

    CheckParseSuccess("foo(foo x, float(*y)[4])",              m_any, R"({type="function taking 2 parameters: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[{name="x"}]}"}, 2. {type="pointer to array of size [num`4`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="float"}]}}",name="{global_scope=false,parts=[{name="y"}]}"}], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  foo  x  ,  float  (  *  y  )  [  4  ]  )  ", m_any, R"({type="function taking 2 parameters: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[{name="x"}]}"}, 2. {type="pointer to array of size [num`4`] of {flags=[],quals=[],name={global_scope=false,parts=[{name="float"}]}}",name="{global_scope=false,parts=[{name="y"}]}"}], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");

    CheckParseFail("foo(x,)",                                  m_any, 6, "Expected a type.");

    // Parameter names:
    // Unnamed.
    CheckParseSuccess("int(int)",                              m_any, R"({type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"}], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    // Named.
    CheckParseSuccess("int(int x)",                            m_any, R"({type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"}], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    // Named and qualified = error. This isn't exactly the best error message, but it's good enough.
    // We COULD make it better by having a hard error on the qualified name, but should we?
    CheckParseFail("int(int x::y)",                            m_any, 8, "Expected `)` or `,` or `...` in function parameter list.");


    // Member pointers.
    CheckParseFail("A::B::*x",                                 m_any, 0, R"(Missing the pointee type for the member pointer.)");
    CheckParseFail("  A  ::  B  ::  *  x  ",                   m_any, 2, R"(Missing the pointee type for the member pointer.)");
    CheckParseSuccess("int A::B::*x",                          m_any, R"({type="pointer-to-member of class {global_scope=false,parts=[{name="A"},{name="B"}]} of type {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    CheckParseSuccess("  int  A  ::  B  ::  *  x  ",           m_any, R"({type="pointer-to-member of class {global_scope=false,parts=[{name="A"},{name="B"}]} of type {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    CheckParseSuccess("int A::B::*C::D::*x",                   m_any, R"({type="pointer-to-member of class {global_scope=false,parts=[{name="C"},{name="D"}]} of type pointer-to-member of class {global_scope=false,parts=[{name="A"},{name="B"}]} of type {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    CheckParseSuccess("  int  A  ::  B  ::  *  C  ::  D  ::  *  x  ", m_any, R"({type="pointer-to-member of class {global_scope=false,parts=[{name="C"},{name="D"}]} of type pointer-to-member of class {global_scope=false,parts=[{name="A"},{name="B"}]} of type {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    CheckParseSuccess("int A::B::*C::D::*E::F::*x",            m_any, R"({type="pointer-to-member of class {global_scope=false,parts=[{name="E"},{name="F"}]} of type pointer-to-member of class {global_scope=false,parts=[{name="C"},{name="D"}]} of type pointer-to-member of class {global_scope=false,parts=[{name="A"},{name="B"}]} of type {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    CheckParseSuccess("  int  A  ::  B  ::  *  C  ::  D  ::  *  E  ::  F  ::  *  x  ", m_any, R"({type="pointer-to-member of class {global_scope=false,parts=[{name="E"},{name="F"}]} of type pointer-to-member of class {global_scope=false,parts=[{name="C"},{name="D"}]} of type pointer-to-member of class {global_scope=false,parts=[{name="A"},{name="B"}]} of type {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    CheckParseSuccess("int A::B::**x",                         m_any, R"({type="pointer to pointer-to-member of class {global_scope=false,parts=[{name="A"},{name="B"}]} of type {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    CheckParseSuccess("  int  A  ::  B  ::  *  *  x  ",        m_any, R"({type="pointer to pointer-to-member of class {global_scope=false,parts=[{name="A"},{name="B"}]} of type {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");


    // Resolving ambiguities based on the parser flags.
    CheckParseSuccess("int(x)",                                m_any, R"(either [{type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="x"}]}}",name="{global_scope=false,parts=[]}"}], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"}] or [{type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"}])");
    CheckParseSuccess("int(x)",                                cppdecl::ParseDeclFlags::accept_unnamed, R"({type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="x"}]}}",name="{global_scope=false,parts=[]}"}], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("int(x)",                                cppdecl::ParseDeclFlags::accept_unqualified_named, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    CheckParseSuccess("int(x)",                                cppdecl::ParseDeclFlags::accept_all_named        , R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}",name="{global_scope=false,parts=[{name="x"}]}"})");
    // Triple ambiguity (two alternatives on the top level, then another two in one of the function parameters).
    CheckParseSuccess("x(y(z))",                               m_any, R"(either [{type="function taking 1 parameter: [1. either [{type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="z"}]}}",name="{global_scope=false,parts=[]}"}], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="y"}]}}",name="{global_scope=false,parts=[]}"}] or [{type="{flags=[],quals=[],name={global_scope=false,parts=[{name="y"}]}}",name="{global_scope=false,parts=[{name="z"}]}"}]], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="x"}]}}",name="{global_scope=false,parts=[]}"}] or [{type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="z"}]}}",name="{global_scope=false,parts=[]}"}], returning {flags=[],quals=[],name={global_scope=false,parts=[{name="x"}]}}",name="{global_scope=false,parts=[{name="y"}]}"}])");

    // C-style variadics.
    CheckParseSuccess("foo(...)",                              m_any, R"({type="function taking no parameters and a C-style variadic parameter, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  ...  )  ",                    m_any, R"({type="function taking no parameters and a C-style variadic parameter, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("foo(x...)",                             m_any, R"({type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="x"}]}}",name="{global_scope=false,parts=[]}"}] and a C-style variadic parameter (with a missing comma before it), returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  x  ...  )  ",                 m_any, R"({type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="x"}]}}",name="{global_scope=false,parts=[]}"}] and a C-style variadic parameter (with a missing comma before it), returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("foo(x,...)",                            m_any, R"({type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="x"}]}}",name="{global_scope=false,parts=[]}"}] and a C-style variadic parameter, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("  foo  (  x  ,  ...  )  ",              m_any, R"({type="function taking 1 parameter: [1. {type="{flags=[],quals=[],name={global_scope=false,parts=[{name="x"}]}}",name="{global_scope=false,parts=[]}"}] and a C-style variadic parameter, returning {flags=[],quals=[],name={global_scope=false,parts=[{name="foo"}]}}",name="{global_scope=false,parts=[]}"})");
    // Bad variadics.
    CheckParseFail("foo(...x)",                                m_any, 7, "Expected `)` after a C-style variadic parameter.");
    CheckParseFail("foo(x...x)",                               m_any, 8, "Expected `)` after a C-style variadic parameter.");
    CheckParseFail("foo(x,...x)",                              m_any, 9, "Expected `)` after a C-style variadic parameter.");


    // Some clever templates.
    CheckParseSuccess("std::map<std::vector<int>, float>",     m_any, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="std"},{name="map",targs=[type{flags=[],quals=[],name={global_scope=false,parts=[{name="std"},{name="vector",targs=[type{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}]}]}},type{flags=[],quals=[],name={global_scope=false,parts=[{name="float"}]}}]}]}}",name="{global_scope=false,parts=[]}"})");
    CheckParseSuccess("std::map<std::vector<int>, float>::iterator", m_any, R"({type="{flags=[],quals=[],name={global_scope=false,parts=[{name="std"},{name="map",targs=[type{flags=[],quals=[],name={global_scope=false,parts=[{name="std"},{name="vector",targs=[type{flags=[],quals=[],name={global_scope=false,parts=[{name="int"}]}}]}]}},type{flags=[],quals=[],name={global_scope=false,parts=[{name="float"}]}}]},{name="iterator"}]}}",name="{global_scope=false,parts=[]}"})");
}
