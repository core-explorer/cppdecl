# cppdecl

All-in-one library to parse and manipulate C/C++ type names and declarations.



Some usecases:

* **Getting type names as strings**, either at compile-time or at runtime (from `typeid`), optionally simplified (e.g. `"std::__cxx11::basic_string<char, ...>"` → `"std::string"`).

  Formatting knobs are included (optional east const, `T *x` vs `T* x`, etc).

* **Explaining type names**, like [cdecl.org](https://cdecl.org/) but more capable. (For one, cppdecl is not limited to built-in type names, and [can handle ambiguities](TODO_faq) that can arise from not knowing what is or isn't a type.)

* Constructing type names / declarations (such as for code generation), or manipulating the strings you got from elsewhere (such as from libclang, without using the libclang machinery itself).

  This includes adding/removing/checking cv-qualifiers, type components (`*`/`&`/`[N]`/etc), and so on.

Examples | Installation | [FAQ](TODO_faq)

## Some examples


### Getting type names

```cpp
#include <cppdecl/type_name.h>

int main()
{
    std::cout << cppdecl::TypeName<std::string>() << '\n'; // std::string

    // Works at compile-time too:
    static_assert(cppdecl::TypeName<std::string>() == "std::string");

    // And at runtime with `std::type_index`:
    std::cout << cppdecl::TypeNameDynamic(typeid(std::string)) << '\n'; // std::string
}
```
Compare this with more naive approaches:

* `typeid(std::string).name()` produces mangled names: `"NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE"`, or on MSVC just ugly names: `"class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >"`.

* `std::source_location::current().function_name()` varies across compilers, and produces something like `"std::basic_string<char>"` at best, and the same `"class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >"` at worst. (Same happens to `__PRETTY_FUNCTION__`/`__FUNCSIG__`-based approaches.)

Cppdecl uses the same two methods of obtaining the type names internally, but additionally simplifies the result to produce (in this case) `"std::string"` on all platforms.

### Explaining type names / declarations

Cppdecl can convert types to human-readable strings, for example:

`char (*(*x())[5])()` → `` `x`, a function taking no parameters, returning a pointer to an array of size [integer 5] of a pointer to a function taking no parameters, returning `char` ``.

A small command-line utility is included for this purpose (called `cppdecl`, needs to be built from source). Or you can do this programmatically:

```cpp
#include <cppdecl/declarations/parse_simple.h>
#include <cppdecl/declarations/to_string.h>

#include <iostream>

int main()
{
    auto type = cppdecl::ParseDecl_Simple("char (*(*x())[5])()");
    std::cout << cppdecl::ToString(type, {}) << '\n';
}
```

Cppdecl can handle arbitrary names without knowing if they are types or something else. Sometimes this causes ambiguities, which we detect, and guess the intended interpretation:

`x(y)` → `` ambiguous, either [unnamed function taking 1 parameter: [unnamed of type `y`], returning `x`] or [`y` of type `x`] or [`x`, a constructor taking 1 parameter: [unnamed of type `y`]]` ``

The alternatives are sorted to put what we think is the intended interpretation first.

### Manipulating types

```cpp
#include <cppdecl/declarations/data.h>
#include <cppdecl/declarations/to_string.h>
#include <cppdecl/declarations/parse_simple.h>

#include <iostream>

int main()
{
    auto decl = cppdecl::ParseDecl_Simple("const char*x");
    std::cout << cppdecl::ToCode(decl, {}) << '\n'; // `const char *x`, notice the automatic formatting.
    std::cout << cppdecl::ToCode(decl, cppdecl::ToCodeFlags::east_const) << '\n'; // `char const *x`
    std::cout << cppdecl::ToCode(decl, cppdecl::ToCodeFlags::left_align_pointer) << '\n'; // `const char* x`

    decl.type.AddQualifiers(cppdecl::CvQualifiers::const_);
    std::cout << cppdecl::ToCode(decl, {}) << '\n'; // `const char *const x`

    decl.type.RemoveQualifiers(cppdecl::CvQualifiers::const_, 1); // Remove constness from the pointee.
    std::cout << cppdecl::ToCode(decl, {}) << '\n'; // `char *const x`

    decl.type.RemoveModifier();
    std::cout << cppdecl::ToCode(decl, {}) << '\n'; // `char x`

    decl.type.AddModifier(cppdecl::Reference{});
    std::cout << cppdecl::ToCode(decl, {}) << '\n'; // `char &x`
}
```

### Simplifying types

As mentioned above, cppdecl includes a lot of type simplification rules. Most of them are applied automatically when calling `cppdecl::TypeName()`, but it can be useful to run them manually:

```cpp
#include <cppdecl/declarations/data.h>
#include <cppdecl/declarations/parse_simple.h>
#include <cppdecl/declarations/simplify.h>
#include <cppdecl/declarations/to_string.h>

#include <iostream>

int main()
{
    auto type = cppdecl::ParseType_Simple("class std::vector<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> >,class std::allocator<class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > >");

    // This prints the above string mostly unmodified.
    std::cout << cppdecl::ToCode(type, {}) << '\n';

    cppdecl::Simplify(cppdecl::SimplifyFlags::all, type);

    // This prints `std::vector<std::string>`.
    std::cout << cppdecl::ToCode(type, {}) << '\n';
}
```

By default, `Simplify()` only handles standard types, but it is modular and support for third-party types can be added as well.

To enable optional support for types from some third-party libraries, pass `cppdecl::FullSimplifyTraits{}` as the third argument to `Simplify()` (`TypeName()` accepts that as well). Currently, the only library we have simplification rules for is [phmap](https://github.com/greg7mdp/parallel-hashmap).

## Installation

Cppdecl is header-only, and should work on more-or-less recent versions of Clang, GCC, and MSVC.

There are some examples and tests that can be compiled with Meson:
```sh
meson setup build
meson compile -C build
```
Amont other things, those include `cppdecl` (an utility explains type names in plain English).

## How to get reasonable compile times

Cppdecl is header-only, which allows it to be `constexpr` on recent compilers.

But this comes with downsides:

* Headers are large and can slow down compilation.

* `constexpr` computations are slow.

In particular, the default behavior of `cppdecl::TypeName<T>()` is do everything at compile-time if possible, so that only the final string appears in the binary. This is great for release builds, but takes about 200ms of compilation time per type.

This can be remedied by using `cppdecl::TypeName<T, cppdecl::TypeNameFlags::no_constexpr>()`. This causes an ugly type name to be saved at compile-time, and then simplified at runtime. Typically you only want this flag in debug builds, so you might want to wrap `TypeName()` in your own function that applies the flag only in debug builds.

Or even better, in debug builds avoid including cppdecl in your headers at all, hide it in a .cpp file, and create a wrapper for `cppdecl::TypeNameDynamic(std::type_index)`, so the rest of your program can get type names without including cppdecl.

If you still want to include cppdecl in your headers, I recommend putting it into a PCH.

## FAQ

* **What you can or can't parse?**

  * We can parse type names (that use any C/C++ features I could think of).

    We can also parse declarations of single entities, i.e. a type plus a name (which includes names in the middle of types, as in `int *a[42]`).

  * We can't parse following, either because I'm not particularly interested in those or haven't bothered yet:

    * Declarations of more than one entity at a time. Cppdecl is primarily intended to parse *types*, supporting declarations at all is a courtesy.

    * Initializers in declarations. Those will be reported as unparsed junk at the end of input. I probably could handle them as token soup.

    * Most specifiers in declarations, like `mutable`/`virtual`/`etc`. Again, because I'm primarily interested in parsing *types*.

    * `template <...>` in declarations. Same goes for `requires`-clauses.

    * Anything involving `...` pack expansions. Again, because cppdecl is primarily intended to parse concrete types as reported by the compiler/libclang.

    * See [known issues](/known_issues.md) for more.



* **How do you distinguish type vs non-type template argument?**

  * We don't. Everything that successfully parses as a type gets treated as a type, and everything else is parsed as a token soup (called "pseudo-expression" in cppdecl, which includes some minimal conveniences, like understanding qualified names, or grouping the contents of braces/parentheses/etc).

* **How do you parse `A<x < y>`?**

  * Currently we don't. `<` is always assumed to begin a template argument. Currently even `A<(x < y)>` doesn't get parsed.

    I don't see it as a big problem, because this library is intended primarily to deal with the types produced by a compiler (or libclang), where template arguments will already be evaluated.

* **How do I construct a type object from scratch?**

  * ```cpp
    // `int` (`void` and all other single-word types go here, `long long` also goes here)
    cppdecl::Type a = cppdecl::Type::FromSingleWord("int"); // If no `::` nor other punctuation.
    // `std::int32_t` (anything with `::` goes here)
    cppdecl::Type b = cppdecl::Type::FromQualifiedName(cppdecl::QualifiedName{}.AddPart("std").AddPart("int32_t"));
    // `std::vector<int>` (here's how you handle template arguments)
    cppdecl::Type c = cppdecl::Type::FromQualifiedName(cppdecl::QualifiedName{}.AddPart("std").AddPart("vector").AddTemplateArgument(cppdecl::Type::FromSingleWord("int")));
    // `int *`
    cppdecl::Type d = cppdecl::Type::FromSingleWord("int").AddModifier(cppdecl::Pointer{});
    ```

* **Can you use cppdecl to post-process compiler diagnostics?** Would be nice to simplify types in compiler output, right?

  * Turns out this doesn't really work. If you [compile](#installation) the examples, there's one called `cppdecl_prettify_errors`. You can pipe compiler output through it, and it'll attempt to prettify anything that looks like a type.

    But turns out that this doesn't really work, because compilers already perform some partial simplification of their own, which confuses cppdecl. The only compiler that doesn't do this seems to be MSVC, which is ironically helpful in our case.
