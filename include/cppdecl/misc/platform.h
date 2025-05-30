#pragma once

#include <version>

// We use `CPPDECL_CONSTEXPR` on most of our constexpr API, to make it non-constexpr if the compiler can't handle that.
//   This is of course only necessary in C++20, since in C++23 `constexpr` is silently ignored on failure anyway.
//   Some simple functions don't use this and are instead unconditionally `constexpr`.
//
// Test reports:
//   GCC:
//     C++20 - not constexpr up to GCC 15 (for unclear reasons), newer versions not tested.
//     C++23 - constexpr in GCC 13 and newer.
//   Clang with libstdc++:
//     C++20/23 - constexpr only on libstdc++ 14 and newer; tested on Clang 19, older versions weren't checked.
//   Clang with libc++:
//     C++20/23 - not constexpr on Clang 18; constexpr on Clang 19
//   MSVC:
//     Doesn't compile our library at the time of testing.
//
// We make a few guesses based on those, and disable constexpr when any of:
#ifndef CPPDECL_IS_CONSTEXPR
#if \
    /* GCC older than 13, or any GCC pre-C++23 */\
    (defined(__GNUC__) && !defined(__clang__) && (__GNUC__ < 13 || __cplusplus <= 202002)) || \
    /* Clang older than 19 */\
    (defined(__clang__) && __clang_major__ < 19) || \
    /* Clang with libstdc++, libstdc++ older than 14 */\
    (defined(__clang__) && defined(_GLIBCXX_RELEASE) && _GLIBCXX_RELEASE < 14)
#define CPPDECL_IS_CONSTEXPR 0
#else
#define CPPDECL_IS_CONSTEXPR 1
#endif
#endif

#if CPPDECL_IS_CONSTEXPR
#define CPPDECL_CONSTEXPR constexpr
#else
#define CPPDECL_CONSTEXPR inline
#endif


// If this is false, you can assume that `cppdecl::Demangler` will always return the input string as is.
#ifndef CPPDECL_NEED_DEMANGLER
#  ifdef _MSC_VER
#    define CPPDECL_NEED_DEMANGLER 0
#  else
#    define CPPDECL_NEED_DEMANGLER 1
#  endif
#endif
