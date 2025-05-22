#pragma once

#include <version>

// We use `CPPDECL_CONSTEXPR` on most of our constexpr API, to make it non-constexpr if the compiler can't handle that.
// This is of course only necessary in C++20, since in C++23 `constexpr` is silently ignored on failure anyway.
// Some simple functions don't use this and are instead unconditionally `constexpr`.
#if defined(_GLIBCXX_DEBUG) && _GLIBCXX_RELEASE <= 13
// In libstdc++ 12 and 13, the debug vectors don't seem to be constexpr.
// In newer versions they are constexpr regardless of the debug mode.
// In older versions they are not constexpr regardless of the debug mode, but we don't support libstdc++ this old for now.
#define CPPDECL_CONSTEXPR inline
#define CPPDECL_IS_CONSTEXPR false
#else
#define CPPDECL_CONSTEXPR constexpr
#define CPPDECL_IS_CONSTEXPR true
#endif
