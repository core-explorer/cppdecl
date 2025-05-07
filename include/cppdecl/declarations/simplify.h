#pragma once

#include "cppdecl/declarations/data.h"
#include "cppdecl/misc/enum_flags.h"

#include <version>

namespace cppdecl
{
    enum class SimplifyTypeNamesFlags
    {
        bit_msvc_remove_ptr32_ptr64 = 1 << 0,

        // Replace `std::__cxx11` with `std`.
        bit_libstdcxx_remove_cxx11_namespace_in_std = 1 << 1,
        // Replace `std::__1` with `std`.
        bit_libcpp_remove_1_namespace_in_std = 1 << 2,

        // Presents for different compilers:

        // MSVC and Clang in MSVC-compatible mode.
        compiler_msvc_like = bit_msvc_remove_ptr32_ptr64,

        compiler_all = compiler_msvc_like,
        compiler_current = 0
            #ifdef _MSC_VER
            | compiler_msvc_like
            #endif
            ,

        // Presets for different standard libraries:
        stdlib_libstdcxx = bit_libstdcxx_remove_cxx11_namespace_in_std,
        stdlib_libcpp = bit_libcpp_remove_1_namespace_in_std,
        stdlib_msvc_stl = 0,

        stdlib_all = stdlib_libstdcxx | stdlib_libcpp | stdlib_msvc_stl,
        stdlib_current = 0
            #ifdef _GLIBCXX_RELEASE
            | stdlib_libstdcxx
            #endif
            #ifdef _LIBCPP_VERSION
            | stdlib_libstdcxx
            #endif
            #ifdef _MSVC_STL_VERSION
            | stdlib_msvc_stl
            #endif
            ,


        // High-level flags:
        platform_current = compiler_current | stdlib_current,
        platform_all = compiler_all | stdlib_all,
    };
    CPPDECL_FLAG_OPERATORS(SimplifyTypeNamesFlags)

    // Simplify a name with the assumption that it's a type name.
    // This is a low-level function, prefer `SimplifyTypeNames()`.
    constexpr void SimplifyTypeQualifiedName(SimplifyTypeNamesFlags flags, cppdecl::QualifiedName &name)
    {
        // Remove the version namespace from std.
        if (bool(flags & (SimplifyTypeNamesFlags::bit_libstdcxx_remove_cxx11_namespace_in_std | SimplifyTypeNamesFlags::bit_libcpp_remove_1_namespace_in_std)))
        {
            // The first part of `name` is `std`, and there are at least two parts.
            bool is_in_std = name.parts.size() >= 2 && name.parts.front().AsSingleWord() == "std";

            bool removed_std_version_namespace = false;
            if (!removed_std_version_namespace && bool(flags & SimplifyTypeNamesFlags::bit_libstdcxx_remove_cxx11_namespace_in_std))
            {
                if (is_in_std && name.parts.at(1).AsSingleWord() == "__cxx11")
                {
                    removed_std_version_namespace = true;
                    name.parts.erase(name.parts.begin() + 1);
                }
            }
            if (!removed_std_version_namespace && bool(flags & SimplifyTypeNamesFlags::bit_libcpp_remove_1_namespace_in_std))
            {
                if (is_in_std && name.parts.at(1).AsSingleWord() == "__1")
                {
                    removed_std_version_namespace = true;
                    name.parts.erase(name.parts.begin() + 1);
                }
            }
        }
    }

    // Simplify cv-qualifiers according to the flags.
    // This is a low-level function, prefer `SimplifyTypeNames()`.
    constexpr void SimplifyTypeCvQualifiers(SimplifyTypeNamesFlags flags, cppdecl::CvQualifiers &quals)
    {
        if (bool(flags & SimplifyTypeNamesFlags::bit_msvc_remove_ptr32_ptr64))
            quals &= ~(CvQualifiers::msvc_ptr32 | CvQualifiers::msvc_ptr64);
    }

    // `target` is typically a `Type` or `Decl`.
    constexpr void SimplifyTypeNames(SimplifyTypeNamesFlags flags, auto &target)
    {
        if (bool(flags))
        {
            target.template VisitEachComponent<QualifiedName>({}, [flags](QualifiedName &name){SimplifyTypeQualifiedName(flags, name);});
            target.template VisitEachComponent<CvQualifiers>({}, [flags](CvQualifiers &quals){SimplifyTypeCvQualifiers(flags, quals);});
        }
    }
}
