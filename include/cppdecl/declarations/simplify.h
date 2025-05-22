#pragma once

#include "cppdecl/declarations/data.h"
#include "cppdecl/misc/enum_flags.h"
#include "cppdecl/misc/overload.h"
#include "cppdecl/misc/platform.h"

#include <cassert>
#include <version>

namespace cppdecl
{
    enum class SimplifyTypeNamesFlags
    {
        // Fixes for compiler/stdlib-specific quirks:
        // Those should do nothing if applied to other platforms.

        // Remove MSVC's `__ptr32` and `__ptr64` cv-qualifiers on pointers.
        bit_msvc_remove_ptr32_ptr64 = 1 << 0,

        // Replace `std::__cxx11` with `std`.
        bit_libstdcxx_remove_cxx11_namespace_in_std = 1 << 1,
        // Replace `std::__1` with `std`.
        bit_libcpp_remove_1_namespace_in_std = 1 << 2,


        // Fixes for common C++ stuff:

        // Remove elaborated type specifiers, `typename`, etc.
        bit_common_remove_type_prefix = 1 << 3,

        // Remove `signed`, except from `signed char`.
        bit_common_remove_redundant_signed = 1 << 4,

        // Remove allocators from template parameters.
        bit_common_remove_defarg_allocator = 1 << 5,
        // Remove `std::char_traits<T>` from template parameters of `std::basic_string`.
        // This typically requires `bit_common_remove_defarg_allocator` as well, since we can't remove non-last template arguments,
        //   and having the allocator after this one will prevent it from being removed.
        bit_common_remove_defarg_char_traits = 1 << 6,
        // Remove `std::less<T>` and `std::equal_to<T>` from ordered and unordered containers respectively.
        // This typically requires `bit_common_remove_defarg_allocator` as well, since we can't remove non-last template arguments,
        //   and having the allocator after this one will prevent it from being removed.
        bit_common_remove_defarg_comparator = 1 << 7,
        // Remove `std::hash<T>` from unordered containers.
        // This typically requires `bit_common_remove_defarg_allocator` and `bit_common_remove_defarg_comparator` as well, since we can't remove non-last template arguments,
        //   and having the allocator and comparator after this one will prevent it from being removed.
        bit_common_remove_defarg_hash_functor = 1 << 8,

        // Remove various default arguments from templates.
        bits_common_remove_defargs = bit_common_remove_defarg_allocator | bit_common_remove_defarg_char_traits | bit_common_remove_defarg_comparator | bit_common_remove_defarg_hash_functor,

        // Rewrite `std::basic_string<char>` to `std::string` and such.
        // This typically requires `bit_common_remove_defarg_hash_functor`, `bit_common_remove_defarg_allocator`, and `bit_common_remove_defarg_comparator` as well,
        //   since this expects the default template arguments to be already stripped.
        bit_common_rewrite_template_specializations_as_typedefs = 1 << 9,

        // Various mostly compiler-independent bits.
        // Note that `bits_common_remove_defargs` isn't needed when you get the types from `__PRETTY_FUNCTION__` or equivalent on Clang.
        common =
            bit_common_remove_type_prefix |
            bit_common_remove_redundant_signed |
            bits_common_remove_defargs |
            bit_common_rewrite_template_specializations_as_typedefs,


        // Fixes for C stuff:

        // Rewrite `_Bool` as `bool`.
        bit_c_normalize_bool = 1 << 3,

        c =
            bit_c_normalize_bool,


        // Presents for different compilers:
        // Those should do nothing if applied to the names from the wrong compiler.

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

        // Absolutely every rule we know. Good if the names come from outside of the program.
        all = common | c | compiler_all | stdlib_all,
        // Only the rules that are relevant on the current compiler and standard library.
        native =
            (common | compiler_current | stdlib_current) // Intentionally excluding `c` here, since this is a C++ library.
            #ifndef _MSC_VER
            // Only MSVC-like compilers print elaborated type specifiers.
            & ~bit_common_remove_type_prefix
            #endif
            ,
        // Only the rules that are relevant on the current compiler and standard library, with the assumption that the type names come from a method based on `__PRETTY_FUNCTION__`, `__FUNCSIG__`, `std::source_location::function_name()`.
        native_func_name_based_only = native
            #ifdef __GNUC__
            // GCC and Clang seem to remove default template arguments automatically.
            & ~bits_common_remove_defargs
            #endif
            ,
    };
    CPPDECL_FLAG_OPERATORS(SimplifyTypeNamesFlags)

    // A CRTP base.
    template <typename Derived>
    struct BasicSimplifyTypeNamesTraits
    {
        // This class should call all member functions of itself though those, to allow customization in derived classes.
        [[nodiscard]]       Derived &GetDerived()       {return static_cast<      Derived &>(*this);}
        [[nodiscard]] const Derived &GetDerived() const {return static_cast<const Derived &>(*this);}

        // Those are only used in this class:

        // If `name` is of the form `std[::{__cxx11,__1}]::A[::...]`, returns A. Otherwise returns an empty string.
        // Only permits the trailing `::...` if `index` isn't null.
        // On success, if `index` isn't null, writes the index of `A` into `*index`. That's typically 1, or 2 if we had to skip the version namespace.
        // On failure intentionally leaves `index` untouched.
        // Handling the version namespaces here is a bit redundant, but it allows us to do stuff independently from removing those namespaces, which is nice.
        [[nodiscard]] CPPDECL_CONSTEXPR std::string_view AsStdName(const cppdecl::QualifiedName &name, std::size_t *index = nullptr)
        {
            if (name.parts.size() < 2)
                return "";
            if (name.parts.front().AsSingleWord() != "std")
                return "";
            std::size_t part_index = 1;
            if (name.parts.at(1).AsSingleWord() == "__cxx11" || name.parts.at(1).AsSingleWord() == "__1")
                part_index++;
            if (index ? name.parts.size() < part_index + 1 : name.parts.size() != part_index + 1)
                return "";
            if (index)
                *index = part_index;

            // Not using `.AsSingleWord()` here to allow template arguments.
            if (auto unqual_name = std::get_if<std::string>(&name.parts.at(part_index).var))
                return *unqual_name;
            else
                return "";
        }
        // Same but for types.
        [[nodiscard]] CPPDECL_CONSTEXPR std::string_view AsStdName(const cppdecl::Type &type, std::size_t *index = nullptr)
        {
            return !type.IsEmpty() && type.IsOnlyQualifiedName() ? GetDerived().AsStdName(type.simple_type.name, index) : "";
        }


        // Those can be overridden:

        // Note that all those must leave `index` untouched if they return false.

        // `A<T, std::char_traits<T>, std::allocator<T>>`
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsStringLike(const cppdecl::QualifiedName &name, std::size_t *index)
        {
            // This has to be a separate variable because we don't want to write to `*index` if the string comparison is false,
            //   but `AsStdName()` has still succeeded.
            std::size_t std_index = 0;
            std::string_view std_name = GetDerived().AsStdName(name, &std_index);
            bool ok =
                std_name == "basic_string";

            if (ok && index)
                *index = std_index;
            return ok;
        }
        // `A<T, std::allocator<T>>`
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsVectorLike(const cppdecl::QualifiedName &name, std::size_t *index)
        {
            std::size_t std_index = 0;
            std::string_view std_name = GetDerived().AsStdName(name, &std_index);
            bool ok =
                std_name == "vector" ||
                std_name == "list" ||
                std_name == "forward_list" ||
                std_name == "deque" ||
                std_name == "hive";

            if (ok && index)
                *index = std_index;
            return ok;
        }
        // `A<T, std::less<T>, std::allocator<T>>`
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsOrderedSetLike(const cppdecl::QualifiedName &name, std::size_t *index)
        {
            std::size_t std_index = 0;
            std::string_view std_name = GetDerived().AsStdName(name, &std_index);
            bool ok =
                std_name == "set" ||
                std_name == "multiset";

            if (ok && index)
                *index = std_index;
            return ok;
        }
        // `A<T, U, std::less<T>, std::allocator<std::pair<const T, U>>>`
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsOrderedMapLike(const cppdecl::QualifiedName &name, std::size_t *index)
        {
            std::size_t std_index = 0;
            std::string_view std_name = GetDerived().AsStdName(name, &std_index);
            bool ok =
                std_name == "map" ||
                std_name == "multimap";

            if (ok && index)
                *index = std_index;
            return ok;
        }
        // `A<T, std::hash<T>, std::equal_to<T>, std::allocator<T>>`
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsUnorderedSetLike(const cppdecl::QualifiedName &name, std::size_t *index)
        {
            std::size_t std_index = 0;
            std::string_view std_name = GetDerived().AsStdName(name, &std_index);
            bool ok =
                std_name == "unordered_set" ||
                std_name == "unordered_multiset";

            if (ok && index)
                *index = std_index;
            return ok;
        }
        // `A<T, U, std::hash<T>, std::equal_to<T>, std::allocator<std::pair<const T, U>>>`
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsUnorderedMapLike(const cppdecl::QualifiedName &name, std::size_t *index)
        {
            std::size_t std_index = 0;
            std::string_view std_name = GetDerived().AsStdName(name, &std_index);
            bool ok =
                std_name == "unordered_map" ||
                std_name == "unordered_multimap";

            if (ok && index)
                *index = std_index;
            return ok;
        }
        // One of the various type that have char traits as the second template argument.
        // I got this list by searching for `preferred_name` (case-independent) in libc++ sources.
        [[nodiscard]] CPPDECL_CONSTEXPR bool HasCharTraits(const cppdecl::QualifiedName &name, std::size_t *index)
        {
            // This has to be a separate variable because we don't want to write to `*index` if the string comparison is false,
            //   but `AsStdName()` has still succeeded.
            std::size_t std_index = 0;
            std::string_view std_name = GetDerived().AsStdName(name, &std_index);
            bool ok =
                // We rely on all those having typedefs without `basic_` for specific character types.
                // This assumption is made in `SpecializationsHaveTypedefsForCharTypes`. If that's not the case for your types, the logic will have to be changed somehow.
                std_name == "basic_string" ||
                std_name == "basic_string_view" ||
                std_name == "basic_ios" ||
                std_name == "basic_filebuf" ||
                std_name == "basic_streambuf" ||
                std_name == "basic_stringbuf" ||
                std_name == "basic_istream" ||
                std_name == "basic_ostream" ||
                std_name == "basic_iostream" ||
                std_name == "basic_stringstream" ||
                std_name == "basic_istringstream" ||
                std_name == "basic_ostringstream" ||
                std_name == "basic_ostringstream" ||
                std_name == "basic_fstream" ||
                std_name == "basic_ifstream" ||
                std_name == "basic_ofstream";

            if (ok && index)
                *index = std_index;
            return ok;
        }
        // Whether this name can be shortened by removing the single template argument of a char type and replacing the `basic_` prefix (or some other?) with the abbreviation of the char type.
        // `resulting_string_base` on success receives the part of the name after `basic_`.
        // `allow_all_char_types` on success receives true if this type understands `char{8,16,32}_t` too, not just `char` and `wchar_t`.
        // The prefix to prepend to `resulting_string_base` is assumed to be always fixed.
        [[nodiscard]] CPPDECL_CONSTEXPR bool SpecializationsHaveTypedefsForCharTypes(const cppdecl::QualifiedName &name, std::size_t *index, std::string_view *resulting_string_base, bool *allow_all_char_types)
        {
            // This has to be a separate variable because we don't want to write to `*index` if the string comparison is false,
            //   but `AsStdName()` has still succeeded.
            std::size_t std_index = 0;

            // Intentionally no `GetDerived().` here.
            if (HasCharTraits(name, &std_index))
            {
                if (index)
                    *index = std_index;

                std::string_view name_word = name.parts.at(std_index).AsSingleWordIgnoringTemplateArgs();
                assert(name_word.starts_with("basic_")); // We assume that the name starts with `basic_`.
                if (resulting_string_base)
                    *resulting_string_base = name_word.substr(6); // Remove `basic_`.
                if (allow_all_char_types)
                    *allow_all_char_types = name_word == "basic_string" || name_word == "basic_string_view";

                return true;
            }

            return false;
        }


        // Is this a character traits type that we should remove from `IsStringLike()` types?
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsCharTraits(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "char_traits";
        }
        // Is this an allocator type that we can remove?
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsAllocator(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "allocator";
        }
        // Is this a pair type that will appear as the allocator parameter in maps?
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsPairInAllocatorParam(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "pair";
        }
        // Is this a less comparator that should be removed from ordered containers?
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsLessComparator(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "less";
        }
        // Is this an equality comparator that should be removed from unordered containers?
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsEqualToComparator(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "equal_to";
        }
        // Is this a hash functor that should be removed from unordered containers?
        [[nodiscard]] CPPDECL_CONSTEXPR bool IsHashFunctor(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "hash";
        }
    };
    struct DefaultSimplifyTypeNamesTraits : BasicSimplifyTypeNamesTraits<DefaultSimplifyTypeNamesTraits> {};

    // Simplify a name with the assumption that it's a type name.
    // This is a low-level function, prefer `SimplifyTypeNames()`.
    template <typename Traits = DefaultSimplifyTypeNamesTraits>
    CPPDECL_CONSTEXPR void SimplifyTypeQualifiedName(SimplifyTypeNamesFlags flags, cppdecl::QualifiedName &name, Traits &&traits = {})
    {
        // Rewrite `_Bool` as `bool`.
        if (bool(flags & SimplifyTypeNamesFlags::bit_c_normalize_bool) && name.AsSingleWord() == "_Bool")
        {
            name.parts.at(0).var = "bool";
            return; // Surely we don't need to check anything else.
        }

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

        // Those need to be in a specific order, since we can only remove the last template argument at the every step:

        // Remove the allocator.
        if (bool(flags & SimplifyTypeNamesFlags::bit_common_remove_defarg_allocator))
        {
            std::size_t name_index = std::size_t(-1);

            const bool is_string_like        = traits.IsStringLike(name, &name_index);
            const bool is_vector_like        = traits.IsVectorLike(name, &name_index);
            const bool is_ordered_set_like   = traits.IsOrderedSetLike(name, &name_index);
            const bool is_ordered_map_like   = traits.IsOrderedMapLike(name, &name_index);
            const bool is_unordered_set_like = traits.IsUnorderedSetLike(name, &name_index);
            const bool is_unordered_map_like = traits.IsUnorderedMapLike(name, &name_index);

            if (
                is_string_like ||
                is_vector_like ||
                is_ordered_set_like ||
                is_ordered_map_like ||
                is_unordered_set_like ||
                is_unordered_map_like
            )
            {
                std::size_t allocator_targ_pos = std::size_t(-2); // `-2` is used to not enter the `if` below if none of the branches is taken and the assert is disabled.
                if (is_string_like)
                    allocator_targ_pos = 2; // After `std::char_traits`.
                else if (is_vector_like)
                    allocator_targ_pos = 1; // As usual.
                else if (is_ordered_set_like)
                    allocator_targ_pos = 2; // After comparator.
                else if (is_ordered_map_like)
                    allocator_targ_pos = 3; // After mapped type and comparator.
                else if (is_unordered_set_like)
                    allocator_targ_pos = 3; // After hash and equality.
                else if (is_unordered_map_like)
                    allocator_targ_pos = 4; // After mapped type, hash and equality.
                else
                    assert(false && "This should be unreachable.");

                const bool allocator_is_map_like = is_ordered_map_like || is_unordered_map_like;

                UnqualifiedName &name_part = name.parts.at(name_index);

                // The allocator must be the last argument, otherwise removing it will mess up the order.
                if (name_part.template_args && name_part.template_args->args.size() == allocator_targ_pos + 1)
                {
                    if (auto allocator_type = std::get_if<Type>(&name_part.template_args->args.back().var))
                    {
                        if (
                            traits.IsAllocator(*allocator_type) &&
                            allocator_type->simple_type.name.parts.back().template_args &&
                            allocator_type->simple_type.name.parts.back().template_args->args.size() == 1
                        )
                        {
                            if (auto allocator_targ = std::get_if<Type>(&allocator_type->simple_type.name.parts.back().template_args->args.front().var))
                            {
                                bool ok = false;
                                if (!allocator_is_map_like)
                                {
                                    if (auto our_targ = std::get_if<Type>(&name_part.template_args->args.at(0).var))
                                    {
                                        if (*our_targ == *allocator_targ)
                                            ok = true;
                                    }
                                }
                                else
                                {
                                    auto our_targ0 = std::get_if<Type>(&name_part.template_args->args.at(0).var);
                                    auto our_targ1 = std::get_if<Type>(&name_part.template_args->args.at(1).var);
                                    if (
                                        our_targ0 && our_targ1 &&
                                        traits.IsPairInAllocatorParam(*allocator_targ) &&
                                        allocator_targ->simple_type.name.parts.back().template_args &&
                                        allocator_targ->simple_type.name.parts.back().template_args->args.size() == 2
                                    )
                                    {
                                        auto allocator_targ0 = std::get_if<Type>(&allocator_targ->simple_type.name.parts.back().template_args->args.at(0).var);
                                        auto allocator_targ1 = std::get_if<Type>(&allocator_targ->simple_type.name.parts.back().template_args->args.at(1).var);
                                        if (
                                            allocator_targ0 && allocator_targ1 &&
                                            allocator_targ0->IsConst() &&
                                            *allocator_targ1 == *our_targ1 && // 0th one differs in constness, so we compare it below.
                                            our_targ0->GetQualifiersMut() // Make sure we can add qualifiers freely.
                                            )
                                        {
                                            // Temporarily add constness to the first template argument of our template, to simplify the comparison.
                                            struct ConstGuard
                                            {
                                                Type *our_targ0;
                                                bool added_constness = false;
                                                ConstGuard(Type *our_targ0)
                                                    : our_targ0(our_targ0)
                                                {
                                                    if (!our_targ0->IsConst())
                                                    {
                                                        added_constness = true;
                                                        our_targ0->AddQualifiers(CvQualifiers::const_);
                                                    }
                                                }
                                                ~ConstGuard()
                                                {
                                                    our_targ0->RemoveQualifiers(CvQualifiers::const_);
                                                }
                                            };
                                            ConstGuard const_guard(our_targ0);

                                            if (*allocator_targ0 == *our_targ0)
                                            {
                                                ok = true; // That's about it?
                                            }
                                        }
                                    }
                                }


                                // Lastly, if the template argument of the allocator matches, remove the allocator.
                                if (ok)
                                    name_part.template_args->args.pop_back();
                            }
                        }
                    }
                }
            }
        }

        // Remove char traits. Must be after removing the allocator.
        if (bool(flags & SimplifyTypeNamesFlags::bit_common_remove_defarg_char_traits))
        {
            std::size_t name_index = std::size_t(-1);

            if (traits.HasCharTraits(name, &name_index))
            {
                UnqualifiedName &name_part = name.parts.at(name_index);

                if (name_part.template_args && name_part.template_args->args.size() == 2)
                {
                    auto targ0 = std::get_if<Type>(&name_part.template_args->args.at(0).var);
                    auto targ1 = std::get_if<Type>(&name_part.template_args->args.at(1).var);
                    if (
                        targ0 && targ1 &&
                        traits.IsCharTraits(*targ1) &&
                        targ1->simple_type.name.parts.back().template_args &&
                        targ1->simple_type.name.parts.back().template_args->args.size() == 1
                    )
                    {
                        if (auto allocator_targ = std::get_if<Type>(&targ1->simple_type.name.parts.back().template_args->args.front().var))
                        {
                            if (*allocator_targ == *targ0)
                            {
                                // Success!
                                name_part.template_args->args.pop_back();
                            }
                        }
                    }
                }
            }
        }

        // Remove `std::less` and `std::equal_to`. Must be after removing the allocator.
        if (bool(flags & SimplifyTypeNamesFlags::bit_common_remove_defarg_comparator))
        {
            std::size_t name_index = std::size_t(-1);

            const bool is_ordered_set_like   = traits.IsOrderedSetLike(name, &name_index);
            const bool is_ordered_map_like   = traits.IsOrderedMapLike(name, &name_index);
            const bool is_unordered_set_like = traits.IsUnorderedSetLike(name, &name_index);
            const bool is_unordered_map_like = traits.IsUnorderedMapLike(name, &name_index);

            if (
                is_ordered_set_like ||
                is_ordered_map_like ||
                is_unordered_set_like ||
                is_unordered_map_like
            )
            {
                std::size_t comparator_targ_pos = std::size_t(-2); // `-2` is used to not enter the `if` below if none of the branches is taken and the assert is disabled.
                if (is_ordered_set_like)
                    comparator_targ_pos = 1; // Right after the element type.
                else if (is_ordered_map_like)
                    comparator_targ_pos = 2; // After the mapped type.
                else if (is_unordered_set_like)
                    comparator_targ_pos = 2; // After the hash functor.
                else if (is_unordered_map_like)
                    comparator_targ_pos = 3; // After the mapped type and the hash.
                else
                    assert(false && "This should be unreachable.");

                const bool container_is_unordered = is_unordered_set_like || is_unordered_map_like;

                UnqualifiedName &name_part = name.parts.at(name_index);

                // The comparator must be the last argument, otherwise removing it will mess up the order.
                if (name_part.template_args && name_part.template_args->args.size() == comparator_targ_pos + 1)
                {
                    if (auto comparator_type = std::get_if<Type>(&name_part.template_args->args.back().var))
                    {
                        if (
                            (container_is_unordered ? traits.IsEqualToComparator(*comparator_type) : traits.IsLessComparator(*comparator_type)) &&
                            comparator_type->simple_type.name.parts.back().template_args &&
                            comparator_type->simple_type.name.parts.back().template_args->args.size() == 1
                        )
                        {
                            if (auto comparator_targ = std::get_if<Type>(&comparator_type->simple_type.name.parts.back().template_args->args.front().var))
                            {
                                if (auto our_targ = std::get_if<Type>(&name_part.template_args->args.at(0).var))
                                {
                                    if (*our_targ == *comparator_targ)
                                        name_part.template_args->args.pop_back();
                                }
                            }
                        }
                    }
                }
            }
        }

        // Remove `std::hash`. Must be after removing the comparator (and the allocator).
        if (bool(flags & SimplifyTypeNamesFlags::bit_common_remove_defarg_hash_functor))
        {
            std::size_t name_index = std::size_t(-1);

            const bool is_unordered_set_like = traits.IsUnorderedSetLike(name, &name_index);
            const bool is_unordered_map_like = traits.IsUnorderedMapLike(name, &name_index);

            if (
                is_unordered_set_like ||
                is_unordered_map_like
            )
            {
                std::size_t hash_targ_pos = std::size_t(-2); // `-2` is used to not enter the `if` below if none of the branches is taken and the assert is disabled.
                if (is_unordered_set_like)
                    hash_targ_pos = 1; // After the element type.
                else if (is_unordered_map_like)
                    hash_targ_pos = 2; // After the mapped type..
                else
                    assert(false && "This should be unreachable.");

                UnqualifiedName &name_part = name.parts.at(name_index);

                // The hash must be the last argument, otherwise removing it will mess up the order.
                if (name_part.template_args && name_part.template_args->args.size() == hash_targ_pos + 1)
                {
                    if (auto hash_type = std::get_if<Type>(&name_part.template_args->args.back().var))
                    {
                        if (
                            traits.IsHashFunctor(*hash_type) &&
                            hash_type->simple_type.name.parts.back().template_args &&
                            hash_type->simple_type.name.parts.back().template_args->args.size() == 1
                        )
                        {
                            if (auto hash_targ = std::get_if<Type>(&hash_type->simple_type.name.parts.back().template_args->args.front().var))
                            {
                                if (auto our_targ = std::get_if<Type>(&name_part.template_args->args.at(0).var))
                                {
                                    if (*our_targ == *hash_targ)
                                        name_part.template_args->args.pop_back();
                                }
                            }
                        }
                    }
                }
            }
        }

        // Rewrite template specializations as typedefs.
        if (bool(flags & SimplifyTypeNamesFlags::bit_common_rewrite_template_specializations_as_typedefs))
        {
            std::size_t name_index = std::size_t(-1);
            std::string_view new_name_base;
            bool allow_all_char_types = false;

            if (traits.SpecializationsHaveTypedefsForCharTypes(name, &name_index, &new_name_base, &allow_all_char_types))
            {
                UnqualifiedName &part = name.parts.at(name_index);

                // A separate condition to avoid going into the `else` below if this fails.
                if (part.template_args && part.template_args->args.size() == 1)
                {
                    if (auto type = std::get_if<Type>(&part.template_args->args.front().var))
                    {
                        const std::string_view type_word = type->AsSingleWord();
                        if (type_word == "char")
                        {
                            part.template_args.reset();
                            part.var = std::string(new_name_base);
                        }
                        else if (type_word == "wchar_t")
                        {
                            part.template_args.reset();
                            part.var.emplace<std::string>("w") += new_name_base;
                        }
                        else if (allow_all_char_types)
                        {
                            if (type_word == "char8_t")
                            {
                                part.template_args.reset();
                                part.var.emplace<std::string>("u8") += new_name_base;
                            }
                            else if (type_word == "char16_t")
                            {
                                part.template_args.reset();
                                part.var.emplace<std::string>("u16") += new_name_base;
                            }
                            else if (type_word == "char32_t")
                            {
                                part.template_args.reset();
                                part.var.emplace<std::string>("u32") += new_name_base;
                            }
                        }
                    }
                }
            }
        }
    }

    // Simplify cv-qualifiers according to the flags.
    // This is a low-level function, prefer `SimplifyTypeNames()`.
    CPPDECL_CONSTEXPR void SimplifyTypeCvQualifiers(SimplifyTypeNamesFlags flags, cppdecl::CvQualifiers &quals)
    {
        if (bool(flags & SimplifyTypeNamesFlags::bit_msvc_remove_ptr32_ptr64))
            quals &= ~(CvQualifiers::msvc_ptr32 | CvQualifiers::msvc_ptr64);
    }

    CPPDECL_CONSTEXPR void SimplifySimpleType(SimplifyTypeNamesFlags flags, SimpleType &simple_type)
    {
        if (bool(flags & SimplifyTypeNamesFlags::bit_common_remove_type_prefix))
            simple_type.prefix = SimpleTypePrefix{};
        if (bool(flags & SimplifyTypeNamesFlags::bit_common_remove_redundant_signed) && bool(simple_type.flags & SimpleTypeFlags::explicitly_signed) && !simple_type.IsNonRedundantlySigned())
            simple_type.flags &= ~SimpleTypeFlags::explicitly_signed;
    }

    // `target` is typically a `Type` or `Decl`.
    template <typename Traits = DefaultSimplifyTypeNamesTraits>
    CPPDECL_CONSTEXPR void SimplifyTypeNames(SimplifyTypeNamesFlags flags, auto &target, Traits &&traits = {})
    {
        if (bool(flags))
        {
            target.template VisitEachComponent<QualifiedName, CvQualifiers, SimpleType>(
                {},
                Overload{
                    [&](QualifiedName &name){SimplifyTypeQualifiedName(flags, name, traits);},
                    [&](CvQualifiers &quals){SimplifyTypeCvQualifiers(flags, quals);},
                    [&](SimpleType &quals){SimplifySimpleType(flags, quals);},
                }
            );
        }
    }
}
