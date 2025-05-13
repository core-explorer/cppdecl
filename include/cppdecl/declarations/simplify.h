#pragma once

#include "cppdecl/declarations/data.h"
#include "cppdecl/misc/enum_flags.h"

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

        // Remove allocators from template parameters.
        bit_common_remove_defarg_allocator = 1 << 3,
        // Remove `std::char_traits<T>` from template parameters of `std::basic_string`.
        // This typically requires `bit_common_remove_defarg_allocator` as well, since we can't remove non-last template arguments,
        //   and having the allocator after this one will prevent it from being removed.
        bit_common_remove_defarg_char_traits = 1 << 4,
        // Remove `std::less<T>` and `std::equal_to<T>` from ordered and unordered containers respectively.
        // This typically requires `bit_common_remove_defarg_allocator` as well, since we can't remove non-last template arguments,
        //   and having the allocator after this one will prevent it from being removed.
        bit_common_remove_defarg_comparator = 1 << 5,

        // Remove various default arguments from templates.
        common_remove_defargs = bit_common_remove_defarg_allocator | bit_common_remove_defarg_char_traits | bit_common_remove_defarg_comparator,

        // Various mostly compiler-independent bits.
        // Note that `common_remove_defargs` isn't needed when you get the types from `__PRETTY_FUNCTION__` or equivalent on Clang.
        common = common_remove_defargs,


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
        all = common | compiler_all | stdlib_all,
        // Only the rules that are relevant on the current compiler and standard library.
        native = common | compiler_current | stdlib_current,
        // Only the rules that are relevant on the current compiler and standard library, with the assumption that the type names come from a method based on `__PRETTY_FUNCTION__`, `__FUNCSIG__`, `std::source_location::function_name()`.
        native_func_name_based_only = native & ~common_remove_defargs,
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
        [[nodiscard]] constexpr std::string_view AsStdName(const cppdecl::QualifiedName &name, std::size_t *index = nullptr)
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
        [[nodiscard]] constexpr std::string_view AsStdName(const cppdecl::Type &type, std::size_t *index = nullptr)
        {
            return type.IsOnlyQualifiedName() ? GetDerived().AsStdName(type.simple_type.name, index) : "";
        }


        // Those can be overridden:

        // Note that all those must leave `index` untouched if they return false.

        // `A<T, std::char_traits<T>, std::allocator<T>>`
        [[nodiscard]] constexpr bool IsStringLike(const cppdecl::QualifiedName &name, std::size_t *index)
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
        [[nodiscard]] constexpr bool IsVectorLike(const cppdecl::QualifiedName &name, std::size_t *index)
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
        [[nodiscard]] constexpr bool IsOrderedSetLike(const cppdecl::QualifiedName &name, std::size_t *index)
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
        [[nodiscard]] constexpr bool IsOrderedMapLike(const cppdecl::QualifiedName &name, std::size_t *index)
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
        [[nodiscard]] constexpr bool IsUnorderedSetLike(const cppdecl::QualifiedName &name, std::size_t *index)
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
        [[nodiscard]] constexpr bool IsUnorderedMapLike(const cppdecl::QualifiedName &name, std::size_t *index)
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


        // Is this a character traits type that we should remove from `IsStringLike()` types?
        [[nodiscard]] constexpr bool IsCharTraits(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "char_traits";
        }
        // Is this an allocator type that we can remove?
        [[nodiscard]] constexpr bool IsAllocator(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "allocator";
        }
        // Is this a pair type that will appear as the allocator parameter in maps?
        [[nodiscard]] constexpr bool IsPairInAllocatorParam(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "pair";
        }
        // Is this a less comparator that should be removed from ordered containers?
        [[nodiscard]] constexpr bool IsLessComparator(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "less";
        }
        // Is this an equality comparator that should be removed from unordered containers?
        [[nodiscard]] constexpr bool IsEqualToComparator(const cppdecl::Type &type)
        {
            return GetDerived().AsStdName(type) == "equal_to";
        }
    };
    struct DefaultSimplifyTypeNamesTraits : BasicSimplifyTypeNamesTraits<DefaultSimplifyTypeNamesTraits> {};

    // Simplify a name with the assumption that it's a type name.
    // This is a low-level function, prefer `SimplifyTypeNames()`.
    template <typename Traits = DefaultSimplifyTypeNamesTraits>
    constexpr void SimplifyTypeQualifiedName(SimplifyTypeNamesFlags flags, cppdecl::QualifiedName &name, Traits &&traits = {})
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
                                            our_targ0->GetTopLevelQualifiersMut() // Make sure we can add qualifiers freely.
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
                                                        our_targ0->AddTopLevelQualifiers(CvQualifiers::const_);
                                                    }
                                                }
                                                ~ConstGuard()
                                                {
                                                    our_targ0->RemoveTopLevelQualifiers(CvQualifiers::const_);
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

            if (traits.IsStringLike(name, &name_index))
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

        // Remove `std::less`. Must be after removing the allocator.
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
    }

    // Simplify cv-qualifiers according to the flags.
    // This is a low-level function, prefer `SimplifyTypeNames()`.
    constexpr void SimplifyTypeCvQualifiers(SimplifyTypeNamesFlags flags, cppdecl::CvQualifiers &quals)
    {
        if (bool(flags & SimplifyTypeNamesFlags::bit_msvc_remove_ptr32_ptr64))
            quals &= ~(CvQualifiers::msvc_ptr32 | CvQualifiers::msvc_ptr64);
    }

    // `target` is typically a `Type` or `Decl`.
    template <typename Traits = DefaultSimplifyTypeNamesTraits>
    constexpr void SimplifyTypeNames(SimplifyTypeNamesFlags flags, auto &target, Traits &&traits = {})
    {
        if (bool(flags))
        {
            target.template VisitEachComponent<QualifiedName>({}, [&](QualifiedName &name){SimplifyTypeQualifiedName(flags, name, traits);});
            target.template VisitEachComponent<CvQualifiers>({}, [&](CvQualifiers &quals){SimplifyTypeCvQualifiers(flags, quals);});
        }
    }
}
