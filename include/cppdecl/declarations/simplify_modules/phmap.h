#pragma once

#include <cppdecl/declarations/data.h>
#include <cppdecl/declarations/simplify.h>
#include <cppdecl/misc/platform.h>

namespace cppdecl::SimplifyModules
{
    // https://github.com/greg7mdp/parallel-hashmap
    template <typename Derived, typename Base>
    struct Phmap : Base
    {
        CPPDECL_CONSTEXPR void SimplifyQualifiedNameNonRecursively(SimplifyFlags flags, QualifiedName &name)
        {
            // Seems to not matter if this is before or after the rest of our stuff.
            Base::SimplifyQualifiedNameNonRecursively(flags, name);

            // Simplify iterators.
            // This has to be BEFORE the default template argument removal below, because this can create new container names that we also need to clean up.
            if (bool(flags & SimplifyFlags::bit_common_normalize_iterators))
            {
                bool success = false;

                if (
                    !success &&
                    name.parts.size() >= 4 && // `phmap::priv::A<...>::[const_]iterator`
                    name.parts.at(0).AsSingleWord() == "phmap" &&
                    name.parts.at(1).AsSingleWord() == "priv" &&
                    name.parts.at(2).template_args &&
                    name.parts.at(2).template_args->args.size() > 0 &&
                    (name.parts.at(3).AsSingleWord() == "iterator" || name.parts.at(3).AsSingleWord() == "const_iterator")
                )
                {
                    UnqualifiedName &part = name.parts.at(2);

                    // flat_hash_set, node_hash_set, flat_hash_map, node_hash_map
                    if (
                        !success &&
                        part.AsSingleWord(SingleWordFlags::ignore_template_args) == "raw_hash_set"
                    )
                    {
                        if (
                            auto targ = part.template_args->args.at(0).AsType();
                            targ &&
                            targ->IsOnlyQualifiedName(SingleWordFlags::ignore_type_prefixes) &&
                            targ->simple_type.name.parts.size() == 3 &&
                            targ->simple_type.name.parts.at(0).AsSingleWord() == "phmap" &&
                            targ->simple_type.name.parts.at(1).AsSingleWord() == "priv" &&
                            targ->simple_type.name.parts.at(2).template_args
                        )
                        {
                            bool is_node_based = false;

                            // flat_hash_set, node_hash_set
                            if (
                                !success &&
                                (
                                    targ->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "FlatHashSetPolicy" ||
                                    (is_node_based = targ->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "NodeHashSetPolicy")
                                ) &&
                                targ->simple_type.name.parts.at(2).template_args->args.size() == 1 &&
                                targ->simple_type.name.parts.at(2).template_args->args.front().IsType()
                            )
                            {
                                success = true;

                                // Move to a temporary variable. Assigning directly to the parent seems unsafe.
                                Type elem_type = std::move(*targ->simple_type.name.parts.at(2).template_args->args.front().AsType());
                                part.var = is_node_based ? "node_hash_set" : "flat_hash_set";
                                part.template_args->args.front().var = std::move(elem_type);
                                name.parts.erase(name.parts.begin() + 1);
                            }

                            // flat_hash_map, node_hash_map
                            if (
                                !success &&
                                (
                                    targ->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "FlatHashMapPolicy" ||
                                    (is_node_based = targ->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "NodeHashMapPolicy")
                                ) &&
                                targ->simple_type.name.parts.at(2).template_args->args.size() == 2 &&
                                targ->simple_type.name.parts.at(2).template_args->args.at(0).IsType() &&
                                targ->simple_type.name.parts.at(2).template_args->args.at(1).IsType()
                            )
                            {
                                success = true;

                                // Move to temporary variables. Assigning directly to the parent seems unsafe.
                                Type elem_type_0 = std::move(*targ->simple_type.name.parts.at(2).template_args->args.at(0).AsType());
                                Type elem_type_1 = std::move(*targ->simple_type.name.parts.at(2).template_args->args.at(1).AsType());
                                part.var = is_node_based ? "node_hash_map" : "flat_hash_map";
                                part.template_args->args.front().var = std::move(elem_type_0);
                                part.template_args->args.emplace(part.template_args->args.begin() + 1, std::move(elem_type_1));
                                name.parts.erase(name.parts.begin() + 1);
                            }
                        }
                    }

                    // parallel_flat_hash_set, parallel_node_hash_set, parallel_flat_hash_map, parallel_node_hash_map
                    if (
                        !success &&
                        part.AsSingleWord(SingleWordFlags::ignore_template_args) == "parallel_hash_set" &&
                        part.template_args->args.size() == 7 // This must be exact, since we'll be appending some arguments at the end.
                    )
                    {
                        auto targ0 = part.template_args->args.at(0).AsPseudoExpr();
                        auto targ1 = part.template_args->args.at(1).AsType();
                        auto targ2 = part.template_args->args.at(2).AsType();
                        auto targ3 = part.template_args->args.at(3).AsType();

                        if (
                            targ0 && // Pasting this to the output as is.
                            targ1 &&
                            targ1->IsOnlyQualifiedName(SingleWordFlags::ignore_type_prefixes) &&
                            targ1->simple_type.name.parts.size() == 3 &&
                            targ1->simple_type.name.parts.at(0).AsSingleWord() == "phmap" &&
                            targ1->simple_type.name.parts.at(1).AsSingleWord() == "priv" &&
                            targ1->simple_type.name.parts.at(2).AsSingleWord() == "raw_hash_set" &&
                            targ2 && // Pasting this to the output as is.
                            targ3 &&
                            targ3->IsOnlyQualifiedName(SingleWordFlags::ignore_template_args | SingleWordFlags::ignore_type_prefixes) &&
                            targ3->simple_type.name.parts.size() == 3 &&
                            targ3->simple_type.name.parts.at(0).AsSingleWord() == "phmap" &&
                            targ3->simple_type.name.parts.at(1).AsSingleWord() == "priv" &&
                            targ3->simple_type.name.parts.at(2).template_args
                        )
                        {
                            bool is_node_based = false;

                            // parallel_flat_hash_set, parallel_node_hash_set
                            if (
                                !success &&
                                (
                                    targ3->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "FlatHashSetPolicy" ||
                                    (is_node_based = targ3->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "NodeHashSetPolicy")
                                ) &&
                                targ3->simple_type.name.parts.at(2).template_args->args.size() == 1 &&
                                targ3->simple_type.name.parts.at(2).template_args->args.front().IsType()
                            )
                            {
                                success = true;

                                part.var = is_node_based ? "parallel_node_hash_set" : "parallel_flat_hash_set";

                                // Move to temporary variables. Assigning directly to the parent seems unsafe.
                                Type elem_type = std::move(*targ3->simple_type.name.parts.at(2).template_args->args.front().AsType());
                                PseudoExpr num_submaps = std::move(*targ0); // The actual number of submaps is 2 to the power of this value.
                                Type mutex_type = std::move(*targ2);

                                part.template_args->args.at(0).var = std::move(elem_type);
                                part.template_args->args.erase(part.template_args->args.begin() + 1, part.template_args->args.begin() + 4);
                                part.template_args->args.emplace_back(std::move(num_submaps));
                                part.template_args->args.emplace_back(std::move(mutex_type));

                                name.parts.erase(name.parts.begin() + 1);
                            }

                            // parallel_flat_hash_map, parallel_node_hash_map
                            if (
                                !success &&
                                (
                                    targ3->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "FlatHashMapPolicy" ||
                                    (is_node_based = targ3->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "NodeHashMapPolicy")
                                ) &&
                                targ3->simple_type.name.parts.at(2).template_args->args.size() == 2 &&
                                targ3->simple_type.name.parts.at(2).template_args->args.at(0).IsType() &&
                                targ3->simple_type.name.parts.at(2).template_args->args.at(1).IsType()
                            )
                            {
                                success = true;

                                part.var = is_node_based ? "parallel_node_hash_map" : "parallel_flat_hash_map";

                                // Move to temporary variables. Assigning directly to the parent seems unsafe.
                                Type elem_type_0 = std::move(*targ3->simple_type.name.parts.at(2).template_args->args.at(0).AsType());
                                Type elem_type_1 = std::move(*targ3->simple_type.name.parts.at(2).template_args->args.at(1).AsType());
                                PseudoExpr num_submaps = std::move(*targ0); // The actual number of submaps is 2 to the power of this value.
                                Type mutex_type = std::move(*targ2);

                                part.template_args->args.at(0).var = std::move(elem_type_0);
                                part.template_args->args.at(1).var = std::move(elem_type_1);
                                part.template_args->args.erase(part.template_args->args.begin() + 2, part.template_args->args.begin() + 4);
                                part.template_args->args.emplace_back(std::move(num_submaps));
                                part.template_args->args.emplace_back(std::move(mutex_type));

                                name.parts.erase(name.parts.begin() + 1);
                            }
                        }
                    }
                }

                // btree_set, btree_map, btree_multiset, btree_multimap
                if (
                    !success &&
                    name.parts.size() >= 3 && // `phmap::priv::A<...>`
                    name.parts.at(0).AsSingleWord() == "phmap" &&
                    name.parts.at(1).AsSingleWord() == "priv" &&
                    name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "btree_iterator" &&
                    name.parts.at(2).template_args &&
                    name.parts.at(2).template_args->args.size() == 3
                )
                {
                    UnqualifiedName &part = name.parts.at(2);

                    bool is_const = false;

                    auto targ0 = part.template_args->args.at(0).AsType();
                    auto targ1 = part.template_args->args.at(1).AsType();
                    auto targ2 = part.template_args->args.at(2).AsType();
                    if (
                        targ0 &&
                        targ1 &&
                        targ1->Is<Reference>() &&
                        targ1->As<Reference>()->kind == RefQualifier::lvalue &&
                        (targ1->As<Reference>()->quals & ~(CvQualifiers::msvc_ptr32 | CvQualifiers::msvc_ptr64)) == CvQualifiers{} &&
                        targ2 &&
                        targ2->Is<Pointer>() &&
                        (targ2->As<Pointer>()->quals & ~(CvQualifiers::msvc_ptr32 | CvQualifiers::msvc_ptr64)) == CvQualifiers{} &&
                        // More checks for `targ0`:
                        targ0->IsOnlyQualifiedName(SingleWordFlags::ignore_type_prefixes | SingleWordFlags::ignore_cv_qualifiers) &&
                        (targ0->simple_type.quals == CvQualifiers{} || (is_const = targ0->simple_type.quals == CvQualifiers::const_)) &&
                        targ0->simple_type.name.parts.size() == 3 &&
                        targ0->simple_type.name.parts.at(0).AsSingleWord() == "phmap" &&
                        targ0->simple_type.name.parts.at(1).AsSingleWord() == "priv" &&
                        targ0->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "btree_node" &&
                        targ0->simple_type.name.parts.at(2).template_args &&
                        targ0->simple_type.name.parts.at(2).template_args->args.size() == 1
                    )
                    {
                        bool is_map = false;

                        if (
                            auto targ00 = targ0->simple_type.name.parts.at(2).template_args->args.front().AsType();
                            targ00 &&
                            targ00->IsOnlyQualifiedName(SingleWordFlags::ignore_type_prefixes) &&
                            targ00->simple_type.name.parts.size() == 3 &&
                            targ00->simple_type.name.parts.at(0).AsSingleWord() == "phmap" &&
                            targ00->simple_type.name.parts.at(1).AsSingleWord() == "priv" &&
                            (
                                targ00->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "set_params" ||
                                (is_map = targ00->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "map_params")
                            ) &&
                            targ00->simple_type.name.parts.at(2).template_args &&
                            targ00->simple_type.name.parts.at(2).template_args->args.size() == 5 + is_map
                        )
                        {
                            auto targ000 = targ00->simple_type.name.parts.at(2).template_args->args.at(0).AsType();
                            auto targ000_mapped = is_map ? targ00->simple_type.name.parts.at(2).template_args->args.at(1).AsType() : nullptr;
                            auto targ001 = targ00->simple_type.name.parts.at(2).template_args->args.at(1 + is_map).AsType();
                            auto targ002 = targ00->simple_type.name.parts.at(2).template_args->args.at(2 + is_map).AsType();
                            auto targ003 = targ00->simple_type.name.parts.at(2).template_args->args.at(3 + is_map).AsPseudoExpr();
                            auto targ004 = targ00->simple_type.name.parts.at(2).template_args->args.at(4 + is_map).AsPseudoExpr();

                            bool is_multi = false;

                            if (
                                targ000 &&
                                (!is_map || targ000_mapped) &&
                                targ001 &&
                                targ002 &&
                                targ003 &&
                                targ003->tokens.size() == 1 &&
                                std::holds_alternative<NumericLiteral>(targ003->tokens.front()) &&
                                std::get<NumericLiteral>(targ003->tokens.front()).ToInteger() == 256 &&
                                targ004 &&
                                targ004->tokens.size() == 1 &&
                                (
                                    std::holds_alternative<SimpleType>(targ004->tokens.front())
                                    ? (
                                        std::get<SimpleType>(targ004->tokens.front()).AsSingleWord() == "false" ||
                                        (is_multi = std::get<SimpleType>(targ004->tokens.front()).AsSingleWord() == "true")
                                    )
                                    // MSVC uses `0`, `1` instead of `false`, `true`.
                                    : (
                                        std::holds_alternative<NumericLiteral>(targ004->tokens.front()) &&
                                        (
                                            std::get<NumericLiteral>(targ004->tokens.front()).ToInteger() == 0 ||
                                            (is_multi = std::get<NumericLiteral>(targ004->tokens.front()).ToInteger() == 1)
                                        )
                                    )
                                )
                            )
                            {
                                // Check that `targ1` and `targ2` are actually a reference and a pointer to the element type,
                                //   which is either `targ000` or a pair of `targ000` and `targ000_mapped`.
                                bool args_match = false;
                                if (!is_map)
                                {
                                    // Note that we've already checked above that `targ0` and `targ1` are a pointer and a reference.
                                    args_match =
                                        is_const
                                        ? targ1->Equals(*targ000, Type::EqualsFlags::as_if_target_is_const, 1) &&
                                          targ2->Equals(*targ000, Type::EqualsFlags::as_if_target_is_const, 1)
                                        : targ1->Equals(*targ000, {}, 1) &&
                                          targ2->Equals(*targ000, {}, 1);
                                }
                                else
                                {
                                    args_match =
                                        this->GetDerived().IsConstNonconstPair(*targ1, *targ000, *targ000_mapped, 1, is_const) &&
                                        this->GetDerived().IsConstNonconstPair(*targ2, *targ000, *targ000_mapped, 1, is_const);
                                }

                                if (args_match)
                                {
                                    success = true;

                                    // Move to temporary variables, because moving directly to the parent sounds unsafe.
                                    Type elem_type = std::move(*targ000);
                                    Type elem_type_mapped;
                                    if (is_map)
                                        elem_type_mapped = std::move(*targ000_mapped);
                                    Type comparator_type = std::move(*targ001);
                                    Type allocator_type = std::move(*targ002);

                                    part.var = is_map ? (is_multi ? "btree_multimap" : "btree_map") : (is_multi ? "btree_multiset" : "btree_set");
                                    part.template_args->args.clear(); // Whatever, this is just easier.
                                    part.template_args->args.emplace_back(std::move(elem_type));
                                    if (is_map)
                                        part.template_args->args.emplace_back(std::move(elem_type_mapped));
                                    part.template_args->args.emplace_back(std::move(comparator_type));
                                    part.template_args->args.emplace_back(std::move(allocator_type));

                                    name.parts[1] = std::move(name.parts[2]);
                                    name.parts[2] = UnqualifiedName{.var = is_const ? "const_iterator" : "iterator", .template_args = {}};
                                }
                            }
                        }
                    }
                }
            }

            // Simplify container names.
            if (
                bool(flags & SimplifyFlags::bit_common_remove_defargs_other) &&
                name.parts.size() >= 2 &&
                name.parts.at(0).AsSingleWord() == "phmap" &&
                name.parts.at(1).template_args &&
                name.parts.at(1).template_args->args.size() > 0
            )
            {
                //                               1    2                 3                    4                                            5                                            6                 7
                // phmap::flat_hash_set         <int, phmap::Hash<int>, phmap::EqualTo<int>, std::allocator<int>                          >
                // phmap::flat_hash_map         <int, float,            phmap::Hash<int>,    phmap::EqualTo<int>,                         std::allocator<std::pair<int const, float>>  >
                // phmap::node_hash_set         <int, phmap::Hash<int>, phmap::EqualTo<int>, std::allocator<int>                          >
                // phmap::node_hash_map         <int, float,            phmap::Hash<int>,    phmap::EqualTo<int>,                         std::allocator<std::pair<int const, float>>  >
                // phmap::parallel_flat_hash_set<int, phmap::Hash<int>, phmap::EqualTo<int>, std::allocator<int>,                         4ul,                                         phmap::NullMutex  >
                // phmap::parallel_flat_hash_map<int, float,            phmap::Hash<int>,    phmap::EqualTo<int>,                         std::allocator<std::pair<int const, float>>, 4ul,              phmap::NullMutex>
                // phmap::parallel_node_hash_set<int, phmap::Hash<int>, phmap::EqualTo<int>, std::allocator<int>,                         4ul,                                         phmap::NullMutex  >
                // phmap::parallel_node_hash_map<int, float,            phmap::Hash<int>,    phmap::EqualTo<int>,                         std::allocator<std::pair<int const, float>>, 4ul,              phmap::NullMutex>
                // phmap::btree_set             <int, phmap::Less<int>, std::allocator<int>  >
                // phmap::btree_map             <int, float,            phmap::Less<int>,    std::allocator<std::pair<int const, float>>>
                // phmap::btree_multiset        <int, phmap::Less<int>, std::allocator<int>  >
                // phmap::btree_multimap        <int, float,            phmap::Less<int>,    std::allocator<std::pair<int const, float>>>

                UnqualifiedName &part = name.parts.at(1);

                const std::string_view container_name = part.AsSingleWord(SingleWordFlags::ignore_type_prefixes | SingleWordFlags::ignore_template_args);

                bool known = false;
                const bool is_flat_hash_set          = !known && container_name == "flat_hash_set"          && (known = true);
                const bool is_flat_hash_map          = !known && container_name == "flat_hash_map"          && (known = true);
                const bool is_node_hash_set          = !known && container_name == "node_hash_set"          && (known = true);
                const bool is_node_hash_map          = !known && container_name == "node_hash_map"          && (known = true);
                const bool is_parallel_flat_hash_set = !known && container_name == "parallel_flat_hash_set" && (known = true);
                const bool is_parallel_flat_hash_map = !known && container_name == "parallel_flat_hash_map" && (known = true);
                const bool is_parallel_node_hash_set = !known && container_name == "parallel_node_hash_set" && (known = true);
                const bool is_parallel_node_hash_map = !known && container_name == "parallel_node_hash_map" && (known = true);
                const bool is_btree_set              = !known && container_name == "btree_set"              && (known = true);
                const bool is_btree_map              = !known && container_name == "btree_map"              && (known = true);
                const bool is_btree_multiset         = !known && container_name == "btree_multiset"         && (known = true);
                const bool is_btree_multimap         = !known && container_name == "btree_multimap"         && (known = true);

                // Can't destroy those variables, even though they are unused, because their initialization sets `known = true`.
                (void)is_flat_hash_set;
                (void)is_node_hash_set;

                if (known)
                {
                    const bool is_map = is_flat_hash_map || is_node_hash_map || is_parallel_flat_hash_map || is_parallel_node_hash_map || is_btree_map || is_btree_multimap;

                    Type *const elem_type = std::get_if<Type>(&part.template_args->args.at(0).var);
                    Type *const mapped_type = is_map ? std::get_if<Type>(&part.template_args->args.at(1).var) : nullptr;

                    if (elem_type && (!is_map || mapped_type))
                    {
                        const bool is_parallel = is_parallel_flat_hash_set || is_parallel_flat_hash_map || is_parallel_node_hash_set || is_parallel_node_hash_map;
                        const bool is_btree = !is_parallel && (is_btree_set || is_btree_map || is_btree_multiset || is_btree_multimap);

                        if (is_parallel)
                        {
                            { // Strip `phmap::NullMutex`.
                                std::size_t expected_targ_list_size = is_map ? 7 : 6;
                                if (expected_targ_list_size == part.template_args->args.size())
                                {
                                    if (auto targ = std::get_if<Type>(&part.template_args->args.back().var))
                                    {
                                        if (
                                            targ->IsOnlyQualifiedName(SingleWordFlags::ignore_type_prefixes) &&
                                            targ->simple_type.name.parts.size() == 2 &&
                                            targ->simple_type.name.parts.at(0).AsSingleWord() == "phmap" &&
                                            targ->simple_type.name.parts.at(1).AsSingleWord() == "NullMutex"
                                        )
                                        {
                                            part.template_args->args.pop_back();
                                        }
                                    }
                                }
                            }

                            { // Strip `4ul`. Documentation says this number `N` gives you `2^N` submaps.
                                std::size_t expected_targ_list_size = is_map ? 6 : 5;
                                if (expected_targ_list_size == part.template_args->args.size())
                                {
                                    if (auto targ = std::get_if<PseudoExpr>(&part.template_args->args.back().var); targ && targ->tokens.size() == 1)
                                    {
                                        if (auto num = std::get_if<NumericLiteral>(&targ->tokens.front()); num && num->ToInteger() == 4)
                                        {
                                            part.template_args->args.pop_back();
                                        }
                                    }
                                }
                            }
                        }

                        { // Strip allocator.
                            std::size_t expected_targ_list_size = 4 + is_map - is_btree;
                            if (expected_targ_list_size == part.template_args->args.size())
                            {
                                if (
                                    auto targ = std::get_if<Type>(&part.template_args->args.back().var);
                                    targ &&
                                    (is_map ? this->GetDerived().IsAllocatorForPair(*targ, *elem_type, *mapped_type) : this->GetDerived().IsAllocatorFor(*targ, *elem_type))
                                )
                                {
                                    part.template_args->args.pop_back();
                                }
                            }
                        }

                        // Strip equality comparator.
                        if (!is_btree)
                        {
                            std::size_t expected_targ_list_size = 3 + is_map;
                            if (expected_targ_list_size == part.template_args->args.size())
                            {
                                if (
                                    auto targ = std::get_if<Type>(&part.template_args->args.back().var);
                                    targ &&
                                    targ->IsOnlyQualifiedName(SingleWordFlags::ignore_type_prefixes) &&
                                    targ->simple_type.name.parts.size() >= 2 &&
                                    targ->simple_type.name.parts.front().AsSingleWord() == "phmap"
                                )
                                {
                                    bool ok = false;

                                    // The normal comparator: `phmap::EqualTo`.
                                    if (
                                        !ok &&
                                        targ->simple_type.name.parts.size() == 2 &&
                                        targ->simple_type.name.parts.at(1).AsSingleWord(SingleWordFlags::ignore_template_args) == "EqualTo" &&
                                        targ->simple_type.name.parts.at(1).TemplateArgsMatch(*elem_type)
                                    )
                                    {
                                        ok = true;
                                    }

                                    // Some custom comparators.
                                    if (
                                        !ok &&
                                        targ->simple_type.name.parts.size() == 4 &&
                                        targ->simple_type.name.parts.at(1).AsSingleWord() == "priv" &&
                                        targ->simple_type.name.parts.at(3).AsSingleWord() == "Eq"
                                    )
                                    {
                                        // The string comparator: `phmap::priv::StringHashEqT<char>::Eq`.
                                        if (
                                            !ok &&
                                            targ->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "StringHashEqT" &&
                                            targ->simple_type.name.parts.at(2).template_args &&
                                            targ->simple_type.name.parts.at(2).template_args->args.size() == 1 &&
                                            std::holds_alternative<Type>(targ->simple_type.name.parts.at(2).template_args->args.front().var) // Don't check the actual type, because what would we compare it with? It's not always `char`, can be wider too.
                                        )
                                        {
                                            ok = true;
                                        }

                                        // The pointer comparator: `phmap::priv::HashEq<int*, void>::Eq`.
                                        if (
                                            !ok &&
                                            targ->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "HashEq" &&
                                            targ->simple_type.name.parts.at(2).TemplateArgsMatch(*elem_type, cppdecl::Type::FromSingleWord("void"))
                                        )
                                        {
                                            ok = true;
                                        }
                                    }

                                    // On success, remove the comparator.
                                    if (ok)
                                        part.template_args->args.pop_back();
                                }
                            }
                        }

                        // Strip hash functor.
                        if (!is_btree)
                        {
                            std::size_t expected_targ_list_size = 2 + is_map;
                            if (expected_targ_list_size == part.template_args->args.size())
                            {
                                if (
                                    auto targ = std::get_if<Type>(&part.template_args->args.back().var);
                                    targ &&
                                    targ->IsOnlyQualifiedName(SingleWordFlags::ignore_type_prefixes) &&
                                    targ->simple_type.name.parts.size() >= 2 &&
                                    targ->simple_type.name.parts.front().AsSingleWord() == "phmap"
                                )
                                {
                                    bool ok = false;

                                    // The normal hash function: `phmap::Hash`.
                                    if (
                                        !ok &&
                                        targ->simple_type.name.parts.size() == 2 &&
                                        targ->simple_type.name.parts.at(1).AsSingleWord(SingleWordFlags::ignore_template_args) == "Hash" &&
                                        targ->simple_type.name.parts.at(1).TemplateArgsMatch(*elem_type)
                                    )
                                    {
                                        ok = true;
                                    }

                                    // Some custom hash functions.
                                    if (
                                        !ok &&
                                        targ->simple_type.name.parts.size() == 4 &&
                                        targ->simple_type.name.parts.at(1).AsSingleWord() == "priv" &&
                                        targ->simple_type.name.parts.at(3).AsSingleWord() == "Hash"
                                    )
                                    {
                                        // The string hash function: `phmap::priv::StringHashEqT<char>::Hash`.
                                        if (
                                            !ok &&
                                            targ->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "StringHashEqT" &&
                                            targ->simple_type.name.parts.at(2).template_args &&
                                            targ->simple_type.name.parts.at(2).template_args->args.size() == 1 &&
                                            std::holds_alternative<Type>(targ->simple_type.name.parts.at(2).template_args->args.front().var) // Don't check the actual type, because what would we compare it with? It's not always `char`, can be wider too.
                                        )
                                        {
                                            ok = true;
                                        }

                                        // The pointer hash function: `phmap::priv::HashEq<int*, void>::Hash`.
                                        if (
                                            !ok &&
                                            targ->simple_type.name.parts.at(2).AsSingleWord(SingleWordFlags::ignore_template_args) == "HashEq" &&
                                            targ->simple_type.name.parts.at(2).TemplateArgsMatch(*elem_type, cppdecl::Type::FromSingleWord("void"))
                                        )
                                        {
                                            ok = true;
                                        }
                                    }

                                    // On success, remove the hash function.
                                    if (ok)
                                        part.template_args->args.pop_back();
                                }
                            }
                        }

                        // Strip less comparator.
                        if (is_btree)
                        {
                            std::size_t expected_targ_list_size = 2 + is_map;
                            if (expected_targ_list_size == part.template_args->args.size())
                            {
                                if (
                                    auto targ = std::get_if<Type>(&part.template_args->args.back().var);
                                    targ &&
                                    targ->IsOnlyQualifiedName(SingleWordFlags::ignore_type_prefixes) &&
                                    targ->simple_type.name.parts.size() >= 2 &&
                                    targ->simple_type.name.parts.at(0).AsSingleWord() == "phmap" &&
                                    targ->simple_type.name.parts.at(1).AsSingleWord(SingleWordFlags::ignore_template_args) == "Less" &&
                                    targ->simple_type.name.parts.at(1).TemplateArgsMatch(*elem_type)
                                )
                                {
                                    part.template_args->args.pop_back();
                                }
                            }
                        }
                    }
                }
            }
        }
    };
}
