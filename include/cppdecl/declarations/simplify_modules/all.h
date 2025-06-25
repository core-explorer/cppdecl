#pragma once

#include "cppdecl/declarations/simplify_modules/phmap.h"
#include "cppdecl/declarations/simplify.h"

namespace cppdecl
{
    template <typename Derived>
    using BasicFullSimplifyTraits = SimplifyTraits<Derived
        , SimplifyModules::Phmap
    >;

    // Combines all simplification modules that we have.
    struct FullSimplifyTraits : BasicFullSimplifyTraits<FullSimplifyTraits> {};
}
