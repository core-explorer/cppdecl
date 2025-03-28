#pragma once

#include "cppdecl/detail/copyable_unique_ptr.h"
#include "cppdecl/detail/enum_flags.h"
#include "cppdecl/detail/overload.h"
#include "cppdecl/detail/string_helpers.h"

#include <algorithm>
#include <cassert>
#include <optional>
#include <string>
#include <variant>
#include <vector>

// This stores the parsed representation of C++ declarations and types.
// The important types are `cppdecl::Decl` and `cppdecl::Type`.
// See `<cppdecl/declarations/parse.h>` for parsing and `<cppdecl/declarations/to_string.h>` for converting back to strings.

namespace cppdecl
{
    // cv-qualifiers, and/or `__restrict`.
    enum class CvQualifiers
    {
        const_ = 1 << 0,
        volatile_ = 1 << 1,
        restrict_ = 1 << 2,
    };
    CPPDECL_FLAG_OPERATORS(CvQualifiers)

    // The kind of reference, if any.
    enum class RefQualifiers
    {
        none,
        lvalue,
        rvalue,
    };

    // Additional information about a type.
    enum class SimpleTypeFlags
    {
        unsigned_ = 1 << 0,

        // Explicitly `signed`. Mutually exclusive with `unsigned_`. This is usually redundant, unless this is a `char`.
        explicitly_signed = 1 << 1,

        // Explicitly `... int`, where the `int` is unnecessary. It's removed from the output and only the flag is kept.
        // E.g. `long int` and `int long`.
        // Note that we don't set this for `signed int` and `unsigned int` for sanity.
        redundant_int = 1 << 2,

        // This will also have the type name set to `"int". We do this when getting `unsigned` and `signed` without the `int`.
        // We don't do this for `long int` and such. This is intentionally inconsistent, for sanity.
        // This is mutally exclusive
        implied_int = 1 << 3,
    };
    CPPDECL_FLAG_OPERATORS(SimpleTypeFlags)

    struct TemplateArgument;

    struct TemplateArgumentList
    {
        std::vector<TemplateArgument> args;

        friend bool operator==(const TemplateArgumentList &, const TemplateArgumentList &);
    };

    struct UnqualifiedName;

    // A qualified name.
    struct QualifiedName
    {
        std::vector<UnqualifiedName> parts;
        bool force_global_scope = false; // True if this has a leading `::`.

        friend bool operator==(const QualifiedName &, const QualifiedName &b);

        // Returns true if this is an invalid empty name.
        [[nodiscard]] bool IsEmpty() const
        {
            assert(parts.empty() <= !force_global_scope);
            return !force_global_scope && parts.empty();
        }

        // Returns true if this name has at least one `::` in it.
        [[nodiscard]] bool IsQualified() const;

        [[nodiscard]] bool LastComponentIsNormalString() const;

        // Returns true if `parts` isn't empty, and the last component is a regular string.
        [[nodiscard]] bool IsConversionOperatorName() const;

        [[nodiscard]] bool IsDestructorName() const;

        // This can have false negatives, because e.g. `A::B` can be a constructor if you do `using A = B;`.
        // So this merely checks that the two last parts are the same string, and the secibd part has no template arguments (arguments on
        //   the first part are ignored).
        [[nodiscard]] bool CertainlyIsQualifiedConstructorName() const;


        enum class EmptyReturnType
        {
            no,
            yes,
            maybe_unqual_constructor, // Just a lone unqualified name, that doesn't look like a type keyword.
            maybe_qual_constructor_using_typedef, // `A::B`, perhaps `A` is a typedef for `B` and this is a constructor.
        };

        [[nodiscard]] EmptyReturnType IsFunctionNameRequiringEmptyReturnType() const;

        // If this name is a single word, returns that word. Otherwise returns empty.
        // This can return `long long`, `long double`, etc. But `unsigned` and such shouldn't be here, they are in `SimpleTypeFlags`.
        [[nodiscard]] std::string_view AsSingleWord() const;

        // If there's only one part and no `::` forcing the global scope,
        //   calls the same method on that (see `UnqualifiedName::IsBuiltInTypeName()` for details).
        // Otherwise returns false.
        [[nodiscard]] bool IsBuiltInTypeName() const;
    };

    // A type, maybe cv-qualified, but without any pointer qualifiers and such.
    // This also corresponds to the decl-specifier-seq, aka the common part of the type shared between all variables in a declaration.
    struct SimpleType
    {
        CvQualifiers quals{};
        SimpleTypeFlags flags{};

        // The type name. Never includes `signed` or `unsigned`, that's in `flags`.
        QualifiedName name;

        friend bool operator==(const SimpleType &, const SimpleType &);

        // Returns true if this is an invalid empty type.
        [[nodiscard]] bool IsEmpty() const
        {
            assert((!name.IsEmpty() || flags == SimpleTypeFlags{}) && "An empty type should have no flags.");
            return name.IsEmpty();
        }
        // This version doesn't assert. Only for internal use.
        [[nodiscard]] bool IsEmptyUnsafe() const
        {
            return name.IsEmpty();
        }

        // If there are no flags and no qualifiers, returns `name.AsSingleWord()`. Otherwise empty.
        // `name.AsSingleWord()` can also return empty if it doesn't consider the name to be a single word.
        [[nodiscard]] std::string_view AsSingleWord() const
        {
            if (quals == CvQualifiers{} && flags == SimpleTypeFlags{})
                return name.AsSingleWord();
            else
                return {};
        }
    };

    struct TypeModifier;

    // A full type.
    struct Type
    {
        SimpleType simple_type;
        // The first modifier is the top-level one, it's the closest one to the variable name in the declaration.
        std::vector<TypeModifier> modifiers;

        friend bool operator==(const Type &, const Type &);

        // Returns true if this is an invalid empty type.
        // While normally an empty `simple_type` implies empty `modifiers`, it's not always the case.
        // Constructors, destructors and conversion operators will have an empty `simple_type` and normally one modifier.
        [[nodiscard]] bool IsEmpty() const
        {
            return simple_type.IsEmpty() && modifiers.empty();
        }
        // This version doesn't assert. Only for internal use.
        [[nodiscard]] bool IsEmptyUnsafe() const
        {
            return simple_type.IsEmptyUnsafe() && modifiers.empty();
        }

        // Check the top-level modifier.
        template <typename T> [[nodiscard]] bool Is() const {return bool(As<T>());}
        // Returns the top-level modifier if you guess the type correctly, or null otherwise (including if no modifiers).
        template <typename T> [[nodiscard]]       T *As();
        template <typename T> [[nodiscard]] const T *As() const;

        // Returns the qualifiers from the top-level modifier (i.e. the first one, if any), or from `simple_type` if there are no modifiers.
        [[nodiscard]] CvQualifiers GetTopLevelQualifiers() const;

        // If there are no modifiers, returns `simple_type.AsSingleWord()`. Otherwise empty.
        // `simple_type.AsSingleWord()` can also return empty if it doesn't consider the name to be a single word.
        [[nodiscard]] std::string_view AsSingleWord() const
        {
            if (modifiers.empty())
                return simple_type.AsSingleWord();
            else
                return {};
        }

        // Asserts that `this->simple_type` is empty. Replaces it with the one from `other`.
        // Appends all modifiers from `other` to this.
        void AppendType(Type other);
    };

    // Represents `operator@`.
    struct OverloadedOperator
    {
        // The operator being overloaded.
        std::string token;

        friend bool operator==(const OverloadedOperator &, const OverloadedOperator &);
    };

    // Represents `operator T`.
    struct ConversionOperator
    {
        Type target_type;

        friend bool operator==(const ConversionOperator &, const ConversionOperator &);
    };

    // Represents `operator""_blah`.
    struct UserDefinedLiteral
    {
        std::string suffix;

        // Do we have a space between `""` and `_blah`?
        // This is deprecated.
        bool space_before_suffix = false;

        friend bool operator==(const UserDefinedLiteral &, const UserDefinedLiteral &);
    };

    // A destructor name of the form `~Blah`.
    struct DestructorName
    {
        SimpleType simple_type;

        friend bool operator==(const DestructorName &, const DestructorName &);
    };

    // An unqualified name, possibly with template arguments.
    struct UnqualifiedName
    {
        using Variant = std::variant<std::string, OverloadedOperator, ConversionOperator, UserDefinedLiteral, DestructorName>;

        Variant var;

        // This is optional to distinguish an empty list from no list.
        // Always empty if `var` holds a `DestructorName`, because in that case any template arguments
        //   are a stored in the destructor's target type.
        std::optional<TemplateArgumentList> template_args;

        friend bool operator==(const UnqualifiedName &, const UnqualifiedName &);

        // If `var` holds a `std::string`, returns that. Otherwise returns empty.
        [[nodiscard]] std::string_view AsSingleWord() const;

        // Whether `var` holds a `std::string`, with a built-in type name.
        // Note that we return true for `long long`, `long double`, and `double long`.
        // But signedness and constness isn't handled here, for that we have `SimpleTypeFlags`.
        [[nodiscard]] bool IsBuiltInTypeName() const;
    };

    // Things for non-type template arguments: [

    // Some punctuation token.
    struct PunctuationToken
    {
        std::string value;

        friend bool operator==(const PunctuationToken &, const PunctuationToken &);
    };

    // Some token that looks like a number.
    struct NumberToken
    {
        std::string value;

        friend bool operator==(const NumberToken &, const NumberToken &);
    };

    // A string or character literal.
    struct StringOrCharLiteral
    {
        enum class Kind
        {
            character,
            string,
            raw_string,
        };
        Kind kind{};

        enum class Type
        {
            normal,
            wide,
            u8,
            u16,
            u32,
        };
        Type type = Type::normal;

        // The contents are not unescaped.
        std::string value;

        // User-defined literal suffix, if any.
        std::string literal_suffix;

        // This can be non-empty only if `kind == raw_string`.
        // This is the user-specified delimiter between `"` and `(`.
        std::string raw_string_delim;

        friend bool operator==(const StringOrCharLiteral &, const StringOrCharLiteral &);
    };

    struct PseudoExpr;

    // Some list of the form `(...)`, `{...}` or `[...]`.
    struct PseudoExprList
    {
        enum class Kind
        {
            parentheses,
            curly,
            square,
        };
        Kind kind{};

        std::vector<PseudoExpr> elems;

        // Only braced lists can have this.
        bool has_trailing_comma = false;

        friend bool operator==(const PseudoExprList &, const PseudoExprList &);
    };

    // This isn't really a proper expression, it's a fairly loose hierarchy of tokens.
    // We use this e.g. for non-type template arguments.
    struct PseudoExpr
    {
        // For simplicity, identifiers go into `SimpleType`, even if not technically types.
        using Token = std::variant<SimpleType, PunctuationToken, NumberToken, StringOrCharLiteral, PseudoExprList, TemplateArgumentList>;

        std::vector<Token> tokens;

        friend bool operator==(const PseudoExpr &, const PseudoExpr &);

        [[nodiscard]] bool IsEmpty() const
        {
            return tokens.empty();
        }
    };

    // ] -- Things for non-type template arguments.

    // A declaration. Maybe named, maybe not.
    struct Decl
    {
        Type type;
        QualifiedName name;

        friend bool operator==(const Decl &, const Decl &);

        // Returns true if this is an invalid empty declaration.
        [[nodiscard]] bool IsEmpty() const
        {
            // Note, not checking the name. It can legally be empty.
            return type.IsEmpty();
        }
    };


    // A wrapper for a list of ambiguous alternatives. Used for `Decl` and `Type` primarily.
    template <typename T>
    struct MaybeAmbiguous : T
    {
        // If the parsing was ambiguous, this can point to the alternative parse result.
        // In theory multiple alternatives could be chained, but I've yet to find input that causes that.
        copyable_unique_ptr<MaybeAmbiguous<T>> ambiguous_alternative;

        // Another possible ambiguity is in nested declarations (function parameters). If this is the case, this is set recursively in all parents.
        bool has_nested_ambiguities = false;

        MaybeAmbiguous() {}
        MaybeAmbiguous(const T &other) : T(other) {}
        MaybeAmbiguous(T &&other) : T(std::move(other)) {}

        friend bool operator==(const MaybeAmbiguous &, const MaybeAmbiguous &) = default;

        // Returns true if the parsing was ambiguous.
        // Then you can consult `ambiguous_alternative` for the list of alternative parses, either in this object or in some nested declarations,
        //   such as function parameters.
        [[nodiscard]] bool IsAmbiguous() const
        {
            return bool(ambiguous_alternative) || has_nested_ambiguities;
        }
    };

    using MaybeAmbiguousDecl = MaybeAmbiguous<Decl>;


    // A template argument.
    struct TemplateArgument
    {
        using Variant = std::variant<Type, PseudoExpr>;
        Variant var;

        friend bool operator==(const TemplateArgument &, const TemplateArgument &);
    };

    // A base class for type modifiers (applied by decorators) that have cv-qualifiers (and/or restrict-qualifiers, so references do count).
    struct QualifiedModifier
    {
        CvQualifiers quals{};

        friend bool operator==(const QualifiedModifier &, const QualifiedModifier &);
    };

    // A pointer to...
    struct Pointer : QualifiedModifier
    {
        friend bool operator==(const Pointer &, const Pointer &);
    };

    // A reference to...
    // Can't have cv-qualifiers, but can have `__restrict`.
    struct Reference : QualifiedModifier
    {
        RefQualifiers kind = RefQualifiers::lvalue; // Will never be `none.

        friend bool operator==(const Reference &, const Reference &);
    };

    // A member pointer to... (a variable/function of type...)
    struct MemberPointer : QualifiedModifier
    {
        QualifiedName base;

        friend bool operator==(const MemberPointer &, const MemberPointer &);
    };

    // An array of... (elements of type...)
    struct Array
    {
        // This can be empty.
        PseudoExpr size;

        friend bool operator==(const Array &, const Array &);
    };

    // A function returning...
    struct Function
    {
        std::vector<MaybeAmbiguousDecl> params;
        CvQualifiers cv_quals{};
        RefQualifiers ref_quals{};

        bool noexcept_ = false;

        // Uses trailing return type.
        bool uses_trailing_return_type = false;

        // This function has no parameters, which as spelled as `(void)` in C style.
        // This can only be set if `params` is empty.
        bool c_style_void_params = false;

        // This function has a trailing C-style variadic `...` parameter.
        bool c_style_variadic = false;
        // This function has a trailing C-style variadic `...` parameter, which lacks the comma before it (illegal since C++26).
        // This can only be set if `c_style_variadic` is also set.
        bool c_style_variadic_without_comma = false;

        friend bool operator==(const Function &, const Function &);
    };

    // Mostly for internal use. Prefer `TypeModifier::SpelledAfterIdentifier()`.
    // Should this modifier type be spelled to the right of the identifier (if any) or to the left?
    template <typename T> struct ModifierIsSpelledAfterIdentifier {};
    template <> struct ModifierIsSpelledAfterIdentifier<Pointer> : std::false_type {};
    template <> struct ModifierIsSpelledAfterIdentifier<Reference> : std::false_type {};
    template <> struct ModifierIsSpelledAfterIdentifier<MemberPointer> : std::false_type {};
    template <> struct ModifierIsSpelledAfterIdentifier<Array> : std::true_type {};
    template <> struct ModifierIsSpelledAfterIdentifier<Function> : std::true_type {};

    // A type modifier that is a part of a declarator (i.e. applies to only variable in a declaration),
    //   such as "pointer", "array" (unbounded or fixed-size), etc.
    struct TypeModifier
    {
        using Variant = std::variant<Pointer, Reference, MemberPointer, Array, Function>;
        Variant var;

        friend bool operator==(const TypeModifier &, const TypeModifier &);

        // Returns the qualifiers of this modifier, if any.
        [[nodiscard]] CvQualifiers GetQualifiers() const
        {
            return std::visit(Overload{
                [](const QualifiedModifier &q){return q.quals;},
                [](const auto &){return CvQualifiers{};},
            }, var);
        }

        // Should this modifier be spelled to the right of the identifier (if any) or to the left?
        // Depends only on the type of the modifier.
        [[nodiscard]] bool SpelledAfterIdentifier() const
        {
            return std::visit([]<typename T>(const T &){return ModifierIsSpelledAfterIdentifier<T>::value;}, var);
        }

        template <typename T> [[nodiscard]] bool Is() const {return bool(As<T>());}
        template <typename T> [[nodiscard]]       T *As()       {return std::get_if<T>(&var);}
        template <typename T> [[nodiscard]] const T *As() const {return std::get_if<T>(&var);}
    };


    // --- Function definitions:

    inline bool operator==(const TemplateArgumentList &, const TemplateArgumentList &) = default;
    inline bool operator==(const OverloadedOperator &, const OverloadedOperator &) = default;
    inline bool operator==(const ConversionOperator &, const ConversionOperator &) = default;
    inline bool operator==(const UserDefinedLiteral &, const UserDefinedLiteral &) = default;
    inline bool operator==(const DestructorName &, const DestructorName &) = default;
    inline bool operator==(const UnqualifiedName &, const UnqualifiedName &) = default;

    inline std::string_view UnqualifiedName::AsSingleWord() const
    {
        if (!template_args)
        {
            if (auto str = std::get_if<std::string>(&var))
                return *str;
        }

        return "";
    }

    inline bool UnqualifiedName::IsBuiltInTypeName() const
    {
        std::string_view word = AsSingleWord();
        if (word.empty())
            return false;

        // See the comment on this function for more information.
        return
            IsTypeNameKeyword(word) ||
            word == "long long" ||
            word == "long double" ||
            word == "double long";
    }

    inline bool operator==(const QualifiedName &, const QualifiedName &) = default;

    inline bool QualifiedName::IsQualified() const
    {
        return force_global_scope || parts.size() > 1;
    }

    inline bool QualifiedName::LastComponentIsNormalString() const
    {
        return !parts.empty() && std::holds_alternative<std::string>(parts.back().var);
    }

    inline bool QualifiedName::IsConversionOperatorName() const
    {
        return !parts.empty() && std::holds_alternative<ConversionOperator>(parts.back().var);
    }

    inline bool QualifiedName::IsDestructorName() const
    {
        return !parts.empty() && std::holds_alternative<DestructorName>(parts.back().var);
    }

    inline bool QualifiedName::CertainlyIsQualifiedConstructorName() const
    {
        // Need at least two parts.
        if (parts.size() < 2)
            return false;

        // Second part must have no template arguments.
        if (parts.back().template_args)
            return false;

        // Both must be equal strings.
        const std::string *a = std::get_if<std::string>(&parts[parts.size() - 2].var);
        const std::string *b = std::get_if<std::string>(&parts.back().var);
        return a && b && *a == *b;
    }

    inline QualifiedName::EmptyReturnType QualifiedName::IsFunctionNameRequiringEmptyReturnType() const
    {
        if (parts.empty())
            return EmptyReturnType::no;

        if (IsConversionOperatorName() || IsDestructorName())
            return EmptyReturnType::yes;
        if (CertainlyIsQualifiedConstructorName())
            return EmptyReturnType::yes;

        if (!LastComponentIsNormalString())
            return EmptyReturnType::no;

        // An unqualified constructor perhaps?
        // Or a qualified one that uses a typedef, e.g. `using A = B;`, `A::B()`.

        // Qualified constructors apparently can't have template arguments.
        // Unqualified could have them before C++20, but we're nor allowing this here.
        if (parts.size() > 1 && parts.back().template_args)
            return EmptyReturnType::no;

        if (parts.size() > 1)
            return EmptyReturnType::maybe_qual_constructor_using_typedef;
        else
            return EmptyReturnType::maybe_unqual_constructor;
    }

    inline std::string_view QualifiedName::AsSingleWord() const
    {
        if (!force_global_scope && parts.size() == 1)
            return parts.front().AsSingleWord();

        return {};
    }

    inline bool QualifiedName::IsBuiltInTypeName() const
    {
        if (!force_global_scope && parts.size() == 1)
            return parts.front().IsBuiltInTypeName();

        return false;
    }

    inline bool operator==(const SimpleType &, const SimpleType &) = default;
    inline bool operator==(const Type &, const Type &) = default;

    template <typename T>       T *Type::As()       {return modifiers.front().As<T>();}
    template <typename T> const T *Type::As() const {return modifiers.front().As<T>();}

    inline CvQualifiers Type::GetTopLevelQualifiers() const
    {
        if (modifiers.empty())
            return simple_type.quals;
        else
            return modifiers.front().GetQualifiers();
    }

    inline void Type::AppendType(Type other)
    {
        assert(simple_type.IsEmpty());

        simple_type = std::move(other.simple_type);
        modifiers.insert(modifiers.end(), std::make_move_iterator(other.modifiers.begin()), std::make_move_iterator(other.modifiers.end()));
    }

    inline bool operator==(const PunctuationToken &, const PunctuationToken &) = default;
    inline bool operator==(const NumberToken &, const NumberToken &) = default;
    inline bool operator==(const StringOrCharLiteral &, const StringOrCharLiteral &) = default;
    inline bool operator==(const PseudoExprList &, const PseudoExprList &) = default;
    inline bool operator==(const PseudoExpr &, const PseudoExpr &) = default;
    inline bool operator==(const Decl &, const Decl &) = default;
    inline bool operator==(const TemplateArgument &, const TemplateArgument &) = default;
    inline bool operator==(const QualifiedModifier &, const QualifiedModifier &) = default;
    inline bool operator==(const Pointer &, const Pointer &) = default;
    inline bool operator==(const Reference &, const Reference &) = default;
    inline bool operator==(const MemberPointer &, const MemberPointer &) = default;
    inline bool operator==(const Array &, const Array &) = default;
    inline bool operator==(const Function &, const Function &) = default;
    inline bool operator==(const TypeModifier &, const TypeModifier &) = default;
}
