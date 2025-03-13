#pragma once

#include "cppdecl/detail/copyable_unique_ptr.h"
#include "cppdecl/detail/enum_flags.h"
#include "cppdecl/detail/overload.h"

#include <cassert>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace cppdecl
{
    enum class ToStringMode
    {
        pretty,
        debug,
    };

    // cv-qualifiers, and/or `__restrict`.
    enum class Qualifiers
    {
        const_ = 1 << 0,
        volatile_ = 1 << 1,
        restrict_ = 1 << 2,
    };
    TYPENAMES_FLAG_OPERATORS(Qualifiers)

    [[nodiscard]] std::string ToString(Qualifiers quals, char sep = ' ');

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
        explicitly_signed = 1 << 1, // Explicitly `signed`. Mutually exclusive with `unsigned_`.
        redundant_int = 1 << 2, // Explicitly `... int`, where the `int` is unnecessary. It's removed from the output and only the flag is kept.
    };
    TYPENAMES_FLAG_OPERATORS(SimpleTypeFlags)

    struct TemplateArgument;

    struct TemplateArgumentList
    {
        std::vector<TemplateArgument> args;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // An unqualified name, possibly with template arguments.
    struct UnqualifiedName
    {
        std::string name;

        // This is optional to distinguish an empty list from no list.
        std::optional<TemplateArgumentList> template_args;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // A qualified name.
    struct QualifiedName
    {
        std::vector<UnqualifiedName> parts;
        bool force_global_scope = false; // True if this has a leading `::`.

        // Returns true if this is an invalid empty name.
        [[nodiscard]] bool IsEmpty() const
        {
            return parts.empty();
        }

        // Returns true if this name has at least one `::` in it.
        [[nodiscard]] bool IsQualified() const
        {
            return force_global_scope || parts.size() > 1;
        }

        // If this name is a single word, returns that word. Otherwise returns empty.
        [[nodiscard]] std::string_view AsSingleWord() const
        {
            if (!force_global_scope && parts.size() == 1 && !parts.front().template_args)
                return parts.front().name;
            else
                return {};
        }

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // A type, maybe cv-qualified, but without any pointer qualifiers and such.
    // This also corresponds to the decl-specifier-seq, aka the common part of the type shared between all variables in a declaration.
    struct SimpleType
    {
        Qualifiers quals{};
        SimpleTypeFlags flags{};

        // The type name. Never includes `signed` or `unsigned`, that's in `flags`.
        QualifiedName name;

        // Returns true if this is an invalid empty type.
        [[nodiscard]] bool IsEmpty() const
        {
            return name.IsEmpty();
        }

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    struct TypeModifier;

    // A full type.
    struct Type
    {
        SimpleType simple_type;
        // The first modifier is the closest to the variable name in the declaration.
        std::vector<TypeModifier> modifiers;

        // Returns true if this is an invalid empty type.
        [[nodiscard]] bool IsEmpty() const
        {
            return simple_type.IsEmpty();
        }

        // Returns the qualifiers from the top-level modifier (i.e. the first one, if any), or from `simple_type` if there are no modifiers.
        [[nodiscard]] Qualifiers GetTopLevelQualifiers() const;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // Things for non-type template arguments: [

    // Some punctuation token.
    struct PunctuationToken
    {
        std::string value;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // Some token that looks like a number.
    struct NumberToken
    {
        std::string value;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // A string or character literal.
    struct StringLiteral
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

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
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

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // This isn't really a proper expression, it's a fairly loose hierarchy of tokens.
    // We use this e.g. for non-type template arguments.
    struct PseudoExpr
    {
        // For simplicity, identifiers go into `SimpleType`, even if not technically types.
        using Token = std::variant<SimpleType, PunctuationToken, NumberToken, StringLiteral, PseudoExprList, TemplateArgumentList>;

        std::vector<Token> tokens;

        [[nodiscard]] bool IsEmpty() const
        {
            return tokens.empty();
        }

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // ] -- Things for non-type template arguments.

    // A declaration. Maybe named, maybe not.
    struct Decl
    {
        Type type;
        QualifiedName name;

        // Returns true if this is an invalid empty declaration.
        [[nodiscard]] bool IsEmpty() const
        {
            // Note, not checking the name. It can legally be empty.
            return type.IsEmpty();
        }

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
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

        // Returns true if the parsing was ambiguous.
        // Then you can consult `ambiguous_alternative` for the list of alternative parses, either in this object or in some nested declarations,
        //   such as function parameters.
        [[nodiscard]] bool IsAmbiguous() const
        {
            return bool(ambiguous_alternative) || has_nested_ambiguities;
        }

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    using MaybeAmbiguousDecl = MaybeAmbiguous<Decl>;
    using MaybeAmbiguousType = MaybeAmbiguous<Type>;


    // A template argument.
    struct TemplateArgument
    {
        using Variant = std::variant<MaybeAmbiguousType, PseudoExpr>;
        Variant var;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // A base class for type modifiers (applied by decorators) that have cv-qualifiers (and/or restrict-qualifiers, so references do count).
    struct QualifiedModifier
    {
        Qualifiers quals{};

        // ToDebugString is implemented in derived classes.
    };

    // A pointer to...
    struct Pointer : QualifiedModifier
    {
        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // A reference to...
    // Can't have cv-qualifiers, but can have `__restrict`.
    struct Reference : QualifiedModifier
    {
        RefQualifiers kind = RefQualifiers::lvalue; // Will never be `none.

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // A member pointer to... (a variable/function of type...)
    struct MemberPointer : QualifiedModifier
    {
        QualifiedName base;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // An array of... (elements of type...)
    struct Array
    {
        // This can be empty.
        PseudoExpr size;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // A function returning...
    struct Function
    {
        std::vector<MaybeAmbiguousDecl> params;
        Qualifiers quals{};
        RefQualifiers ref_quals{};

        bool noexcept_ = false;

        // This function has no parameters, which as spelled as `(void)` in C style.
        // This can only be set if `params` is empty.
        bool c_style_void_params = false;

        // This function has a trailing C-style variadic `...` parameter.
        bool c_style_variadic = false;
        // This function has a trailing C-style variadic `...` parameter, which lacks the comma before it (illegal since C++26).
        // This can only be set if `c_style_variadic` is also set.
        bool c_style_variadic_without_comma = false;

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };

    // A type modifier that is a part of a declarator (i.e. applies to only variable in a declaration),
    //   such as "pointer", "array" (unbounded or fixed-size), etc.
    struct TypeModifier
    {
        using Variant = std::variant<Pointer, Reference, MemberPointer, Array, Function>;
        Variant var;

        // Returns the qualifiers of this modifier, if any.
        [[nodiscard]] Qualifiers GetQualifiers() const
        {
            return std::visit(Overload{
                [](const QualifiedModifier &q){return q.quals;},
                [](const auto &){return Qualifiers{};},
            }, var);
        }

        [[nodiscard]] std::string ToDebugString(ToStringMode mode) const;
    };


    // --- Function definitions:

    inline std::string ToString(Qualifiers quals, char sep)
    {
        std::string ret;

        bool first = true;
        auto quals_copy = quals;
        for (Qualifiers bit{1}; bool(quals_copy); bit <<= 1)
        {
            if (bool(quals_copy & bit))
            {
                if (first)
                    first = false;
                else
                    ret += sep;

                quals_copy &= ~bit;

                switch (bit)
                {
                  case Qualifiers::const_:
                    ret += "const";
                    continue;
                  case Qualifiers::volatile_:
                    ret += "volatile";
                    continue;
                  case Qualifiers::restrict_:
                    ret += "restrict";
                    continue;
                }
                assert(false && "Unknown enum.");
                ret += "??";
            }
        }
        return ret;
    }

    inline std::string TemplateArgumentList::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                std::string ret = "[";
                bool first = true;
                for (const TemplateArgument &arg : args)
                {
                    if (first)
                        first = false;
                    else
                        ret += ',';

                    ret += arg.ToDebugString(mode);
                }
                ret += ']';
                return ret;
            }
            break;
          case ToStringMode::pretty:
            {
                std::string ret;
                if (args.empty())
                {
                    ret = "empty template arguments";
                }
                else
                {
                    ret = std::to_string(args.size());
                    ret += " template argument";
                    if (args.size() != 1)
                        ret += 's';
                    ret += ": [";

                    std::size_t i = 0;
                    for (const TemplateArgument &arg : args)
                    {
                        if (i > 0)
                            ret += ", ";

                        i++;
                        ret += std::to_string(i);
                        ret += ". ";
                        ret += arg.ToDebugString(mode);
                    }
                    ret += ']';
                }
                return ret;
            }
            break;
        }
    }

    inline std::string UnqualifiedName::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                std::string ret = "{name=\"" + name + "\"";
                if (template_args)
                {
                    ret += ",targs=";
                    ret += template_args->ToDebugString(mode);
                }

                ret += '}';
                return ret;
            }
            break;
          case ToStringMode::pretty:
            {
                std::string ret;
                ret += '`';
                ret += name;
                ret += '`';
                if (template_args)
                {
                    ret += " with ";
                    ret += template_args->ToDebugString(mode);
                }
                return ret;
            }
            break;
        }
    }

    inline std::string QualifiedName::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                std::string ret;
                ret += "{global_scope=";
                ret += force_global_scope ? "true" : "false";
                ret += ",parts=[";
                for (bool first = true; const auto &part : parts)
                {
                    if (first)
                        first = false;
                    else
                        ret += ',';

                    ret += part.ToDebugString(mode);
                }
                ret += "]}";
                return ret;
            }
            break;
          case ToStringMode::pretty:
            {
                std::string ret;
                if (force_global_scope)
                    ret += "::";

                bool first = true;
                for (const auto &part : parts)
                {
                    if (first)
                        first = false;
                    else
                        ret += "::";

                    ret += part.ToDebugString(mode);
                }
                return ret;
            }
            break;
        }
    }

    inline std::string SimpleType::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                std::string ret = "{flags=[";
                { // Flags.
                    bool first = true;
                    auto flags_copy = flags;
                    for (SimpleTypeFlags bit{1}; bool(flags_copy); bit <<= 1)
                    {
                        if (bool(flags_copy & bit))
                        {
                            if (first)
                                first = false;
                            else
                                ret += ',';

                            flags_copy &= ~bit;

                            switch (bit)
                            {
                              case SimpleTypeFlags::unsigned_:
                                ret += "unsigned";
                                continue;
                              case SimpleTypeFlags::explicitly_signed:
                                ret += "explicitly_signed";
                                continue;
                              case SimpleTypeFlags::redundant_int:
                                ret += "redundant_int";
                                continue;
                            }
                            assert(false && "Unknown enum.");
                            ret += "??";
                        }
                    }
                }

                ret += "],quals=[";
                ret += ToString(quals, ',');

                ret += "],name=";
                ret += name.ToDebugString(mode);

                ret += '}';
                return ret;
            }
            break;
          case ToStringMode::pretty:
            {
                std::string ret;

                ret += ToString(quals);

                if (bool(flags & SimpleTypeFlags::unsigned_))
                    ret += "unsigned ";
                if (bool(flags & SimpleTypeFlags::explicitly_signed))
                    ret += "explicitly signed ";

                ret += name.ToDebugString(mode);

                if (bool(flags & SimpleTypeFlags::redundant_int))
                    ret += " with explicit `int`";

                return ret;
            }
            break;
        }
    }

    inline Qualifiers Type::GetTopLevelQualifiers() const
    {
        if (modifiers.empty())
            return simple_type.quals;
        else
            return modifiers.front().GetQualifiers();
    }

    inline std::string Type::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
          case ToStringMode::pretty:
            {
                std::string ret;
                for (const TypeModifier &mod : modifiers)
                {
                    ret += mod.ToDebugString(mode);
                    ret += ' ';
                }
                ret += simple_type.ToDebugString(mode);
                return ret;
            }
            break;
        }

    }

    inline std::string PunctuationToken::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                return "punct`" + value + "`";
            }
            break;
          case ToStringMode::pretty:
            {
                return "punctuation `" + value + "`";
            }
            break;
        }
    }

    inline std::string NumberToken::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                return "num`" + value + "`";
            }
            break;
          case ToStringMode::pretty:
            {
                return "number " + value;
            }
            break;
        }
    }

    inline std::string StringLiteral::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                std::string ret;
                switch (kind)
                {
                    case Kind::character:  ret += "char`"; break;
                    case Kind::string:     ret += "str`"; break;
                    case Kind::raw_string: ret += "rawstr`"; break;
                }
                ret += value;
                ret += '`';
                if (!literal_suffix.empty())
                {
                    ret += "(suffix`";
                    ret += literal_suffix;
                    ret += "`)";
                }
                if (!raw_string_delim.empty())
                {
                    ret += "(delim`";
                    ret += raw_string_delim;
                    ret += "`)";
                }
                return ret;
            }
            break;
          case ToStringMode::pretty:
            {
                std::string ret;
                switch (kind)
                {
                    case Kind::character:  ret += "character `"; break;
                    case Kind::string:     ret += "string `"; break;
                    case Kind::raw_string: ret += "raw string `"; break;
                }
                ret += value;
                ret += '`';
                if (!literal_suffix.empty())
                {
                    ret += "with suffix `";
                    ret += literal_suffix;
                    ret += "`";
                }
                if (!raw_string_delim.empty())
                {
                    ret += "with delimiter `";
                    ret += raw_string_delim;
                    ret += "`";
                }
                return ret;
            }
            break;
        }
    }

    inline std::string PseudoExprList::ToDebugString(ToStringMode mode) const
    {
        const char *braces = nullptr;
        switch (kind)
        {
            case Kind::parentheses: braces = "()"; break;
            case Kind::curly:       braces = "{}"; break;
            case Kind::square:      braces = "[]"; break;
        }

        switch (mode)
        {
          case ToStringMode::debug:
            {
                std::string ret = "list";
                ret += braces[0];

                bool first = true;
                for (const auto &elem : elems)
                {
                    if (first)
                        first = false;
                    else
                        ret += ',';

                    ret += elem.ToDebugString(mode);
                }

                ret += braces[1];

                if (has_trailing_comma)
                    ret += "(has trailing comma)";
                return ret;
            }
            break;
          case ToStringMode::pretty:
            {
                std::string ret = "list ";
                ret += braces[0];

                bool first = true;
                for (const auto &elem : elems)
                {
                    if (first)
                        first = false;
                    else
                        ret += ", ";

                    ret += elem.ToDebugString(mode);
                }
                if (has_trailing_comma)
                    ret += ",";

                ret += braces[1];

                if (has_trailing_comma)
                    ret += " with trailing comma";
                return ret;
            }
            break;
        }
    }

    inline std::string PseudoExpr::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                std::string ret = "[";
                bool first = true;
                for (const auto &token : tokens)
                {
                    if (first)
                        first = false;
                    else
                        ret += ',';

                    ret += std::visit([&](const auto &elem){return elem.ToDebugString(mode);}, token);
                }
                ret += ']';
                return ret;
            }
            break;
          case ToStringMode::pretty:
            {
                std::string ret = "expression [";
                bool first = true;
                for (const auto &token : tokens)
                {
                    if (first)
                        first = false;
                    else
                        ret += ", ";

                    ret += std::visit([&](const auto &elem){return elem.ToDebugString(mode);}, token);
                }
                ret += ']';
                return ret;
            }
            break;
        }
    }

    inline std::string Decl::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                std::string ret = "{type=\"";
                ret += type.ToDebugString(mode);
                ret += "\",name=\"";
                ret += name.ToDebugString(mode);
                ret += "\"}";
                return ret;
            }
            break;
          case ToStringMode::pretty:
            {
                std::string ret;
                if (name.IsEmpty())
                {
                    ret += "unnamed";
                }
                else
                {
                    ret += "named ";
                    ret += name.ToDebugString(mode);
                }
                ret += " of type: ";
                ret += type.ToDebugString(mode);
                return ret;
            }
            break;
        }
    }

    inline std::string TemplateArgument::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
            {
                return std::visit(Overload{
                    [&](const MaybeAmbiguousType &type){return "type" + type.ToDebugString(mode);},
                    [&](const PseudoExpr &expr){return "expr" + expr.ToDebugString(mode);},
                }, var);
            }
            break;
          case ToStringMode::pretty:
            {
                return std::visit(Overload{
                    [&](const MaybeAmbiguousType &type){return "possibly type: " + type.ToDebugString(mode);},
                    [&](const PseudoExpr &expr){return expr.ToDebugString(mode);}, // This shouldn't need a prefix.
                }, var);
            }
            break;
        }
    }

    template <typename T>
    inline std::string MaybeAmbiguous<T>::ToDebugString(ToStringMode mode) const
    {
        if (!ambiguous_alternative)
        {
            return T::ToDebugString(mode);
        }
        else
        {
            switch (mode)
            {
              case ToStringMode::debug:
                {
                    std::string ret = "either [";
                    ret += T::ToDebugString(mode);
                    ret += "] or [";
                    ret += ambiguous_alternative->ToDebugString(mode);
                    ret += ']';
                    return ret;
                }
                break;
              case ToStringMode::pretty:
                {
                    std::string ret = "ambiguous, either [";
                    ret += T::ToDebugString(mode);

                    MaybeAmbiguous<T> *cur = ambiguous_alternative.get();
                    do
                    {
                        ret += "] or [";
                        ret += cur->ToDebugString(mode);
                        ret += ']';

                        cur = cur->ambiguous_alternative.get();
                    }
                    while (cur);

                    return ret;
                }
                break;
            }
        }
    }

    inline std::string Pointer::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
          case ToStringMode::pretty:
            {
                std::string ret = ToString(quals);
                if (!ret.empty())
                    ret += ' ';
                ret += "pointer to";
                return ret;
            }
            break;
        }
    }

    inline std::string Reference::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
          case ToStringMode::pretty:
            {
                std::string ret = ToString(quals);
                if (!ret.empty())
                    ret += ' ';

                switch (kind)
                {
                  case RefQualifiers::none:
                    // This should be unreachable.
                    break;
                  case RefQualifiers::lvalue:
                    ret += "lvalue";
                    break;
                  case RefQualifiers::rvalue:
                    ret += "rvalue";
                    break;
                }
                assert(!ret.empty() && "Unknown enum.");
                if (ret.empty())
                    ret += "??";

                ret += " reference to";
                return ret;
            }
            break;
        }
    }

    inline std::string MemberPointer::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
          case ToStringMode::pretty:
            {
                std::string ret = ToString(quals);
                if (!ret.empty())
                    ret += ' ';
                ret += "pointer-to-member of class ";
                ret += base.ToDebugString(mode);
                ret += " of type";
                return ret;
            }
            break;
        }
    }

    inline std::string Array::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
          case ToStringMode::pretty:
            {
                std::string ret;
                if (size.IsEmpty())
                {
                    ret = "array of unknown bound of";
                }
                else
                {
                    ret = "array of size ";
                    ret += size.ToDebugString(mode);
                    ret += " of";
                }
                return ret;
            }
            break;
        }
    }

    inline std::string Function::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
          case ToStringMode::pretty:
            {
                std::string ret = ToString(quals, '-');
                if (!ret.empty())
                    ret += "-qualified ";

                const char *ref_str = nullptr;
                switch (ref_quals)
                {
                  case RefQualifiers::none:
                    ref_str = "";
                    break;
                  case RefQualifiers::lvalue:
                    ref_str = "lvalue-ref";
                    break;
                  case RefQualifiers::rvalue:
                    ref_str = "rvalue-ref";
                    break;
                }
                ret += ref_str;
                if (*ref_str != '\0')
                    ret += "-qualified ";

                if (noexcept_)
                    ret += "noexcept ";

                ret += "function taking ";
                if (params.empty())
                {
                    ret += "no parameters";
                    if (c_style_void_params)
                        ret += " (spelled with C-style void)";
                }
                else
                {
                    ret += std::to_string(params.size());
                    ret += " parameter";
                    if (params.size() != 1)
                        ret += 's';
                    ret += ": [";
                    std::size_t i = 0;
                    for (const auto &elem : params)
                    {
                        if (i > 0)
                            ret += ", ";

                        i++;

                        ret += std::to_string(i);
                        ret += ". ";

                        ret += elem.ToDebugString(mode);
                    }
                    ret += ']';
                }
                if (c_style_variadic)
                {
                    ret += " and a C-style variadic parameter";
                    if (c_style_variadic_without_comma)
                        ret += " (with a missing comma before it)"; // Illegal since C++26.
                }

                ret += ", returning";
                return ret;
            }
            break;
        }
    }

    inline std::string TypeModifier::ToDebugString(ToStringMode mode) const
    {
        switch (mode)
        {
          case ToStringMode::debug:
          case ToStringMode::pretty:
            {
                return std::visit([&](const auto &elem){return elem.ToDebugString(mode);}, var);
            }
            break;
        }
    }
}
