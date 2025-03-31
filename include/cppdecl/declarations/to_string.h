#pragma once

#include "cppdecl/declarations/data.h"
#include "cppdecl/detail/enum_flags.h"
#include "cppdecl/detail/overload.h"
#include "cppdecl/detail/string_helpers.h"

#include <cassert>
#include <cstddef>
#include <string>
#include <utility>

// Converts parsed types/declarations back to code or to a textual description.
// * `ToCode(x, {})` converts to code.
// * `ToString(x, {})` converts to textual description.
// You can pass some flags instead of `{}`, see below. But the default flags should usually be good enough.

namespace cppdecl
{
    enum class ToCodeFlags
    {
        // Style flags: [

        // Avoid printing spaces after commas.
        no_space_after_comma = 1 << 0,

        // `int const` instead of `const int`.
        east_const = 1 << 1,

        // Do `int*x` instead of `int *x`. If you also want to add the space after it, consider using `left_align_pointer` directly.
        no_space_before_pointer = 1 << 2,
        // Do `int * x` instead of `int *x`. If you also want to remove the space before it, consider using `left_align_pointer` directly.
        add_space_after_pointer = 1 << 3,

        // Do `int* x` instead of `int *x`.
        left_align_pointer = no_space_before_pointer | add_space_after_pointer,

        // ] End style flags.

        // Refuse to print trailing return types, convert them to the normal spelling.
        // This is good for making C++-style declarations usable in C.
        force_no_trailing_return_type = 1 << 4,

        // This is only for `Type`s. For other things this will result in an unpredictable behavior.
        // Causes only a half of the type to be emitted, either the left half or the right half. The identifier if any goes between them.
        only_left_half_type = 1 << 5,
        only_right_half_type = 1 << 6,

        only_any_half_type = only_left_half_type | only_right_half_type,
    };
    CPPDECL_FLAG_OPERATORS(ToCodeFlags)

    enum class ToStringFlags
    {
        // Output the debug representation instead of the normal one.
        debug = 1 << 0,
    };
    CPPDECL_FLAG_OPERATORS(ToStringFlags)


    // If `user_friendly` is true, uses `restrict` instead of `__restrict`.
    [[nodiscard]] inline std::string CvQualifiersToString(CvQualifiers quals, char sep = ' ', bool user_friendly = false)
    {
        std::string ret;

        bool first = true;
        auto quals_copy = quals;
        for (CvQualifiers bit{1}; bool(quals_copy); bit <<= 1)
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
                  case CvQualifiers::const_:
                    ret += "const";
                    continue;
                  case CvQualifiers::volatile_:
                    ret += "volatile";
                    continue;
                  case CvQualifiers::restrict_:
                    // `__restrict` is universal. MSVC chokes on `__restrict__`, and `restrict` is C-only (could be an extension in C++).
                    ret += user_friendly ? "restrict" : "__restrict";
                    continue;
                }
                assert(false && "Unknown enum.");
                ret += "??";
            }
        }
        return ret;
    }

    [[nodiscard]] inline std::string RefQualifiersToString(RefQualifiers quals)
    {
        switch (quals)
        {
            case RefQualifiers::none:   return "";
            case RefQualifiers::lvalue: return "&";
            case RefQualifiers::rvalue: return "&&";
        }
        assert(false && "Unknown enum.");
        return "??";
    }


    // Some declarations to break cyclic references: [
    [[nodiscard]] inline std::string ToCode(const TemplateArgument &target, ToCodeFlags flags);
    [[nodiscard]] inline std::string ToString(const TemplateArgument &target, ToStringFlags flags);
    [[nodiscard]] inline std::string ToCode(const Type &target, ToCodeFlags flags, std::size_t skip_first_modifiers = 0);
    [[nodiscard]] inline std::string ToString(const Type &target, ToStringFlags flags);
    [[nodiscard]] inline std::string ToCode(const SimpleType &target, ToCodeFlags flags);
    [[nodiscard]] inline std::string ToString(const SimpleType &target, ToStringFlags flags);
    [[nodiscard]] inline std::string ToCode(const TypeModifier &target, ToCodeFlags flags);
    [[nodiscard]] inline std::string ToString(const TypeModifier &target, ToStringFlags flags);
    [[nodiscard]] inline std::string ToCode(const PseudoExpr &target, ToCodeFlags flags);
    [[nodiscard]] inline std::string ToString(const PseudoExpr &target, ToStringFlags flags);
    // ]


    [[nodiscard]] inline std::string ToCode(const TemplateArgumentList &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret = "<";

        bool first = true;
        for (const TemplateArgument &arg : target.args)
        {
            if (first)
            {
                first = false;
            }
            else
            {
                ret += ',';
                if (!bool(flags & ToCodeFlags::no_space_after_comma))
                    ret += ' ';
            }

            ret += ToCode(arg, flags);
        }

        ret += '>';
        BreakMaximumMunch(ret, 1);

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const TemplateArgumentList &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = "[";
            bool first = true;
            for (const TemplateArgument &arg : target.args)
            {
                if (first)
                    first = false;
                else
                    ret += ',';

                ret += ToString(arg, flags);
            }
            ret += ']';
            return ret;
        }
        else
        {
            std::string ret;
            if (target.args.empty())
            {
                ret = "empty template arguments";
            }
            else
            {
                ret = std::to_string(target.args.size());
                ret += " template argument";
                if (target.args.size() != 1)
                    ret += 's';
                ret += ": [";

                std::size_t i = 0;
                for (const TemplateArgument &arg : target.args)
                {
                    if (i > 0)
                        ret += ", ";

                    i++;

                    if (target.args.size() != 1)
                    {
                        ret += std::to_string(i);
                        ret += ". ";
                    }

                    ret += ToString(arg, flags);
                }
                ret += ']';
            }
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const UnqualifiedName &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret;
        std::visit(Overload{
            [&](const std::string &name)
            {
                ret = name;
            },
            [&](const OverloadedOperator &op)
            {
                ret = "operator";
                ret += op.token;
            },
            [&](const ConversionOperator &conv)
            {
                ret = "operator ";
                ret += ToCode(conv.target_type, flags);
            },
            [&](const UserDefinedLiteral &udl)
            {
                ret = "operator\"\"";
                if (udl.space_before_suffix)
                    ret += ' ';
                ret += udl.suffix;
            },
            [&](const DestructorName &dtor)
            {
                ret = '~';
                ret += ToCode(dtor.simple_type, flags);
            },
        }, target.var);

        if (target.template_args)
            ret += ToCode(*target.template_args, flags);
        return ret;
    }

    [[nodiscard]] inline std::string ToString(const UnqualifiedName &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = "{";

            std::visit(Overload{
                [&](const std::string &name)
                {
                    ret += "name=\"";
                    ret += name;
                    ret += '\"';
                },
                [&](const OverloadedOperator &op)
                {
                    ret += "op=`";
                    ret += op.token;
                    ret += '`';
                },
                [&](const ConversionOperator &conv)
                {
                    ret += "conv=`";
                    ret += ToString(conv.target_type, flags);
                    ret += '`';
                },
                [&](const UserDefinedLiteral &udl)
                {
                    ret += "udl=`";
                    ret += udl.suffix;
                    ret += '`';
                    if (udl.space_before_suffix)
                        ret += "(with space before suffix)";
                },
                [&](const DestructorName &dtor)
                {
                    ret += "dtor=`";
                    ret += ToString(dtor.simple_type, flags);
                    ret += '`';
                },
            }, target.var);

            if (target.template_args)
            {
                ret += ",targs=";
                ret += ToString(*target.template_args, flags);
            }

            ret += '}';
            return ret;
        }
        else
        {
            std::string ret;

            std::visit(Overload{
                [&](const std::string &name)
                {
                    ret += '`';
                    ret += name;
                    ret += '`';
                },
                [&](const OverloadedOperator &op)
                {
                    ret += "overloaded operator `";
                    ret += op.token;
                    ret += '`';
                },
                [&](const ConversionOperator &conv)
                {
                    ret += "conversion operator to [";
                    ret += ToString(conv.target_type, flags);
                    ret += ']';
                },
                [&](const UserDefinedLiteral &udl)
                {
                    ret += "user-defined literal `";
                    ret += udl.suffix;
                    ret += '`';
                    if (udl.space_before_suffix)
                        ret += " (with deprecated space before suffix)";
                },
                [&](const DestructorName &dtor)
                {
                    ret += "destructor for type [";
                    ret += ToString(dtor.simple_type, flags);
                    ret += ']';
                },
            }, target.var);

            if (target.template_args)
            {
                ret += " with ";
                ret += ToString(*target.template_args, flags);
            }
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const QualifiedName &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret;
        if (target.force_global_scope)
            ret = "::";

        bool first = true;
        for (const auto &part : target.parts)
        {
            if (first)
                first = false;
            else
                ret += "::";

            ret += ToCode(part, flags);
        }

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const QualifiedName &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret;
            ret += "{global_scope=";
            ret += target.force_global_scope ? "true" : "false";
            ret += ",parts=[";
            for (bool first = true; const auto &part : target.parts)
            {
                if (first)
                    first = false;
                else
                    ret += ',';

                ret += ToString(part, flags);
            }
            ret += "]}";
            return ret;
        }
        else
        {
            std::string ret;

            if (target.IsEmpty())
            {
                ret += "nothing";
            }
            else
            {
                if (target.force_global_scope)
                    ret += "::";

                bool first = true;
                for (const auto &part : target.parts)
                {
                    if (first)
                        first = false;
                    else
                        ret += "::";

                    ret += ToString(part, flags);
                }
            }

            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const SimpleType &target, ToCodeFlags flags)
    {
        std::string ret;

        auto WriteName = [&]
        {
            if (bool(target.flags & SimpleTypeFlags::explicitly_signed))
            {
                if (!ret.empty())
                    ret += ' ';
                ret += "signed";
            }
            if (bool(target.flags & SimpleTypeFlags::unsigned_))
            {
                if (!ret.empty())
                    ret += ' ';
                ret += "unsigned";
            }

            if (bool(target.flags & SimpleTypeFlags::implied_int))
            {
                assert(target.name.AsSingleWord() == "int");
            }
            else
            {
                if (!ret.empty())
                    ret += ' ';
                ret += ToCode(target.name, flags);
            }

            if (bool(target.flags & SimpleTypeFlags::redundant_int))
            {
                if (!ret.empty())
                    ret += ' ';
                ret += "int";
            }

            // Don't need to handle `implied_int` here because that comes with the type being set to `"int"`.
        };

        auto WriteQuals = [&]
        {
            if (!ret.empty())
                ret += ' ';
            ret += CvQualifiersToString(target.quals);
        };

        if (bool(flags & ToCodeFlags::east_const))
        {
            WriteName();
            WriteQuals();
        }
        else
        {
            WriteQuals();
            WriteName();
        }

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const SimpleType &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = "{flags=[";
            { // Flags.
                bool first = true;
                auto flags_copy = target.flags;
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
                          case SimpleTypeFlags::implied_int:
                            ret += "implied_int";
                            continue;
                        }
                        assert(false && "Unknown enum.");
                        ret += "??";
                    }
                }
            }

            ret += "],quals=[";
            ret += CvQualifiersToString(target.quals, ',', true);

            ret += "],name=";
            ret += ToString(target.name, flags);

            ret += '}';
            return ret;
        }
        else
        {
            std::string ret;

            ret += CvQualifiersToString(target.quals, ' ', true);

            if (bool(target.flags & SimpleTypeFlags::unsigned_))
                ret += "unsigned ";
            if (bool(target.flags & SimpleTypeFlags::explicitly_signed))
                ret += target.name.AsSingleWord() == "char" ? "signed " : "explicitly signed ";

            if (bool(target.flags & SimpleTypeFlags::implied_int))
            {
                assert(target.name.AsSingleWord() == "int");
                ret += "implied ";
            }

            ret += ToString(target.name, flags);

            if (bool(target.flags & SimpleTypeFlags::redundant_int))
                ret += " with explicit `int`";

            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    // If `skip_first_modifiers > 0`, will skip several top-level (first) modifiers.
    [[nodiscard]] inline std::string ToCode(const Type &target, ToCodeFlags flags, std::size_t skip_first_modifiers /* =0 */)
    {
        bool uses_trailing_return_type = false;

        // If `uses_trailing_return_type == false` this is equal to `modifiers.size()`.
        std::size_t trailing_return_type_start_index = skip_first_modifiers;

        while (trailing_return_type_start_index < target.modifiers.size())
        {
            if (!bool(flags & ToCodeFlags::force_no_trailing_return_type))
            {
                if (auto func = std::get_if<Function>(&target.modifiers[trailing_return_type_start_index].var); func && func->uses_trailing_return_type)
                {
                    uses_trailing_return_type = true;
                    trailing_return_type_start_index++;
                    break;
                }
            }
            trailing_return_type_start_index++;
        }

        std::string ret;
        if (!bool(flags & ToCodeFlags::only_right_half_type))
        {
            ret = uses_trailing_return_type ? "auto" : ToCode(target.simple_type, flags & ~ToCodeFlags::only_any_half_type);
        }


        // Normally doesn't erase the space after `::*` unless `even_after_member_pointer == true`.
        auto MaybeErasePrecedingSpace = [&](bool even_after_member_pointer = false)
        {
            // Erase preceding space if needed.
            if (
                ret.ends_with(' ') &&
                (
                    even_after_member_pointer ||
                    // If there's a `:*` before the space, don't erase it. Member pointers look better with spaces.
                    ret.size() < 3 ||
                    ret[ret.size() - 2] != '*' ||
                    ret[ret.size() - 3] != ':'
                )
            )
            {
                ret.pop_back();
            }
        };


        std::size_t pos = trailing_return_type_start_index;

        // This goes deeper as we approach the center of the declaration.
        // This returns true if we need to stop the recursion because the rest is in a trailing return type.
        auto lambda = [&](auto &lambda) -> void
        {
            if (pos <= skip_first_modifiers)
                return;

            pos--;
            const TypeModifier &m = target.modifiers[pos];

            bool spelled_after_identifier = m.SpelledAfterIdentifier();
            bool need_parens =
                ( // Because we're switching from spelled-after-identifier to spelled-before-identifier modifiers.
                    pos >= skip_first_modifiers + 1 &&
                    spelled_after_identifier &&
                    !target.modifiers[pos-1].SpelledAfterIdentifier()
                ) ||
                ( // Because this is a member pointer with a class name that starts with `::`.
                    // This is ambiguous if the target type name isn't a keyword, and needs the parentheses to disambiguate.
                    // But we just add them unconditionally. Relying on keywords rejecting `::` after them sounds jank.
                    std::get_if<MemberPointer>(&m.var) && std::get<MemberPointer>(m.var).base.force_global_scope
                )
                ;

            if (!bool(flags & ToCodeFlags::only_right_half_type))
            {
                if (need_parens)
                {
                    // Space before?
                    // This one is unconditional for now, we could add a flag.
                    if (!ret.empty() && IsIdentifierChar(ret.back()))
                        ret += ' ';

                    ret += '(';
                }

                if (!spelled_after_identifier)
                {
                    MaybeErasePrecedingSpace(std::holds_alternative<MemberPointer>(m.var));

                    // Space before?
                    if (
                        std::holds_alternative<MemberPointer>(m.var)
                        ? !ret.ends_with('(')
                        : (
                            !bool(flags & ToCodeFlags::no_space_before_pointer) &&
                            !ret.empty() && IsIdentifierChar(ret.back())
                        )
                    )
                    {
                        ret += ' ';
                    }

                    ret += ToCode(m, flags & ~ToCodeFlags::only_any_half_type);

                    // Space after?
                    if (std::holds_alternative<MemberPointer>(m.var) || bool(flags & ToCodeFlags::add_space_after_pointer))
                        ret += ' ';
                }
            }

            lambda(lambda);

            if (!bool(flags & ToCodeFlags::only_left_half_type))
            {
                if (need_parens)
                {
                    MaybeErasePrecedingSpace(true);
                    ret += ')';
                }

                MaybeErasePrecedingSpace();

                if (spelled_after_identifier)
                    ret += ToCode(m, flags & ~ToCodeFlags::only_any_half_type);
            }
        };
        lambda(lambda);

        if (uses_trailing_return_type)
        {
            if (!bool(flags & ToCodeFlags::only_left_half_type))
                ret += ToCode(target, flags & ~ToCodeFlags::only_any_half_type, trailing_return_type_start_index);
        }
        else
        {
            // This is in the `else` as a little optimization.
            if (!bool(flags & ToCodeFlags::only_left_half_type))
                MaybeErasePrecedingSpace(true);
        }

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const Type &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret;
            for (const TypeModifier &mod : target.modifiers)
            {
                ret += ToString(mod, flags);
                ret += ' ';
            }
            ret += ToString(target.simple_type, flags);
            return ret;
        }
        else
        {
            std::string ret;
            if (target.IsEmpty())
            {
                ret += "no type";
            }
            else
            {
                for (const TypeModifier &mod : target.modifiers)
                {
                    ret += ToString(mod, flags);
                    ret += ' ';
                }
            }
            ret += ToString(target.simple_type, flags);
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const PunctuationToken &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        return target.value;
    }

    [[nodiscard]] inline std::string ToString(const PunctuationToken &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            return "punct`" + target.value + "`";
        }
        else
        {
            return "punctuation `" + target.value + "`";
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const NumberToken &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        return target.value;
    }

    [[nodiscard]] inline std::string ToString(const NumberToken &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            return "num`" + target.value + "`";
        }
        else
        {
            return "number " + target.value;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const StringOrCharLiteral &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret;

        switch (target.type)
        {
            case StringOrCharLiteral::Type::normal: break;
            case StringOrCharLiteral::Type::wide:   ret += "L"; break;
            case StringOrCharLiteral::Type::u8:     ret += "u8"; break;
            case StringOrCharLiteral::Type::u16:    ret += "u"; break;
            case StringOrCharLiteral::Type::u32:    ret += "U"; break;
        }

        switch (target.kind)
        {
          case StringOrCharLiteral::Kind::character:
            ret += '\'';
            break;
          case StringOrCharLiteral::Kind::string:
            ret += '"';
            break;
          case StringOrCharLiteral::Kind::raw_string:
            ret += "R\"";
            ret += target.raw_string_delim;
            ret += '(';
            break;
        }

        ret += target.value;

        switch (target.kind)
        {
          case StringOrCharLiteral::Kind::character:
            ret += '\'';
            break;
          case StringOrCharLiteral::Kind::string:
            ret += '"';
            break;
          case StringOrCharLiteral::Kind::raw_string:
            ret += ')';
            ret += target.raw_string_delim;
            ret += '"';
            break;
        }

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const StringOrCharLiteral &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret;
            switch (target.kind)
            {
                case StringOrCharLiteral::Kind::character:  ret += "char`"; break;
                case StringOrCharLiteral::Kind::string:     ret += "str`"; break;
                case StringOrCharLiteral::Kind::raw_string: ret += "rawstr`"; break;
            }
            ret += target.value;
            ret += '`';
            if (!target.literal_suffix.empty())
            {
                ret += "(suffix`";
                ret += target.literal_suffix;
                ret += "`)";
            }
            if (!target.raw_string_delim.empty())
            {
                ret += "(delim`";
                ret += target.raw_string_delim;
                ret += "`)";
            }
            return ret;
        }
        else
        {
            std::string ret;
            switch (target.kind)
            {
                case StringOrCharLiteral::Kind::character:  ret += "character `"; break;
                case StringOrCharLiteral::Kind::string:     ret += "string `"; break;
                case StringOrCharLiteral::Kind::raw_string: ret += "raw string `"; break;
            }
            ret += target.value;
            ret += '`';
            if (!target.literal_suffix.empty())
            {
                ret += "with suffix `";
                ret += target.literal_suffix;
                ret += "`";
            }
            if (!target.raw_string_delim.empty())
            {
                ret += "with delimiter `";
                ret += target.raw_string_delim;
                ret += "`";
            }
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const PseudoExprList &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret;

        switch (target.kind)
        {
            case PseudoExprList::Kind::parentheses: ret += '('; break;
            case PseudoExprList::Kind::curly:       ret += '{'; break;
            case PseudoExprList::Kind::square:      ret += '['; break;
        }

        bool first = true;
        for (const PseudoExpr &elem : target.elems)
        {
            if (first)
            {
                first = false;
            }
            else
            {
                ret += ',';
                if (!bool(flags & ToCodeFlags::no_space_after_comma))
                    ret += ' ';
            }

            ret += ToCode(elem, flags);
        }

        switch (target.kind)
        {
            case PseudoExprList::Kind::parentheses: ret += ')'; break;
            case PseudoExprList::Kind::curly:       ret += '}'; break;
            case PseudoExprList::Kind::square:      ret += ']'; break;
        }

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const PseudoExprList &target, ToStringFlags flags)
    {
        const char *braces = nullptr;
        switch (target.kind)
        {
            case PseudoExprList::Kind::parentheses: braces = "()"; break;
            case PseudoExprList::Kind::curly:       braces = "{}"; break;
            case PseudoExprList::Kind::square:      braces = "[]"; break;
        }

        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = "list";
            ret += braces[0];

            bool first = true;
            for (const auto &elem : target.elems)
            {
                if (first)
                    first = false;
                else
                    ret += ',';

                ret += ToString(elem, flags);
            }

            ret += braces[1];

            if (target.has_trailing_comma)
                ret += "(has trailing comma)";
            return ret;
        }
        else
        {
            std::string ret = "list ";
            ret += braces[0];

            bool first = true;
            for (const auto &elem : target.elems)
            {
                if (first)
                    first = false;
                else
                    ret += ", ";

                ret += ToString(elem, flags);
            }
            if (target.has_trailing_comma)
                ret += ",";

            ret += braces[1];

            if (target.has_trailing_comma)
                ret += " with trailing comma";
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const PseudoExpr &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret;

        for (const auto &token : target.tokens)
        {
            std::string elem_str = std::visit([&](const auto &elem){return ToCode(elem, flags);}, token);

            // Separating whitespace between identifiers.
            bool need_separating_whitespace = !ret.empty() && !elem_str.empty() && IsIdentifierChar(ret.back()) && IsIdentifierChar(elem_str.front());
            if (need_separating_whitespace)
                ret += ' ';

            ret += elem_str;


            // Avoid maximum munch by inserting whitespace between tokens.
            if (!need_separating_whitespace)
                BreakMaximumMunch(ret, elem_str.size());
        }

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const PseudoExpr &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = "[";
            bool first = true;
            for (const auto &token : target.tokens)
            {
                if (first)
                    first = false;
                else
                    ret += ',';

                ret += std::visit([&](const auto &elem){return ToString(elem, flags);}, token);
            }
            ret += ']';
            return ret;
        }
        else
        {
            std::string ret = "[";
            bool first = true;
            for (const auto &token : target.tokens)
            {
                if (first)
                    first = false;
                else
                    ret += ", ";

                ret += std::visit([&](const auto &elem){return ToString(elem, flags);}, token);
            }
            ret += ']';
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const Decl &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret;

        if (target.name.IsEmpty())
        {
            // Purely an optimization, to avoid assmebling the type from two halves.
            ret = ToCode(target.type, flags);
        }
        else
        {
            ret = ToCode(target.type, flags | ToCodeFlags::only_left_half_type);

            std::string name_str = ToCode(target.name, flags);

            // Separating whitespace if needed.
            if (!ret.empty() && !name_str.empty() && IsIdentifierChar(ret.back()) && IsIdentifierChar(name_str.front()))
                ret += ' ';
            ret += name_str;

            ret += ToCode(target.type, flags | ToCodeFlags::only_right_half_type);
        }

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const Decl &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = "{type=\"";
            ret += ToString(target.type, flags);
            ret += "\",name=\"";
            ret += ToString(target.name, flags);
            ret += "\"}";
            return ret;
        }
        else
        {
            std::string ret;
            std::string type_str = ToString(target.type, flags);
            std::string_view type_view = type_str;

            // If this is a function type that returns nothing, adjust it to say "a constructor" instead of "a function".
            if (
                target.type.simple_type.IsEmpty() && !target.type.modifiers.empty() &&
                std::holds_alternative<Function>(target.type.modifiers.front().var) &&
                target.name.LastComponentIsNormalString() &&
                ConsumeWord(type_view, "a function")
            )
            {
                static constexpr std::string_view suffix = ", returning nothing";
                if (type_view.ends_with(suffix))
                    type_view.remove_suffix(suffix.size());
                else
                    assert(false); // Hmm.

                std::string new_str = "a constructor";
                new_str += type_view;
                type_str = std::move(new_str);
                type_view = type_str;
            }

            // Similarly for destructors.
            if (
                target.type.simple_type.IsEmpty() && !target.type.modifiers.empty() &&
                std::holds_alternative<Function>(target.type.modifiers.front().var) &&
                target.name.IsDestructorName() &&
                ConsumeWord(type_view, "a function")
            )
            {
                static constexpr std::string_view suffix = ", returning nothing";
                if (type_view.ends_with(suffix))
                    type_view.remove_suffix(suffix.size());
                else
                    assert(false); // Hmm.

                std::string new_str = "a destructor";
                new_str += type_view;
                type_str = std::move(new_str);
                type_view = type_str;
            }

            if (target.name.IsEmpty())
            {
                ret += "unnamed";
                if (ConsumeWord(type_view, "a") || ConsumeWord(type_view, "an"))
                {
                    // If the type starts with `a `, remove that.
                    ret += type_view;
                }
                else if (target.type.IsEmpty())
                {
                    ret += " with no type"; // This shouldn't happen?
                }
                else
                {
                    ret += " of type ";
                    ret += type_str;
                }
            }
            else
            {
                ret += ToString(target.name, flags);
                if (StartsWithWord(type_str, "a"))
                {
                    ret += ", ";
                    ret += type_str;
                }
                else if (target.type.IsEmpty())
                {
                    if (target.name.IsConversionOperatorName() || target.name.IsDestructorName())
                    {
                        // Nothing.
                    }
                    else if (target.name.LastComponentIsNormalString())
                    {
                        ret += ", a constructor without a parameter list"; // Right?
                    }
                    else
                    {
                        ret += " with no type"; // This shouldn't happen?
                    }
                }
                else
                {
                    ret += " of type ";
                    ret += type_str;
                }
            }

            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const TemplateArgument &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        return std::visit([&](const auto &elem){return ToCode(elem, flags);}, target.var);
    }

    [[nodiscard]] inline std::string ToString(const TemplateArgument &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            return std::visit(Overload{
                [&](const Type &type){return "type:" + ToString(type, flags);},
                [&](const PseudoExpr &expr){return "expr" + ToString(expr, flags);},
            }, target.var);
        }
        else
        {
            return std::visit(Overload{
                [&](const Type &type)
                {
                    std::string type_str = ToString(type, flags);
                    std::string_view type_view = type_str;
                    (void)ConsumePunctuation(type_view, "type "); // Remove the word "type", if any.

                    std::string ret = "possibly type: ";
                    ret += type_view;
                    return ret;
                },
                [&](const PseudoExpr &expr)
                {
                    return "non-type: " + ToString(expr, flags);
                },
            }, target.var);
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    // `MaybeAmbiguous` doesn't need a custom `ToCode()` here we don't want to print ambiguities there.
    // So only `ToString()`.
    template <typename T>
    [[nodiscard]] inline std::string ToString(const MaybeAmbiguous<T> &target, ToStringFlags flags)
    {
        if (!target.ambiguous_alternative)
        {
            return ToString(static_cast<const T &>(target), flags);
        }
        else
        {
            if (bool(flags & ToStringFlags::debug))
            {
                std::string ret = "either [";
                ret += ToString(static_cast<const T &>(target), flags);
                ret += "] or [";
                ret += ToString(*target.ambiguous_alternative, flags);
                ret += ']';
                return ret;
            }
            else
            {
                std::string ret = "ambiguous, either [";
                ret += ToString(static_cast<const T &>(target), flags);

                MaybeAmbiguous<T> *cur = target.ambiguous_alternative.get();
                do
                {
                    ret += "] or [";
                    ret += ToString(static_cast<const T &>(*cur), flags);

                    cur = cur->ambiguous_alternative.get();
                }
                while (cur);

                ret += ']';

                return ret;
            }
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const Pointer &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        return "*" + CvQualifiersToString(target.quals);
    }

    [[nodiscard]] inline std::string ToString(const Pointer &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = CvQualifiersToString(target.quals, ' ', true);
            if (!ret.empty())
                ret += ' ';
            ret += "pointer to";
            return ret;
        }
        else
        {
            std::string ret = "a ";
            ret += CvQualifiersToString(target.quals, ' ', true);
            if (target.quals != CvQualifiers{})
                ret += ' ';
            ret += "pointer to";
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const Reference &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret = RefQualifiersToString(target.kind);
        ret += CvQualifiersToString(target.quals);
        return ret;
    }

    [[nodiscard]] inline std::string ToString(const Reference &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = CvQualifiersToString(target.quals, ' ', true);
            if (!ret.empty())
                ret += ' ';

            switch (target.kind)
            {
              case RefQualifiers::none:
                assert(false && "No reference type specified.");
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
        else
        {
            std::string ret;
            if (target.kind != RefQualifiers::none && target.quals == CvQualifiers{})
                ret = "an ";
            else
                ret = "a ";

            ret += CvQualifiersToString(target.quals, ' ', true);
            if (target.kind != RefQualifiers::none)
                ret += ' ';

            switch (target.kind)
            {
              case RefQualifiers::lvalue:
                ret += "lvalue reference to";
                break;
              case RefQualifiers::rvalue:
                ret += "rvalue reference to";
                break;
              default:
                assert(false && "Unknown enum.");
            }
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const MemberPointer &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret = ToCode(target.base, flags);
        ret += "::*";
        ret += CvQualifiersToString(target.quals);
        return ret;
    }

    [[nodiscard]] inline std::string ToString(const MemberPointer &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret = CvQualifiersToString(target.quals, ' ', true);
            if (!ret.empty())
                ret += ' ';
            ret += "pointer-to-member of class ";
            ret += ToString(target.base, flags);
            ret += " of type";
            return ret;
        }
        else
        {
            std::string ret = "a ";
            ret += CvQualifiersToString(target.quals, ' ', true);
            if (target.quals != CvQualifiers{})
                ret += ' ';
            ret += "pointer-to-member of class ";
            ret += ToString(target.base, flags);
            ret += " of type";
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const Array &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        std::string ret = "[";
        ret += ToCode(target.size, flags);
        ret += ']';
        return ret;
    }

    [[nodiscard]] inline std::string ToString(const Array &target, ToStringFlags flags)
    {
        if (bool(flags & ToStringFlags::debug))
        {
            std::string ret;
            if (target.size.IsEmpty())
            {
                ret = "array of unknown bound of";
            }
            else
            {
                ret = "array of size ";
                ret += ToString(target.size, flags);
                ret += " of";
            }
            return ret;
        }
        else
        {
            std::string ret;
            if (target.size.IsEmpty())
            {
                ret = "an array of unknown bound of";
            }
            else
            {
                ret = "an array of size ";
                ret += ToString(target.size, flags);
                ret += " of";
            }
            return ret;
        }

        assert(false && "Unknown enum.");
        return "??";
    }

    [[nodiscard]] inline std::string ToCode(const Function &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        // It's up to the caller to replace their type with `auto` if any of the function modifiers have that flag set.
        // And also the caller must paste the trailing return type after this string (we add `->` ourselves).

        std::string ret = "(";

        bool first = true;
        for (const auto &param : target.params)
        {
            if (first)
            {
                first = false;
            }
            else
            {
                ret += ',';
                if (!bool(flags & ToCodeFlags::no_space_after_comma))
                    ret += ' ';
            }

            ret += ToCode(param, flags);
        }

        if (target.c_style_variadic)
        {
            if (!target.c_style_variadic_without_comma)
            {
                ret += ',';
                if (!bool(flags & ToCodeFlags::no_space_after_comma))
                    ret += ' ';
            }
            ret += "...";
        }

        if (target.params.empty() && !target.c_style_variadic && target.c_style_void_params)
            ret += "void";

        ret += ')';

        if (target.cv_quals != CvQualifiers{})
        {
            ret += ' ';
            ret += CvQualifiersToString(target.cv_quals);
        }
        if (target.ref_quals != RefQualifiers::none)
        {
            ret += ' ';
            ret += RefQualifiersToString(target.ref_quals);
        }

        if (target.noexcept_)
            ret += " noexcept";

        if (target.uses_trailing_return_type && !bool(flags & ToCodeFlags::force_no_trailing_return_type))
            ret += " -> "; // The caller must add the type after this.

        return ret;
    }

    [[nodiscard]] inline std::string ToString(const Function &target, ToStringFlags flags)
    {
        std::string ret = "a function ";

        bool parens = false;
        auto AddDetail = [&]
        {
            if (!parens)
            {
                parens = true;
                ret += '(';
            }
            else
            {
                ret += ", ";
            }
        };

        if (target.cv_quals != CvQualifiers{})
        {
            AddDetail();
            ret += CvQualifiersToString(target.cv_quals, '-', true);
            ret += "-qualified";
        }

        if (target.ref_quals != RefQualifiers{})
        {
            AddDetail();
            switch (target.ref_quals)
            {
              case RefQualifiers::lvalue:
                ret += "lvalue-ref-qualified";
                break;
              case RefQualifiers::rvalue:
                ret += "rvalue-ref-qualified";
                break;
              default:
                assert(false && "Unknown enum.");
            }
        }

        if (target.noexcept_)
        {
            AddDetail();
            ret += "noexcept";
        }

        if (parens)
            ret += ") ";

        ret += "taking ";
        if (target.params.empty())
        {
            ret += "no parameters";
            if (target.c_style_void_params)
                ret += " (spelled with C-style void)";
        }
        else
        {
            ret += std::to_string(target.params.size());
            ret += " parameter";
            if (target.params.size() != 1)
                ret += 's';
            ret += ": [";
            std::size_t i = 0;
            for (const auto &elem : target.params)
            {
                if (i > 0)
                    ret += ", ";

                i++;

                if (target.params.size() != 1)
                {
                    ret += std::to_string(i);
                    ret += ". ";
                }

                ret += ToString(elem, flags);
            }
            ret += ']';
        }
        if (target.c_style_variadic)
        {
            ret += " and a C-style variadic parameter";
            if (target.c_style_variadic_without_comma)
                ret += " (with a missing comma before it)"; // Illegal since C++26.
        }

        ret += ", returning";
        if (target.uses_trailing_return_type)
            ret += " (via trailing return type)";

        return ret;
    }

    [[nodiscard]] inline std::string ToCode(const TypeModifier &target, ToCodeFlags flags)
    {
        assert(!bool(flags & ToCodeFlags::only_any_half_type));

        return std::visit([&](const auto &elem){return ToCode(elem, flags);}, target.var);
    }

    [[nodiscard]] inline std::string ToString(const TypeModifier &target, ToStringFlags flags)
    {
        return std::visit([&](const auto &elem){return ToString(elem, flags);}, target.var);
    }
}
