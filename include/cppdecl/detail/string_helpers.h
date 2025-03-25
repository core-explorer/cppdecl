#pragma once

#include "cppdecl/detail/enum_flags.h"

#include <string_view>
#include <string>
#include <type_traits>

namespace cppdecl
{
    // --- Character classification:

    [[nodiscard]] constexpr bool IsWhitespace(char ch)
    {
        return ch == ' ' || ch == '\t';
    }
    // Remove any prefix whitespace from `str`.
    // Returns true if at least one removed.
    constexpr bool TrimLeadingWhitespace(std::string_view &str)
    {
        bool ret = false;
        while (!str.empty() && IsWhitespace(str.front()))
        {
            str.remove_prefix(1);
            ret = true;
        }
        return ret;
    }

    [[nodiscard]] constexpr bool IsAlphaLowercase(char ch)
    {
        return ch >= 'a' && ch <= 'z';
    }
    [[nodiscard]] constexpr bool IsAlphaUppercase(char ch)
    {
        return ch >= 'A' && ch <= 'Z';
    }
    [[nodiscard]] constexpr bool IsAlpha(char ch)
    {
        return IsAlphaLowercase(ch) || IsAlphaUppercase(ch);
    }
    // Whether `ch` is a letter or an other non-digit identifier character.
    [[nodiscard]] constexpr bool IsNonDigitIdentifierChar(char ch)
    {
        // `$` is non-standard, but seems to work on all compilers (sometimes you need to disable some warnings).
        return ch == '_' || ch == '$' || IsAlpha(ch);
    }
    [[nodiscard]] constexpr bool IsDigit(char ch)
    {
        return ch >= '0' && ch <= '9';
    }
    // Whether `ch` can be a part of an identifier.
    [[nodiscard]] constexpr bool IsIdentifierChar(char ch)
    {
        return IsNonDigitIdentifierChar(ch) || IsDigit(ch);
    }

    // Is this a keyword that is a type name?
    // `long long` and other multiword types are not handled here.
    [[nodiscard]] constexpr bool IsTypeNameKeyword(std::string_view name)
    {
        return
            name == "char" ||
            name == "short" ||
            name == "int" ||
            name == "long" ||
            name == "float" ||
            name == "double";
    }

    // Is `name` a type or a keyword related to types?
    // We use this to detect clearly invalid variable names that were parsed from types.
    [[nodiscard]] constexpr bool IsTypeRelatedKeyword(std::string_view name)
    {
        return
            IsTypeNameKeyword(name) ||
            name == "signed" ||
            name == "unsigned" ||
            name == "const" ||
            name == "volatile" ||
            name == "auto" ||
            // Not adding the C/nonstandard spelling `restrict` here, since this list is just for better error messages, and the lack of it isn't going
            //   to break its parsing or anything.
            name == "__restrict" ||
            name == "__restrict__";
    }

    // Is `name` a type or a keyword related to names?
    [[nodiscard]] constexpr bool IsNameRelatedKeyword(std::string_view name)
    {
        return name == "operator"; // Anything else?
    }

    // If `input` starts with word `word` (as per `.starts_with()`), removes that prefix and returns true.
    // Otherwise leaves it unchanged and returns false.
    [[nodiscard]] constexpr bool ConsumePunctuation(std::string_view &input, std::string_view word)
    {
        bool ret = input.starts_with(word);
        if (ret)
            input.remove_prefix(word.size());
        return ret;
    }

    // Returns true if `input` starts with `word` followed by end of string or whitespace or
    //   a punctuation character (which is anything other `IsIdentifierChar()`).
    [[nodiscard]] constexpr bool StartsWithWord(std::string_view input, std::string_view word)
    {
        return (input.size() <= word.size() || !IsIdentifierChar(input[word.size()])) && input.starts_with(word);
    }

    // If `input` starts with word `word` (as per `StartsWithWord`), removes that prefix and returns true.
    // Otherwise leaves it unchanged and returns false.
    [[nodiscard]] constexpr bool ConsumeWord(std::string_view &input, std::string_view word)
    {
        bool ret = StartsWithWord(input, word);
        if (ret)
            input.remove_prefix(word.size());
        return ret;
    }


    enum class ConsumeOperatorTokenFlags
    {
        // Mostly for internal use. Don't allow operators with a single character name.
        reject_single_character_operators = 1 << 0,
    };
    CPPDECL_FLAG_OPERATORS(ConsumeOperatorTokenFlags)

    // Tries to read an operator name token from `input`. On success returns true and makes the `out_token` point
    //   to that token (stored statically, so it will remain valid forever).
    // On failure, returns false and sets `out_token` to empty.
    // The initial value of `out_token` is ignored.
    // Only accepts tokens that can be after `operator`.
    [[nodiscard]] constexpr bool ConsumeOperatorToken(std::string_view &input, std::string_view &out_token, ConsumeOperatorTokenFlags flags = {})
    {
        using namespace std::string_view_literals;

        for (std::string_view token : {
            // The OVERLOADABLE operators, a subset of https://eel.is/c++draft/lex.operators#nt:operator-or-punctuator
            // Single-character operators are handled below.
            "->"sv , "->*"sv,
            "+="sv, "-="sv, "*="sv , "/="sv , "%="sv , "^="sv, "&="sv, "|="sv,
            "=="sv, "!="sv, "<="sv , ">="sv , "<=>"sv, "&&"sv, "||"sv,
            "<<"sv, ">>"sv, "<<="sv, ">>="sv, "++"sv , "--"sv,
        })
        {
            if (ConsumePunctuation(input, token))
            {
                out_token = token;
                return true;
            }
        }

        if (!bool(flags & ConsumeOperatorTokenFlags::reject_single_character_operators))
        {
            for (std::string_view token : {
                "~"sv, "!"sv, "+"sv, "-"sv, "*"sv, "/"sv, "%"sv, "^"sv, "&"sv, "|"sv, "="sv, "<"sv, ">"sv
            })
            {
                if (ConsumePunctuation(input, token))
                {
                    out_token = token;
                    return true;
                }
            }
        }

        { // Try to consume `()` and `[]`. Those can have whitespace in them...
            std::string_view input_copy = input;
            if (ConsumePunctuation(input_copy, "("))
            {
                TrimLeadingWhitespace(input_copy);
                if (ConsumePunctuation(input_copy, ")"))
                {
                    out_token = "()";
                    input = input_copy;
                    return true;
                }
            }

            if (ConsumePunctuation(input_copy, "["))
            {
                TrimLeadingWhitespace(input_copy);
                if (ConsumePunctuation(input_copy, "]"))
                {
                    out_token = "[]";
                    input = input_copy;
                    return true;
                }
            }
        }

        out_token = {};
        return false;
    }

    // Maybe inserts a whitespace into `input` at `.end() - last_token_len` if that is needed to split up tokens to avoid maximum munch.
    // Returns true if the space was inserted, false if it wasn't needed.
    inline bool BreakMaximumMunch(std::string &input, std::size_t last_token_len)
    {
        if (input.empty() || input.size() <= last_token_len)
            return false; // Either lhs or rhs is empty.

        // X + Y
        if (last_token_len == 1 && input.size() >= 2)
        {
            std::string_view tmp = std::string_view(input).substr(input.size() - 2);
            std::string_view token;
            if (ConsumeOperatorToken(tmp, token, ConsumeOperatorTokenFlags::reject_single_character_operators) && tmp.empty())
            {
                input.insert(input.end() - last_token_len, ' ');
                return true;
            }
        }
        // XY + Z
        if (last_token_len == 1 && input.size() >= 3)
        {
            std::string_view tmp = std::string_view(input).substr(input.size() - 3);
            std::string_view token;
            if (ConsumeOperatorToken(tmp, token, ConsumeOperatorTokenFlags::reject_single_character_operators) && tmp.empty())
            {
                input.insert(input.end() - last_token_len, ' ');
                return true;
            }
        }
        // X + YZ
        if (last_token_len == 2 && input.size() >= 3)
        {
            std::string_view tmp = std::string_view(input).substr(input.size() - 3);
            std::string_view token;
            if (ConsumeOperatorToken(tmp, token, ConsumeOperatorTokenFlags::reject_single_character_operators) && tmp.empty())
            {
                input.insert(input.end() - last_token_len, ' ');
                return true;
            }
        }

        return false;
    }
}
