#pragma once

#include <cstdint>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace cppdecl
{
    // --- Character classification:

    [[nodiscard]] constexpr bool IsWhitespace(char ch)
    {
        return ch == ' ' || ch == '\t';
    }
    // Remove any prefix whitespace from `str`.
    constexpr void TrimLeadingWhitespace(std::string_view &str)
    {
        while (!str.empty() && IsWhitespace(str.front()))
            str.remove_prefix(1);
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

    // Is `name` a type or a keyword related to types?
    // We use this to detect clearly invalid variable names that were parsed from types.
    [[nodiscard]] constexpr bool IsTypeRelatedKeyword(std::string_view name)
    {
        return
            name == "char" ||
            name == "short" ||
            name == "int" ||
            name == "long" ||
            name == "float" ||
            name == "double" ||
            name == "signed" ||
            name == "unsigned" ||
            name == "const" ||
            name == "volatile" ||
            name == "__restrict" ||
            name == "__restrict__";
        // Not adding the C/nonstandard spelling `restrict` here, since this list is just for better error messages, and the lack of it isn't going
        //   to break its parsing or anything.
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

    #if 0
    // ---

    template <typename UserData = std::type_identity<void>>
    struct BracketStack
    {
        struct Entry
        {
            // The location of the opening brace, one of: `(`, `[`, `{`, `<`, `"`, `'`.
            const char *begin = nullptr;
            char end_symbol = 0;

            #ifdef _MSC_VER
            [[msvc::no_unique_address]]
            #else
            [[no_unique_address]]
            #endif
            UserData userdata{};
        };

        std::vector<Entry> entries;
    };

    // Given the `input` string, removes the suffix to leave only the characters matching `pred(...)`.
    // But also handles all kinds of brackets and strings, ignoring the predicate for their contents.
    // Returns the parsing error message on failure, or null on success. The parsing errors are missing closing brackets or quotes.
    template <typename UserData>
    [[nodiscard]] constexpr const char *TraverseStringWithBrackets(
        // The input string.
        std::string_view &input,
        // This is cleared when the function starts, so you can safely reuse it between calls.
        // You can inspect this from the callbacks below too!
        BracketStack<UserData> &stack,
        // `(const char *loc) -> const char *`.
        // Return 1 if the character should be a part of input, 0 if not, or an error message to stop.
        // The return value is ignored if the stack is not empty.
        auto &&pred,
        // `(const char *loc) -> const char *`.
        // Called right AFTER adding a bracket to the stack ("after" matters for accessing the user data in the stack).
        // Return an error message to stop.
        // You can take `loc` BY REFERENCE and modify it to parse the contents of the brackets yourself. Just make sure to NOT consume the closing bracket.
        auto &&on_bracket_open,
        // `(std::string_view loc) -> const char *`.
        // The argument includes both brackes (at the first and last position respectively).
        // This is called right BEFORE popping a bracket from the stack (which matters for accessing the user data in the stack).
        // Return an error message to stop.
        auto &&on_bracket_close
    )
    {
        stack.entries.clear();

        // Here, in the condition we call the predicate first, to allow it to see all symbols, even if the stack is not empty.
        for (const char *cur = input.data(); cur < input.data() + input.size(); cur++)
        {
            if (const char *error = pred(std::as_const(cur)))
            {
                if (!error && stack.entries.empty())
                    break;
                if (error && std::uintptr_t(error) != 1)
                    return error;
            }

            if (!stack.entries.empty())
            {
                char end_symbol = stack.entries.back().end_symbol;

                // Strings.
                if (end_symbol == '"' || end_symbol == '\'')
                {
                    // Escape sequences.
                    if (*cur == '\\')
                    {
                        cur++; // `pred` never sees this symbol, which is ok.
                        if (cur == input.data() + input.size())
                        {
                            // Note that we of course don't validate the escape sequences. This only catches `\` at the very end of input,
                            //   not invalid escape sequences (nor incomplete multicharacter ones).
                            return "Incomplete escape sequence at the end of input.";
                        }
                        continue;
                    }

                    // Ending the string.
                    if (*cur == end_symbol)
                    {
                        // Callback first, then pop.
                        if (const char *error = on_bracket_close(std::string_view(stack.entries.back().begin, cur + 1)))
                            return error;
                        stack.entries.pop_back();
                        continue;
                    }
                }

                // Closing brackets.
                if (*cur == end_symbol)
                {
                    // Callback first, then pop.
                    if (const char *error = on_bracket_close(std::string_view(stack.entries.back().begin, cur + 1)))
                        return error;
                    stack.entries.pop_back();
                    continue;
                }
            }

            // Opening brackets.
            if (*cur == '"' || *cur == '\'')
            {
                // Push first, then run the callback.
                stack.entries.push_back();
                if (const char *error = on_bracket_open(cur))
                    return error;
            }
        }

        // Complain about a missing closing bracket or quote.
        if (!stack.entries.empty())
        {
            char ch = stack.entries.back().end_symbol;
            if (ch == '"' || ch == '\'')
                return "Expected a closing quote to match this one.";
            else
                return "Expected a closing bracket to match this one.";
        }

        return nullptr; // Success.
    }
    #endif
}
