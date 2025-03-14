#pragma once

#include "cppdecl/detail/string_helpers.h"
#include "cppdecl/parsing/result.h"

#include <algorithm>
#include <iterator>
#include <variant>

// Those functions parse various language constructs. There's a lot here, but you mainly want two functions:
// * `ParseType()` to parse types.
// * `ParseDecl()` to parse types or declarations (this is a superset of `ParseType`).
// In any case, the return value is a `std::variant` of either the result or a parsing error.
// The input `std::string_view` has a prefix of it removed. On failure, the new start points to the error.
// On success it contains the unparsed suffix.
// On success you should probably check that it's empty. (The trailing whitespace should be stripped automatically.)


namespace cppdecl
{
    struct ParseError
    {
        const char *message = nullptr;
    };


    using ParseTemplateArgumentListResult = std::variant<std::optional<TemplateArgumentList>, ParseError>;
    [[nodiscard]] inline ParseTemplateArgumentListResult ParseTemplateArgumentList(std::string_view &input);


    enum class ParseTypeFlags
    {
        // This is for target types of conversion operators.
        // Accept only the declarators that would be to the left of a variable name, stop on those that would be to the right.
        // Also don't accept `(`.
        only_left_side_declarators_without_parens = 1 << 0,
    };
    CPPDECL_FLAG_OPERATORS(ParseTypeFlags)

    using ParseTypeResult = std::variant<MaybeAmbiguousType, ParseError>;
    [[nodiscard]] inline ParseTypeResult ParseType(std::string_view &input, ParseTypeFlags flags = {});


    enum class ParseSimpleTypeFlags
    {
        // Reject any types containing `::`.
        // This is needed for destructor types as in `A::~B::C` (this must reject `::C`).
        only_unqualified = 1 << 0,
    };
    CPPDECL_FLAG_OPERATORS(ParseSimpleTypeFlags)

    using ParseSimpleTypeResult = std::variant<SimpleType, ParseError>;
    [[nodiscard]] inline ParseSimpleTypeResult ParseSimpleType(std::string_view &input, ParseSimpleTypeFlags flags = {});


    using ParseQualifiersResult = std::variant<CvQualifiers, ParseError>;

    // Returns a bit-or of 0 or more qualifiers. Silently fails if there are no qualifiers to parse.
    // Returns an error on duplicate qualifiers.
    // Removes leading but not trailing whitespace.
    [[nodiscard]] inline ParseQualifiersResult ParseCvQualifiers(std::string_view &input)
    {
        ParseQualifiersResult ret{};
        CvQualifiers &ret_quals = std::get<CvQualifiers>(ret);

        while (true)
        {
            auto input_copy = input;
            TrimLeadingWhitespace(input_copy);
            auto input_copy_after_whitespace = input_copy;

            CvQualifiers bit{};

            if (ConsumeWord(input_copy, "const"))
                bit = CvQualifiers::const_;
            else if (ConsumeWord(input_copy, "volatile"))
                bit = CvQualifiers::volatile_;
            // Here we include the non-conformant (`restrict`) spelling too. TODO a flag to only allow conformant C++ spellings?
            else if (ConsumeWord(input_copy, "__restrict") || ConsumeWord(input_copy, "__restrict__") || ConsumeWord(input_copy, "restrict"))
                bit = CvQualifiers::restrict_;

            if (!bool(bit))
                return ret;

            if (bool(bit & ret_quals))
            {
                input = input_copy_after_whitespace;
                return ret = ParseError{.message = "Duplicate cv-qualifier."}, ret;
            }

            ret_quals |= bit;
            input = input_copy;
        }

        return ret;
    }

    // Trims leading whitespace and consumes 0..2 `&`.
    [[nodiscard]] inline RefQualifiers ParseRefQualifiers(std::string_view &input)
    {
        RefQualifiers ret = RefQualifiers::none;
        TrimLeadingWhitespace(input);
        if (input.starts_with('&'))
        {
            input.remove_prefix(1);
            ret = RefQualifiers::lvalue;
            // Don't trim whitespace here! Whitespace between the two ampersands is illegal.
            if (input.starts_with('&'))
            {
                input.remove_prefix(1);
                ret = RefQualifiers::rvalue;
            }
        }
        return ret;
    }


    enum class ParseQualifiedNameFlags
    {
        // Only parse an unqualified name, stop at any `::` (including the leading one).
        only_unqualified = 1 << 0,

        // Allow destructor names that literally start with `~`, e.g. `~A` and `~A::B` (we don't police name equality because typedefs mess that up).
        // Things like `A::~B` are always allowed regardless of this flag.
        allow_unqualified_destructors = 1 << 1,
    };
    CPPDECL_FLAG_OPERATORS(ParseQualifiedNameFlags)

    // NOTE: This can return either a `QualifiedName` OR a `MemberPointer` on success (the latter is returned if it's followed by `:: * [cv]`.
    using ParseQualifiedNameResult = std::variant<QualifiedName, MemberPointer, ParseError>;

    // Returns a `QualifiedName` with no elements if there's nothing to parse.
    // Ignores leading whitespace. Modifies `input` to remove the parsed prefix (unless there was an error).
    // When `input` is modified, the trailing whitespace is stripped automatically. This happens even if there was nothing to parse.
    // If the input ends with `:: * [cv]` (as in a member pointer), returns a `MemberPointer` instead of a `QualifiedName`.
    // NOTE: This doesn't understand `long long` (hence "Low"), use `ParseDecl()` to support that.
    [[nodiscard]] inline ParseQualifiedNameResult ParseQualifiedName(std::string_view &input, ParseQualifiedNameFlags flags = {})
    {
        ParseQualifiedNameResult ret;
        QualifiedName &ret_name = std::get<QualifiedName>(ret);

        TrimLeadingWhitespace(input);

        if (input.empty())
            return ret; // Nothing to parse.

        std::string_view s = input;

        bool only_unqualified = bool(flags & ParseQualifiedNameFlags::only_unqualified);

        // Handle leading `::`.
        if (s.starts_with("::"))
        {
            if (only_unqualified)
                return ret; // Nope.

            ret_name.force_global_scope = true;
            s.remove_prefix(2);
            TrimLeadingWhitespace(s);
        }

        bool allow_destructors = bool(flags & ParseQualifiedNameFlags::allow_unqualified_destructors);

        if ((!s.empty() && IsNonDigitIdentifierChar(s.front())) || (allow_destructors && s.starts_with("~")))
        {
            while (true)
            {
                // A destructor?
                if (allow_destructors && ConsumePunctuation(s, "~"))
                {
                    // Looks like a destructor.
                    TrimLeadingWhitespace(s);

                    auto type_result = ParseSimpleType(s, ParseSimpleTypeFlags::only_unqualified);
                    if (auto error = std::get_if<ParseError>(&type_result))
                    {
                        input = s;
                        return ret = *error, ret;
                    }

                    UnqualifiedName new_unqual_part;
                    DestructorName &dtor = new_unqual_part.var.emplace<DestructorName>();
                    dtor.simple_type = std::move(std::get<SimpleType>(type_result));

                    ret_name.parts.push_back(std::move(new_unqual_part));

                    input = s;
                }
                else
                {
                    // Not a destructor.

                    const char *cur = s.data();

                    do
                        cur++;
                    while (cur < input.data() + input.size() && IsIdentifierChar(*cur));

                    std::string_view new_word = std::string_view(s.data(), cur);
                    s.remove_prefix(std::size_t(cur - s.data()));
                    TrimLeadingWhitespace(s);

                    UnqualifiedName new_unqual_part;

                    // Is this something weird?
                    if (new_word == "operator")
                    {
                        // The whitespace was already stripped at this point.

                        if (ConsumePunctuation(s, "\"\""))
                        {
                            // User-defined literal.

                            UserDefinedLiteral &udl = new_unqual_part.var.emplace<UserDefinedLiteral>();

                            udl.space_before_suffix = TrimLeadingWhitespace(s);
                            if (s.empty() || !IsNonDigitIdentifierChar(s.front()))
                            {
                                input = s;
                                ret = ParseError{.message = "Expected identifier after `\"\"` in a user-defined literal."};
                                return ret;
                            }

                            do
                            {
                                udl.suffix += s.front();
                                s.remove_prefix(1);
                            }
                            while (!s.empty() && IsIdentifierChar(s.front()));
                        }
                        else if (std::string_view op_token; ConsumeOperatorToken(s, op_token))
                        {
                            // Overloaded operator.
                            OverloadedOperator &op = new_unqual_part.var.emplace<OverloadedOperator>();
                            op.token = op_token;
                        }
                        else
                        {
                            // Has to be a conversion operator at this point.
                            auto type_result = ParseType(s, ParseTypeFlags::only_left_side_declarators_without_parens);
                            if (auto error = std::get_if<ParseError>(&type_result))
                            {
                                input = s;
                                return ret = *error, ret;
                            }

                            ConversionOperator &conv = new_unqual_part.var.emplace<ConversionOperator>();
                            conv.target_type = std::move(std::get<MaybeAmbiguousType>(type_result));
                        }
                    }

                    // If this isn't a special `operator` after all the checks above...

                    if (auto name = std::get_if<std::string>(&new_unqual_part.var))
                        *name = new_word;

                    // Advance `input` to parse the template argument list. That's parsed with the real `input` to show a good error.
                    input = s;

                    // Consume the template arguments, if any.
                    auto arglist_result = ParseTemplateArgumentList(input);
                    if (auto error = std::get_if<ParseError>(&arglist_result))
                        return ret = *error, ret;
                    new_unqual_part.template_args = std::move(std::get<std::optional<TemplateArgumentList>>(arglist_result));

                    ret_name.parts.push_back(std::move(new_unqual_part));

                    s = input;
                }

                // Can be redundant in some cases, but just in case.
                TrimLeadingWhitespace(s);

                // Allow destructors on the next iterations, if not already.
                allow_destructors = true;

                // Make sure we have `:: letter` after this, or break.
                if (!only_unqualified && s.starts_with("::"))
                {
                    s.remove_prefix(2);
                    TrimLeadingWhitespace(s);
                    if (!s.empty())
                    {
                        if (IsNonDigitIdentifierChar(s.front()) || (allow_destructors && s.starts_with('~')))
                            continue;
                        if (s.front() == '*') // This looks like a member pointer.
                        {
                            s.remove_prefix(1);
                            auto parsed_quals = ParseCvQualifiers(s);
                            input = s;
                            if (auto error = std::get_if<ParseError>(&parsed_quals))
                                return ret = *error, ret;
                            MemberPointer memptr;
                            memptr.quals = std::get<CvQualifiers>(parsed_quals);
                            memptr.base = std::move(ret_name);
                            ret = std::move(memptr);
                            return ret;
                        }
                    }
                }

                break;
            }
        }

        // Reset `force_global_scope` if the input was empty and `::` was just junk.
        if (ret_name.IsEmpty())
            ret_name.force_global_scope = false;

        return ret;
    }


    // True on success, false if nothing to do, error if this looks illegal.
    using TryAddNameToTypeResult = std::variant<bool, ParseError>;

    // Tries to modify this type by adding another name to it.
    // This always succeeds if the type is empty, and then it only accepts things like
    //   adding `long` to another `long`, or `unsigned` plus something else, etc.
    // NOTE: This does nothing if `name` is empty, and in that case you should probably stop whatever you're doing to avoid infinite loops.
    [[nodiscard]] inline TryAddNameToTypeResult TryAddNameToType(SimpleType &type, const QualifiedName &new_name)
    {
        if (new_name.IsEmpty())
            return false; // The name is empty, nothing to do.

        const std::string_view word = new_name.AsSingleWord();
        const std::string_view existing_word = type.name.AsSingleWord();

        if (word == "const")
        {
            if (bool(type.quals & CvQualifiers::const_))
                return ParseError{.message = "Repeated `const`."};
            type.quals |= CvQualifiers::const_;
            return true;
        }
        if (word == "volatile")
        {
            if (bool(type.quals & CvQualifiers::volatile_))
                return ParseError{.message = "Repeated `volatile`."};
            type.quals |= CvQualifiers::volatile_;
            return true;
        }
        if (word == "unsigned")
        {
            if (bool(type.flags & SimpleTypeFlags::unsigned_))
                return ParseError{.message = "Repeated `unsigned`."};
            if (bool(type.flags & SimpleTypeFlags::explicitly_signed))
                return ParseError{.message = "Both `signed` and `unsigned` on the same type."};
            type.flags |= SimpleTypeFlags::unsigned_;
            return true;
        }
        if (word == "signed")
        {
            if (bool(type.flags & SimpleTypeFlags::explicitly_signed))
                return ParseError{.message = "Repeated `signed`."};
            if (bool(type.flags & SimpleTypeFlags::unsigned_))
                return ParseError{.message = "Both `unsigned` and `signed` on the same type."};
            type.flags |= SimpleTypeFlags::explicitly_signed;
            return true;
        }
        // Combining together all the `[long [long]] [int]` bullshit:
        // int + short, int + long  -> remove the `int`, keep the new spelling and set the flag.
        if (existing_word == "int" && (word == "short" || word == "long"))
        {
            if (bool(type.flags & SimpleTypeFlags::redundant_int))
                return ParseError{.message = "Repeated `int`."};
            type.flags |= SimpleTypeFlags::redundant_int;
            type.name.parts.front().var = std::string(word);
            return true;
        }
        // short + int, long + int, long long + int  -> set the flag and ignore `int`
        if (word == "int" && (existing_word == "short" || existing_word == "long" || existing_word == "long long"))
        {
            if (bool(type.flags & SimpleTypeFlags::redundant_int))
                return ParseError{.message = "Repeated `int`."};
            type.flags |= SimpleTypeFlags::redundant_int;
            return true;
        }
        // long + long
        if (word == "long" && existing_word == "long")
        {
            type.name.parts.front().var = "long long";
            return true;
        }
        // long + double
        if ((word == "long" && existing_word == "double") || (word == "double" && existing_word == "long"))
        {
            type.name.parts.front().var = "long double";
            return true;
        }

        // The original type was empty, replace it completely.
        // Note that this has to be late,
        if (type.IsEmpty())
        {
            type.name = std::move(new_name);
            return true;
        }

        // The name being added is a keyword that looks suspicious.
        if (IsTypeRelatedKeyword(word))
            return ParseError{.message = "Can't add this keyword to the preceding type."};

        return false; // Don't know what this is.
    }

    // Parse a "simple type". Very similar to `ParseQualifiedName`, but also combines `long` + `long`, and similar things.
    // Returns an empty type if nothing to parse.
    [[nodiscard]] inline ParseSimpleTypeResult ParseSimpleType(std::string_view &input, ParseSimpleTypeFlags flags)
    {
        ParseSimpleTypeResult ret;
        SimpleType &ret_type = std::get<SimpleType>(ret);

        const ParseQualifiedNameFlags qual_name_flags =
            bool(flags & ParseSimpleTypeFlags::only_unqualified) * ParseQualifiedNameFlags::only_unqualified;

        while (true)
        {
            const std::string_view input_before_name;

            auto name_result = ParseQualifiedName(input, qual_name_flags);
            if (auto error = std::get_if<ParseError>(&name_result))
                return ret = *error, ret;

            if (std::holds_alternative<MemberPointer>(name_result))
            {
                input = input_before_name;
                break; // Can't use this for anything, undo this element and stop.
            }

            QualifiedName &new_name = std::get<QualifiedName>(name_result);

            if (new_name.IsEmpty())
                break; // No more names to parse, stop.

            auto add_name_result = TryAddNameToType(ret_type, new_name);
            if (auto error = std::get_if<ParseError>(&name_result))
                return ret = *error, ret;

            bool added = std::get<bool>(add_name_result);
            if (!added)
            {
                input = input_before_name;
                break; // Some unknown syntax here, undo this element and stop.
            }
        }

        return ret;
    }


    enum class ParsePseudoExprFlags
    {
        // Stop parsing on `>`. Good for template argument lists.
        // Otherwise will assume that it's a punctuation symbol.
        stop_on_gt_sign = 1 << 0,
    };
    CPPDECL_FLAG_OPERATORS(ParsePseudoExprFlags)

    using ParsePseudoExprResult = std::variant<PseudoExpr, ParseError>;
    // Parse an expression. Even though we call those expressions, it's a fairly loose collection of tokens.
    // We continue parsing until we hit a comma or a closing bracket: `)`,`}`,`]`,`>`.
    // Can return an empty expression.
    [[nodiscard]] inline ParsePseudoExprResult ParsePseudoExpr(std::string_view &input, ParsePseudoExprFlags flags = {})
    {
        ParsePseudoExprResult ret;
        PseudoExpr &ret_expr = std::get<PseudoExpr>(ret);

        while (true)
        {
            TrimLeadingWhitespace(input);

            if (
                input.empty() ||
                input.starts_with(',') || input.starts_with(')') || input.starts_with('}') || input.starts_with(']') ||
                (bool(flags & ParsePseudoExprFlags::stop_on_gt_sign) && input.starts_with('>'))
            )
            {
                return ret;
            }

            // Number.
            if (IsDigit(input.front()))
            {
                NumberToken num;
                do
                {
                    num.value += input.front();
                    input.remove_prefix(1);
                }
                while (
                    !input.empty() &&
                    (
                        IsDigit(input.front()) ||
                        IsAlpha(input.front()) ||
                        input.front() == '.' ||
                        input.front() == '\'' ||
                        (
                            (input.front() == '+' || input.front() == '-') &&
                            !num.value.empty() &&
                            (
                                num.value.back() == 'e' ||
                                num.value.back() == 'E' ||
                                num.value.back() == 'p' ||
                                num.value.back() == 'P'
                            )
                        )
                    )
                );

                ret_expr.tokens.emplace_back(std::move(num));
                continue;
            }

            { // String or character literal.
                std::string_view input_copy = input;

                StringOrCharLiteral lit;

                if (ConsumePunctuation(input_copy, "L"))
                    lit.type = StringOrCharLiteral::Type::wide;
                else if (ConsumePunctuation(input_copy, "u8")) // Check before `u` since that is a substring of this.
                    lit.type = StringOrCharLiteral::Type::u8;
                else if (ConsumePunctuation(input_copy, "u"))
                    lit.type = StringOrCharLiteral::Type::u16;
                else if (ConsumePunctuation(input_copy, "U"))
                    lit.type = StringOrCharLiteral::Type::u32;

                bool ok = true;
                if (ConsumePunctuation(input_copy, "\""))
                    lit.kind = StringOrCharLiteral::Kind::string;
                else if (ConsumePunctuation(input_copy, "'"))
                    lit.kind = StringOrCharLiteral::Kind::character;
                else if (ConsumePunctuation(input_copy, "R\""))
                    lit.kind = StringOrCharLiteral::Kind::raw_string;
                else
                    ok = false;

                if (ok)
                {
                    // Now we're sure that it's a string literal.

                    const std::string_view input_at_start_of_literal = input;
                    input = input_copy;

                    if (lit.kind != StringOrCharLiteral::Kind::raw_string)
                    {
                        char quote = lit.kind == StringOrCharLiteral::Kind::character ? '\'' : '"';
                        while (true)
                        {
                            if (input.empty())
                            {
                                const char *error = nullptr;
                                switch (lit.kind)
                                {
                                    case StringOrCharLiteral::Kind::character:  error = "Unterminated character literal."; break;
                                    case StringOrCharLiteral::Kind::string:     error = "Unterminated string literal."; break;
                                    case StringOrCharLiteral::Kind::raw_string: break; // Unreachable.
                                }
                                input = input_at_start_of_literal;
                                return ret = ParseError{.message = error}, ret;
                            }

                            const auto input_at_character = input;

                            if (ConsumePunctuation(input, "\\"))
                            {
                                if (input.empty())
                                {
                                    input = input_at_character;
                                    return ret = ParseError{.message = "Unterminated escape sequence."}, ret;
                                }
                                // Add the escape sequence to the literal as is.
                                lit.value += '\\';
                                lit.value += input.front();
                                input.remove_prefix(1);
                            }

                            if (input.starts_with(quote))
                            {
                                input.remove_prefix(1);
                                break; // Ending quote.
                            }

                            // Add the character to the literal.
                            lit.value += input.front();
                            input.remove_prefix(1);
                        }
                    }
                    else
                    {
                        // Get the delimiter.
                        while (true)
                        {
                            if (input.empty())
                                return input = input_at_start_of_literal, ret = ParseError{.message = "Unterminated opening delimiter of a raw string literal."}, ret;

                            if (ConsumePunctuation(input, "("))
                                break; // End of delimiter.

                            // If not a valid delimiter character...
                            // Valid characters are as specified in https://eel.is/c++draft/tab:lex.charset.basic minus those in https://eel.is/c++draft/lex.string#nt:d-char
                            if (!(IsAlpha(input.front()) || IsDigit(input.front()) || std::string_view("!\"#$%&\')*+,-./:;<=>?@[]^_`{|}~").find(input.front()) != std::string_view::npos))
                                return ret = ParseError{.message = "Invalid character in a raw string literal delimiter."}, ret;

                            if (lit.raw_string_delim.size() >= 16)
                            {
                                input.remove_prefix(1);
                                return ret = ParseError{.message = "Raw string literal delimiter is too long."}, ret;
                            }

                            lit.raw_string_delim += input.front();
                            input.remove_prefix(1);
                        }

                        // Parse the string.
                        while (true)
                        {
                            if (input.empty())
                                return input = input_at_start_of_literal, ret = ParseError{.message = "Unterminated raw string literal."}, ret;

                            if (input.front() == ')' && input.size() >= lit.raw_string_delim.size() + 2 && input[lit.raw_string_delim.size()+1] == '"' && input.substr(1, lit.raw_string_delim.size()) == lit.raw_string_delim)
                            {
                                // Found the final delimiter, stop.
                                input.remove_prefix(lit.raw_string_delim.size() + 2);
                                break;
                            }

                            lit.value += input.front();
                            input.remove_prefix(1);
                        }
                    }

                    // Parse the literal suffix if any.
                    if (!input.empty() && IsNonDigitIdentifierChar(input.front()))
                    {
                        do
                        {
                            lit.literal_suffix += input.front();
                            input.remove_prefix(1);
                        }
                        while (!input.empty() && IsIdentifierChar(input.front()));
                    }

                    ret_expr.tokens.emplace_back(std::move(lit));
                    continue;
                }
            }

            { // List in brackets.
                PseudoExprList list;

                std::string_view closing_bracket;
                if (ConsumePunctuation(input, "("))
                    list.kind = PseudoExprList::Kind::parentheses, closing_bracket = ")";
                else if (ConsumePunctuation(input, "{"))
                    list.kind = PseudoExprList::Kind::curly,       closing_bracket = "}";
                else if (ConsumePunctuation(input, "["))
                    list.kind = PseudoExprList::Kind::square,      closing_bracket = "]";

                if (!closing_bracket.empty())
                {
                    // Now we're sure this is a list.

                    TrimLeadingWhitespace(input);

                    // Is not empty?
                    if (!ConsumePunctuation(input, closing_bracket))
                    {
                        // Parse the elements.
                        while (true)
                        {
                            auto expr_result = ParsePseudoExpr(input);
                            if (auto error = std::get_if<ParseError>(&expr_result))
                                return ret = *error, ret;

                            list.elems.push_back(std::move(std::get<PseudoExpr>(expr_result)));
                            TrimLeadingWhitespace(input);

                            if (ConsumePunctuation(input, closing_bracket))
                                break; // End of list.

                            if (ConsumePunctuation(input, ","))
                            {
                                bool allow_trailing_comma = list.kind == PseudoExprList::Kind::curly;
                                if (allow_trailing_comma)
                                {
                                    TrimLeadingWhitespace(input);
                                    if (ConsumePunctuation(input, closing_bracket))
                                    {
                                        list.has_trailing_comma = true;
                                        break; // End of list.
                                    }
                                }

                                continue; // Next element.
                            }

                            // Invalid syntax at this point.
                            const char *error = nullptr;
                            switch (list.kind)
                            {
                                case PseudoExprList::Kind::parentheses: error = "Expected expression or `)` or `,`."; break;
                                case PseudoExprList::Kind::curly:       error = "Expected expression or `}` or `,`."; break;
                                case PseudoExprList::Kind::square:      error = "Expected expression or `]` or `,`."; break;
                            }

                            return ret = ParseError{.message = error}, ret;
                        }
                    }

                    ret_expr.tokens.emplace_back(std::move(list));
                    continue;
                }
            }

            { // Template argument list.
                auto arglist_result = ParseTemplateArgumentList(input);
                if (auto error = std::get_if<ParseError>(&arglist_result))
                    return ret = *error, ret;
                auto &arglist_opt = std::get<std::optional<TemplateArgumentList>>(arglist_result);
                if (arglist_opt)
                {
                    ret_expr.tokens.emplace_back(std::move(*arglist_opt));
                    continue;
                }

                // If we're here, this means there was no `<` at all, so we continue.
            }

            { // `SimpleType`, which includes identifiers.
                auto type_result = ParseSimpleType(input);
                if (auto error = std::get_if<ParseError>(&type_result))
                    return ret = *error, ret;

                auto &type = std::get<SimpleType>(type_result);
                if (!type.IsEmpty())
                {
                    ret_expr.tokens.emplace_back(std::move(std::get<SimpleType>(type_result)));
                    continue;
                }
            }

            { // Punctuation. This must be last, this catches all unknown tokens.
                PunctuationToken punct;
                using namespace std::string_view_literals;

                bool found = false;

                // Some known tokens.
                for (std::string_view token : {
                    // All multicharacter tokens from: https://eel.is/c++draft/lex.operators#nt:operator-or-punctuator
                    // Excluding identifier-like operator names, we don't bother supporting those. (Because then what about all the other keywords
                    //   what were catched by `SimpleType`? Screw that.)
                    // Also excluding operators (that can be after `operator`, these are handled below).
                    "<:"sv, ":>"sv, "<%"sv , "%>"sv , "..."sv,
                    "::"sv, ".*"sv,
                })
                {
                    if (ConsumePunctuation(input, token))
                    {
                        found = true;
                        punct.value = token;
                        break;
                    }
                }

                // Handle operator tokens.
                // `ConsumeOperatorToken` can accept `()` and `[]`, which we don't care about, but they should've been already handled
                //   by the "list" token type above.
                if (std::string_view op_token; !found && ConsumeOperatorToken(input, op_token))
                {
                    found = true;
                    punct.value = op_token;
                }

                if (!found)
                {
                    // Didn't find any multicharacter tokens, add just one character and stop.
                    punct.value = input.substr(0, 1);
                    input.remove_prefix(1);
                }

                ret_expr.tokens.emplace_back(std::move(punct));
            }
        }
    }


    // Not setting any flags is an error.
    enum class ParseDeclFlags
    {
        // Accept unnamed declarations.
        accept_unnamed = 1 << 0,
        // Accept named declarations with unqualified names only.
        accept_unqualified_named = 1 << 1,
        // Accept named declarations with both unqualified and qualified names.
        accept_all_named = 1 << 2 | accept_unqualified_named,

        // Accept both named and unnamed declarations.
        accept_everything = accept_unnamed | accept_all_named,

        // This is for target types of conversion operators.
        // Accept only the declarators that would be to the left of a variable name, stop on those that would be to the right.
        // Also don't accept `(`, and don't accept any names.
        // This shouldn't be used with any `accept_...` other than `accept_unnamed`.
        accept_unnamed_only_left_side_declarators_without_parens = 1 << 3 | accept_unnamed,
    };
    CPPDECL_FLAG_OPERATORS(ParseDeclFlags)
    [[nodiscard]] inline bool DeclFlagsAcceptName(ParseDeclFlags flags, const QualifiedName &name)
    {
        if (name.IsEmpty())
        {
            return bool(flags & ParseDeclFlags::accept_unnamed);
        }
        else if (name.IsQualified())
        {
            return (flags & ParseDeclFlags::accept_all_named) == ParseDeclFlags::accept_all_named;
        }
        else
        {
            return bool(flags & ParseDeclFlags::accept_unqualified_named);
        }
    }

    using ParseDeclResult = std::variant<MaybeAmbiguousDecl, ParseError>;
    // Parses a declaration (named or unnamed), returns `ParseError` on failure.
    // Tries to resolve ambiguities based on `flags`, and based on the amount of characters consumed (more is better).
    // If any ambiguities remain after that, returns one of them, preferring those without redundant parentheses (preferring to interpret them
    //   as function parameters), sets `.IsAmbiguous() == true` in the result, and attaches the ambiguous alternatives
    //   (see `.ambiguous_alternative`). Note that ambiguities can happen not only at the top level, but also in function parameters. `.IsAmbiguous()`
    //   checks for that recursively.
    [[nodiscard]] inline ParseDeclResult ParseDecl(std::string_view &input, ParseDeclFlags flags)
    {
        ParseDeclResult ret;
        MaybeAmbiguousDecl &ret_decl = std::get<MaybeAmbiguousDecl>(ret);

        // Make sure the flags are ok.
        if (!bool(flags & (ParseDeclFlags::accept_unnamed | ParseDeclFlags::accept_unqualified_named)))
            return ret = ParseError{.message = "Bad usage, invalid flags passed to the parsing function."}, ret;


        // First declare the declarator stack.
        // It's quite early, since we didn't parse the decl-specifier-seq yet, but parsing that can immediately emit
        //   a single member-pointer modifier, and that must be pushed to the stack rather than directly to the return type.

        struct OpenParen
        {
            std::string_view input; // The remaining input starting with this `(`.
            MaybeAmbiguousDecl ret_backup; // The backup of `ret_decl` at this position.
        };

        struct DeclaratorStackEntry
        {
            using Var = std::variant<OpenParen, Pointer, Reference, MemberPointer>;
            Var var;

            std::string_view location; // Copy of the input string right BEFORE this was parsed.
        };

        // We don't normally pop from this (except when trying different parsing strategies to parse a function or something).
        // Instead we modify `declarator_stack_pos`.
        std::vector<DeclaratorStackEntry> declarator_stack;


        { // Parse the decl-specifier-seq. This can also fill some extra data... (A single member pointer, or a declaration name.)
            while (true)
            {
                // `ParseQualifiedName` would automatically trim this, but I want to store the correct position for the error message, if we get one.
                TrimLeadingWhitespace(input);

                const auto input_before_parse = input;
                ParseQualifiedNameResult result = ParseQualifiedName(input);
                if (auto error = std::get_if<ParseError>(&result))
                    return ret = *error, ret;

                if (auto memptr = std::get_if<MemberPointer>(&result))
                {
                    if (ret_decl.IsEmpty())
                    {
                        input = input_before_parse;
                        return ret = ParseError{.message = "Missing the pointee type for the member pointer."}, ret;
                    }
                    // Note, we're pushing to the declarator stack, not directly to the result. That would produce a wrong order.
                    declarator_stack.emplace_back(std::move(*memptr), input_before_parse);
                    break;
                }

                QualifiedName &name = std::get<QualifiedName>(result);

                if (name.IsEmpty())
                    break;

                auto adding_name_result = TryAddNameToType(ret_decl.type.simple_type, name);
                if (auto error = std::get_if<ParseError>(&adding_name_result))
                    return ret = *error, input = input_before_parse, ret;
                bool name_added = std::get<bool>(adding_name_result);

                if (!name_added)
                {
                    // This is probably a variable name (or function name) at this point.

                    if (!DeclFlagsAcceptName(flags, name))
                    {
                        // We didn't expect a variable name (or expected an unqualified one and got qualified).
                        // Roll it back and stop. This isn't an error here (for no particular reason, it just seems to make sense?).
                        // But it is an error down below when we're inside of a declarator.
                        input = input_before_parse;
                        return ret;
                    }
                    else
                    {
                        ret_decl.name = std::move(name);
                        break;
                    }
                }
            }
        }

        // If the type is empty...
        if (ret_decl.IsEmpty())
        {
            // Add implcit `int` if we have `unsigned` or `signed`, otherwise abort.
            if (bool(ret_decl.type.simple_type.flags & (SimpleTypeFlags::unsigned_ | SimpleTypeFlags::explicitly_signed)))
                ret_decl.type.simple_type.name.parts.push_back(UnqualifiedName{.var = "int", .template_args = {}});
            else
                return ret = ParseError{.message = "Expected a type."}, ret;
        }

        // Now the declarators:

        bool have_any_parens_in_declarator_on_initial_parse = false;

        // We parse what looks like the variable name into this.
        ParseQualifiedNameResult candidate_decl_name;
        std::string_view input_before_candidate_decl_name;

        // Is this a target type of a conversion operator?
        // Then we don't accept the declarators that go after the variable name,
        //   and moreover stop at any `(` whatsoever.
        // This implies `accept_unnamed`.
        bool left_side_only_and_no_parens = (flags & ParseDeclFlags::accept_unnamed_only_left_side_declarators_without_parens) == ParseDeclFlags::accept_unnamed_only_left_side_declarators_without_parens;

        // If we didn't already get a variable name from parsing the decl-specifier-seq, parse until we find one, or until we're sure there's none.
        if (ret_decl.name.IsEmpty())
        {
            while (true)
            {
                TrimLeadingWhitespace(input);
                if (!left_side_only_and_no_parens && input.starts_with('('))
                {
                    declarator_stack.emplace_back(OpenParen{.input = input, .ret_backup = ret_decl}, input);
                    input.remove_prefix(1);
                    have_any_parens_in_declarator_on_initial_parse = true;
                    continue;
                }
                if (input.starts_with('*'))
                {
                    const std::string_view input_at_ptr = input;

                    input.remove_prefix(1);
                    Pointer ptr;

                    auto parsed_quals = ParseCvQualifiers(input);
                    if (auto error = std::get_if<ParseError>(&parsed_quals))
                        return ret = *error, ret;
                    ptr.quals = std::get<CvQualifiers>(parsed_quals);

                    declarator_stack.emplace_back(std::move(ptr), input_at_ptr);
                    continue;
                }
                if (input.starts_with('&'))
                {
                    const std::string_view input_at_ref = input;

                    Reference ref;
                    ref.kind = ParseRefQualifiers(input);

                    const auto input_at_quals = input;
                    auto parsed_quals = ParseCvQualifiers(input);
                    if (auto error = std::get_if<ParseError>(&parsed_quals))
                        return ret = *error, ret;
                    ref.quals = std::get<CvQualifiers>(parsed_quals);

                    if (bool(ref.quals & CvQualifiers::const_))
                        return input = input_at_quals, TrimLeadingWhitespace(input), ret = ParseError{.message = "References can't be const-qualified."}, ret;
                    if (bool(ref.quals & CvQualifiers::volatile_))
                        return input = input_at_quals, TrimLeadingWhitespace(input), ret = ParseError{.message = "References can't be volatile-qualified."}, ret;

                    declarator_stack.emplace_back(std::move(ref), input_at_ref);
                    continue;
                }


                // Now what looks like the variable name:

                // Trimming whitespace would happen automatically in `ParseQualifiedName` anyway,
                //   but I want to record the location after the whitespace.
                TrimLeadingWhitespace(input);

                input_before_candidate_decl_name = input;
                candidate_decl_name = ParseQualifiedName(input, ParseQualifiedNameFlags::allow_unqualified_destructors);
                if (auto error = std::get_if<ParseError>(&candidate_decl_name))
                    return ret = *error, ret;

                if (auto memptr = std::get_if<MemberPointer>(&candidate_decl_name))
                {
                    declarator_stack.emplace_back(std::move(*memptr), input_before_candidate_decl_name);
                    candidate_decl_name = {}; // Nuke the name. It's checked before this loop, so it should be empty, just in case.
                    continue;
                }

                // If this is a conversion operator target type, undo parsing the name and nope out of here.
                if (left_side_only_and_no_parens)
                {
                    candidate_decl_name = {};
                    input = input_before_candidate_decl_name;
                    break; // Nope out of here.
                }

                // Now we break with possibily non-empty `candidate_decl_name`, to continue handling it in `ParseRemainingDecl`.
                // We must do that because any errors in it must cause a reparse
                break;
            }
        }


        auto ParseRemainingDecl = [&]() -> ParseDeclResult
        {
            // Must not touch `ret` in this function. This variable shadows it.
            [[maybe_unused]] constexpr int ret = -1;

            // Continue parsing the variable name, if any.
            // This can only happen on the first parse. Repeated attempts will always have an empty name.
            QualifiedName &name = std::get<QualifiedName>(candidate_decl_name);
            if (!name.IsEmpty())
            {
                if (IsTypeRelatedKeyword(name.AsSingleWord()))
                {
                    input = input_before_candidate_decl_name;
                    return ParseError{.message = "Can't add this keyword to the preceding type."};
                }

                if (!DeclFlagsAcceptName(flags, name))
                {
                    input = input_before_candidate_decl_name;
                    if (have_any_parens_in_declarator_on_initial_parse)
                    {
                        if (!bool(flags & ParseDeclFlags::accept_unqualified_named))
                            return ParseError{.message = "Expected only a type but got a named declaration."};
                        else
                            return ParseError{.message = "Expected an unqualified name but got a qualified one."};
                    }
                    return ret_decl; // Refuse to parse the rest, the declaration ends here. Not emit a hard error either, maybe it's just junk?
                }

                ret_decl.name = std::move(name);
            }


            std::size_t declarator_stack_pos = declarator_stack.size();

            // Make sure we don't have empty `()` parentheses without a declarator between them.
            // I'm not sure if it's possible to trigger this at all without falling back to the function parameter list parsing.
            TrimLeadingWhitespace(input);
            if (ret_decl.name.IsEmpty() && declarator_stack_pos > 0 && std::holds_alternative<OpenParen>(declarator_stack[declarator_stack_pos-1].var) && input.starts_with(')'))
                return ParseError{.message = "Parentheses around a declarator can't be empty."};


            // Decrements `declarator_stack_pos` and processes the element removed in this manner.
            // Returns true when popping `()`.
            // Writes an error to `out_error` and returns true on failure.
            auto PopDeclaratorFromStack = [&](std::optional<ParseError> &out_error) -> bool
            {
                DeclaratorStackEntry &this_elem = declarator_stack[declarator_stack_pos-1];

                bool done = std::visit([&]<typename T>(T &elem)
                {
                    // Check for illegal modifier combinations.
                    // Here we only check the modifiers that appear to the left of the name.
                    if (!ret_decl.type.modifiers.empty())
                    {
                        if (std::holds_alternative<Reference>(this_elem.var))
                        {
                            if (std::holds_alternative<Pointer>(ret_decl.type.modifiers.back().var))
                            {
                                out_error = ParseError{.message = "Pointers to references are not allowed."};
                                input = this_elem.location;
                                return true;
                            }
                            if (std::holds_alternative<MemberPointer>(ret_decl.type.modifiers.back().var))
                            {
                                out_error = ParseError{.message = "Member pointers to references are not allowed."};
                                input = this_elem.location;
                                return true;
                            }
                            if (std::holds_alternative<Array>(ret_decl.type.modifiers.back().var))
                            {
                                out_error = ParseError{.message = "Arrays of references are not allowed."};
                                input = this_elem.location;
                                return true;
                            }
                        }
                    }

                    if constexpr (std::is_same_v<T, OpenParen>)
                    {
                        return true;
                    }
                    else
                    {
                        ret_decl.type.modifiers.emplace_back(std::move(elem));
                        return false;
                    }
                }, this_elem.var);

                declarator_stack_pos--;

                return done;
            };

            // Continue parsing to the end of the declaration.
            if (!left_side_only_and_no_parens)
            {
                while (true)
                {
                    TrimLeadingWhitespace(input);

                    const std::string_view input_before_modifier = input;

                    if (input.starts_with(')'))
                    {
                        bool done = false;
                        while (!done)
                        {
                            if (declarator_stack_pos == 0)
                                return ret_decl; // Extra `)` after input, but this is not an error. This is important e.g. for the last function parameter.

                            std::optional<ParseError> error;
                            done = PopDeclaratorFromStack(error);
                            if (error)
                                return *error;
                        }

                        input.remove_prefix(1);
                        continue;
                    }

                    if (ConsumePunctuation(input, "["))
                    {
                        // Check for banned element type modifiers.
                        if (!ret_decl.type.modifiers.empty())
                        {
                            if (std::holds_alternative<Function>(ret_decl.type.modifiers.front().var))
                            {
                                input = input_before_modifier;
                                return ParseError{.message = "Function return type can't be an array."};
                            }
                        }

                        auto expr_result = ParsePseudoExpr(input);
                        if (auto error = std::get_if<ParseError>(&expr_result))
                            return *error;

                        Array arr;
                        arr.size = std::move(std::get<PseudoExpr>(expr_result));

                        if (!ConsumePunctuation(input, "]"))
                            return ParseError{.message = "Expected `]` after array size."};

                        ret_decl.type.modifiers.emplace_back(std::move(arr));
                        continue;
                    }

                    if (ConsumePunctuation(input, "("))
                    {
                        // Check for banned return type modifiers.
                        if (!ret_decl.type.modifiers.empty())
                        {
                            if (std::holds_alternative<Array>(ret_decl.type.modifiers.front().var))
                            {
                                input = input_before_modifier;
                                return ParseError{.message = "Arrays of functions are not allowed."};
                            }
                            if (std::holds_alternative<Function>(ret_decl.type.modifiers.front().var))
                            {
                                input = input_before_modifier;
                                return ParseError{.message = "Function return type can't be a function."};
                            }
                        }

                        Function func;

                        TrimLeadingWhitespace(input);

                        if (ConsumePunctuation(input, "..."))
                        {
                            TrimLeadingWhitespace(input);
                            if (!ConsumePunctuation(input, ")"))
                                return ParseError{.message = "Expected `)` after a C-style variadic parameter."};
                            func.c_style_variadic = true;
                        }
                        else if (ConsumePunctuation(input, ")"))
                        {
                            // Empty argument list.
                        }
                        else
                        {
                            TrimLeadingWhitespace(input);

                            // Check for C-style `(void)` parameter list.
                            func.c_style_void_params = false;
                            if (ConsumeWord(input, "void"))
                            {
                                std::string_view input_copy = input;
                                TrimLeadingWhitespace(input_copy);
                                if (input_copy.starts_with(')'))
                                {
                                    input = input_copy;
                                    input.remove_prefix(1);
                                    func.c_style_void_params = true;
                                }
                            }

                            // Parse the parameter list properly.
                            if (!func.c_style_void_params)
                            {
                                while (true)
                                {
                                    const auto input_before_param = input;

                                    auto param_result = ParseDecl(input, ParseDeclFlags::accept_unnamed | ParseDeclFlags::accept_unqualified_named);
                                    if (auto error = std::get_if<ParseError>(&param_result))
                                        return *error;
                                    MaybeAmbiguousDecl &param_decl = std::get<MaybeAmbiguousDecl>(param_result);

                                    if (param_decl.IsEmpty())
                                        return input = input_before_param, ParseError{.message = "Expected a function parameter."};

                                    // Propagate the ambiguity flag.
                                    ret_decl.has_nested_ambiguities = param_decl.IsAmbiguous();
                                    func.params.push_back(std::move(param_decl));

                                    if (ConsumePunctuation(input, ")"))
                                    {
                                        break; // No more parameters.
                                    }
                                    if (ConsumePunctuation(input, ","))
                                    {
                                        // Check for C-style variadic.
                                        TrimLeadingWhitespace(input);
                                        if (ConsumePunctuation(input, "..."))
                                        {
                                            TrimLeadingWhitespace(input);
                                            if (!ConsumePunctuation(input, ")"))
                                                return ParseError{.message = "Expected `)` after a C-style variadic parameter."};

                                            func.c_style_variadic = true;
                                            break; // No more parameters.
                                        }

                                        continue; // Have another parameter.
                                    }
                                    if (ConsumePunctuation(input, "..."))
                                    {
                                        // C-style variadic without the comma.

                                        TrimLeadingWhitespace(input);
                                        if (!ConsumePunctuation(input, ")"))
                                            return ParseError{.message = "Expected `)` after a C-style variadic parameter."};

                                        func.c_style_variadic = true;
                                        func.c_style_variadic_without_comma = true;
                                        break; // No more parameters.
                                    }

                                    return ParseError{.message = "Expected `)` or `,` or `...` in function parameter list."};
                                }
                            }
                        }

                        // Parse cv-qualifiers.
                        auto cvref_result = ParseCvQualifiers(input);
                        if (auto error = std::get_if<ParseError>(&cvref_result))
                            return *error;
                        func.cv_quals = std::get<CvQualifiers>(cvref_result);

                        // Parse ref-qualifiers. This automatically removes leading whitespace.
                        func.ref_quals = ParseRefQualifiers(input);
                        TrimLeadingWhitespace(input);

                        // Noexcept?
                        if (ConsumeWord(input, "noexcept"))
                        {
                            TrimLeadingWhitespace(input);
                            func.noexcept_ = true;
                            // Not trimming trailing whitespace here, it's not strictly necessary.
                        }

                        // Trailing return type?
                        const std::string_view input_before_trailing_arrow = input;
                        if (ConsumePunctuation(input, "->"))
                        {
                            // Complain if the return type wasn't `auto` before.
                            // Note the `.simple_type.` part. We don't want to reject non-empty `.modifiers`, because
                            //   the ones we have at this parsing stage don't apply to the return type, but rather to the function itself.
                            // And for the same reason we check `declarator_stack_pos`, since if it's positive, it means
                            if (
                                ret_decl.type.simple_type.AsSingleWord() != "auto" ||
                                // For the same reason, make sure the remaining declarator doesn't have anything other than parentheses,
                                //   since those would add unwanted stuff to our `auto` type.
                                std::any_of(declarator_stack.begin(), declarator_stack.begin() + declarator_stack_pos, [](const DeclaratorStackEntry &e){return !std::holds_alternative<OpenParen>(e.var);})
                            )
                            {
                                input = input_before_trailing_arrow;
                                return ParseError{.message = "A trailing return type is specified, but the previousy specified return type wasn't `auto`."};
                            }

                            auto ret_result = ParseType(input);
                            if (auto error = std::get_if<ParseError>(&ret_result))
                                return *error;

                            func.uses_trailing_return_type = true;

                            // Replace the return type with the new one.

                            auto &new_type = std::get<MaybeAmbiguousType>(ret_result);

                            ret_decl.type.simple_type = std::move(new_type.simple_type);
                            // Append modifiers to the end, after the "function" modifier.
                            ret_decl.type.modifiers.emplace_back(std::move(func));
                            ret_decl.type.modifiers.insert(ret_decl.type.modifiers.end(), std::make_move_iterator(new_type.modifiers.begin()), std::make_move_iterator(new_type.modifiers.end()));

                            continue;
                        }
                        else
                        {
                            ret_decl.type.modifiers.emplace_back(std::move(func));
                            continue;
                        }
                    }

                    break; // End of string or unknown syntax, nothing more to do.
                }
            }

            // Comsume the rest of the declarator stack.
            while (declarator_stack_pos != 0)
            {
                std::optional<ParseError> error;
                bool found_open_paren = PopDeclaratorFromStack(error);
                if (error)
                    return *error;

                if (found_open_paren)
                    return ParseError{.message = "Expected `)` to match this one."};
            }

            return ret_decl;
        };


        // If we only accept named declarations, don't bother with reparse candidates, do everything in one go.
        // Parses after the first one are never named anyway.
        if (!bool(flags & ParseDeclFlags::accept_unnamed))
            return ParseRemainingDecl();


        struct CandidateResult
        {
            ParseDeclResult ret;
            std::string_view input; // The state of input after parsing.
        };

        std::vector<CandidateResult> candidates;
        auto ret_backup = ret;
        candidates.emplace_back().ret = ParseRemainingDecl();
        candidates.back().input = input;
        ret = std::move(ret_backup);
        candidate_decl_name = {}; // Reset the name. It's only meaningful during the initial parse. All retries will always be unnamed.

        while (!declarator_stack.empty())
        {
            auto paren = std::get_if<OpenParen>(&declarator_stack.back().var);
            if (paren)
            {
                input = paren->input;
                ret_decl = std::move(paren->ret_backup);
            }
            declarator_stack.pop_back();
            if (paren)
            {
                ret_backup = ret;
                candidates.emplace_back().ret = ParseRemainingDecl();
                candidates.back().input = input;
                ret = std::move(ret_backup);
            }
        }

        std::size_t candidate_index = 0;

        // If there's more than one candidate, pick the best one.
        // We sort by successful parse, then by the amount of characters consumed (more is better, even on failure),
        //   then pick the latter candidate (normally there are only two ambiguous candidates, with the first one having redundant parentheses,
        //   so picking the second one makes more sense).
        // We always prefer successful parse, even if it consumed less characters than a failed one.
        // Other than that, both successful and failed parses are compared by the number of consumed characters, the more the better.
        bool ambiguous = false;
        if (candidates.size() > 1)
        {
            std::size_t min_unparsed_len = std::size_t(-1);
            bool have_successful_parse = false;

            for (std::size_t i = 0; const CandidateResult &c : candidates)
            {
                // Prefer candidates that don't have errors in them and have less unparsed junk at the end.
                if (
                    // First successful parse is always taken.
                    (!have_successful_parse && std::holds_alternative<MaybeAmbiguousDecl>(c.ret)) ||
                    // Otherwise sort by the number of characters consumed.
                    (have_successful_parse == std::holds_alternative<MaybeAmbiguousDecl>(c.ret) && c.input.size() <= min_unparsed_len)
                )
                {
                    if (have_successful_parse)
                        ambiguous = c.input.size() == min_unparsed_len;

                    // We update the info even if the input size matches, see above for rationale.
                    min_unparsed_len = c.input.size();
                    candidate_index = i;
                    have_successful_parse = std::holds_alternative<MaybeAmbiguousDecl>(c.ret);
                }

                i++;
            }

            // If we have ambiguities, stack them into a linked list.
            if (ambiguous)
            {
                MaybeAmbiguousDecl *cur_candidate = &std::get<MaybeAmbiguousDecl>(candidates[candidate_index].ret);
                for (std::size_t i = candidate_index; i-- > 0;)
                {
                    if (auto decl = std::get_if<MaybeAmbiguousDecl>(&candidates[i].ret); decl && candidates[i].input.size() == min_unparsed_len)
                    {
                        cur_candidate->ambiguous_alternative = std::make_unique<MaybeAmbiguousDecl>(std::move(*decl));
                        cur_candidate = decl;
                    }
                }
            }
        }

        // Return the selected candidate.
        ret = std::move(candidates[candidate_index].ret);
        // Restore the parse state for that candidate too.
        input = candidates[candidate_index].input;
        return ret;
    }

    // A subset of `ParseDecl()` that rejects named declarations.
    [[nodiscard]] inline ParseTypeResult ParseType(std::string_view &input, ParseTypeFlags flags)
    {
        ParseTypeResult ret;

        ParseDeclFlags decl_flags = ParseDeclFlags::accept_unnamed;
        if (bool(flags & ParseTypeFlags::only_left_side_declarators_without_parens))
            decl_flags |= ParseDeclFlags::accept_unnamed_only_left_side_declarators_without_parens;

        ParseDeclResult decl_result = ParseDecl(input, decl_flags);
        std::visit(Overload{
            [&](MaybeAmbiguousDecl &decl)
            {
                MaybeAmbiguousDecl *cur_in = &decl;
                MaybeAmbiguousType *cur_out = &std::get<MaybeAmbiguousType>(ret);

                *cur_out = std::move(cur_in->type);

                while (true)
                {
                    cur_in = cur_in->ambiguous_alternative.get();
                    if (!cur_in)
                        break;

                    cur_out->ambiguous_alternative = std::make_unique<MaybeAmbiguousType>(std::move(cur_in->type));
                    cur_out = cur_out->ambiguous_alternative.get();
                }
            },
            [&](ParseError &error)
            {
                ret = error;
            },
        }, decl_result);

        return ret;
    }

    // Parses a template argument list.
    // Returns null only if `input` (after skipping whitespace) doesn't start with `<`.
    [[nodiscard]] inline ParseTemplateArgumentListResult ParseTemplateArgumentList(std::string_view &input)
    {
        ParseTemplateArgumentListResult ret;

        TrimLeadingWhitespace(input);
        const std::string_view input_before_list = input;
        if (!ConsumePunctuation(input, "<"))
            return ret; // No argument list here, return nullopt.

        TemplateArgumentList &ret_list = std::get<std::optional<TemplateArgumentList>>(ret).emplace();

        TrimLeadingWhitespace(input);

        if (!ConsumePunctuation(input, ">"))
        {
            while (true)
            {
                TemplateArgument new_arg;

                // Try a declaration (unnamed).
                bool decl_ok = false;
                const std::string_view input_before_arg = input;
                auto type_result = ParseType(input);
                if (auto type = std::get_if<MaybeAmbiguousType>(&type_result))
                {
                    TrimLeadingWhitespace(input);
                    if (input.starts_with('>') || input.starts_with(','))
                    {
                        new_arg.var = std::move(*type);
                        decl_ok = true;
                    }
                }
                // Ignore any parse errors in `decl_result`, and ignore it completely if it's not followed by `>` or `,`.

                if (!decl_ok)
                {
                    input = input_before_arg;
                    auto expr_result = ParsePseudoExpr(input, ParsePseudoExprFlags::stop_on_gt_sign);
                    if (auto error = std::get_if<ParseError>(&expr_result))
                        return ret = *error, ret; // This is fatal.

                    auto &expr = std::get<PseudoExpr>(expr_result);
                    if (expr.IsEmpty())
                        return ret = ParseError{.message = "Expected template argument."}, ret;

                    new_arg.var = std::move(expr);
                }

                ret_list.args.push_back(std::move(new_arg));

                TrimLeadingWhitespace(input);
                if (ConsumePunctuation(input, ">"))
                    break;
                if (ConsumePunctuation(input, ","))
                    continue;

                if (input.empty())
                    return input = input_before_list, ret = ParseError{.message = "Unterminated template argument list."}, ret;

                // How did we get here?
                return ret = ParseError{.message = "Internal error: unparsed junk at the end of a template argument."}, ret;
            }
        }

        return ret;
    }
}
