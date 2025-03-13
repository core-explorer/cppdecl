#pragma once

#include "cppdecl/parsing/helpers.h"
#include "cppdecl/parsing/result.h"

#include <algorithm>
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


    using ParseTemplateArgumentListResult = std::variant<std::optional<TemplateArgumentList>, ParseError>;
    [[nodiscard]] inline ParseTemplateArgumentListResult ParseTemplateArgumentList(std::string_view &input);



    // NOTE: This can return either a `QualifiedName` OR a `MemberPointer` on success (the latter is returned if it's followed by `:: * [cv]`.
    using ParseQualifiedNameResult = std::variant<QualifiedName, MemberPointer, ParseError>;

    // Returns a `QualifiedName` with no elements if there's nothing to parse.
    // Ignores leading whitespace. Modifies `input` to remove the parsed prefix (unless there was an error).
    // When `input` is modified, the trailing whitespace is stripped automatically. This happens even if there was nothing to parse.
    // If the input ends with `:: * [cv]` (as in a member pointer), returns a `MemberPointer` instead of a `QualifiedName`.
    // NOTE: This doesn't understand `long long` (hence "Low"), use `ParseDecl()` to support that.
    [[nodiscard]] inline ParseQualifiedNameResult ParseQualifiedName(std::string_view &input)
    {
        ParseQualifiedNameResult ret;
        QualifiedName &ret_name = std::get<QualifiedName>(ret);

        TrimLeadingWhitespace(input);

        if (input.empty())
            return ret; // Nothing to parse.

        std::string_view s = input;

        // Handle leading `::`.
        if (s.starts_with("::"))
        {
            ret_name.force_global_scope = true;
            s.remove_prefix(2);
            TrimLeadingWhitespace(s);
        }

        if (!s.empty() && IsNonDigitIdentifierChar(s.front()))
        {
            while (true)
            {
                const char *cur = s.data();

                do
                    cur++;
                while (cur < input.data() + input.size() && IsIdentifierChar(*cur));

                UnqualifiedName &this_part = ret_name.parts.emplace_back();
                this_part.name = std::string_view(s.data(), cur);
                s.remove_prefix(std::size_t(cur - s.data()));
                TrimLeadingWhitespace(s);

                // Advance `input`. If there will be no closing `>`, we want to point at the opening one.
                input = s;

                // Consume the template arguments, if any.
                auto arglist_result = ParseTemplateArgumentList(input);
                if (auto error = std::get_if<ParseError>(&arglist_result))
                    return ret = *error, ret;
                this_part.template_args = std::move(std::get<std::optional<TemplateArgumentList>>(arglist_result));

                s = input;

                // At this point the trailing whitespace is already skipped.

                // Make sure we have `:: letter` after this, or break.
                if (s.starts_with("::"))
                {
                    s.remove_prefix(2);
                    TrimLeadingWhitespace(s);
                    if (!s.empty())
                    {
                        if (IsNonDigitIdentifierChar(s.front()))
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
            type.name.parts.front().name = word;
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
            type.name.parts.front().name = "long long";
            return true;
        }
        // long + double
        if ((word == "long" && existing_word == "double") || (word == "double" && existing_word == "long"))
        {
            type.name.parts.front().name = "long double";
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

    using ParseSimpleTypeResult = std::variant<SimpleType, ParseError>;
    // Parse a "simple type". Very similar to `ParseQualifiedName`, but also combines `long` + `long`, and similar things.
    // Returns an empty type if nothing to parse.
    [[nodiscard]] inline ParseSimpleTypeResult ParseSimpleType(std::string_view &input)
    {
        ParseSimpleTypeResult ret;
        SimpleType &ret_type = std::get<SimpleType>(ret);

        while (true)
        {
            const std::string_view input_before_name;

            auto name_result = ParseQualifiedName(input);
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


    using ParsePseudoExprResult = std::variant<PseudoExpr, ParseError>;
    // Parse an expression. Even though we call those expressions, it's a fairly loose collection of tokens.
    // We continue parsing until we hit a comma or a closing bracket: `)`,`}`,`]`,`>`.
    // Can return an empty expression.
    [[nodiscard]] inline ParsePseudoExprResult ParsePseudoExpr(std::string_view &input)
    {
        ParsePseudoExprResult ret;
        PseudoExpr &ret_expr = std::get<PseudoExpr>(ret);

        while (true)
        {
            TrimLeadingWhitespace(input);

            if (input.empty() || input.starts_with(',') || input.starts_with(')') || input.starts_with('}') || input.starts_with(']'))
                return ret;

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

                StringLiteral lit;

                if (ConsumePunctuation(input_copy, "L"))
                    lit.type = StringLiteral::Type::wide;
                else if (ConsumePunctuation(input_copy, "u8")) // Check before `u` since that is a substring of this.
                    lit.type = StringLiteral::Type::u8;
                else if (ConsumePunctuation(input_copy, "u"))
                    lit.type = StringLiteral::Type::u16;
                else if (ConsumePunctuation(input_copy, "U"))
                    lit.type = StringLiteral::Type::u32;

                bool ok = true;
                if (ConsumePunctuation(input_copy, "\""))
                    lit.kind = StringLiteral::Kind::string;
                else if (ConsumePunctuation(input_copy, "'"))
                    lit.kind = StringLiteral::Kind::character;
                else if (ConsumePunctuation(input_copy, "R\""))
                    lit.kind = StringLiteral::Kind::raw_string;
                else
                    ok = false;

                if (ok)
                {
                    // Now we're sure that it's a string literal.

                    const std::string_view input_at_start_of_literal = input;
                    input = input_copy;

                    if (lit.kind != StringLiteral::Kind::raw_string)
                    {
                        char quote = lit.kind == StringLiteral::Kind::character ? '\'' : '"';
                        while (true)
                        {
                            if (input.empty())
                            {
                                const char *error = nullptr;
                                switch (lit.kind)
                                {
                                    case StringLiteral::Kind::character:  error = "Unterminated character literal that starts here."; break;
                                    case StringLiteral::Kind::string:     error = "Unterminated string literal that starts here."; break;
                                    case StringLiteral::Kind::raw_string: break; // Unreachable.
                                }
                                input = input_at_start_of_literal;
                                return ret = ParseError{.message = error}, ret;
                            }

                            if (ConsumePunctuation(input, "\\"))
                            {
                                if (input.empty())
                                    return ret = ParseError{.message = "Unterminated escape sequence."}, ret;
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
                                return input = input_at_start_of_literal, ret = ParseError{.message = "Unterminated delimiter at the beginning of a raw string that starts here."}, ret;

                            if (ConsumePunctuation(input, "("))
                                break; // End of delimiter.

                            // If not a valid delimiter character...
                            // Valid characters are as specified in https://eel.is/c++draft/tab:lex.charset.basic minus those in https://eel.is/c++draft/lex.string#nt:d-char
                            if (!(IsAlpha(input.front()) || IsDigit(input.front()) || std::string_view("!\"#$%&\')*+,-./:;<=>?@[]^_`{|}~").contains(input.front())))
                                return ret = ParseError{.message = "Invalid character in a raw string literal delimiter."}, ret;

                            if (lit.raw_string_delim.size() >= 16)
                                return ret = ParseError{.message = "Raw string literal delimiter is too long."}, ret;

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
                                case PseudoExprList::Kind::parentheses: error = "Expected expression or `)`  or `,`."; break;
                                case PseudoExprList::Kind::curly:       error = "Expected expression or `}`  or `,`."; break;
                                case PseudoExprList::Kind::square:      error = "Expected expression or `]`  or `,`."; break;
                            }

                            return ret = ParseError{.message = error}, ret;
                        }
                    }

                    ret_expr.tokens.emplace_back(std::move(list));
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

                ret_expr.tokens.emplace_back(std::move(std::get<SimpleType>(type_result)));
            }

            { // Punctuation. This must be last, this catches all unknown tokens.
                PunctuationToken punct;
                using namespace std::string_view_literals;

                bool found = false;

                for (std::string_view token : {
                    // All multicharacter tokens from: https://eel.is/c++draft/lex.operators#nt:operator-or-punctuator
                    // Excluding identifier-like operator names, we don't bother supporting those. (Because then what about all the other keywords
                    //   what were catched by `SimpleType`? Screw that.)
                    "<:"sv, ":>"sv, "<%"sv , "%>"sv , "..."sv,
                    "::"sv, ".*"sv, "->"sv , "->*"sv,
                    "+="sv, "-="sv, "*="sv , "/="sv , "%="sv , "^="sv, "&="sv, "|="sv,
                    "=="sv, "!="sv, "<="sv , ">="sv , "<=>"sv, "&&"sv, "||"sv,
                    "<<"sv, ">>"sv, "<<="sv, ">>="sv, "++"sv , "--"sv,
                })
                {
                    if (ConsumePunctuation(input, token))
                    {
                        found = true;
                        punct.value = token;
                        break;
                    }
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
    };
    TYPENAMES_FLAG_OPERATORS(ParseDeclFlags)
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
                ret_decl.type.simple_type.name.parts.push_back(UnqualifiedName{.name = "int", .template_args = {}});
            else
                return ret = ParseError{.message = "Expected a type."}, ret;
        }

        // Now the declarators:

        bool have_any_parens_in_declarator_on_initial_parse = false;

        // We parse what looks like the variable name into this.
        ParseQualifiedNameResult candidate_decl_name;
        std::string_view input_before_candidate_decl_name;

        // If we didn't already get a variable name from parsing the decl-specifier-seq, parse until we find one, or until we're sure there's none.
        if (ret_decl.name.IsEmpty())
        {
            while (true)
            {
                TrimLeadingWhitespace(input);
                if (input.starts_with('('))
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
                const auto input_before_name = input;

                input_before_candidate_decl_name = input;
                candidate_decl_name = ParseQualifiedName(input);
                if (auto error = std::get_if<ParseError>(&candidate_decl_name))
                    return ret = *error, ret;

                if (auto memptr = std::get_if<MemberPointer>(&candidate_decl_name))
                {
                    declarator_stack.emplace_back(std::move(*memptr), input_before_name);
                    candidate_decl_name = {}; // Nuke the name. It's checked before this loop, so it should be empty, just in case.
                    continue;
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
                bool done = std::visit([&]<typename T>(T &elem)
                {
                    if (std::holds_alternative<Reference>(declarator_stack[declarator_stack_pos-1].var) && !ret_decl.type.modifiers.empty())
                    {
                        out_error = ParseError{.message = "Reference can only apply at the top level."};
                        input = declarator_stack[declarator_stack_pos-1].location;
                        return true;
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
                }, declarator_stack[declarator_stack_pos-1].var);

                declarator_stack_pos--;

                return done;
            };

            // Continue parsing to the end of the declaration.
            while (true)
            {
                TrimLeadingWhitespace(input);

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

                if (input.starts_with('('))
                {
                    // const auto input_before_parens = input;
                    input.remove_prefix(1);

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
                    func.quals = std::get<CvQualifiers>(cvref_result);

                    // Parse ref-qualifiers. This automatically removes leading whitespace.
                    func.ref_quals = ParseRefQualifiers(input);
                    TrimLeadingWhitespace(input);

                    // Noexcept?
                    if (ConsumeWord(input, "noexcept"))
                    {
                        func.noexcept_ = true;
                        // Not trimming trailing whitespace here, it's not strictly necessary.
                    }

                    ret_decl.type.modifiers.emplace_back(std::move(func));
                    continue;
                }

                break; // End of string or unknown syntax, nothing more to do.
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
    using ParseTypeResult = std::variant<MaybeAmbiguousType, ParseError>;
    [[nodiscard]] inline ParseTypeResult ParseType(std::string_view &input)
    {
        ParseTypeResult ret;

        ParseDeclResult decl_result = ParseDecl(input, ParseDeclFlags::accept_unnamed);
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
                    auto expr_result = ParsePseudoExpr(input);
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
