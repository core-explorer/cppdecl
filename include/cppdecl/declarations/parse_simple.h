#pragma once

#include "cppdecl/declarations/data.h"
#include "cppdecl/declarations/parse.h"
#include "cppdecl/misc/string_helpers.h"

#include <stdexcept>

// Some simple parsing functions that throw on failure, and throw if some part of the string was left unparsed.
// Those also don't support all of the entities, only the most popular ones.
//
// If you need more control than this, use `parse.h` directly. Same if you don't want exceptions.

namespace cppdecl
{
    [[nodiscard]] CPPDECL_CONSTEXPR Type ParseType_Simple(std::string_view input, ParseTypeFlags flags = {})
    {
        const std::string_view input_before_parse = input;
        ParseTypeResult ret = ParseType(input, flags);

        // Handle parse error.
        if (auto error = std::get_if<ParseError>(&ret))
            throw std::runtime_error("cppdecl: Parse error in type `" + std::string(input_before_parse) + "` at position " + NumberToString(input.data() - input_before_parse.data()) + ": " + error->message);

        // Handle unparsed junk at the end.
        assert(input.empty() || !IsWhitespace(input.back())); // `ParseType()` is expected to leave no whitespace.
        TrimLeadingWhitespace(input); // But trim it just in case.
        if (!input.empty())
            throw std::runtime_error("cppdecl: Unparsed junk in type `" + std::string(input_before_parse) + "` starting from position " + NumberToString(input.data() - input_before_parse.data()) + ": `" + std::string(input) + "`.");

        return std::get<Type>(ret);
    }

    // Note that the default value of `flags`, `ParseDeclFlags::accept_everything`, will happily accept unnamed declarations (i.e. just types).
    // If that's not what's desired, pass either `accept_all_named` or `accept_unqualified_named`.
    [[nodiscard]] CPPDECL_CONSTEXPR MaybeAmbiguousDecl ParseDecl_Simple(std::string_view input, ParseDeclFlags flags = ParseDeclFlags::accept_everything)
    {
        const std::string_view input_before_parse = input;
        ParseDeclResult ret = ParseDecl(input, flags);

        // Handle parse error.
        if (auto error = std::get_if<ParseError>(&ret))
            throw std::runtime_error("cppdecl: Parse error in declaration `" + std::string(input_before_parse) + "` at position " + NumberToString(input.data() - input_before_parse.data()) + ": " + error->message);

        // Handle unparsed junk at the end.
        assert(input.empty() || !IsWhitespace(input.back())); // `ParseDecl()` is expected to leave no whitespace.
        TrimLeadingWhitespace(input); // But trim it just in case.
        if (!input.empty())
            throw std::runtime_error("cppdecl: Unparsed junk in declaration `" + std::string(input_before_parse) + "` starting from position " + NumberToString(input.data() - input_before_parse.data()) + ": `" + std::string(input) + "`.");

        return std::get<MaybeAmbiguousDecl>(ret);
    }

    [[nodiscard]] CPPDECL_CONSTEXPR QualifiedName ParseQualifiedName_Simple(std::string_view input, ParseQualifiedNameFlags flags = {})
    {
        const std::string_view input_before_parse = input;
        ParseQualifiedNameResult ret = ParseQualifiedName(input, flags);

        // Handle parse error.
        if (auto error = std::get_if<ParseError>(&ret))
            throw std::runtime_error("cppdecl: Parse error in qualified name `" + std::string(input_before_parse) + "` at position " + NumberToString(input.data() - input_before_parse.data()) + ": " + error->message);

        // Handle unparsed junk at the end.
        // `ParseQualifiedName` is not guaranteed to not leave whitespace.
        if (!input.empty())
            throw std::runtime_error("cppdecl: Unparsed junk in qualified name `" + std::string(input_before_parse) + "` starting from position " + NumberToString(input.data() - input_before_parse.data()) + ": `" + std::string(input) + "`.");

        // Complain if this is a member pointer. Here we only want qualified names.
        if (std::holds_alternative<MemberPointer>(ret))
            throw std::runtime_error("cppdecl: Expected `" + std::string(input_before_parse) + "` to be a qualified name, but it includes a `::*`, which makes it a member pointer.");

        return std::get<QualifiedName>(ret);
    }
}
