// This little demo is designed to have compiler output piped through it, and tries to simplify any type names found in it.
// Turns out to not work that well in practice, because compilers already emit half-simplified output, which we can't properly simplify.

#include "cppdecl/declarations/parse.h"
#include "cppdecl/declarations/simplify.h"
#include "cppdecl/declarations/to_string.h"
#include "cppdecl/misc/string_helpers.h"

#include <iostream>
#include <string>

int main()
{
    std::string line;

    std::string out;
    while (std::getline(std::cin, line))
    {
        out.clear();
        out.reserve(line.size());

        std::string_view line_view = line;

        bool known_good_line = false;

        // While the remaining part of the line isn't empty...
        while (!line_view.empty())
        {
            // Try to find a punctuation that normally indicates the start of a type.

            std::size_t prefix_len = 0;
            std::string_view expected_suffix;

            if (line_view.starts_with('\'')) // Clang and GCC use `'foo'`.
            {
                prefix_len = 1;
                expected_suffix = "'";
            }
            else if (line_view.starts_with('=')) // I've seen MSVC use `T=foo`.
            {
                prefix_len = 1;
                // `expected_suffix` stays empty, I think it ends at the end of the line.
            }
            else if (line_view.starts_with("\u00AB")) // `LEFT-POINTING DOUBLE ANGLE QUOTATION MARK`, GCC uses this at least in the Russian locale.
            {
                prefix_len = 2;
                expected_suffix = "\u00BB"; // `RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK`
            }

            if (!prefix_len)
            {
                out += line_view.front();
                line_view.remove_prefix(1);
                continue;
            }

            out += line_view.substr(0, prefix_len);
            line_view.remove_prefix(prefix_len);
            if (line_view.empty())
                break;

            // Ensure we have an identifier character after the opening punctuation.
            if (!cppdecl::IsIdentifierChar(line_view.front()))
            {
                out += line_view.front();
                line_view.remove_prefix(1);
                continue;
            }

            // If we're not sure we should process this line yet...
            if (!known_good_line)
            {
                bool ok = false;
                if (auto pos = out.find(": "); pos != std::string::npos)
                {
                    std::string_view prefix(out.data(), pos);

                    // Check if before the `: ` we have `(123)` line number, MSVC style.
                    if (prefix.ends_with(')'))
                    {
                        prefix.remove_suffix(1);
                        if (!prefix.empty() && cppdecl::IsDigit(prefix.back()))
                        {
                            do
                                prefix.remove_suffix(1);
                            while (!prefix.empty() && cppdecl::IsDigit(prefix.back()));

                            if (prefix.ends_with(',')) // Handle `/diagnostics:column` adding a second comma-separated number.
                            {
                                prefix.remove_suffix(1);
                                if (!prefix.empty() && cppdecl::IsDigit(prefix.back()))
                                {
                                    do
                                        prefix.remove_suffix(1);
                                    while (!prefix.empty() && cppdecl::IsDigit(prefix.back()));

                                    if (prefix.ends_with('('))
                                        ok = true;
                                }
                            }
                            else
                            {
                                if (prefix.ends_with('('))
                                    ok = true;
                            }
                        }
                    }
                    // Or if before `: ` we have `:123:456` line number and position, GCC style.
                    else if (!prefix.empty() && cppdecl::IsDigit(prefix.back()))
                    {
                        do
                            prefix.remove_suffix(1);
                        while (!prefix.empty() && cppdecl::IsDigit(prefix.back()));

                        if (prefix.ends_with(':'))
                        {
                            prefix.remove_suffix(1);

                            if (!prefix.empty() && cppdecl::IsDigit(prefix.back()))
                            {
                                do
                                    prefix.remove_suffix(1);
                                while (!prefix.empty() && cppdecl::IsDigit(prefix.back()));

                                if (prefix.ends_with(':'))
                                    ok = true;
                            }
                        }
                    }
                }

                if (!ok)
                {
                    // Discard output, dump the entire input line as is.
                    std::cout << line;
                    out.clear();
                    break;
                }

                known_good_line = true;
            }

            std::string_view part_to_parse = line_view;

            // Try parsing.
            cppdecl::ParseTypeResult result = cppdecl::ParseType(part_to_parse);

            // Any errors, or didn't parse until the expected suffix?
            if (std::holds_alternative<cppdecl::ParseError>(result) || !part_to_parse.starts_with(expected_suffix))
            {
                // Skip this block of identifier characters, followed by any punctuation.
                while (!line_view.empty() && cppdecl::IsIdentifierChar(line_view.front()))
                {
                    out += line_view.front();
                    line_view.remove_prefix(1);
                }
                // Now the punctuation...
                while (!line_view.empty() && !cppdecl::IsIdentifierChar(line_view.front()))
                {
                    out += line_view.front();
                    line_view.remove_prefix(1);
                }

                continue;
            }

            auto &type = std::get<cppdecl::Type>(result);

            cppdecl::SimplifyTypeNames(cppdecl::SimplifyTypeNamesFlags::all, type);
            out += cppdecl::ToCode(type, {});

            // Count any trailing whitespace.
            std::size_t num_trailing_ws = 0;
            while (part_to_parse.data() > line_view.data() && cppdecl::IsWhitespace(part_to_parse.data()[-1-num_trailing_ws]))
                num_trailing_ws++;

            line_view.remove_prefix(part_to_parse.data() - line_view.data() - num_trailing_ws);
        }

        std::cout << out << '\n';
    }
}
