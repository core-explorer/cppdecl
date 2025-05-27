#include "cppdecl/declarations/parse.h"
#include "cppdecl/declarations/simplify.h"
#include "cppdecl/declarations/to_string.h"

#include <iostream>
#include <string>

int main(int argc, char **argv)
{
    bool use_argv = argc > 1;

    int i = 1;
    std::string line;

    while (true)
    {
        std::cout << "\n--- ";

        if (use_argv)
        {
            if (i >= argc)
                break;
            std::cout << i << ". ";
        }

        std::cout << "Declaration to parse:\n";
        const char *input_ptr = nullptr;
        if (use_argv)
        {
            input_ptr = argv[i++];
            std::cout << input_ptr << '\n';
        }
        else
        {
            if (!std::getline(std::cin, line))
                break;
            input_ptr = line.c_str();
        }

        std::string_view input = input_ptr;
        auto ret = cppdecl::ParseDecl(input, cppdecl::ParseDeclFlags::accept_everything);

        if (!input.empty() || std::holds_alternative<cppdecl::ParseError>(ret))
        {
            std::cout << std::string(std::size_t(input.data() - input_ptr), ' ') << "^\n";
        }

        if (auto error = std::get_if<cppdecl::ParseError>(&ret))
        {
            std::cout << "Parse error: " << error->message << '\n';
        }
        else
        {
            if (!input.empty())
                std::cout << "Unparsed junk at the end of input.\n";

            auto &decl = std::get<cppdecl::MaybeAmbiguousDecl>(ret);

            std::cout << "\n--- Parsed to:\n";
            std::cout << cppdecl::ToString(decl, {}) << '\n';

            cppdecl::MaybeAmbiguousDecl simplified_decl = decl;
            cppdecl::SimplifyTypeNames(cppdecl::SimplifyTypeNamesFlags::all, simplified_decl);
            if (simplified_decl != decl)
            {
                std::cout << "\n--- Simplifies to:\n";
                std::cout << cppdecl::ToCode(simplified_decl, {}) << '\n';

                std::cout << "\n--- The simplified version parses to:\n";
                std::cout << cppdecl::ToString(simplified_decl, {}) << '\n';
            }
        }
    }
}
