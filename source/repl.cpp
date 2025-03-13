#include "cppdecl/parsing/parse.h"

#include <iostream>
#include <string>

int main(int argc, char **argv)
{
    bool use_argv = argc > 1;

    int i = 1;
    std::string line;

    while (true)
    {
        if (use_argv)
        {
            if (i >= argc)
                break;
            std::cout << i << ". ";
        }

        std::cout << "\nDeclaration to parse:\n";
        const char *input_ptr = nullptr;
        if (use_argv)
        {
            input_ptr = argv[i++];
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

            std::cout << "Parsed to:\n";
            std::cout << std::get<cppdecl::MaybeAmbiguous<cppdecl::Decl>>(ret).ToDebugString(cppdecl::ToStringMode::pretty) << '\n';
        }
    }
}
