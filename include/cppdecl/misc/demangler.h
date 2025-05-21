#pragma once

#ifndef _MSC_VER
#include <cxxabi.h>
#include <cstddef> // For `std::size_t`.
#include <cstdlib> // For `std::free`.
#endif

namespace cppdecl
{
    class Demangler
    {
        #ifndef _MSC_VER
        // We keep those here so that `abi::__cxa_demangle()` can reuse its buffer between calls.
        char *buf_ptr = nullptr;
        std::size_t buf_size = 0;
        #endif

      public:
        Demangler() {}
        Demangler(const Demangler &) = delete;
        Demangler &operator=(const Demangler &) = delete;
        ~Demangler()
        {
            #ifndef _MSC_VER
            std::free(buf_ptr); // Does nothing if `buf_ptr` is null.
            #endif
        }

        // Demangles the `typeid(...).name()` if necessary, or returns `name` as is if this platform doesn't mangle the type names.
        // The resulting pointer is either owned by this `Demangler` instance (and is reused on the next call), or is just `name` as is.
        // So for portable use ensure that both the `Demangler` instance and the `name` stay alive as long as you use the resulting pointer.
        // Returns null on demangling failure, but I've never seen that happen.
        [[nodiscard]] const char *operator()(const char *name)
        {
            #ifdef _MSC_VER
            return name;
            #else
            int status = -4; // Some custom error code, in case `abi::__cxa_demangle()` doesn't modify it at all for some reason.
            buf_ptr = abi::__cxa_demangle(name, buf_ptr, &buf_size, &status);
            if (status != 0) // -1 = out of memory, -2 = invalid string, -3 = invalid usage
                return nullptr; // Unable to demangle.
            return buf_ptr;
            #endif
        }
    };
}
