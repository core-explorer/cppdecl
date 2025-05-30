#pragma once

#include <cassert>
#include <concepts>
#include <memory>
#include <utility>

namespace cppdecl
{
    // Similar to C++26 `std::indirect`, but properly nullable.
    // Essentially a copyable `std::unique_ptr` without support for derived classes, with added const correctness, and with comparisons comparing the pointed object.
    // We don't use `std::unique_ptr` internally to make this constexpr even on C++20, whereas `std::unique_ptr` is only constexpr in C++23.
    template <typename T>
    class IndirectOptional
    {
        T *target = nullptr;

        // Must use `std::allocator` to be constexpr.
        using A = std::allocator<T>;
        using AT = std::allocator_traits<A>;

        template <typename ...P>
        constexpr void ConstructValue(P &&... params)
        {
            assert(!target);

            struct Guard
            {
                IndirectOptional *self;
                constexpr ~Guard() noexcept
                {
                    if (self)
                    {
                        A a;
                        AT::deallocate(a, self->target, 1);
                        self->target = nullptr;
                    }
                }
            };
            A a;
            target = AT::allocate(a, 1);
            Guard guard{this}; // This frees the memory if the construction throws.
            AT::construct(a, target, std::forward<P>(params)...);
            guard.self = nullptr; // Disarm the guard.
        }

      public:
        constexpr IndirectOptional() {}
        constexpr IndirectOptional(const IndirectOptional &other) {if (other) ConstructValue(*other);}
        constexpr IndirectOptional(IndirectOptional &&other) noexcept : target(other.target) {other.target = nullptr;}
        constexpr IndirectOptional &operator=(IndirectOptional other) noexcept {std::swap(target, other.target); return *this;}
        constexpr ~IndirectOptional()
        {
            if (target)
            {
                A a;
                AT::destroy(a, target);
                AT::deallocate(a, target, 1);
            }
        }

        constexpr IndirectOptional(const T &other) {ConstructValue(other);}
        constexpr IndirectOptional(T &&other) {ConstructValue(std::move(other));}

        [[nodiscard]] constexpr explicit operator bool() const {return target;}

        [[nodiscard]] constexpr       T &operator*()       {assert(target); return *target;}
        [[nodiscard]] constexpr const T &operator*() const {assert(target); return *target;}

        [[nodiscard]] constexpr       T *operator->()       {assert(target); return target;}
        [[nodiscard]] constexpr const T *operator->() const {assert(target); return target;}

        // This can return null.
        [[nodiscard]] constexpr       T *get()       {return target;}
        [[nodiscard]] constexpr const T *get() const {return target;}
    };

    // This is a non-friend to allow passing an incomplete type as the template parameter `IndirectOptional<T>`,
    //   and not having the wrong value of `std::equality_comparable` baked.
    template <std::equality_comparable T>
    [[nodiscard]] constexpr bool operator==(const IndirectOptional<T> &a, const IndirectOptional<T> &b)
    {
        return
            bool(a) == bool(b) &&
            (!a || *a == *b);
    }
}
