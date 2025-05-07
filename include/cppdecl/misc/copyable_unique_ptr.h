#pragma once

#include <concepts>
#include <memory>

namespace cppdecl
{
    // Like `std::unique_ptr`, but copyable (which doesn't work properly with derived classes).
    // Also `==` compares the pointed value for those.
    template <typename T>
    struct copyable_unique_ptr : std::unique_ptr<T>
    {
        using std::unique_ptr<T>::unique_ptr;

        constexpr copyable_unique_ptr(const copyable_unique_ptr<T> &other) : copyable_unique_ptr(static_cast<const std::unique_ptr<T> &>(other)) {}
        constexpr copyable_unique_ptr(copyable_unique_ptr<T> &&other) noexcept : copyable_unique_ptr(static_cast<std::unique_ptr<T> &&>(other)) {}
        constexpr copyable_unique_ptr &operator=(const copyable_unique_ptr<T> &other) {return *this = static_cast<const std::unique_ptr<T> &>(other);}
        constexpr copyable_unique_ptr &operator=(copyable_unique_ptr<T> &&other) noexcept {return *this = static_cast<std::unique_ptr<T> &&>(other);}

        constexpr copyable_unique_ptr(const std::unique_ptr<T> &other) : std::unique_ptr<T>(other ? std::make_unique<T>(*other) : nullptr) {}
        constexpr copyable_unique_ptr(std::unique_ptr<T> &&other) noexcept : std::unique_ptr<T>(std::move(other)) {}
        constexpr copyable_unique_ptr &operator=(const std::unique_ptr<T> &other)
        {
            std::unique_ptr<T>::operator=(other ? std::make_unique<T>(*other) : nullptr);
            return *this;
        }
        constexpr copyable_unique_ptr &operator=(std::unique_ptr<T> &&other) noexcept
        {
            std::unique_ptr<T>::operator=(std::move(other));
            return *this;
        }

        // Need this, otherwise assigning nullptr is ambiguous.
        constexpr copyable_unique_ptr &operator=(std::nullptr_t)
        {
            std::unique_ptr<T>::operator=(nullptr);
            return *this;
        }
    };

    // This is a non-friend to allow passing an incomplete type as the template parameter `copyable_unique_ptr<T>`,
    //   and not having the wrong value of `std::equality_comparable` baked.
    template <std::equality_comparable T>
    [[nodiscard]] constexpr bool operator==(const copyable_unique_ptr<T> &a, const copyable_unique_ptr<T> &b)
    {
        return
            bool(a) == bool(b) &&
            (!a || *a == *b);
    }
}
