#pragma once

namespace boost_legacy
{
    class no_copyable
    {
    public:
        no_copyable() = default;
        no_copyable(no_copyable&&) = default;
        virtual ~no_copyable() = default;

    private:
        no_copyable(const no_copyable&) = delete;
        virtual no_copyable& operator=(const no_copyable&) = delete;
    };
}

namespace boost_legacy
{
    template <typename T>
    struct is_reference_wrapper : public std::false_type
    {
    };

    template <typename T>
    struct is_reference_wrapper<std::reference_wrapper<T>> : public std::true_type
    {
    };

    template <typename T>
    constexpr bool is_reference_wrapper_v = is_reference_wrapper<T>::value;
}

// MPL Containers 
namespace boost_legacy
{
    class end_t {};

    template <typename Predicate, typename... Ts>
    struct find_if;

    template <typename Predicate, typename T, typename... Ts>
    struct find_if<Predicate, T, Ts...>
    {
    private:
        using result = typename Predicate::template type<T>;
    public:
        using type = std::conditional_t<
            result::value,
            T,
            typename find_if<Predicate, Ts...>::type
        >;
    };

    template <typename Predicate, typename T>
    struct find_if<Predicate, T>
    {
    private:
        using result = typename Predicate::template type<T>;
    public:
        using type = std::conditional_t<
            result::value,
            T,
            end_t
        >;
    };

    template <typename Predicate>
    struct find_if<Predicate>
    {
        using type = end_t;
    };

    template <typename Predicate, typename... Ts>
    using find_if_t = typename find_if<Predicate, Ts...>::type;
}