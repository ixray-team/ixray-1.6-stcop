/***************************************************************************************
* Copyright (C) Anton Kovalev (vertver), 2023. All rights reserved.
* GRR - "Games Require Reflection", library for integrating reflection into games
* MIT License
***************************************************************************************/
#ifndef GRR_SERIALIZATION_HPP_INCLUDED
#define GRR_SERIALIZATION_HPP_INCLUDED

// NOTE: This code section is still under development. Don't use it in production
namespace grr
{
    inline std::uint64_t stoull(const char* str, int base = 10)
    {
        char* end_ptr;
        return std::strtoull(str, &end_ptr, base);
    }

    inline int64_t stoll(const char* str, int base = 10)
    {
        char* end_ptr;
        return std::strtoll(str, &end_ptr, base);
    }

    inline std::uint32_t stoul(const char* str)
    {
        return std::uint32_t(stoull(str));
    }

    inline double stof(const char* str)
    {
        char* end_ptr;
        return std::strtod(str, &end_ptr);
    }

    inline float stod(const char* str)
    {
        return float(stof(str));
    }

    struct custom_type
    {
        int hello;
        int world;
    };

    namespace detail
    { 
        constexpr bool is_digit(char c) 
        {
            return c <= '9' && c >= '0';
        }			

        template<typename T>
        constexpr T stoi_impl(const char* str, T value = 0)
        {
            return *str ? is_digit(*str) ? stoi_impl<T>(str + 1, (*str - '0') + value * 10) : throw "compile-time-error: not a digit": value;
        }

        template<typename T>
        constexpr T stoi_impl(const string_view& str)
        {
            if constexpr (std::is_floating_point_v<T>) {
                T left_part = 0.0f;
                T right_part = 0.0f;
                bool is_right_part = false;
                for (const auto& sym : str) {
                    if (sym == '.') {
                        is_right_part = true;
                    } else if (!is_digit(sym)) {
                        break;
                    }

                    if (is_right_part) { 
                        right_part = sym - '0' + right_part * (T)10;
                    } else {
                        left_part = sym - '0' + left_part * (T)10;
                    }
                }
            } else {
                T value = 0;
                for (const auto& sym : str) {
                    if (!is_digit(sym)) {
                        break;
                    }

                    value = sym - '0' + value * 10;
                }

                return value;
            }
        }

        template<typename T>
        constexpr T stoi(const char* str) {
            return stoi_impl<T>(str);
        }		
        
        template<typename T>
        constexpr T stoi(const string_view& str) {
            return stoi_impl<T>(str);
        }

        template<typename T>
        inline string numeric_to_string(T value)
        {
            char buffer[32] = {};

            if constexpr (std::is_same_v<T, std::uint8_t>) {
                snprintf(buffer, 32, "%u", value);
            } else if constexpr (std::is_same_v<T, std::uint16_t>) {
                snprintf(buffer, 32, "%u", value);
            } else if constexpr (std::is_same_v<T, std::uint32_t>) {
                snprintf(buffer, 32, "%u", value);
            } else if constexpr (std::is_same_v<T, std::uint64_t>) {
                snprintf(buffer, 32, "%llu", value);
            } else if constexpr (std::is_same_v<T, float> || std::is_same_v<T, double>) {
                snprintf(buffer, 32, "%f", value);
            } else if constexpr (std::is_same_v<T, std::int8_t>) {
                snprintf(buffer, 32, "%d", value);
            } else if constexpr (std::is_same_v<T, std::int16_t>) {
                snprintf(buffer, 32, "%d", value);
            } else if constexpr (std::is_same_v<T, std::int32_t>) {
                snprintf(buffer, 32, "%d", value);
            } else if constexpr (std::is_same_v<T, std::int64_t>) {
                snprintf(buffer, 32, "%l", value);
            } else {
                snprintf(buffer, 32, "%i", value);
            }

            return buffer;
        }

        template<typename T>
        inline grr::string stringify_item(T&& value)
        {
            using ValueCleanType = grr::clean_type<decltype(value)>;

            if constexpr (std::is_same_v<ValueCleanType, bool>) {
                return grr::string(value == true ? "true" : "false");
            } else if constexpr (std::is_same_v<ValueCleanType, grr::string>) {
                return value;
            } else if constexpr (std::is_same_v<ValueCleanType, grr::string_view>) {
                return value.data();
            } else if constexpr (std::is_enum_v<ValueCleanType>) {
                return numeric_to_string(static_cast<int64_t>(value));
            } else if constexpr (std::is_integral_v<ValueCleanType>) {
                return numeric_to_string(static_cast<ValueCleanType>(value));
            } else {
                return "";
                //return grr::to_string<ValueCleanType>(value);
            }
        }

        template<typename T>
        inline T unstringify_item(grr::string_view&& value)
        {
            using ValueCleanType = grr::clean_type<T>;
            if constexpr (std::is_same_v<ValueCleanType, bool>) {
                constexpr std::uint64_t true_hash = grr::binhash<std::uint64_t>("true");
                constexpr std::uint64_t false_hash = grr::binhash<std::uint64_t>("false");
                std::uint64_t hash = grr::binhash<std::uint64_t>(value);
                if (hash == true_hash) {
                    return true;
                }		
                
                if (hash == false_hash) {
                    return false;
                }

                return false;
            } else if constexpr (std::is_same_v<ValueCleanType, grr::string>) {
                return grr::string(value.begin(), value.end());
            } else if constexpr (std::is_same_v<ValueCleanType, grr::string_view>) {
                return value;
            } else if constexpr (std::is_enum_v<ValueCleanType>) {
                return static_cast<ValueCleanType>(stoi<std::uint64_t>(value));
            } else if constexpr (std::is_integral_v<ValueCleanType>) {
                return stoi<std::uint64_t>(value);
            } else {
                return {};
            }
        }
    }

    template<typename T>
    struct custom_serializer
    {
        static constexpr bool exists = false;
    };

    template<>
    struct custom_serializer<custom_type>
    {
        static constexpr bool exists = true;

        static inline grr::type_id id()
        {
            return grr::obtain_id("custom_type");
        }

        static inline bool verify_id(grr::type_id cmp_id)
        {
            return (id() == cmp_id);
        }

        static inline grr::string stringify(custom_type value)
        {
            (void)value;
            return detail::numeric_to_string(value.hello) + " " + detail::numeric_to_string(value.world);
        }

        static inline custom_type unstrigify(grr::string_view value)
        {
            (void)value;
            return {};
        }

        static inline std::size_t serialize_size(custom_type value)
        {
            (void)value;
            return sizeof(custom_type);
        }

        static inline void serialize(custom_type value, void* memory)
        {
            *reinterpret_cast<custom_type*>(memory) = value;
        }

        static inline custom_type deserialize(const void* memory)
        {
            return (*reinterpret_cast<const custom_type*>(memory));
        }
    };

    template<typename T>
    inline grr::string stringify(T&& value)
    {
        using CleanType = grr::clean_type<T>;
        static_assert(!std::is_pointer_v<CleanType> || std::is_same_v<CleanType, char*>, "Pointers are not supported by grr::stringify.");

        grr::string string_value;  
        if constexpr (custom_serializer<CleanType>::exists) {
            return custom_serializer<CleanType>::stringify(value);
        } else {
            if constexpr (std::is_same_v<CleanType, char*>) {
                return grr::string(value);
            } else if constexpr (grr::is_container_v<CleanType>) {
                grr::string vector_string;
                vector_string += "{ ";
                if constexpr (grr::is_key_value_map_v<CleanType>) {
                    for (const auto& [key, val] : value) {
                        vector_string += "(";
                        vector_string += grr::stringify(key);
                        vector_string += " ";
                        vector_string += grr::stringify(val);
                        vector_string += ")";
                        vector_string += " ";
                    }
                } else {
                    std::uint64_t counter = 0;
                    for (const auto& elem : value) {
                        counter++;
                        auto elem_string = grr::stringify(elem);
                        vector_string += elem_string;					
                        if (counter < value.size()) {
                            vector_string += ", ";
                        }
                    }
                }

                vector_string += "}";	
                return vector_string;
            } else {
                if constexpr (custom_serializer<CleanType>::exists) {
                    return custom_serializer<CleanType>::stringify(value);
                } else {
                    return grr::detail::stringify_item(value);
                }
            }
        }
    }

    template<typename T>
    inline T unstringify(grr::string_view&& value, std::error_code& err)
    {
        using CleanType = grr::clean_type<T>;
        static_assert(!std::is_pointer_v<CleanType> || std::is_same_v<CleanType, char*>, "Pointers are not supported by grr::unstringify.");

        if constexpr (std::is_same_v<CleanType, char*>) {
            return value.data();
        } else if constexpr (grr::is_container_v<CleanType>) {
            using value_type = typename CleanType::value_type;
            CleanType out_value;
            std::size_t offset = value.find_first_of('{');
            if (offset == std::size_t(-1)) {
                err = make_error_code(errors::parsing_failed);
                return out_value;
            }

            offset = value.find_first_not_of(' ', offset + 1);
            if (offset == std::size_t(-1)) {
                err = make_error_code(errors::parsing_failed);
                return out_value;
            }

            std::size_t offset_end = value.find_last_of('}');
            if (offset_end == std::size_t(-1)) {
                offset_end = value.size() - 1;
            }

            while (offset != std::size_t(-1) && offset < offset_end) {
                if constexpr (grr::is_key_value_map_v<CleanType>) {
                    /*
                    const std::size_t key_offset_begin = value.find_first_not_of(' ', begin_offset);
                    const std::size_t key_offset_end = value.find_first_of(' ', key_offset_begin);
                    if (key_offset_begin == std::size_t(-1) || key_offset_end == std::size_t(-1)) {
                        err = make_error_code(errors::parsing_failed);
                        return out_value;
                    }

                    const std::size_t value_offset_begin = value.find_first_not_of(' ', key_offset_end);
                    const std::size_t value_offset_end = value.find_first_of(' ', value_offset_begin);
                    if (value_offset_begin == std::size_t(-1) || value_offset_end == std::size_t(-1)) {
                        err = make_error_code(errors::parsing_failed);
                        return out_value;
                    }

                    const grr::string_view key_string = grr::string_view(value.data() + key_offset_begin, value.data() + key_offset_end);
                    const grr::string_view value_string = grr::string_view(value.data() + value_offset_begin, value.data() + value_offset_end);
                    */
                } else {
                    std::size_t value_offset_begin = value.find_first_of('{', offset);
                    std::size_t value_offset_end = value.find_first_of('}', value_offset_begin);
                    if (value_offset_begin == std::size_t(-1)) {
                        err = make_error_code(errors::parsing_failed);
                        return out_value;
                    }

                    if (value_offset_end == std::size_t(-1)) {
                        value_offset_end = offset_end;
                    } else {
                        if constexpr (grr::is_container_v<value_type>) {
                            value_offset_end++;
                        }
                    }

                    offset = value_offset_end + 1;
                    //auto value_string = grr::string_view(value.data() + value_offset_begin, value.data() + value_offset_end);
                    //out_value.emplace_back(grr::unstringify<value_type>(std::move(value_string), err));
                }
            }

            return out_value;
        } else {
            if constexpr (custom_serializer<CleanType>::exists) {
                return custom_serializer<CleanType>::unstrigify(std::move(value));
            } else {
                return grr::detail::unstringify_item<CleanType>(std::move(value));
            }
        }
    }
}

#endif