/***************************************************************************************
* Copyright (C) Anton Kovalev (vertver), 2023. All rights reserved.
* GRR - "Games Require Reflection", library for integrating reflection into games
* MIT License
***************************************************************************************/
#ifndef GRR_BASE_HPP_INCLUDED
#define GRR_BASE_HPP_INCLUDED

namespace grr
{
    enum class errors : int
    {
        invalid_argument,
        invalid_type,
        invalid_ordering,
        unregistered_id,
        already_registered,
        parsing_failed,
        out_of_range
    };

    struct error_category : public std::error_category
    {
        std::string message(int c) const
        {
            static const char* err_msg[] =
            {
                "Invalid argument",
                "Unregisted ID",
                "Already registered",
                "Parsing failed",
                "Out of range"
            };

            return err_msg[c];
        }

        const char* name() const noexcept { return "GRR Error code"; }
        const static error_category& get()
        {
            const static error_category category_const;
            return category_const;
        }
    };

    static inline std::error_code make_error_code(errors e)
    {
        return std::error_code(static_cast<int>(e), error_category::get());
    }

    struct field
    {
        GRR_CONSTEXPR field(const char* new_name, type_id new_id, std::size_t new_offset)
            : name(new_name), id(new_id), offset(new_offset) {}

        GRR_CONSTEXPR field(const string_view& new_name, type_id new_id, std::size_t new_offset)
            : name(new_name), id(new_id), offset(new_offset) {}

        field() = default;
        field(const field&) = default;
        field(field&&) = default;
        field& operator=(const field&) = default;
        field& operator=(field&&) = default;

        std::size_t offset;
        type_id id;
        string name;
    };

    struct type_context
    {
        type_id base_type;
        std::size_t size;
        string name;
        vector<field> fields;
    };

    class context
    {
    private:
        using storage_map = hash_map<type_id, type_context>;
        hash_map<type_id, type_context> storage;

    public:
        type_context& at(type_id id)
        {
            return storage.at(id);
        }

        const type_context& at(type_id id) const
        {
            return storage.at(id);
        }

        bool contains(type_id id) const
        {
#ifdef GRR_CXX20
            return storage.contains(id);
#else
            return storage.find(id) != storage.end();
#endif
        }

        std::size_t size(type_id id) const
        {
            const auto& it = storage.find(id);
            if (it == storage.end()) {
                return static_cast<std::size_t>(-1);
            }

            return it->second.size;
        }

        const type_context& obtain(type_id id) const
        {
            return storage.at(id);
        }

        storage_map::const_iterator begin() const
        {
            return storage.begin();
        }

        storage_map::const_iterator end() const
        {
            return storage.end();
        }

        void rename(type_id id, const char* new_name, std::error_code& err)
        {
            const auto& it = storage.find(id);
            if (it == storage.end()) {
                err = make_error_code(errors::unregistered_id);
                return;
            }

            it->second.name = new_name;
        }

        void rename(type_id id, const string_view& new_name, std::error_code& err)
        {
            const auto& it = storage.find(id);
            if (it == storage.end()) {
                err = make_error_code(errors::unregistered_id);
                return;
            }

            it->second.name = string(new_name.begin(), new_name.end());
        }

        void add(type_id id, const type_context& type)
        {
            storage.emplace(std::make_pair(id, type));
        }

        void add(type_id id, type_context&& type)
        {
            storage.emplace(std::make_pair(id, type));
        }
    };

    template<typename T>
    static constexpr auto type_name()
    {
        if constexpr (grr::is_reflectable_v<T>) {
            return nameof::nameof_short_type<T>();
        } else {
            return nameof::nameof_type<T>();
        }
    }

    static inline const char* type_name(const context& ctx, type_id id)
    {
        if (!ctx.contains(id)) {
            return "";
        }

        return ctx.at(id).name.c_str();
    }

    template<typename T>
    static inline grr::string runtime_type_name()
    {
        constexpr auto name = type_name<T>();
        return grr::string(name.begin(), name.end());
    }


    template<typename T>
    static constexpr T binhash(const std::string_view& str)
    {
        T hash = T(5381);
        for (const char& sym : str) {
            hash *= 0x21;
            hash += sym;
        }

        return hash;
    }

    template<typename T>
    static constexpr T binhash(const char* str, std::size_t size)
    {
        T hash = T(5381);
        for (std::size_t i = 0; i < size; i++) {
            hash *= 0x21;
            hash += str[i];
        }

        return hash;
    }

    static constexpr type_id obtain_id(const grr::string_view& name)
    {
        return binhash<type_id>(name);
    }

    template<typename T>
    static constexpr type_id obtain_id()
    {
        return obtain_id(type_name<grr::clean_type<T>>());
    }

    static inline std::size_t size(const context& ctx, type_id id)
    {
        return ctx.size(id);
    }

    template<typename T>
    static inline bool contains(const context& ctx)
    {
        constexpr type_id id = obtain_id<grr::clean_type<T>>();
        return ctx.contains(id);
    }

    static inline bool contains(const context& ctx, type_id id)
    {
        return ctx.contains(id);
    }

    static inline void rename(context& ctx, type_id id, std::size_t field_idx, const char* new_name, std::error_code& err)
    {
        if (!ctx.contains(id)) {
            err = make_error_code(errors::unregistered_id);
            return;
        }

        auto& fields = ctx.at(id).fields;
        if (field_idx >= fields.size()) {
            err = make_error_code(errors::invalid_argument);
            return;
        }

        fields.at(field_idx).name = new_name;
    }

    static inline void rename(context& ctx, type_id id, std::size_t field_idx, const string_view& new_name, std::error_code& err)
    {
        if (!ctx.contains(id)) {
            err = make_error_code(errors::unregistered_id);
            return;
        }

        auto& fields = ctx.at(id).fields;
        if (field_idx >= fields.size()) {
            err = make_error_code(errors::invalid_argument);
            return;
        }

        fields.at(field_idx).name = string(new_name.begin(), new_name.end());
    }

    static inline void rename(context& ctx, type_id id, const char* new_name, std::error_code& err)
    {
        ctx.rename(id, new_name, err);
    }

    static inline void rename(context& ctx, type_id id, const string_view& new_name, std::error_code& err)
    {
        ctx.rename(id, new_name, err);
    }

    static inline std::size_t offset(const context& ctx, type_id id, std::size_t field_idx, std::error_code& err)
    {
        if (!ctx.contains(id)) {
            err = make_error_code(errors::unregistered_id);
            return 0;
        }

        auto& fields = ctx.at(id).fields;
        if (field_idx >= fields.size()) {
            err = make_error_code(errors::invalid_argument);
            return 0;
        }

        return fields.at(field_idx).offset;
    }

    static inline type_id base_type(context& ctx, type_id id)
    {
        return ctx.at(id).base_type;
    }

    template<typename T>
    static constexpr type_id base_type()
    {
        return obtain_id<std::remove_pointer_t<grr::clean_type<T>>>();
    }

    template<typename T>
    static inline void rename(context& ctx, std::size_t field_idx, const string_view& new_name, std::error_code& err)
    {
        constexpr type_id id = obtain_id<grr::clean_type<T>>();
        rename(ctx, id, field_idx, new_name, err);
    }

    template<typename T>
    static inline void rename(context& ctx, std::size_t field_idx, const char* new_name, std::error_code& err)
    {
        constexpr type_id id = obtain_id<grr::clean_type<T>>();
        rename(ctx, id, field_idx, new_name, err);
    }

    template<typename T>
    static inline void rename(context& ctx, const string_view& new_name, std::error_code& err)
    {
        ctx.rename(obtain_id<grr::clean_type<T>>(), new_name, err);
    }

    template<typename T>
    static inline void rename(context& ctx, const char* new_name, std::error_code& err)
    {
        ctx.rename(obtain_id<grr::clean_type<T>>(), new_name, err);
    }

    template<typename T>
    static inline bool size(const context& ctx)
    {
        return ctx.size(obtain_id<grr::clean_type<T>>());
    }

    template<typename T>
    static inline std::size_t offset(const context& ctx, std::size_t field_idx, std::error_code& err)
    {
        constexpr type_id id = obtain_id<grr::clean_type<T>>();
        return offset(ctx, id, field_idx, err);
    }

    
    template<typename T, std::size_t recursion_level = 0, typename Function>
    static inline void visit(const grr::context& ctx, T& data, std::error_code& err, Function&& func)
    {
        auto call_function = [](auto&& func, auto argument, const char* name) -> bool {
            using ArgumentLReference = std::add_lvalue_reference_t<decltype(*argument)>;
            constexpr bool callable_1 = std::is_invocable_r_v<bool, decltype(func), ArgumentLReference>;
            constexpr bool callable_2 = std::is_invocable_r_v<bool, decltype(func), ArgumentLReference, const char*>;
            constexpr bool callable_3 = std::is_invocable_r_v<void, decltype(func), ArgumentLReference>;
            constexpr bool callable_4 = std::is_invocable_r_v<void, decltype(func), ArgumentLReference, const char*>;
            static_assert(callable_1 || callable_2 || callable_3 || callable_4, "Captured function is not accepted");

            if constexpr (callable_1) {
                return func(*argument);
            } else if constexpr (callable_2) {
                return func(*argument, name);
            } else if constexpr (callable_3) {
                func(*argument);
                return true;
            } else if constexpr (callable_4) {
                func(*argument, name);
                return true;
            } else {
                return false;
            }
        };

        constexpr type_id id = grr::obtain_id<T>();
        using CleanType = grr::clean_type<T>;
        if (!ctx.contains(id)) {
            err = make_error_code(errors::unregistered_id);
            return;
        }

        const auto& type_info = ctx.at(id);
        if constexpr (pfr::is_implicitly_reflectable_v<CleanType, CleanType>) {
            if (type_info.fields.empty()) {
                err = make_error_code(errors::invalid_type);
                return;
            }

            std::size_t index = 0;
            pfr::for_each_field(data, [&err, &index, &type_info, &call_function, &func](auto& field) {
                if (err || index >= type_info.fields.size()) {
                    err = make_error_code(errors::invalid_ordering);
                    return;
                }

                const auto& field_info = type_info.fields.at(index);
                if (grr::obtain_id<decltype(field)>() != field_info.id) {
                    err = make_error_code(errors::invalid_type);
                    return;
                }

                call_function(func, &field, field_info.name.c_str());
                index++;
            });
        } else {
            call_function(func, &data, "val");
            return;
        }
    }

    namespace detail
    {

        template<typename T, typename Function, typename Data>
        static constexpr void visit_static(Data data, type_id id, const char* name, bool& called, Function&& func)
        {
            using CleanDataType = std::remove_pointer_t<decltype(data)>;
            auto call_function = [](auto&& func, auto argument, const char* name) -> bool {
                // #TODO: (auto& field, std::size_t idx)
                using ArgumentLReference = std::add_lvalue_reference_t<decltype(*argument)>;
                constexpr bool callable_1 = std::is_invocable_r_v<bool, decltype(func), ArgumentLReference>;
                constexpr bool callable_2 = std::is_invocable_r_v<bool, decltype(func), ArgumentLReference, const char*>;
                constexpr bool callable_3 = std::is_invocable_r_v<void, decltype(func), ArgumentLReference>;
                constexpr bool callable_4 = std::is_invocable_r_v<void, decltype(func), ArgumentLReference, const char*>;
                static_assert(callable_1 || callable_2 || callable_3 || callable_4, "Captured function is not accepted");

                if constexpr (callable_1) {
                    return func(*argument);
                } else if constexpr (callable_2) {
                    return func(*argument, name);
                } else if constexpr (callable_3) {
                    func(*argument);
                    return true;
                } else if constexpr (callable_4) {
                    func(*argument, name);
                    return true;
                }

                return false;
            };

            if (called) {
                return;
            }

            constexpr type_id current_ptr_id = grr::obtain_id<T*>();

            if constexpr (!std::is_same_v<T, void>) {
                constexpr type_id current_id = grr::obtain_id<T>();
                if (current_id == id) {
                    if constexpr (std::is_const_v<CleanDataType>) {
                        called = call_function(func, reinterpret_cast<const T*>(data), name);
                    } else {
                        called = call_function(func, reinterpret_cast<T*>(data), name);
                    }
                }

                if (called) {
                    return;
                }

                constexpr type_id vector_id = grr::obtain_id<grr::vector<grr::vector<T>>>();
                constexpr type_id vectored_vector_id = grr::obtain_id<grr::vector<T>>();
                if (id == vector_id) {
                    if constexpr (std::is_const_v<CleanDataType>) {
                        called = call_function(func, reinterpret_cast<const grr::vector<T>*>(reinterpret_cast<size_t>(data)), name);
                    } else {
                        called = call_function(func, reinterpret_cast<grr::vector<T>*>(reinterpret_cast<size_t>(data)), name);
                    }
                }

                if (called) {
                    return;
                }

                if (id == vectored_vector_id) {
                    if constexpr (std::is_const_v<CleanDataType>) {
                        called = call_function(func, reinterpret_cast<const grr::vector<grr::vector<T>>*>(reinterpret_cast<size_t>(data)), name);
                    }
                    else {
                        called = call_function(func, reinterpret_cast<grr::vector<grr::vector<T>>*>(reinterpret_cast<size_t>(data)), name);
                    }
                }
            }

            if (called) {
                return;
            }

            if (current_ptr_id == id) {
                if constexpr (std::is_const_v<CleanDataType>) {
                    called = call_function(func, reinterpret_cast<const T**>(reinterpret_cast<size_t>(data)), name);
                } else {
                    called = call_function(func, reinterpret_cast<T**>(reinterpret_cast<size_t>(data)), name);
                }
            }

            if (called) {
                return;
            }

            constexpr type_id vector_ptr_id = grr::obtain_id<grr::vector<grr::vector<T*>>>();
            constexpr type_id vectored_vector_ptr_id = grr::obtain_id<grr::vector<T*>>();
            if (id == vector_ptr_id) {
                if constexpr (std::is_const_v<CleanDataType>) {
                    called = call_function(func, reinterpret_cast<const grr::vector<T*>*>(reinterpret_cast<size_t>(data)), name);
                }
                else {
                    called = call_function(func, reinterpret_cast<grr::vector<T*>*>(reinterpret_cast<size_t>(data)), name);
                }
            }

            if (called) {
                return;
            }

            if (id == vectored_vector_ptr_id) {
                if constexpr (std::is_const_v<CleanDataType>) {
                    called = call_function(func, reinterpret_cast<const grr::vector<grr::vector<T*>>*>(reinterpret_cast<size_t>(data)), name);
                }
                else {
                    called = call_function(func, reinterpret_cast<grr::vector<grr::vector<T*>>*>(reinterpret_cast<size_t>(data)), name);
                }
            }
        }

        template<typename... Types, typename Function, typename Data>
        static constexpr bool visit_static(Data data, const char* name, type_id id, Function&& func)
        {
            bool called = false;
            (visit_static<Types>(data, id, name, called, func), ...);
            return called;
        }

        template<typename... Types, typename Function, typename Data>
        static constexpr bool visit_static(Data data, type_id id, Function&& func)
        {
            bool called = false;
            (visit_static<Types>(data, id, "var", called, func), ...);
            return called;
        }

        template<typename T, typename Function, typename Data>
        static constexpr void visit_static_reflectable(const grr::context& ctx, type_id id, Data data, Function&& func, bool& called)
        {
            using CleanType = grr::clean_type<T>;
            if constexpr (grr::is_reflectable_v<CleanType>) {
                if (called) {
                    return;
                }

                visit_static<CleanType>(&data, id, "dummy", called, [&ctx, &func](auto& value) {
                    if constexpr (std::is_same_v<grr::clean_type<decltype(value)>, CleanType>) {
                        std::error_code err;
                        grr::visit<CleanType>(ctx, value, err, func);
                    }
                });
            }
        }

        template<typename... Types, typename Function, typename Data>
        static constexpr bool visit_static_reflectable(const grr::context& ctx, type_id id, Data data, Function&& func)
        {
            bool called = false;
            (visit_static_reflectable<Types>(ctx, id, data, func, called), ...);
            return called;
        }

        template<std::size_t recursion_level = 0, typename Function, typename Data>
        static inline void visit(const grr::context& ctx, Data data, type_id id, std::error_code& err, Function&& func)
        {
            // [](auto& field, const char* name)
            auto call_function = [](auto&& func, auto ptr, auto id, auto size) -> bool {
                auto pair = std::make_pair(size, std::make_pair(ptr, id));
                
                // #TODO: (auto& field, std::size_t idx)
                using PairLReference = std::add_lvalue_reference_t<decltype(pair)>;
                constexpr bool callable_1 = std::is_invocable_r_v<bool, decltype(func), PairLReference>;
                constexpr bool callable_2 = std::is_invocable_r_v<bool, decltype(func), PairLReference, const char*>;
                constexpr bool callable_3 = std::is_invocable_r_v<void, decltype(func), PairLReference>;
                constexpr bool callable_4 = std::is_invocable_r_v<void, decltype(func), PairLReference, const char*>;
                static_assert(callable_1 || callable_2 || callable_3 || callable_4, "Captured function is not acceptable");

                if constexpr (callable_1) {
                    return func(pair);
                } else if constexpr (callable_2) {
                    return func(pair, "var0");
                } else if constexpr (callable_3) {
                    func(pair);
                    return true;
                } else if constexpr (callable_4) {
                    func(pair, "var0");
                    return true;
                } 
                
                return false;
            };

            const auto& type_info = ctx.obtain(id);
            if (type_info.fields.empty()) {
                auto type_ptr = data;
                if constexpr (std::is_const_v<std::remove_pointer_t<decltype(data)>>) {
                    type_ptr = static_cast<const char*>(data);
                } else {
                    type_ptr = static_cast<char*>(data);
                }

                if (!detail::visit_static<GRR_TYPES>(type_ptr, id, func)) {
                    return;
                }

                if (!ctx.contains(id)) {
                    err = make_error_code(errors::unregistered_id);
                    return;
                }
    
                if (!call_function(func, type_ptr, id, type_info.size)) {
                    err = make_error_code(errors::invalid_argument);
                    return;
                }
            } else {
                if (visit_static_reflectable<GRR_TYPES>(ctx, id, data, func)) {
                    return;
                }

                for (const auto& cfield : type_info.fields) {
                    auto field_ptr = data;
                    const type_id field_id = cfield.id;
                    if constexpr (std::is_const_v<std::remove_pointer_t<decltype(data)>>) {
                        field_ptr = static_cast<const char*>(data) + cfield.offset;
                    } else {
                        field_ptr = static_cast<char*>(data) + cfield.offset;
                    }

                    if constexpr (recursion_level > 0) {
                        visit<recursion_level - 1>(ctx, field_ptr, field_id);
                    } else {
                        if (detail::visit_static<GRR_TYPES>(field_ptr, cfield.name.data(), cfield.id, func)) {
                            continue;
                        }

                        if (!ctx.contains(field_id)) {
                            err = make_error_code(errors::unregistered_id);
                            return;
                        }

                        auto& field_type = ctx.obtain(field_id);
                        if (!call_function(func, field_ptr, field_id, field_type.size)) {
                            err = make_error_code(errors::invalid_argument);
                            return;
                        }
                    }
                }
            }
        }
    }

    template<std::size_t recursion_level = 0, typename Function, typename Data>
    static inline void visit(const grr::context& ctx, Data data, type_id id, std::error_code& err, Function&& func)
    {
        if (!ctx.contains(id)) {
            err = make_error_code(errors::unregistered_id);
            return;
        }

        grr::detail::visit<recursion_level>(ctx, data, id, err, func);
        if (err) {
            return;
        }
    }

    template<typename T, typename... Args>
    static inline void construct(T* memory_to_construct, Args... args)
    {
        *memory_to_construct = T(args...);
    }

    template<typename T, typename... Args>
    static inline void destruct(T* memory_to_construct)
    {
        memory_to_construct->~T();
    }

    static inline void construct(const grr::context& ctx, void* memory_to_construct, type_id id, std::error_code& err)
    {
        grr::visit(ctx, memory_to_construct, id, err, [](auto& field) {
            using CleanType = grr::clean_type<decltype(field)>;
            if constexpr (!grr::is_fallback_type_v<CleanType>) {
                grr::construct<CleanType>(&field);
            }
        });
    }

    static inline void destruct(const grr::context& ctx, void* memory_to_destruct, type_id id, std::error_code& err)
    {
        grr::visit(ctx, memory_to_destruct, id, err, [](auto& field) {
            using CleanType = grr::clean_type<decltype(field)>;
            if constexpr (!grr::is_fallback_type_v<CleanType>) {
                grr::destruct<CleanType>(&field);
            }
        });
    }
        
    class type_declaration
    {
    public:
        bool aggregate = false;
        const context* ctx;
        std::int64_t index;
        std::size_t size;
        string name;
        type_id id;
        vector<field> fields;

        GRR_CONSTEXPR type_declaration() = delete;
        GRR_CONSTEXPR type_declaration(type_declaration&) = delete;
        GRR_CONSTEXPR type_declaration(const type_declaration&) = delete;
        GRR_CONSTEXPR type_declaration(type_declaration&&) = default;

        GRR_CONSTEXPR type_declaration(const context& in_context, type_id in_id, const char* type_name) noexcept
            : ctx(&in_context), name(type_name), id(in_id), size(0), index(-1) {}

        GRR_CONSTEXPR type_declaration(const context& in_context, type_id in_id, const string_view& type_name) noexcept
            : ctx(&in_context), name(type_name.begin(), type_name.end()), id(in_id), size(0), index(-1) {}

        GRR_CONSTEXPR type_declaration(const context& in_context, type_id in_id, const char* type_name, std::size_t new_size) noexcept
            : ctx(&in_context), name(type_name), id(in_id), size(new_size), index(-1) {}

        GRR_CONSTEXPR type_declaration(const context& in_context, type_id in_id, const string_view& type_name, std::size_t new_size) noexcept
            : ctx(&in_context), name(type_name.begin(), type_name.end()), id(in_id), size(new_size), index(-1) {}

        GRR_CONSTEXPR type_declaration(const context& in_context, const char* type_name) noexcept
            : ctx(&in_context), name(type_name), id(obtain_id(type_name)), size(0), index(-1) {}
        
        GRR_CONSTEXPR type_declaration(const context& in_context, const string_view& type_name) noexcept
            : ctx(&in_context), name(type_name.begin(), type_name.end()), id(obtain_id(type_name)), size(0), index(-1) {}
        
        GRR_CONSTEXPR type_declaration(const context& in_context, const char* type_name, std::size_t new_size) noexcept
            : ctx(&in_context), name(type_name), id(obtain_id(type_name)), size(new_size), index(-1) {}
        
        GRR_CONSTEXPR type_declaration(const context& in_context, const string_view& type_name, std::size_t new_size) noexcept
            : ctx(&in_context), name(type_name.begin(), type_name.end()), id(obtain_id(type_name)), size(new_size), index(-1) {}

        bool field_erase(const char* field_name)
        {
            const auto field_hash = binhash<std::uint32_t>(field_name);
            for (auto it = fields.begin(); it != fields.end(); it++) {
                const auto& field_elem = *it;
                if (field_hash == binhash<std::uint32_t>(field_elem.name.data())) {
                    fields.erase(it);
                    return true;
                }
            }

            return false;
        }

        bool field_erase(std::size_t idx)
        {
            if (idx >= fields.size()) {
                return false;
            }

            fields.erase(fields.begin() + idx);
            return true;
        }

        template<typename T>
        void emplace(const char* field_name, std::error_code& err)
        {
            constexpr type_id current_id = obtain_id<T>();
            if (!grr::contains(*ctx, current_id)) {
                err = make_error_code(errors::unregistered_id);
                return;
            }

            const std::size_t offset = fields.empty() ? 0 : fields.back().offset + grr::size(*ctx, fields.back().id);
            fields.emplace_back(std::move(field(field_name, current_id, offset)));
        }

        template<typename T>
        void emplace(const char* field_name, std::size_t offset, std::error_code& err)
        {
            constexpr type_id current_id = obtain_id<T>();
            if (!grr::contains(*ctx, current_id)) {
                err = make_error_code(errors::unregistered_id);
                return;
            }

            fields.emplace_back(std::move(field(field_name, current_id, offset)));
        }

        void erase(std::size_t idx, std::error_code& err)
        {
            if (!field_erase(idx)) {
                err = make_error_code(errors::invalid_argument);
                return;
            }
        }

        void erase(const char* field_name, std::error_code& err)
        {
            if (!field_erase(field_name)) {
                err = make_error_code(errors::invalid_argument);
                return;
            }
        }
    };

    static inline void add_type(context& ctx, const type_declaration& type, std::error_code& err)
    {
        if (ctx.contains(type.id)) {
            err = make_error_code(errors::already_registered);
            return;
        }

        ctx.add(type.id, std::move(type_context{ type.id, type.size, type.name, type.fields }));
    }

    static inline void add_type(context& ctx, const type_declaration& type, type_id base_type, std::error_code& err)
    {
        if (ctx.contains(type.id)) {
            err = make_error_code(errors::already_registered);
            return;
        }

        ctx.add(type.id, std::move(type_context{ base_type, type.size, type.name, type.fields }));
    }

    template<typename BaseType>
    static constexpr void add_type(context& ctx, const type_declaration& type, std::error_code& err)
    {
        if (ctx.contains(type.id)) {
            err = make_error_code(errors::already_registered);
            return;
        }

        ctx.add(type.id, std::move(type_context{ obtain_id<BaseType>(), type.size, type.name, type.fields }));
    }

    template<typename T> 
    static void add_type(context& ctx, std::error_code& err)
    {
        using CleanType = grr::clean_type<T>;
        type_declaration new_type = type_declaration(ctx, grr::obtain_id<CleanType>(), grr::type_name<CleanType>());
        constexpr bool is_aggregate = std::is_aggregate<CleanType>();

#ifdef GRR_PREDECLARE_FIELDS
        if constexpr (is_aggregate) {
            constexpr bool is_visitable = visit_struct::traits::is_visitable<CleanType>::value;
            constexpr bool is_reflectable = pfr::is_implicitly_reflectable_v<CleanType, CleanType>;
            static_assert(grr::is_reflectable_v<CleanType>, "GRR reflection supports only aggregate types (such as PODs)");

            const CleanType val = {};
            if constexpr (is_visitable) {
                visit_struct::for_each(val, [&err, &val, &new_type](const char* name, const auto& field) {
                    const std::ptrdiff_t offset = reinterpret_cast<std::ptrdiff_t>(&field) - reinterpret_cast<std::ptrdiff_t>(&val);
                    new_type.emplace<grr::clean_type<decltype(field)>>(name, offset, err);
                    if (err) {
                        return;
                    }

                    new_type.size += sizeof(grr::clean_type<decltype(field)>);
                });
            } else if constexpr (is_reflectable) {
                pfr::for_each_field(val, [&err, &val, &new_type](const auto& field) {
                    const std::ptrdiff_t offset = reinterpret_cast<std::ptrdiff_t>(&field) - reinterpret_cast<std::ptrdiff_t>(&val);
                    
                    char field_name[16] = {};
                    std::snprintf(field_name, 16, "var%u", static_cast<std::uint32_t>(offset));

                    new_type.emplace<grr::clean_type<decltype(field)>>(field_name, offset, err);
                    if (err) {
                        return;
                    }

                    new_type.size += sizeof(grr::clean_type<decltype(field)>);
                });
            }
        } else {
            if constexpr (!std::is_same_v<CleanType, void>) {
                new_type.size = sizeof(CleanType);
            }
        }
#else
        new_type.size = sizeof(CleanType);
#endif

        #ifndef GRR_RETURN_IF_FAILED
        #define GRR_RETURN_IF_FAILED(x) \
            (x); \
            if (err) { \
                return;\
            }
        #endif

        new_type.aggregate = is_aggregate;
        GRR_RETURN_IF_FAILED(grr::add_type(ctx, new_type, err));
        if constexpr (!std::is_same_v<CleanType, void>) {
            GRR_RETURN_IF_FAILED(grr::add_type<grr::vector<CleanType>>(ctx, { ctx, grr::type_name<grr::vector<CleanType>>(), sizeof(grr::vector<CleanType>) }, err));
            GRR_RETURN_IF_FAILED(grr::add_type< grr::vector<grr::vector<CleanType>>>(ctx, { ctx, grr::type_name<grr::vector<grr::vector<CleanType>>>(), sizeof(grr::vector<grr::vector<CleanType>>) }, err));
        }

        #undef GRR_RETURN_IF_FAILED
    }

    namespace detail
    {
        template<typename... Types>
        static void add_types(context& ctx, std::error_code& err)
        {
            (grr::add_type<Types>(ctx, err), ...);
        }
    }

    static inline context make_context(std::error_code& err)
    {
        context out_context;
        detail::add_types<GRR_TYPES>(out_context, err);
        return out_context;
    }
}

#endif