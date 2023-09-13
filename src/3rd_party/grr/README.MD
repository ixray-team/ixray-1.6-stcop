# GRR - The reflection for games
![image](grr_logo.png)

# How is it works?
Pretty simple! There is an a example of usage:
```cpp
    #include <grr/grr.hpp>
    #include <iostream>

    struct my_custom_struct
    {
        int hello;
        std::uint64_t world;
        grr::string new_string;
    };
    // No other declaration required!

    // You also can use the "GRR_REFLECT" define to declare fields with names at compile time.
    // Anyway, it equires to write every field of POD.
    struct my_custom_reflected_struct
    {
        int another_hello;
        std::uint64_t another_world;
        grr::string another_new_string;
    };
    GRR_REFLECT(my_custom_reflected_struct, another_hello, another_world, another_new_string);

    int main()
    {
        std::error_code err;
        grr::context context = grr::make_context(err);
        if (err) {
            return -1;
        }

        // Register compile-time types
        grr::add_type<my_custom_struct>(context, err);
        grr::add_type<my_custom_reflected_struct>(context, err);

        // Register run-time type
        grr::type_declaration custom_type = grr::type_declaration(context, "My custom type");
        custom_type.emplace<int>("hello", err);
        custom_type.emplace<std::uint64_t>("world", err);
        custom_type.emplace<grr::string>("string", err);
        grr::add_type(context, custom_type, err);

        {
            my_custom_struct data;
            std::cout << "Reflecting compile-time type without pre-declaration...";
            grr::visit(context, data, err, 
            //[](auto& field, const char* name) { // this also allowed, but names will be presented in "varX" format.
            [](auto& field) {
                field = {};
            });
        }

        auto const_visit = [](const auto& field, const char* name) {
            using FieldType = stl::clean_type<decltype(field)>;
            if constexpr (std::is_same_v<FieldType, grr::string>) {
                std::cout << name << ": " << field;
            } else {
                std::cout << name << ": " << std::to_string(field);
            }
        };

        {
            const my_custom_reflected_struct data = { 1, 2, "string" };  
            std::cout << "Reflecting compile-time type with pre-declaration...";    
            grr::visit(context, data, err, const_visit);
        }

        {
            char data[64] = {};
            std::cout << "Reflecting run-time type...";
            grr::construct(context, data, custom_type.id, err);     // Call constructor on every field
            grr::visit(context, data, err, const_visit);
            grr::destruct(context, data, custom_type.id, err);      // Call destructor on every field
        }
    }
```

# TODO List:
- Optimization
 - [ ] `grr::details::visit_static` algorithm must be optimized from O(n) to O(1)
 - [ ] Compile-time `grr::visit`
 - [ ] Fast run-time `grr::serializable_hash` 
  - [ ] Best solution will be the deleting of `grr::serializable_hash` function and move all string operations to `grr::type_name`
- Bugs
 - [ ] `const void*` and `void*` don't appear in `grr::reflect` function
- Features
 - [ ] Portable `grr::serializable_hash` between compilers
  - [ ] Custom hasher for containers because of template arguments aliasing
  - [x] Delete all platform-dependend parts in the name when hashing
  - [ ] Make type string more serialization-friendly
 - [ ] Serialization/Deserialization support
  - [ ] Binary serialization
  - [ ] String serialization
  - [ ] Custom formats support
 - [ ] Full containers support (reflect all containers with pre-defined scalar types)
 - [ ] Fields meta-data (markers, flags, etc)
- Integrations
 - [ ] EnTT 
 - [ ] RapidJSON
- Dependencies
 - [ ] pfr replacement
 - [ ] visit_struct replacement
