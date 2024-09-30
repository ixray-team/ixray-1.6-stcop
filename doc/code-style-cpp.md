# Code styles (C++)

English | [Русский](./code-style-cpp.rus.md)

## Files

Accepted extensions:

- `*.cpp` for source files
- `*.h`/`*.hpp` for header files

Files must be saved in UTF-8 encoding with CRLF line endings and must end with one empty line

New comments should be written using English

PascalCase should be used for the names of new files, and the style that has already been adopted in the library or project should be used for existing ones

## Comments

- File headers are not used in the project, however, if necessary, insert a description of the file at the beginning:

  ```cpp
  // Description of file
  ```

- It is permissible to report incomplete functionality, for example, `// TODO: Description`
- It is permissible to report a bug, for example, `// BUG: Description`
- It is mandatory to respect the maximum line length
- If a kludge or hack is added, it is mandatory to report it in a comment, for example, `// HACK: Description`

## Includes

- Don't use old include guards
  - Use `#pragma once` instead
- All includes should be placed before the main code at the top of the file and should be grouped in the following order:

  ```cpp
  // Precompiled header
  #include "stdafx.h"

  // Internal API
  #include "xrCore.h"
  ```

- It's necessary to sort includes within categories with great care

## Naming

- Already existing names of public and protected functions, methods, and classes should be left as is to preserve API compatibility
- Names of parameters of functions and methods, local variables and objects must begin with a small letter, complying with camelCase
- Names of logical variables must begin with a verb:

  ```cpp
  bool hasChildren;
  bool isEnabled;
  ```

- Names of lambda functor objects must end with the postfix `Lambda`

  ```cpp
  auto addLambda = [](auto a, auto b)
  {
      return a + b;
  };
  ```

- Names of global objects must begin with the prefix `g_`

  ```cpp
  u32 g_SomeGlobalValue;
  ```

- Names of the `private` fields of classes and structures should start with the `_` sign, and then there should be a name with a small letter, complying with camelCase

  ```cpp
  u32 _someValue1;
  u64 _someValue2;
  ```

- Interface names must begin with the prefix `I`
- Names of the new classes and structures, `public` and `protected` methods should be in PascalCase
- Template parameter names should have descriptive names, unless the one-letter name speaks for itself and a descriptive name adds value
- Should consider using the name `T` as the name of the template parameter if a single parameter is used
- Prefix `T` should be added to the names of template parameters

## Standard functionality

- Standard libraries cannot be used directly
  - Use of standard libraries is allowed inside platform-dependent conditional compilation blocks in agreement with the team or the maintainer of the project
- X-Ray types, containers, and functions are preferable as opposed to the standard ones
  - If this type, container, or function is missing, declare the appropriate alias:

  ```cpp
  using xr_string_view = std::string_view;
  ```

### Types

| STD                  | X-Ray        |
|----------------------|--------------|
| `unsigned int`       | `u32`        |
| `unsigned long long` | `u64`        |
| `const char[32]`     | `string32`   |
| `const char[64]`     | `string64`   |
| `const char[128]`    | `string128`  |
| `const wchar_t[32]`  | `wstring32`  |
| `const wchar_t[64]`  | `wstring64`  |
| `const wchar_t[128]` | `wstring128` |

The full description of the types is in [this](../src/xrCore/_types.h) file

### Containers

| STL                  | X-Ray         |
|----------------------|---------------|
| `std::vector`        | `xr_vector`   |
| `std::unordered_map` | `xr_hash_map` |
| `std::map`           | `xr_map`      |
| `std::string`        | `xr_string`   |
| `std::set`           | `xr_set`      |

The full description of the containers is in [this](../src/xrCore/_stl_extensions.h) file

### Functions

| STD       | X-Ray        |
|-----------|--------------|
| `strlen`  | `xr_strlen`  |
| `strext`  | `xr_strext`  |
| `strcmp`  | `xr_strcmp`  |
| `strcmpi` | `xr_strcmpi` |
| `strcpy`  | `xr_strcpy`  |
| `strcat`  | `xr_strcat`  |
| `sprintf` | `xr_sprintf` |

The full description of the containers is in [этом](../src/xrCore/_std_extensions.h) file

## Type casting

- C-style casts are allowed and preferred over `static_cast`
- Use `smart_cast` instead of `dynamic_cast` if `smart_cast` is available

## Platform dependency

Platform-dependent code has to be placed under the conditional compilation block with an indication of one of the valid macros in the condition:

- `IXR_WINDOWS`
- `IXR_LINUX`
- `IXR_APPLE_SERIES`
- `IXR_BSD_SERIES`

## Formatting

- Always use 4 spaces instead of tabs
- Always use curly braces to indicate conditions and cycles
- Curly braces should be on a new line
- There must be 1 space between the keyword and the condition
  - In range expressions the colon should be highlighted on both sides

  ```cpp
  if (...)
  {
      ...
  }

  for (...)
  {
      ...
  }

  while (...)
  {
      ...
  }

  for (auto value : someCollection)
  {
      ...
  }

  for (auto& [id, name] : someMap) // Instead `for (auto it : someMap)`
  {
      ...
  }
  ```

- The ternary operator is allowed on one line
  - Split by lines with a more complex condition or result
- Branching operators must follow a pattern
  - When using brackets for `case`, it's necessary to put brackets for all other `case` blocks

  ```cpp
  switch (condition)
  {
      case 1:
          ...
          // falls through

      case 2:
          ...
          break;

      case 3:
          ...
          return;

      case 4:
      case 5:
          ...
          break;

      default:
          break;
  }
  ```

- Transfer and string splitting of inherited classes and interfaces has to be conducted

  ```cpp
  class SomeClass :
      public IInterface,
      public BaseClass
  {
      ...
  }
  ```

## Limitations

- Properties, getters/setters, methods, constructors, and destructors should be grouped by access modifiers in the specified order
- `using` should be used instead of `typedef` where possible
- The use of `malloc`, `calloc`, and `realloc` is prohibited; `xr_alloc` should be used instead
- Memory should be allocated using the `new` operator and freed using the `xr_delete` operator, which automatically sets pointers to `nullptr`
- C++ exceptions in any form are prohibited
  - C-style exception handling should be avoided unless discussed with the team or the project maintainer
- Multiple class inheritance is prohibited
- Methods that do not modify class fields must be declared as `const`
  - Declaring variables as `const` is optional
- Functions should accept parameters by reference when dealing with types larger than the machine word (8 bytes)
- Overridable virtual methods must be specified using the `override` keyword:

  ```cpp
  virtual void SomeFunc() override;
  ```

- `nullptr` should be used instead of `NULL` and for checks
- The use of strongly typed enumerations (`enum class`) should be prevalent
- The use of anonymous enumerations and structures is prohibited
- Large code nesting should be avoided
- A constructor and a destructor should always be defined
- Direct full inclusion of a namespace, like, `using namespace` is prohibited
- Classes that do not have subclasses should be marked with the final keyword
- The auto keyword should be used only when the compiler can uniquely infer the type from the expression
- The default keyword should be used where possible
- The delete keyword should be used to disable functions that should not be implemented or overridden
