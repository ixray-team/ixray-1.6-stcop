# Code styles (C++)

English | [Русский](./code-style-cpp.rus.md)

Formatting is defined by [`.clang-format`](../.clang-format), naming by [`.clang-tidy`](../.clang-tidy). This document describes both and the rules that the tools do not check. If the document and a config disagree, the config wins. Basic patterns (RAII, namespace or class, interfaces, platform code) are described in the [Coding Guidelines](https://ixray-team.github.io/ixray-1.6-stcop/en/main/coding-guidelines.html).

## Tools

- Format changed code with `clang-format` before committing. Visual Studio and VS Code pick up `.clang-format` from the repository root.
- Check names with `clang-tidy` (`readability-identifier-naming`).
- Only reformat the code you are changing. Do not reformat whole legacy files in the same commit as a functional change.

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

- `clang-format` does not sort includes (`SortIncludes: Never`). The order is set by hand: the precompiled header always comes first, and the order inside a group is kept because some headers depend on it

## Naming

`.clang-tidy` requires PascalCase (`CamelCase` in clang-tidy terms) for almost every name:

| Entity | Style | Example |
|--------|-------|---------|
| Classes, structures, enumerations | PascalCase | `class SceneLoader`, `enum class LoadMode` |
| Namespaces | PascalCase | `namespace NameUtils` |
| Functions and methods | PascalCase | `void GenObjectName()` |
| Local, global and static variables | PascalCase | `u32 ObjectCount` |
| Class and structure fields (any access) | PascalCase | `u32 RefCount` |
| Constants, `constexpr`, enumerators, `const` parameters | PascalCase | `constexpr u32 MaxObjects`, `LoadMode::Append` |
| Macros | UPPER_CASE | `#define IXR_WINDOWS` |

- Already existing names of public and protected functions, methods, and classes should be left as is to preserve API compatibility
- Prefixes allowed in front of a PascalCase name:
  - `I` for interfaces: `IReader`
  - `C`, `E`, `F` in legacy type names: `CRenderDevice`, `EScene`. New types do not need them
  - `g_` for global objects: `u32 g_ObjectCount;`
  - `xr_` for IXR aliases of standard types and functions: `xr_vector`, `xr_strcpy`
- Interface names must begin with the prefix `I`
- Names of the fields do not use the `_` or `m_` prefixes:

  ```cpp
  class SomeClass
  {
  public:
      u32 GetValue() const
      {
          return Value;
      }

  private:
      u32 Value = 0;
      u64 TotalSize = 0;
  };
  ```

- Names of the parameters are written in PascalCase too. `clang-tidy` only checks `const` parameters, but the same style is used for all of them
- Names of logical variables must begin with a verb:

  ```cpp
  bool HasChildren;
  bool IsEnabled;
  ```

- Names of lambda functor objects must end with the postfix `Lambda`

  ```cpp
  auto AddLambda = [](auto A, auto B)
  {
      return A + B;
  };
  ```

- One-letter names are upper case as well (`I`, `T`). Prefer a range-based `for` or a descriptive name (`Index`) over a loop counter
- Template parameter names should have descriptive names, unless the one-letter name speaks for itself and a descriptive name adds value
- Should consider using the name `T` as the name of the template parameter if a single parameter is used
- Prefix `T` should be added to the names of template parameters

## Standard functionality

- X-Ray types, containers, and functions are used instead of the standard ones
  - Code that runs before `Memory._initialize` (process start, exception paths) uses `std::` containers
  - If there is no X-Ray analog, the standard type, container, or function may be used. For a commonly used one, declare an alias:

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

| STL                  | X-Ray           |
|----------------------|-----------------|
| `std::vector`        | `xr_vector`     |
| `std::unordered_map` | `xr_hash_map`   |
| `std::unordered_set` | `xr_hash_set`   |
| `std::map`           | `xr_map`        |
| `std::string`        | `xr_string`     |
| `std::string_view`   | `xr_string_view`|
| `std::set`           | `xr_set`        |
| `std::unique_ptr`    | `xr_unique_ptr` |

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

The full description of the functions is in [this](../src/xrCore/_std_extensions.h) file

## Type casting

- C-style casts are allowed and preferred over `static_cast`
- Use `smart_cast` instead of `dynamic_cast` if `smart_cast` is available

## Platform dependency

Platform-dependent code lives in the `Platform` implementations (`Windows`, `Linux`, `macOS`) behind a common interface. Game, render, and other portable modules do not include platform headers and do not use platform APIs.

Conditional compilation is allowed only inside the platform code, or when the difference cannot be moved behind the interface. The condition uses one of the valid macros:

- `IXR_WINDOWS`
- `IXR_LINUX`
- `IXR_APPLE_SERIES`
- `IXR_BSD_SERIES`

## Formatting

The rules below are applied by `clang-format`. The examples use spaces for readability; in the code, indentation is done with tabs.

### Indentation and lines

- Indent with tabs, one tab is 4 columns wide (`UseTab: Always`, `IndentWidth: 4`)
- There is no line length limit (`ColumnLimit: 0`). `clang-format` does not wrap long lines; split them by hand where it helps readability
- No more than 2 empty lines in a row
- A block does not start with an empty line
- Pointers and references stick to the type: `int* Ptr`, `const xr_string& Name`

### Braces

- Curly braces are always on a new line (Allman style), including functions, classes, namespaces, and lambdas
- Curly braces are mandatory for every `if`, `else`, `for`, `while`, and `do`, even for a single statement. `clang-format` inserts missing ones (`InsertBraces: true`)
- `if`, loops, and blocks are never written on one line
- A short function may be written on one line only when it is defined inside the class body:

  ```cpp
  class SomeClass
  {
  public:
      u32 GetValue() const { return Value; }
  };

  u32 SomeClass::GetSize() const
  {
      return Size;
  }
  ```

- There must be 1 space between the keyword and the condition
  - In range expressions the colon should be highlighted on both sides

  ```cpp
  if (...)
  {
      ...
  }
  else
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

  for (auto Value : SomeCollection)
  {
      ...
  }

  for (auto& [Id, Name] : SomeMap) // Instead `for (auto It : SomeMap)`
  {
      ...
  }
  ```

### Classes

- Access modifiers are on the same level as the `class` keyword, with an empty line before them
- Inherited classes and interfaces are either on one line or, when split, the line breaks after the colon and each base is on its own line:

  ```cpp
  class SomeClass :
      public IInterface,
      public BaseClass
  {
  public:
      SomeClass();

  private:
      u32 Value = 0;
  };
  ```

- The constructor initializer list starts on a new line with a colon:

  ```cpp
  SomeClass::SomeClass(u32 Value, u32 Size)
      : Value(Value), Size(Size)
  {
  }
  ```

### Function calls and declarations

- Arguments and parameters are either all on one line, or each on its own line. In the second case, the line breaks after the opening parenthesis and the closing parenthesis is on its own line:

  ```cpp
  DoSomething(FirstArgument, SecondArgument, ThirdArgument);

  DoSomething(
      FirstArgument,
      SecondArgument,
      ThirdArgument
  );
  ```

### Operators

- The ternary operator is allowed on one line
  - With a more complex condition or result it is split by lines, and the line breaks before `?` and `:`

  ```cpp
  auto Result = IsEnabled ? GetValue() : 0;

  auto Result = HasChildren
                    ? CalculateChildrenSize(Node, Flags)
                    : CalculateOwnSize(Node);
  ```

- Branching operators must follow a pattern
  - `case` labels are indented inside `switch`
  - A `case` is written either on one line (`AllowShortCaseLabelsOnASingleLine: true`), or as a block in brackets. A `case` body on several lines without brackets is prohibited
  - The opening bracket is on the line after the label, `break` and `return` are inside the brackets
  - Several labels with one body go one after another, each on its own line

  ```cpp
  switch (Condition)
  {
      case LoadMode::Append: AppendObjects(); break;
      case LoadMode::Replace: ReplaceObjects(); return;

      case LoadMode::Merge:
      case LoadMode::Update:
      {
          u32 Count = GetCount();
          Process(Count);
          break;
      }

      default: break;
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
- Unnamed namespaces (`namespace { ... }`) and unnamed structures (`struct { ... } Value;`) are prohibited. Give the namespace a name, and declare internal helper functions as `static`:

  ```cpp
  // Instead of `namespace { bool ParseIndex(...); }`
  static bool ParseIndex(const char* Str, u32& Index);

  // Instead of `struct { u32 Min, Max; } Range;`
  struct IndexRange
  {
      u32 Min = 0;
      u32 Max = 0;
  };

  IndexRange Range;
  ```
- Large code nesting should be avoided
- A constructor and a destructor should always be defined
- Direct full inclusion of a namespace, like, `using namespace` is prohibited
- Classes that do not have subclasses should be marked with the final keyword
- The auto keyword should be used only when the compiler can uniquely infer the type from the expression
- The default keyword should be used where possible
- The delete keyword should be used to disable functions that should not be implemented or overridden
