# Coding Guidelines

This page describes the basic patterns used in the engine code. Formatting and naming rules are in the [C++ code style](https://github.com/ixray-team/ixray-1.6-stcop/blob/default/doc/code-style-cpp.md).

## RAII (Resource Acquisition Is Initialization)

### Description

Use the **RAII** idiom to manage resources. A resource is acquired when an object is created and released automatically when the object is destroyed.

This applies to memory, files, threads, mutexes, OS handles, SDL/HID objects, graphics resources, and any other objects that require explicit release.

> [!WARNING]
> If a resource type is used only once in the whole engine, a dedicated RAII wrapper is not required. Manual resource management is allowed in such cases, as long as it does not hurt readability and maintenance.

#### When to use

Use RAII when working with:

- memory
- files
- threads
- mutexes
- graphics APIs

#### Advantages

- ✅ Resources are released automatically
- ✅ Safe on early return from a function
- ✅ Fewer memory and resource leaks
- ✅ The code is simpler and easier to maintain

## Grouping functionality (Class vs Namespace)

### Description

Group related functionality into a **namespace** or a **class** instead of adding many global functions.

The choice depends on whether there is state.

- Use a **namespace** if the functions are independent, store no data, and form a set of utilities
- Use a **class** if the object has state, manages resources, or describes a separate entity, manager, service, etc

If a class is a service or an engine component, it should contain all the logic related to it. This reduces coupling and makes the interface clear.

> [!WARNING]
> The exception is special classes that split responsibility between several subsystems. For example, `CGamepadService` manages devices, while user input is handled by the separate `CInput` system.

> [!NOTE]
> A namespace must always have a name. Unnamed namespaces (`namespace { ... }`) are prohibited. A helper function used in a single `.cpp` file is declared as `static`.

### When to use

#### Use a namespace for

- math functions
- utilities
- conversion functions
- serialization
- stateless algorithms

#### Use a class for

- services
- managers
- game entities
- objects with a lifetime
- classes that own resources
- objects that store state

### Advantages

- ✅ Related code is kept in one place
- ✅ The number of global functions is minimal
- ✅ Functionality is easier to maintain and extend
- ✅ It is clear who is responsible for a particular operation

### Disadvantages

- ❌ Excessive encapsulation sometimes makes small functions harder to reuse

## Unified interfaces

### Description

When adding an alternative implementation of an existing system, use a common interface.

For example, if the engine supports raster and vector images, both implementations should provide one interface for loading, releasing, and accessing the resource.

This isolates the differences between implementations and avoids `if`, `switch`, and `#ifdef` scattered across the code.

> [!WARNING]
> If the implementations differ too much to build a clear common interface, separate APIs are allowed. Do not create an abstraction for its own sake.

### When to use

- alternative implementations of one subsystem
- different resource formats
- platform-dependent implementations
- interchangeable services

### Advantages

- ✅ The code does not depend on a specific implementation
- ✅ Fewer branches (`if`, `switch`, `#ifdef`)

## Isolating platform-dependent code

### Description

Do not use platform-dependent APIs, data types, and header files in game, render, and other platform-independent modules.

All platform-dependent code lives in the corresponding `Platform` implementations (`Windows`, `Linux`, `macOS`, etc.) and provides one interface for the rest of the engine.

This minimizes the use of `#ifdef`, simplifies maintenance, and makes adding new platforms easier.

> [!WARNING]
> `#ifdef` is allowed only inside platform modules, or when there is no reasonable way to move the difference behind a common interface. The condition uses one of the macros `IXR_WINDOWS`, `IXR_LINUX`, `IXR_APPLE_SERIES`, `IXR_BSD_SERIES`.

### When to use

- file system access
- window creation
- thread management
- system calls
- interaction with the OS

### Advantages

- ✅ Game code does not depend on a specific platform
- ✅ New platforms are easier to support
- ✅ Far fewer `#ifdef`
- ✅ Platform logic is kept in one place

### Disadvantages

- ❌ A separate implementation has to be maintained for each platform

## Using internal types

### Description

Use the internal IXR types and aliases instead of standard library types or platform-dependent types.

This keeps the implementation under central control, simplifies support for different platforms, and keeps the code consistent across the project.

For example:

- `std::string` → `xr_string`
- `std::string_view` → `xr_string_view`
- `std::unordered_map` → `xr_hash_map`
- `std::unordered_set` → `xr_hash_set`
- `std::vector` → `xr_vector`
- `std::unique_ptr` → `xr_unique_ptr`
- `std::uint32_t` → `u32`

> [!WARNING]
> If the type has no internal analog, the standard library type may be used.

> [!NOTE]
> Code that may run **before the engine's internal allocators are initialized** (for example, at application startup or during exception handling) should use standard library containers (`std::*`). IXR internal types may not be available yet at that point.

### When to use

- STL containers
- string types
- fixed-width integer types
- smart pointers
- other types that have internal analogs

### Advantages

- ✅ One code style across the project
- ✅ The implementation of internal types can be changed in one place
- ✅ The project is easier to port and maintain
