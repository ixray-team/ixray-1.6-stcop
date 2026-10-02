# Coding guidelines

Source: [coding guidelines](https://ixray-team.github.io/ixray-1.6-stcop/main/coding-guidelines.html). File layout, names, and braces: [doc/code-style-cpp.md](../../doc/code-style-cpp.md).

## RAII

Acquire the resource in the constructor and release it in the destructor. Applies to memory, files, threads, mutexes, OS handles, SDL/HID objects, and graphics objects.

A type used once in the whole engine does not need its own wrapper. Manual acquire/release is enough there.

## Namespace or class

| Form | When |
| --- | --- |
| `namespace` | Stateless utilities: math, conversion, serialization, algorithms |
| `class` | State, owned resources, lifetime, a service, a manager, a game entity |

A service class holds the logic for that service. Split types stay split: `CGamepadService` owns devices, `CInput` owns user input.

## One interface

Alternate implementations of one subsystem (formats, platforms, swappable services) share load, release, and access. That replaces scattered `if`, `switch`, and `#ifdef`.

Separate APIs when the implementations do not share a clear interface.

## Platform code

Game, render, and other portable modules do not include platform headers, types, or APIs. Those live under the `Platform` implementation (`Windows`, `Linux`, `macOS`) behind one interface.

`#ifdef` stays inside that platform code, or in the rare case the difference cannot sit behind the interface. Filesystem, windows, threads, and OS calls go through it.

## IXR types

| STL | IXR |
| --- | --- |
| `std::string` | `xr_string` |
| `std::string_view` | `xr_string_view` |
| `std::vector` | `xr_vector` |
| `std::unordered_map` | `xr_hash_map` |
| `std::unordered_set` | `xr_hash_set` |
| `std::unique_ptr` | `xr_unique_ptr` (`xr_make_unique`) |
| `std::uint32_t` | `u32` |

Use the IXR name when it exists. A missing analog may stay `std::`. Code that runs before `Memory._initialize` (process start, exception paths) uses `std::` containers. `xr_unique_ptr` frees through the engine heap.
