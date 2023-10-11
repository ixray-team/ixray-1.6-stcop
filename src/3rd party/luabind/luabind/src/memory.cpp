

#include <luabind/config.hpp>
#include <luabind/memory.hpp>

namespace luabind
{
    allocator_func allocator = nullptr;
    void* allocator_context = nullptr;
}
