set(RegistryFile "${LuaSourceDir}/luabind/src/class_registry.cpp")
file(READ "${RegistryFile}" Content)
set(Old [=[
        static lua_State* cache_key = 0;
        static class_registry* registry_cache = 0;
        if (cache_key == L) return registry_cache;
]=])
set(New [=[
        static lua_State* cache_key = 0;
        static class_registry* registry_cache = 0;
]=])
string(FIND "${Content}" "${Old}" Position)
if(Position EQUAL -1)
    message(FATAL_ERROR "Expected luabind registry cache block was not found in ${RegistryFile}")
endif()
string(REPLACE "${Old}" "${New}" Content "${Content}")
set(Old [=[
        cache_key = L;
        registry_cache = p;
]=])
set(New [=[
        if (cache_key == L && registry_cache == p) return registry_cache;
        cache_key = L;
        registry_cache = p;
]=])
string(FIND "${Content}" "${Old}" Position)
if(Position EQUAL -1)
    message(FATAL_ERROR "Expected luabind registry assignment block was not found in ${RegistryFile}")
endif()
string(REPLACE "${Old}" "${New}" Content "${Content}")
file(WRITE "${RegistryFile}" "${Content}")
