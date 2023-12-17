#pragma once
struct lua_State;

class IGame_ObjectFactory
{

public:
	virtual	void register_script() const = 0;
	virtual	void init() = 0;
	virtual	void init_script(const char* str) = 0;
	virtual	void export_classes(lua_State* L) = 0;
};

extern ENGINE_API IGame_ObjectFactory* g_object_factory;