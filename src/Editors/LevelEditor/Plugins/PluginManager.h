#pragma once

enum class EPluginType
{
	Lua,
	Python
};

class IPluginBase
{
	friend class CPluginsManagers;

public:
	EPluginType Type;
	xr_string Name;
	xr_string Path;
	xr_string Desc;

public:
	virtual void Run() = 0;

private:
	virtual xr_string ReadDesc() const = 0;
};

class CPluginsManagers
{
public:
	xr_vector<IPluginBase*> Plugins;

public:
	static CPluginsManagers& Instance();

private:
	void Init();
};