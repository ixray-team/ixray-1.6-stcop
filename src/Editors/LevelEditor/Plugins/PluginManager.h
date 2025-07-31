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

	mutable xr_map<xr_string, xr_string> InputArgsName;
	xr_map<xr_string, string256> InputArgsValues;

public:
	virtual void Run() = 0;
	virtual bool IsSimple() const { return InputArgsName.empty(); }

private:
	virtual xr_string ReadDesc() const = 0;
};

class CPluginsManagers
{
public:
	xr_vector<IPluginBase*> Plugins;

public:
	static CPluginsManagers& Instance();
	void Reinit();

private:
	void Init();
	~CPluginsManagers();
	CPluginsManagers();
};