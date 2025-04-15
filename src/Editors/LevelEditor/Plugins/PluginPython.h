#pragma once
#include "PluginManager.h"

class CPluginPython :
	public IPluginBase
{
public:
	CPluginPython();
	virtual void Run() override;

private:
	virtual xr_string ReadDesc() const;

	bool IsPythonInstalled() const;
	xr_string RunCommand(const xr_string& command);
};