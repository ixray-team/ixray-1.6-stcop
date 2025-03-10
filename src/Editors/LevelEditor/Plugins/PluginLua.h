#pragma once
#include "PluginManager.h"

class CPluginLua :
	public IPluginBase
{
public:
	CPluginLua();
	virtual void Run() override;

private:
	virtual xr_string ReadDesc() const;
};