#pragma once
#include "Level.h"

class CLevelEditor:
	public CLevel
{
public:
	CLevelEditor();
	virtual ~CLevelEditor();
	virtual bool net_Start(const char* op_server, const char* op_client);
	virtual void LoadEditor(shared_str LevelName) override;
};