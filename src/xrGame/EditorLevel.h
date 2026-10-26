#pragma once
#include "Level.h"

class CLevelEditor:
	public CLevel
{
public:
	CLevelEditor();
	virtual ~CLevelEditor() override;
	virtual bool net_Start(const char* op_server, const char* op_client) override;
	virtual void LoadEditor(shared_str LevelName) override;
};