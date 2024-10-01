#pragma once
#include "Level.h"

class CLevelEditor:
	public CLevel
{
public:
	CLevelEditor();
	virtual ~CLevelEditor();
	virtual BOOL net_Start(LPCSTR op_server, LPCSTR op_client);
	virtual void LoadEditor(shared_str LevelName) override;
};