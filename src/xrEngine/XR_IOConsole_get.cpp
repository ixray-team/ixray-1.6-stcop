////////////////////////////////////////////////////////////////////////////
//	Module 		: XR_IOConsole_get.cpp
//	Created 	: 17.05.2008
//	Author		: Evgeniy Sokolov
//	Description : Console`s get-functions class implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "xr_ioc_cmd.h"

bool CConsole::GetBool(const char* cmd) const {
	IConsole_Command* cc	= GetCommand(cmd);
	CCC_Mask32* cf			= smart_cast<CCC_Mask32*>(cc);
	if (cf) {
		return cf->GetValue() != 0;
	}

	CCC_Integer* ci			= smart_cast<CCC_Integer*>(cc);
	if (ci) {
		return ci->GetValue() != 0;
	}

	CCC_Boolean* cb			= smart_cast<CCC_Boolean*>(cc);
	if (cb) {
		return cb->GetValue() != 0;
	}
	return false;
}

float CConsole::GetFloat(const char* cmd, float& min, float& max) const {
	min						= 0.0f;
	max						= 0.0f;
	IConsole_Command* cc	= GetCommand(cmd);
	CCC_Float* cf			= smart_cast<CCC_Float*>(cc);
	if (cf) {
		cf->GetBounds(min, max);
		return cf->GetValue(); 
	}
	return 0.0f;
}

IConsole_Command* CConsole::GetCommand(const char* cmd) const
{
	const auto it = Commands.find(cmd);

	if (it == Commands.end())
	{
		return nullptr;
	}

	return it->second;
}


int CConsole::GetInteger(const char* cmd, int& min, int& max) const {
	min						= 0;
	max						= 1;
	IConsole_Command* cc	= GetCommand(cmd);

	CCC_Integer* cf			= smart_cast<CCC_Integer*>(cc);
	if (cf) {
		cf->GetBounds(min, max);
		return cf->GetValue();
	}
	CCC_Mask32* cm = smart_cast<CCC_Mask32*>(cc);
	if (cm) {
		min = 0;
		max = 1;
		return cm->GetValue() ? 1 : 0;
	}
	return 0;
}

const char* CConsole::GetString(const char* cmd) const {
	IConsole_Command* cc	= GetCommand(cmd);
	if (!cc) {
		return nullptr;
	}

	static IConsole_Command::TStatus stat;
	cc->Status				( stat );
	return					stat;
}

const char* CConsole::GetToken(const char* cmd) const {
	return GetString( cmd );
}

xr_token* CConsole::GetXRToken(const char* cmd) const {
	IConsole_Command* cc	= GetCommand(cmd);
	
	CCC_Token* cf			= smart_cast<CCC_Token*>(cc);
	if (cf) {
		return cf->GetToken();
	}
	return nullptr;
}

Fvector* CConsole::GetFVectorPtr(const char* cmd) const {
	IConsole_Command* cc	= GetCommand(cmd);
	CCC_Vector3* cf			= smart_cast<CCC_Vector3*>(cc);
	if (cf) {
		return cf->GetValuePtr();
	}
	return					nullptr;
}

Fvector CConsole::GetFVector(const char* cmd) const {
	Fvector* pV = GetFVectorPtr( cmd );
	if (pV) {
		return *pV;
	}
	return Fvector().set( 0.0f, 0.0f, 0.0f );
}
