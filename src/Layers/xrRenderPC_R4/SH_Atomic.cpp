#include "stdafx.h"
#include "SH_Atomic.h"
#include "ResourceManager.h"
#include "dxRenderDeviceRender.h"

///////////////////////////////////////////////////////////////////////
void SShaderCompileState::MarkCompiled()
{
	IsCompiled.store(true, std::memory_order_release);
	IsCompiled.notify_all();
}

void SShaderCompileState::WaitCompiled()
{
	if (IsCompiled.load(std::memory_order_acquire))
	{
		return;
	}

	PROF_EVENT("SShaderCompileState::WaitCompiled");
	IsCompiled.wait(false, std::memory_order_acquire);
}

///////////////////////////////////////////////////////////////////////
//	SVS
SVS::SVS() : vs(nullptr)
{
}

SVS::~SVS()
{
	DEV->_DeleteVS(this);
	_RELEASE(vs);
	_RELEASE(Signature);
}

///////////////////////////////////////////////////////////////////////
//	SPS
SPS::~SPS								()			{	_RELEASE(ps);		DEV->_DeletePS			(this);	}

///////////////////////////////////////////////////////////////////////
//	SGS
SGS::~SGS								()			{	_RELEASE(gs);		DEV->_DeleteGS			(this);	}
SHS::~SHS								()			{	_RELEASE(sh);		DEV->_DeleteHS			(this);	}
SDS::~SDS								()			{	_RELEASE(sh);		DEV->_DeleteDS			(this);	}
SCS::~SCS								()			{	_RELEASE(sh);		DEV->_DeleteCS			(this);	}

///////////////////////////////////////////////////////////////////////
//	SState
SState::~SState							()			{	_RELEASE(state);	DEV->_DeleteState		(this);	}
