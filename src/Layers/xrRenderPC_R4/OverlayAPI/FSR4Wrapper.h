#pragma once

#include "fsr4bridge/Fsr4Backend.h"

#include <FidelityFX/api/include/ffx_api_types.h>

class FSR4Wrapper
{
public:
	using ContextParameters = Fsr4ContextParameters;
	using DrawParameters    = Fsr4DrawParameters;

	bool GetRenderScale(float& RenderScale);
	bool Create(ContextParameters params);
	void Destroy();

	bool Draw(const DrawParameters& params);

	bool IsCreated() const { return Created; }
	FfxApiDimensions2D GetDisplaySize() const { return ContextParams.displaySize; }
	
	bool             IsFsr4Active()            const { return Backend.IsFsr4Active(); }
	Fsr4Status       GetStatus()               const { return Backend.GetStatus(); }
	const xr_string& GetActiveVersionString()  const { return Backend.GetActiveVersionString(); }

	~FSR4Wrapper();
private:
	bool              Created = false;
	Fsr4Backend       Backend;
	ContextParameters ContextParams;
};

extern FSR4Wrapper g_Fsr4Wrapper;