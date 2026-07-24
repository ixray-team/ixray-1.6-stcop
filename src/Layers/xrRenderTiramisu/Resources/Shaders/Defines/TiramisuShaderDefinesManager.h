#pragma once

#include "TiramisuRenderTypes.h"
class TiramisuShaderDefinesContainer;

// Создаёт и переиспользует наборы shader defines.
class TiramisuShaderDefinesManager
{
public:
								TiramisuShaderDefinesManager		() = default;
								~TiramisuShaderDefinesManager		();
	TiramisuShaderDefinesContainer*	RegistryContainer			(const TiramisuShaderDefinesContainer& Container);

private:
	xr_vector<TiramisuShaderDefinesContainer*>	Defines;
};
