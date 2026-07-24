#include "stdafx.h"
#include "TiramisuShaderDefinesManager.h"
#include "TiramisuShaderDefinesContainer.h"

TiramisuShaderDefinesManager::~TiramisuShaderDefinesManager()
{
	for (TiramisuShaderDefinesContainer* i : Defines)
	{
		xr_delete(i);
	}
}

TiramisuShaderDefinesContainer* TiramisuShaderDefinesManager::RegistryContainer(const TiramisuShaderDefinesContainer& Container)
{
	TiramisuShaderDefinesContainer* ContainerItem = nullptr;

	auto LowerItem = std::lower_bound
	(
		Defines.begin(), Defines.end(), Container,
		[](const TiramisuShaderDefinesContainer* left, const TiramisuShaderDefinesContainer& right)
		{
			return (*left) < right;
		}
	);

	if (LowerItem == Defines.end())
	{
		ContainerItem = new TiramisuShaderDefinesContainer();
		ContainerItem->Copy(Container);
		Defines.push_back(ContainerItem);
	}
	else if ((**LowerItem) != Container)
	{
		ContainerItem = new TiramisuShaderDefinesContainer();
		ContainerItem->Copy(Container);
		Defines.insert(LowerItem, ContainerItem);
	}
	else
	{
		ContainerItem = *LowerItem;
	}

	return ContainerItem;
}