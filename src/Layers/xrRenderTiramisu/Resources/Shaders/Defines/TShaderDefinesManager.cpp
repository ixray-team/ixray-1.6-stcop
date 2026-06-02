#include "stdafx.h"
#include "TShaderDefinesManager.h"
#include "TShaderDefinesContainer.h"

TShaderDefinesManager::~TShaderDefinesManager()
{
	for (TShaderDefinesContainer* i : Defines)
	{
		xr_delete(i);
	}
}

TShaderDefinesContainer* TShaderDefinesManager::RegistryContainer(const TShaderDefinesContainer& Container)
{
	TShaderDefinesContainer* ContainerItem = nullptr;

	auto LowerItem = std::lower_bound
	(
		Defines.begin(), Defines.end(), Container,
		[](const TShaderDefinesContainer* left, const TShaderDefinesContainer& right)
		{
			return (*left) < right;
		}
	);

	if (LowerItem == Defines.end())
	{
		ContainerItem = new TShaderDefinesContainer();
		ContainerItem->Copy(Container);
		Defines.push_back(ContainerItem);
	}
	else if ((**LowerItem) != Container)
	{
		ContainerItem = new TShaderDefinesContainer();
		ContainerItem->Copy(Container);
		Defines.insert(LowerItem, ContainerItem);
	}
	else
	{
		ContainerItem = *LowerItem;
	}

	return ContainerItem;
}