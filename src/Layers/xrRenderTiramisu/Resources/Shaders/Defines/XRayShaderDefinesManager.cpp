#include "stdafx.h"
#include "XRayShaderDefinesManager.h"
#include "XRayShaderDefinesContainer.h"

XRayShaderDefinesManager::~XRayShaderDefinesManager()
{
	for (XRayShaderDefinesContainer* i : Defines)
	{
		xr_delete(i);
	}
}

XRayShaderDefinesContainer* XRayShaderDefinesManager::RegistryContainer(const XRayShaderDefinesContainer& Container)
{
	XRayShaderDefinesContainer* ContainerItem = nullptr;

	auto LowerItem = std::lower_bound
	(
		Defines.begin(), Defines.end(), Container,
		[](const XRayShaderDefinesContainer* left, const XRayShaderDefinesContainer& right)
		{
			return (*left) < right;
		}
	);

	if (LowerItem == Defines.end())
	{
		ContainerItem = new XRayShaderDefinesContainer();
		ContainerItem->Copy(Container);
		Defines.push_back(ContainerItem);
	}
	else if ((**LowerItem) != Container)
	{
		ContainerItem = new XRayShaderDefinesContainer();
		ContainerItem->Copy(Container);
		Defines.insert(LowerItem, ContainerItem);
	}
	else
	{
		ContainerItem = *LowerItem;
	}

	return ContainerItem;
}