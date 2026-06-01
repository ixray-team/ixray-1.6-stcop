#pragma once
class XRayShaderDefinesContainer;

class XRayShaderDefinesManager
{
public:
								XRayShaderDefinesManager	() = default;
								~XRayShaderDefinesManager	();
	XRayShaderDefinesContainer*	RegistryContainer			(const XRayShaderDefinesContainer& Container);

private:
	xr_vector<XRayShaderDefinesContainer*>	Defines;
};