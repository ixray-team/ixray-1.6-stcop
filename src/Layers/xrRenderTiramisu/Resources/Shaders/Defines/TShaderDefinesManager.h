#pragma once
class TShaderDefinesContainer;

class TShaderDefinesManager
{
public:
								TShaderDefinesManager		() = default;
								~TShaderDefinesManager		();
	TShaderDefinesContainer*	RegistryContainer			(const TShaderDefinesContainer& Container);

private:
	xr_vector<TShaderDefinesContainer*>	Defines;
};