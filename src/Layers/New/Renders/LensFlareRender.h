#pragma once
#include "../../Include/xrRender/LensFlareRender.h"

namespace Rendering
{
	class FlareRender : public IFlareRender
	{
	public:
		~FlareRender();
		void Copy(IFlareRender& _in) override;

		void CreateShader(LPCSTR sh_name, LPCSTR tex_name) override;
		void DestroyShader() override;
	};	
	
	class LensFlareRender : public ILensFlareRender
	{
	public:
		~LensFlareRender();
		void Copy(ILensFlareRender& _in) override;

		void Render(CLensFlare& owner, BOOL bSun, BOOL bFlares, BOOL bGradient) override;
		void OnDeviceCreate() override;
		void OnDeviceDestroy() override;
	};
}