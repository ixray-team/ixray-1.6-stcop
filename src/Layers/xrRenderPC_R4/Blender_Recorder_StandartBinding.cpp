#include "stdafx.h"


#include "ResourceManager.h"
#include "blenders/Blender_Recorder.h"
#include "blenders/Blender.h"

#include "../../xrEngine/IGame_Persistent.h"

#include "dxRenderDeviceRender.h"

class UIIconBinding final : public RHIShaderConstant::Setup
{
public:
	void setup() override
	{
		CBUIIcon& Icon = RCache.Constants.Write<CBUIIcon>();
		if (const auto* Settings = Device.m_pRender->GetUIIconLighting())
		{
			Icon.ui_icon_light.set(Settings->Color.x * Settings->Intensity, Settings->Color.y * Settings->Intensity, Settings->Color.z * Settings->Intensity, Settings->Ambient);
			Icon.ui_icon_direction.set(Settings->Direction.x, Settings->Direction.y, Settings->Direction.z, 0.0f);
		}

		// World rendering stays neutral; the icon draw overrides this constant explicitly.
		Icon.ui_emissive_tint.set(1.0f, 1.0f, 1.0f, 1.0f);
	}
};

static UIIconBinding g_UIIconBinding;

class VisorRainBinding final : public RHIShaderConstant::Setup
{
public:
	void setup() override
	{
		const float Amount = g_pGamePersistent ? g_pGamePersistent->VisorRain.GetAmount() : 0.f;
		RCache.Constants.Write<CBVisor>().visor_rain.set(Amount, 0.f, 0.f, 0.f);
	}
};

static VisorRainBinding g_VisorRainBinding;

// Standart constant-binding
void	CBlender_Compile::SetMapping()
{
	r_Setup("ui_emissive_tint", &g_UIIconBinding);
	r_Setup("visor_rain", &g_VisorRainBinding);

	if (detail_scaler)
	{
		r_Setup("dt_params", detail_scaler);
	}

	for (const auto& [Name, Setup] : DEV->v_constant_setup)
	{
		r_Setup(*Name, Setup);
	}
}
