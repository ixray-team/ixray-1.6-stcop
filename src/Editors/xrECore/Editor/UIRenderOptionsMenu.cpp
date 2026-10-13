#include "stdafx.h"
#include "UIRenderOptionsMenu.h"

namespace
{
	struct SQualityItem
	{
		const char* Name;
		const char* TemporalPreset;
		float Value;
	};

	constexpr SQualityItem QualityItems[] =
	{
		{ "25%", "Ultra Performance", 0.25f },
		{ "50%", "Performance", 0.5f },
		{ "100%", "Native AA", 1.f },
	};

	struct SUpscalerItem
	{
		const char* Name;
		// vid_scale_mode value
		u32 Mode;
	};

	constexpr SUpscalerItem UpscalerItems[] =
	{
		{ "Linear", 1 },
		{ "DLSS", 2 },
		{ "FSR", 3 },
	};

	void DrawQualityMenu()
	{
		if (!ImGui::BeginMenu("Quality"))
		{
			return;
		}

		const bool Temporal = GEditorDraw.Upscaler >= 2;
		for (u32 Index = 0; Index < std::size(QualityItems); ++Index)
		{
			const SQualityItem& Item = QualityItems[Index];
			const float Lower = Index > 0 ? (QualityItems[Index - 1].Value + Item.Value) * 0.5f : 0.f;
			bool Selected = GEditorDraw.ScreenQuality >= Lower;
			if (Index + 1 < std::size(QualityItems))
			{
				Selected = Selected && GEditorDraw.ScreenQuality < (Item.Value + QualityItems[Index + 1].Value) * 0.5f;
			}

			if (ImGui::MenuItem(Item.Name, Temporal ? Item.TemporalPreset : "", &Selected))
			{
				EContext.UI->SetRenderQuality(Item.Value);
				EContext.UI->RedrawScene();
			}
		}

		ImGui::EndMenu();
	}

	void DrawUpscalerMenu()
	{
		if (!ImGui::BeginMenu("Upscaler"))
		{
			return;
		}

		for (const SUpscalerItem& Item : UpscalerItems)
		{
			bool Selected = GEditorDraw.Upscaler == Item.Mode;
			if (ImGui::MenuItem(Item.Name, "", &Selected))
			{
				GEditorDraw.Upscaler = Item.Mode;
				EContext.UI->RedrawScene();
			}
		}

		ImGui::EndMenu();
	}

	void DrawFillModeMenu()
	{
		if (!ImGui::BeginMenu("Fill Mode"))
		{
			return;
		}

		const char* Names[] = { "Point", "Wireframe", "Solid" };
		const u32 Modes[] = { D3DFILL_POINT, D3DFILL_WIREFRAME, D3DFILL_SOLID };

		for (u32 Index = 0; Index < std::size(Modes); ++Index)
		{
			bool Selected = GEditorDraw.FillMode == Modes[Index];
			if (ImGui::MenuItem(Names[Index], "", &Selected))
			{
				GEditorDraw.FillMode = Modes[Index];
				EContext.UI->RedrawScene();
			}
		}

		ImGui::EndMenu();
	}

	void DrawShadeModeMenu()
	{
		if (!ImGui::BeginMenu("Shader Mode"))
		{
			return;
		}

		const char* Names[] = { "Flat", "Gouraud" };
		const u32 Modes[] = { D3DSHADE_FLAT, D3DSHADE_GOURAUD };

		for (u32 Index = 0; Index < std::size(Modes); ++Index)
		{
			bool Selected = GEditorDraw.ShadeMode == Modes[Index];
			if (ImGui::MenuItem(Names[Index], "", &Selected))
			{
				GEditorDraw.ShadeMode = Modes[Index];
				EContext.UI->RedrawScene();
			}
		}

		ImGui::EndMenu();
	}

	void DrawDeviceFlag(const char* Name, u32 Flag)
	{
		bool Selected = psDeviceFlags.test(Flag);
		if (ImGui::MenuItem(Name, "", &Selected))
		{
			psDeviceFlags.set(Flag, Selected);
			EContext.UI->RedrawScene();
		}
	}
}

namespace EditorRenderMenu
{
	void DrawItems()
	{
		DrawQualityMenu();
		DrawUpscalerMenu();
		DrawFillModeMenu();
		DrawShadeModeMenu();
		DrawDeviceFlag("Edged Faces", rsEdgedFaces);

		ImGui::Separator();
		{
			bool Selected = !Caps.bForceGPU_SW;
			if (ImGui::MenuItem("RenderHW", "", &Selected))
			{
				Caps.bForceGPU_SW = !Selected;
				EContext.UI->Resize();
			}
		}

		ImGui::Separator();
		DrawDeviceFlag("Filter Linear", rsFilterLinear);
		DrawDeviceFlag("Textures", rsRenderTextures);
	}
}
