/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
#include "stdafx.h"
#include "VideoSettings.h"
#include "xr_ioc_cmd.h"
#include "IGame_Persistent.h"
#include "Environment.h"

extern ENGINE_API u32 ps_proxy_r_scale_mode;
extern ENGINE_API u32 ps_render_scale_preset;
extern ENGINE_API float ps_render_scale;

namespace
{
	using enum EVideoSetting;

	constexpr u32 SettingCount = u32(EVideoSetting::Count);
	constexpr u32 NoValue = u32(-1);
	constexpr u32 Custom = 6;

	enum class EApply : u8
	{
		Live,
		Recompile,
		Restart,
		Level
	};

	enum class EReason : u8
	{
		None,
		Unavailable,
		Hardware,
		Shading,
		Shadow,
		Occlusion,
		AntiAliasing,
		Msaa,
		Upscaler,
		Dof
	};

	const char* const ReasonNames[] = { "", "st_reason_unavailable", "st_reason_hardware", "st_reason_shading", "st_reason_shadow", "st_reason_occlusion",
		"st_reason_anti_aliasing", "st_reason_msaa", "st_reason_upscaler", "st_reason_dof" };

	const char* const ApplyNames[] = { "live", "recompile", "restart", "level" };

	xr_token OffOn[] = { { "off", 0 }, { "on", 1 }, {} };
	xr_token RenderThreads[] = { { "off", 0 }, { "on", 1 }, { "pipelined", 2 }, {} };
	xr_token LowHigh[] = { { "low", 0 }, { "high", 1 }, {} };
	xr_token LowToHigh[] = { { "low", 0 }, { "medium", 1 }, { "high", 2 }, {} };
	xr_token LowToUltra[] = { { "low", 0 }, { "medium", 1 }, { "high", 2 }, { "ultra", 3 }, {} };
	xr_token OffToUltra[] = { { "off", 0 }, { "low", 1 }, { "medium", 2 }, { "high", 3 }, { "ultra", 4 }, {} };
	xr_token OffLowHigh[] = { { "off", 0 }, { "low", 1 }, { "high", 2 }, {} };
	xr_token OffMediumHigh[] = { { "off", 0 }, { "medium", 1 }, { "high", 2 }, {} };
	xr_token UpscalerTokens[] = { { "linear", 0 }, { "nearest", 1 }, { "dlss", 2 }, { "fsr", 3 }, { "xess", 4 }, {} };
	xr_token ScaleTokens[] = { { "native", 0 }, { "quality", 1 }, { "balanced", 2 }, { "performance", 3 }, { "ultra_performance", 4 }, {} };
	xr_token DlssTokens[] = { { "preset_e", 0 }, { "preset_k", 1 }, { "preset_l", 2 }, { "preset_m", 3 }, {} };
	xr_token AntiAliasingTokens[] = { { "off", 0 }, { "fxaa", 1 }, { "smaa", 2 }, { "taa", 3 }, { "msaa", 4 }, {} };
	xr_token MsaaTokens[] = { { "2x", 0 }, { "4x", 1 }, { "8x", 2 }, {} };
	xr_token PhysicalDofTokens[] = { { "autofocus", int(EDofMode::Autofocus) }, { "manual", int(EDofMode::Manual) }, {} };
	xr_token PresetTokens[] = { { "minimal", 0 }, { "low", 1 }, { "medium", 2 }, { "high", 3 }, { "ultra", 4 }, { "extreme", 5 }, { "custom", Custom }, {} };

	constexpr u32 All = ~0u;
#ifdef DEBUG_DRAW
	constexpr u32 UpscalerTiers = All;
#else
	constexpr u32 UpscalerTiers = All & ~2u;
#endif

	// Tiers: a bit for every value the renderer draws today; the rest are stored and saved, and run as the nearest one
	struct Row
	{
		const char* Name;
		xr_token* Tokens;
		u32 Default;
		u32 Tiers;
		EApply Apply;
		bool IsPreset;
	};

	const Row Rows[] =
	{
		{ "r_upscaler", UpscalerTokens, 0, UpscalerTiers, EApply::Restart, false },
		{ "r_scale", ScaleTokens, 0, All, EApply::Restart, false },
		{ "r_dlss_preset", DlssTokens, 1, All, EApply::Restart, false },
		{ "r_anti_aliasing", AntiAliasingTokens, 0, All, EApply::Recompile, false },
		{ "r_msaa", MsaaTokens, 0, All, EApply::Recompile, false },
		{ "r_motion_blur", OffLowHigh, 0, All, EApply::Live, false },
		{ "r_dof", OffLowHigh, 0, All, EApply::Live, false },
		{ "r_dof_reload", OffOn, 1, All, EApply::Live, false },
		{ "r_dof_dialogue", OffOn, 1, All, EApply::Live, false },
		{ "r_dof_zoom", OffOn, 0, All, EApply::Live, false },
		{ "r_shading_quality", LowToUltra, 2, All, EApply::Level, true },
		{ "r_shadow_quality", OffToUltra, 3, 0b01111, EApply::Recompile, true },
		{ "r_pcss", OffOn, 0, All, EApply::Recompile, true },
		{ "r_volumetric_quality", OffToUltra, 2, All, EApply::Recompile, true },
		{ "r_occlusion_quality", OffMediumHigh, 1, 0b011, EApply::Recompile, true },
		{ "r_screen_space_quality", LowHigh, 0, All, EApply::Restart, true },
		{ "r_reflection_quality", LowToUltra, 1, 0b0111, EApply::Recompile, true },
		{ "r_gi_quality", LowToHigh, 0, 0b001, EApply::Live, true },
		{ "r_culling_quality", LowHigh, 1, All, EApply::Live, true },
		{ "r_geometry_quality", LowHigh, 1, 0b10, EApply::Live, true },
		{ "r_streaming_quality", LowToUltra, 2, All, EApply::Level, true },
		{ "r_streaming_priority", LowToHigh, 1, 0b010, EApply::Live, true },
		{ "r_post_processing_quality", OffMediumHigh, 1, 0b011, EApply::Live, true },
		{ "r_grass_quality", LowToUltra, 2, All, EApply::Live, true },
		{ "r_distance_quality", LowToUltra, 2, All, EApply::Live, true },
		{ "r_motion_vectors_quality", LowHigh, 1, All, EApply::Recompile, false },
		{ "r_wet_surfaces", OffOn, 1, All, EApply::Restart, true },
		{ "r_soft_particles", OffOn, 1, All, EApply::Recompile, true },
		{ "r_soft_water", OffOn, 1, All, EApply::Recompile, true },
		{ "r_water_reflections", OffOn, 1, All, EApply::Recompile, true },
		{ "r_parallax", OffOn, 1, All, EApply::Level, true },
		{ "r_detail_bump", OffOn, 1, All, EApply::Level, true },
		{ "r_variable_shading", OffOn, 0, All, EApply::Recompile, false },
		{ "r_raindrops", OffOn, 1, All, EApply::Live, false },
		{ "r_vignette", OffOn, 0, All, EApply::Live, false },
		{ "r_aberration", OffOn, 0, All, EApply::Live, false },
		{ "r_gasmask", OffOn, 0, All, EApply::Live, false },
		// The renderer's frame on a thread of its own, one frame behind the game: docs/engine/render-thread-plan.md.
		// Stored and saved; nothing reads it until the plan's stage 6
		{ "r_render_thread", RenderThreads, 0, All, EApply::Restart, false },
	};

	static_assert(std::size(Rows) == SettingCount);

	struct Values
	{
		u32 Stored[SettingCount] = {};
		u32 Effective[SettingCount] = {};
		EReason Reasons[SettingCount] = {};
	};

	struct VideoState
	{
		Values Applied;
		Values Previewed;
		u32 Preview[SettingCount] = {};
		bool IsPreview = false;
		xr_token Offered[SettingCount][8];
		u32 PresetValues[SettingCount] = {};
		u32 PresetBase = 3;
		u32 PresetLoaded = NoValue;
		u32 Version = 0;
		u64 RestartApplied = u64(-1);
		bool (*Supported)(EVideoSetting Setting, u32 Value) = nullptr;
		// The effective values of the EApply::Level settings the loaded level was built with
		bool IsLevel = false;
		u32 Built[SettingCount] = {};
		u64 Deferred = 0;
	};

	VideoState g_Video;
	u32 g_PhysicalDofMode = u32(EDofMode::Autofocus);

	u32 Find(const char* Name)
	{
		u32 Index = 0;

		while (Index < SettingCount && xr_strcmp(Rows[Index].Name, Name))
		{
			++Index;
		}

		return Index;
	}

	const char* TokenName(const xr_token* Tokens, u32 Value)
	{
		for (; Tokens->name; ++Tokens)
		{
			if (u32(Tokens->id) == Value)
			{
				return Tokens->name;
			}
		}

		return "?";
	}

	// r_scale holds a token below 5 and a percentage from 50 on
	void Print(u32 Index, u32 Value, IConsole_Command::TStatus& S)
	{
		if (Index == u32(Scale) && Value >= 50)
		{
			xr_sprintf(S, "%u", Value);
			return;
		}

		xr_strcpy(S, TokenName(Rows[Index].Tokens, Value));
	}

	bool Parse(u32 Index, const char* Args, u32& Value)
	{
		for (const xr_token* Token = Rows[Index].Tokens; Token->name; ++Token)
		{
			if (!_stricmp(Token->name, Args))
			{
				Value = u32(Token->id);
				return true;
			}
		}

		// The options menu writes its check boxes as 1 and 0
		if (Rows[Index].Tokens == OffOn && (!xr_strcmp(Args, "1") || !xr_strcmp(Args, "0")))
		{
			Value = Args[0] == '1';
			return true;
		}

		if (Index == u32(Scale) && Args[0] >= '0' && Args[0] <= '9')
		{
			Value = u32(std::clamp(atoi(Args), 50, 100));
			return true;
		}

		return false;
	}

	EReason Drawn(u32 Index, u32 Value)
	{
		if (Index == u32(Scale))
		{
			return EReason::None;
		}

		if (!(Rows[Index].Tiers >> Value & 1))
		{
			return EReason::Unavailable;
		}

		const bool IsVendor = Index == u32(Upscaler) && (Value == 2 || Value == 4);

		if ((IsVendor && Engine.External.GetAPI() != ERHI_API_LAYER::D3D12) || (g_Video.Supported && !g_Video.Supported(EVideoSetting(Index), Value)))
		{
			return EReason::Hardware;
		}

		return EReason::None;
	}

	u32 TokenCount(u32 Index)
	{
		u32 Size = 0;

		while (Rows[Index].Tokens[Size].name)
		{
			++Size;
		}

		return Size;
	}

	// A value the renderer does not draw runs as the nearest one below it, else above it; a setting with one value
	// drawn is not offered at all
	void Clamp(Values& V, u32 Index)
	{
		const u32 Size = TokenCount(Index);
		u32 DrawnCount = 0, Below = NoValue, Above = NoValue;

		for (u32 Value = 0; Value < Size; ++Value)
		{
			if (Drawn(Index, Value) == EReason::None)
			{
				++DrawnCount;
				Below = Value <= V.Stored[Index] ? Value : Below;
				Above = Value > V.Stored[Index] && Above == NoValue ? Value : Above;
			}
		}

		if (const EReason Reason = Drawn(Index, V.Effective[Index]); Reason != EReason::None)
		{
			V.Effective[Index] = Below != NoValue ? Below : Above != NoValue ? Above : 0;
			V.Reasons[Index] = Reason;
		}

		if (DrawnCount == 1 && Index != u32(Scale))
		{
			V.Reasons[Index] = EReason::Unavailable;
		}
	}

	u32 Get(EVideoSetting Setting)
	{
		return g_Video.Applied.Effective[u32(Setting)];
	}

	// Rules from the weakest to the strongest: a later one overrides what an earlier one forced
	void ApplyRules(Values& V)
	{
		const auto Get = [&V](EVideoSetting Setting) { return V.Effective[u32(Setting)]; };

		const auto Force = [&V](EVideoSetting Setting, u32 Value, EReason Reason)
		{
			V.Effective[u32(Setting)] = Value;
			V.Reasons[u32(Setting)] = Reason;
		};

		const bool IsUpscaling = Get(Upscaler) >= 2;

		if (Get(Upscaler) != 2)
		{
			V.Reasons[u32(DlssPreset)] = EReason::Upscaler;
		}

		if (Get(Dof) == 0)
		{
			for (const EVideoSetting Setting : { DofReload, DofDialogue, DofZoom })
			{
				Force(Setting, 0, EReason::Dof);
			}
		}

		// The upscalers take the place of the anti-aliasing passes; the samples of MSAA stay
		if (IsUpscaling && Get(AntiAliasing) >= 1 && Get(AntiAliasing) <= 3)
		{
			Force(AntiAliasing, 0, EReason::Upscaler);
		}

		if (Get(AntiAliasing) != 4)
		{
			V.Reasons[u32(Msaa)] = EReason::AntiAliasing;
		}
		else
		{
			Force(VariableShading, 0, EReason::Msaa);
		}

		// Screen space reflections trace the depth the occlusion pass splits off
		if (Get(OcclusionQuality) == 0)
		{
			Force(ReflectionQuality, 0, EReason::Occlusion);
		}

		if (Get(ShadowQuality) == 0)
		{
			Force(Pcss, 0, EReason::Shadow);
		}

		if (Get(ShadingQuality) < 2)
		{
			for (const EVideoSetting Setting : { ScreenSpaceQuality, ReflectionQuality, GiQuality, DetailBump })
			{
				Force(Setting, 0, EReason::Shading);
			}
		}

		// Low shading is baked: no cascades, no lamp shadows, no screen-space passes, no bloom
		if (Get(ShadingQuality) == 0)
		{
			for (const EVideoSetting Setting : { ShadowQuality, Pcss, VolumetricQuality, OcclusionQuality, GeometryQuality, StreamingPriority,
				MotionVectorsQuality, PostProcessingQuality })
			{
				Force(Setting, 0, EReason::Shading);
			}
		}

		if (Get(AntiAliasing) == 3 || IsUpscaling)
		{
			Force(MotionVectorsQuality, 1, IsUpscaling ? EReason::Upscaler : EReason::AntiAliasing);
		}
	}

	void Derive(Values& V, bool IsFrozen = true)
	{
		std::copy(std::begin(V.Stored), std::end(V.Stored), V.Effective);
		std::fill(std::begin(V.Reasons), std::end(V.Reasons), EReason::None);

		for (u32 Index = 0; Index < SettingCount; ++Index)
		{
			Clamp(V, Index);
			V.Effective[Index] = IsFrozen && g_Video.IsLevel && Rows[Index].Apply == EApply::Level ? g_Video.Built[Index] : V.Effective[Index];
		}

		ApplyRules(V);
	}

	// One line for each new set of the level's settings that wait for the next load
	void LogDeferred()
	{
		Values Wanted = g_Video.Applied;
		Derive(Wanted, false);
		string256 Names = {};
		u64 Key = 0;

		for (u32 Index = 0; Index < SettingCount && g_Video.IsLevel; ++Index)
		{
			if (Rows[Index].Apply == EApply::Level && Wanted.Effective[Index] != g_Video.Built[Index])
			{
				Key = Key * 257 + Index * 8 + Wanted.Effective[Index] + 1;
				xr_strcat(Names, Names[0] ? ", " : "");
				xr_strcat(Names, Rows[Index].Name);
			}
		}

		if (Key && Key != g_Video.Deferred && Device.b_is_Ready)
		{
			Msg("- %s: applied at the next level load", Names);
		}

		g_Video.Deferred = Key;
	}

	void DerivePreview()
	{
		for (u32 Index = 0; Index < SettingCount; ++Index)
		{
			g_Video.Previewed.Stored[Index] = g_Video.Preview[Index] != NoValue ? g_Video.Preview[Index] : g_Video.Applied.Stored[Index];
		}

		Derive(g_Video.Previewed);
	}

	const Values& Current()
	{
		return g_Video.IsPreview ? g_Video.Previewed : g_Video.Applied;
	}

	void ApplyEngine()
	{
		g_NewDofMode = Get(Dof) == 2 ? g_PhysicalDofMode : u32(EDofMode::Static);

		static constexpr int TextureLods[] = { 2, 1, 0, 0 };
		psTextureLOD = TextureLods[Get(StreamingQuality)];

		// The editors keep their own view scale and draw distance
		if (Device.IsEditorMode())
		{
			return;
		}

		static constexpr u32 ScaleModes[] = { 1, 0, 2, 3, 4 };
		static constexpr float Ratios[] = { 1.0f, 1.0f / 1.5f, 1.0f / 1.724f, 1.0f / 2.0f, 1.0f / 3.0f };
		static constexpr float Distances[] = { 100.0f, 300.0f, 500.0f, 2000.0f };
		const u32 ScaleValue = Get(Scale);
		ps_proxy_r_scale_mode = ScaleModes[Get(Upscaler)];
		ps_render_scale_preset = ScaleValue < 5 ? ScaleValue : 5;
		ps_render_scale = ScaleValue < 5 ? Ratios[ScaleValue] : float(ScaleValue) / 100.0f;
		psViewDistance = Distances[Get(DistanceQuality)];
	}

	u64 RestartKey()
	{
		u64 Key = Get(ReflectionQuality) >= 2;

		for (u32 Index = 0; Index < SettingCount; ++Index)
		{
			Key = Rows[Index].Apply == EApply::Restart ? Key * 257 + g_Video.Applied.Effective[Index] : Key;
		}

		return Key;
	}

	void LoadPreset(u32 Preset, bool IsApplied)
	{
		std::fill(std::begin(g_Video.PresetValues), std::end(g_Video.PresetValues), NoValue);
		g_Video.PresetBase = g_Video.PresetLoaded = Preset;

		string_path Path;
		xr_strconcat(Path, "ixray_settings\\rspec_", PresetTokens[Preset].name, ".ltx");
		IReader* File = FS.exist(_game_config_, Path) ? FS.r_open(_game_config_, Path) : nullptr;

		if (!File)
		{
			Msg("! r_quality_preset: no %s", Path);
			return;
		}

		while (!File->eof())
		{
			string1024 Line;
			string128 Name, Value;
			File->r_string(Line, sizeof(Line));

			if (sscanf(Line, "%127s %127s", Name, Value) != 2 || Name[0] == ';')
			{
				continue;
			}

			const u32 Index = Find(Name);
			u32 Parsed;

			// What a preset sets outside the table is a plain command, as the grass distance
			if (Index == SettingCount && Console->GetCommand(Name))
			{
				if (IsApplied)
				{
					Console->Execute(Line);
				}

				continue;
			}

			if (Index == SettingCount || !Rows[Index].IsPreset || !Parse(Index, Value, Parsed))
			{
				Msg("! %s: '%s %s' is not a preset setting", Path, Name, Value);
				continue;
			}

			g_Video.PresetValues[Index] = Parsed;
			g_Video.Applied.Stored[Index] = IsApplied ? Parsed : g_Video.Applied.Stored[Index];
		}

		FS.r_close(File);

		if (IsApplied)
		{
			VideoSettings::Resolve();
		}
	}

	bool Differs(const Values& V, u32 Index)
	{
		if (g_Video.PresetLoaded != g_Video.PresetBase)
		{
			LoadPreset(g_Video.PresetBase, false);
		}

		return Rows[Index].IsPreset && g_Video.PresetValues[Index] != NoValue && V.Stored[Index] != g_Video.PresetValues[Index];
	}

	bool IsCustom(const Values& V)
	{
		for (u32 Index = 0; Index < SettingCount; ++Index)
		{
			if (Differs(V, Index))
			{
				return true;
			}
		}

		return false;
	}

	class CCC_VideoSetting final : public CCC_Token
	{
		u32 Index;

	public:
		CCC_VideoSetting(u32 Index) : CCC_Token(Rows[Index].Name, &g_Video.Applied.Stored[Index], Rows[Index].Tokens), Index(Index) {}

		void Execute(const char* Args) override
		{
			u32 Value;

			if (!Parse(Index, Args, Value))
			{
				InvalidSyntax();
				return;
			}

			VideoSettings::Set(EVideoSetting(Index), Value);
		}

		void Status(TStatus& S) override
		{
			Print(Index, g_Video.Applied.Stored[Index], S);
		}

		void Info(TInfo& I) override
		{
			I[0] = 0;

			for (const xr_token* Token = tokens; Token->name; ++Token)
			{
				xr_strcat(I, I[0] ? "/" : "");
				xr_strcat(I, Token->name);
			}

			xr_strcat(I, Index == u32(Scale) ? " or a percentage 50-100" : "");
		}

		// The values the renderer draws: the menu offers no other
		xr_token* GetToken() override
		{
			return g_Video.Offered[Index];
		}
	};

	// r_quality_preset <preset> loads ixray_settings\rspec_<preset>.ltx. It reads custom once a setting it covers
	// differs from it; custom <preset> names the preset without loading it, as the saved config does
	class CCC_QualityPreset final : public CCC_Token
	{
		u32 Shown = 3;

	public:
		CCC_QualityPreset() : CCC_Token("r_quality_preset", &Shown, PresetTokens) {}

		void Execute(const char* Args) override
		{
			string128 Name = {}, Base = {};
			sscanf(Args, "%127s %127s", Name, Base);
			u32 Preset = 0;

			while (PresetTokens[Preset].name && xr_strcmp(PresetTokens[Preset].name, Name))
			{
				++Preset;
			}

			if (!PresetTokens[Preset].name)
			{
				InvalidSyntax();
				return;
			}

			if (Preset != Custom)
			{
				LoadPreset(Preset, true);
				return;
			}

			for (u32 Index = 0; Index < Custom; ++Index)
			{
				g_Video.PresetBase = xr_strcmp(PresetTokens[Index].name, Base) ? g_Video.PresetBase : Index;
			}
		}

		void Status(TStatus& S) override
		{
			Shown = IsCustom(Current()) ? Custom : g_Video.PresetBase;
			xr_strcpy(S, PresetTokens[Shown].name);
		}

		void Save(IWriter* F) override
		{
			F->w_printf("%s %s%s\r\n", cName, IsCustom(g_Video.Applied) ? "custom " : "", PresetTokens[g_Video.PresetBase].name);
		}

		void Info(TInfo& I) override
		{
			xr_strcpy(I, "minimal/low/medium/high/ultra/extreme, or custom <preset>");
		}
	};

	// r4_ndof_mode: how the physical depth of field of r_dof high focuses, for the DOF setup tool
	class CCC_PhysicalDof final : public CCC_Token
	{
	public:
		CCC_PhysicalDof() : CCC_Token("r4_ndof_mode", &g_PhysicalDofMode, PhysicalDofTokens) {}

		void Execute(const char* Args) override
		{
			CCC_Token::Execute(Args);
			VideoSettings::Resolve();
		}
	};

	// r_settings: every setting as stored, as it runs and why
	class CCC_VideoSettingsDump final : public IConsole_Command
	{
	public:
		CCC_VideoSettingsDump() : IConsole_Command("r_settings")
		{
			bEmptyArgsHandled = true;
		}

		void Execute(const char*) override
		{
			for (u32 Index = 0; Index < SettingCount; ++Index)
			{
				TStatus Stored, Effective;
				Print(Index, g_Video.Applied.Stored[Index], Stored);
				Print(Index, g_Video.Applied.Effective[Index], Effective);
				Msg("- %s %s: %s %s (%s)%s", Rows[Index].Name, Stored, Effective, ReasonNames[u32(g_Video.Applied.Reasons[Index])], ApplyNames[u32(Rows[Index].Apply)],
					Differs(g_Video.Applied, Index) ? " differs from the preset" : "");
			}

			Msg("- r_quality_preset base %s%s", PresetTokens[g_Video.PresetBase].name, VideoSettings::IsRestartPending() ? ", r_restart pending" : "");
		}
	};
}

u32 VideoSettings::Stored(EVideoSetting Setting)
{
	return g_Video.Applied.Stored[u32(Setting)];
}

u32 VideoSettings::Effective(EVideoSetting Setting)
{
	return g_Video.Applied.Effective[u32(Setting)];
}

u32 VideoSettings::Version()
{
	return g_Video.Version;
}

void VideoSettings::Set(EVideoSetting Setting, u32 Value)
{
	const bool WasPending = IsRestartPending();
	g_Video.Applied.Stored[u32(Setting)] = Value;
	Resolve();

	if (!WasPending && IsRestartPending() && Device.b_is_Ready && !Device.IsEditorMode())
	{
		Msg("- %s is applied by r_restart", Rows[u32(Setting)].Name);
	}
}

void VideoSettings::Resolve()
{
	Derive(g_Video.Applied);
	LogDeferred();

	for (u32 Index = 0; Index < SettingCount; ++Index)
	{
		u32 Size = 0;

		for (const xr_token* Token = Rows[Index].Tokens; Token->name; ++Token)
		{
			g_Video.Offered[Index][Size] = *Token;
			Size += Drawn(Index, u32(Token->id)) == EReason::None;
		}

		g_Video.Offered[Index][Size] = {};
	}

	++g_Video.Version;
	ApplyEngine();

	if (g_Video.IsPreview)
	{
		DerivePreview();
	}
}

void VideoSettings::SetSupportQuery(bool (*Query)(EVideoSetting Setting, u32 Value))
{
	g_Video.Supported = Query;
	Resolve();
}

bool VideoSettings::IsRestartPending()
{
	return g_Video.RestartApplied != RestartKey();
}

void VideoSettings::MarkRestarted()
{
	g_Video.RestartApplied = RestartKey();
}

void VideoSettings::SetLevelLoaded(bool IsLoaded)
{
	g_Video.IsLevel = false;
	Resolve();
	std::copy(std::begin(g_Video.Applied.Effective), std::end(g_Video.Applied.Effective), g_Video.Built);
	g_Video.IsLevel = IsLoaded;
	g_Video.Deferred = 0;
}

const char* VideoSettings::Reason(const char* Name)
{
	const u32 Index = Find(Name);
	return Index < SettingCount ? ReasonNames[u32(Current().Reasons[Index])] : "";
}

const char* VideoSettings::EffectiveToken(const char* Name)
{
	static IConsole_Command::TStatus Token;
	const u32 Index = Find(Name);

	if (Index == SettingCount)
	{
		const char* Status = Console->GetString(Name);
		return Status ? Status : "";
	}

	Print(Index, Current().Effective[Index], Token);
	return Token;
}

const char* VideoSettings::ApplyClass(const char* Name)
{
	const u32 Index = Find(Name);
	return Index < SettingCount ? ApplyNames[u32(Rows[Index].Apply)] : "";
}

bool VideoSettings::DiffersFromPreset(const char* Name)
{
	const u32 Index = Find(Name);
	return Index < SettingCount && Differs(Current(), Index);
}

const char* VideoSettings::PresetBase()
{
	return PresetTokens[g_Video.PresetBase].name;
}

void VideoSettings::SetPreview(const char* Name, const char* Token)
{
	const u32 Index = Find(Name);
	u32 Value;

	if (Index == SettingCount || !Parse(Index, Token, Value))
	{
		return;
	}

	if (!g_Video.IsPreview)
	{
		std::fill(std::begin(g_Video.Preview), std::end(g_Video.Preview), NoValue);
		g_Video.IsPreview = true;
	}

	g_Video.Preview[Index] = Value;
	DerivePreview();
}

void VideoSettings::ClearPreview()
{
	g_Video.IsPreview = false;
}

void VideoSettings::RegisterCommands()
{
	for (u32 Index = 0; Index < SettingCount; ++Index)
	{
		g_Video.Applied.Stored[Index] = Rows[Index].Default;
	}

	Resolve();

	static xr_vector<CCC_VideoSetting> Commands;
	Commands.reserve(SettingCount);

	for (u32 Index = 0; Index < SettingCount; ++Index)
	{
		Console->AddCommand(&Commands.emplace_back(Index));
	}

	static CCC_QualityPreset Preset;
	static CCC_VideoSettingsDump Dump;
	static CCC_PhysicalDof PhysicalDof;
	Console->AddCommand(&Preset);
	Console->AddCommand(&Dump);
	Console->AddCommand(&PhysicalDof);
}
