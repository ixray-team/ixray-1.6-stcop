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
#pragma once

// The value a token setting stores is what the player chose; the renderer reads the effective one, which the
// resolver derives from all the stored ones, so a choice a lower tier forces off comes back when the tier is raised
enum class EVideoSetting : u32
{
	Upscaler,
	Scale,
	DlssPreset,
	AntiAliasing,
	Msaa,
	MotionBlur,
	Dof,
	DofReload,
	DofDialogue,
	DofZoom,
	ShadingQuality,
	ShadowQuality,
	Pcss,
	VolumetricQuality,
	OcclusionQuality,
	ScreenSpaceQuality,
	ReflectionQuality,
	OffscreenReflectionQuality,
	GiQuality,
	CullingQuality,
	GeometryQuality,
	StreamingQuality,
	StreamingPriority,
	PostProcessingQuality,
	GrassQuality,
	DistanceQuality,
	MotionVectorsQuality,
	WetSurfaces,
	SoftParticles,
	SoftWater,
	WaterReflections,
	Parallax,
	DetailBump,
	VariableShading,
	Raindrops,
	Vignette,
	Aberration,
	Gasmask,
	RenderThread,
	Count
};

namespace VideoSettings
{
	ENGINE_API u32 Stored(EVideoSetting Setting);
	ENGINE_API u32 Effective(EVideoSetting Setting);
	ENGINE_API void Set(EVideoSetting Setting, u32 Value);
	// Changes with every resolve: the renderer applies the effective values when it differs from the one it applied
	ENGINE_API u32 Version();
	ENGINE_API void Resolve();
	// The renderer answers whether the device draws a value; asked again by every resolve
	ENGINE_API void SetSupportQuery(bool (*Query)(EVideoSetting Setting, u32 Value));
	ENGINE_API bool IsRestartPending();
	ENGINE_API void MarkRestarted();
	// While a level is loaded the settings its scene was built with (EApply::Level) keep their effective values; a
	// change to them applies at the next load
	ENGINE_API void SetLevelLoaded(bool IsLoaded);

	// By command name, for the options menu: the string id of why a setting is forced or not offered ("" when free),
	// its effective token, how it applies (live, recompile, restart, level), and whether it differs from the last preset
	ENGINE_API const char* Reason(const char* Name);
	ENGINE_API const char* EffectiveToken(const char* Name);
	ENGINE_API const char* ApplyClass(const char* Name);
	ENGINE_API bool DiffersFromPreset(const char* Name);
	ENGINE_API const char* PresetBase();

	// The menu's pending values on top of the stored ones: the queries above and the r_quality_preset token answer
	// from them until cleared; nothing is stored or applied
	ENGINE_API void SetPreview(const char* Name, const char* Token);
	ENGINE_API void ClearPreview();

	void RegisterCommands();
}
