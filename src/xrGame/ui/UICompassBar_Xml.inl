// Internal XML helpers for CUICompassBar (included into UICompassBar.cpp).
#include "../../xrUI/UIXmlInit.h"

namespace
{
	struct SStyleSheet
	{
		SUITextureShadowParams shadow;
		bool hasShadow = false;
	};

	namespace
	{
		xr_hash_map<shared_str, bool> s_warnedFeatures;
	}

	void WarnLegacyOnce(const char* featureKey, const char* message);

	EUIItemAlign ParseAlign(const char* alignStr)
	{
		if (!alignStr || !*alignStr)
		{
			return alCenter;
		}
		if (alignStr[0] == 'l' || alignStr[0] == 'L')
		{
			return alLeft;
		}
		if (alignStr[0] == 'r' || alignStr[0] == 'R')
		{
			return alRight;
		}
		return alCenter;
	}

	bool HasAttrib(CUIXml& uiXml, const char* path, int index, const char* attrib)
	{
		if (!path || !attrib || !*attrib)
		{
			return false;
		}
		return uiXml.ReadAttrib(path, index, attrib, nullptr) != nullptr;
	}

	float ReadPreferFlt(
		CUIXml& uiXml,
		const char* path,
		int index,
		const char* modernAttrib,
		const char* legacyAttrib,
		float defaultValue,
		bool* usedLegacy)
	{
		if (usedLegacy)
		{
			*usedLegacy = false;
		}

		if (modernAttrib && HasAttrib(uiXml, path, index, modernAttrib))
		{
			if (legacyAttrib && HasAttrib(uiXml, path, index, legacyAttrib) && usedLegacy)
			{
				*usedLegacy = true;
			}
			return uiXml.ReadAttribFlt(path, index, modernAttrib, defaultValue);
		}

		if (legacyAttrib)
		{
			if (usedLegacy && HasAttrib(uiXml, path, index, legacyAttrib))
			{
				*usedLegacy = true;
			}
			return uiXml.ReadAttribFlt(path, index, legacyAttrib, defaultValue);
		}

		return defaultValue;
	}

	int ReadPreferInt(
		CUIXml& uiXml,
		const char* path,
		int index,
		const char* modernAttrib,
		const char* legacyAttrib,
		int defaultValue,
		bool* usedLegacy)
	{
		if (usedLegacy)
		{
			*usedLegacy = false;
		}

		if (modernAttrib && HasAttrib(uiXml, path, index, modernAttrib))
		{
			if (legacyAttrib && HasAttrib(uiXml, path, index, legacyAttrib) && usedLegacy)
			{
				*usedLegacy = true;
			}
			return uiXml.ReadAttribInt(path, index, modernAttrib, defaultValue);
		}

		if (legacyAttrib)
		{
			if (usedLegacy && HasAttrib(uiXml, path, index, legacyAttrib))
			{
				*usedLegacy = true;
			}
			return uiXml.ReadAttribInt(path, index, legacyAttrib, defaultValue);
		}

		return defaultValue;
	}

	SCompassLayoutScalar MakeLayoutScalar(float value, bool forceAbsolute)
	{
		return MakeCompassLayoutScalar(value, forceAbsolute);
	}

	float ResolveLayoutValue(const SCompassLayoutScalar& value, float parentLen)
	{
		return ResolveCompassLayoutValue(value, parentLen);
	}

	bool ParseSizePairText(const char* text, float& outW, float& outH)
	{
		if (!text || !*text)
		{
			return false;
		}

		float w = outW;
		float h = outH;
		if (sscanf(text, "%f %f", &w, &h) == 2)
		{
			outW = w;
			outH = h;
			return true;
		}
		if (sscanf(text, "%f", &w) == 1)
		{
			outW = w;
			outH = w;
			return true;
		}
		return false;
	}

	bool ReadSizePxPair(CUIXml& uiXml, const char* path, int index, float& outW, float& outH)
	{
		return ParseSizePairText(uiXml.ReadAttrib(path, index, "size_px", nullptr), outW, outH);
	}

	bool ReadSizePair(
		CUIXml& uiXml,
		const char* path,
		int index,
		SCompassLayoutScalar& outW,
		SCompassLayoutScalar& outH)
	{
		float w = outW.value;
		float h = outH.value;
		if (ParseSizePairText(uiXml.ReadAttrib(path, index, "size_px", nullptr), w, h))
		{
			outW = MakeLayoutScalar(w, true);
			outH = MakeLayoutScalar(h, true);
			return true;
		}
		if (ParseSizePairText(uiXml.ReadAttrib(path, index, "size", nullptr), w, h))
		{
			outW = MakeLayoutScalar(w, false);
			outH = MakeLayoutScalar(h, false);
			return true;
		}
		return false;
	}

	SCompassLayoutScalar ReadLayoutAttrib(
		CUIXml& uiXml,
		const char* path,
		int index,
		const char* modernAttrib,
		const char* absoluteAttrib,
		float defaultValue)
	{
		if (absoluteAttrib && HasAttrib(uiXml, path, index, absoluteAttrib))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(path, index, absoluteAttrib, defaultValue), true);
		}
		if (modernAttrib && HasAttrib(uiXml, path, index, modernAttrib))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(path, index, modernAttrib, defaultValue), false);
		}
		return MakeLayoutScalar(defaultValue, std::abs(defaultValue) > 1.0f);
	}

	void ReadWidgetLayout(CUIXml& uiXml, const char* path, SCompassWidgetLayout& outLayout, float defaultW, float defaultH)
	{
		outLayout = SCompassWidgetLayout();
		if (!path || !uiXml.NavigateToNode(path, 0))
		{
			return;
		}

		outLayout.hasNode = true;
		outLayout.x = ReadLayoutAttrib(uiXml, path, 0, "x", nullptr, 0.0f);
		outLayout.y = ReadLayoutAttrib(uiXml, path, 0, "y", nullptr, 0.0f);
		outLayout.width = ReadLayoutAttrib(uiXml, path, 0, "width", nullptr, defaultW);
		outLayout.height = ReadLayoutAttrib(uiXml, path, 0, "height", nullptr, defaultH);
	}

	SCompassLayoutScalar ReadActiveOffsetY(CUIXml& uiXml, const char* targetPath)
	{
		if (HasAttrib(uiXml, targetPath, 0, "offset_y_px"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(targetPath, 0, "offset_y_px", 0.0f), true);
		}
		if (HasAttrib(uiXml, targetPath, 0, "offset_y"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(targetPath, 0, "offset_y", 0.0f), false);
		}
		if (HasAttrib(uiXml, targetPath, 0, "active_offset_y"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(targetPath, 0, "active_offset_y", 0.0f), true);
		}
		if (HasAttrib(uiXml, targetPath, 0, "y"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(targetPath, 0, "y", 0.0f), false);
		}
		return SCompassLayoutScalar();
	}

	SCompassLayoutScalar ReadActivePadding(CUIXml& uiXml, const char* targetPath, float defaultPadding)
	{
		if (HasAttrib(uiXml, targetPath, 0, "padding_px"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(targetPath, 0, "padding_px", defaultPadding), true);
		}
		if (HasAttrib(uiXml, targetPath, 0, "padding"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(targetPath, 0, "padding", defaultPadding), false);
		}
		if (HasAttrib(uiXml, targetPath, 0, "active_target_padding"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(targetPath, 0, "active_target_padding", defaultPadding), true);
		}
		return MakeLayoutScalar(defaultPadding, true);
	}

	float ReadActiveSmoothing(CUIXml& uiXml, const char* targetPath, float defaultSmoothing)
	{
		return ReadPreferFlt(uiXml, targetPath, 0, "smoothing", "smoothing_speed", defaultSmoothing, nullptr);
	}

	void ReadStripTextureDrawUnits(
		CUIXml& uiXml,
		const char* texPath,
		float& outScaleX,
		float& outScaleY,
		SCompassLayoutScalar& outOffsetX,
		SCompassLayoutScalar& outOffsetY,
		bool& outStretch)
	{
		if (HasAttrib(uiXml, texPath, 0, "width"))
		{
			outScaleX = uiXml.ReadAttribFlt(texPath, 0, "width", 1.0f);
		}
		else if (HasAttrib(uiXml, texPath, 0, "scale_x"))
		{
			outScaleX = uiXml.ReadAttribFlt(texPath, 0, "scale_x", 1.0f);
		}
		else if (HasAttrib(uiXml, texPath, 0, "scale"))
		{
			outScaleX = uiXml.ReadAttribFlt(texPath, 0, "scale", 1.0f);
		}
		else if (HasAttrib(uiXml, texPath, 0, "draw_scale_x"))
		{
			outScaleX = uiXml.ReadAttribFlt(texPath, 0, "draw_scale_x", 1.0f);
		}
		else if (HasAttrib(uiXml, texPath, 0, "draw_scale"))
		{
			outScaleX = uiXml.ReadAttribFlt(texPath, 0, "draw_scale", 1.0f);
		}
		else
		{
			outScaleX = 1.0f;
		}

		if (HasAttrib(uiXml, texPath, 0, "height"))
		{
			outScaleY = uiXml.ReadAttribFlt(texPath, 0, "height", 1.0f);
		}
		else if (HasAttrib(uiXml, texPath, 0, "scale_y"))
		{
			outScaleY = uiXml.ReadAttribFlt(texPath, 0, "scale_y", 1.0f);
		}
		else if (HasAttrib(uiXml, texPath, 0, "scale"))
		{
			outScaleY = uiXml.ReadAttribFlt(texPath, 0, "scale", 1.0f);
		}
		else if (HasAttrib(uiXml, texPath, 0, "draw_scale_y"))
		{
			outScaleY = uiXml.ReadAttribFlt(texPath, 0, "draw_scale_y", 1.0f);
		}
		else if (HasAttrib(uiXml, texPath, 0, "draw_scale"))
		{
			outScaleY = uiXml.ReadAttribFlt(texPath, 0, "draw_scale", 1.0f);
		}
		else
		{
			outScaleY = 1.0f;
		}

		if (HasAttrib(uiXml, texPath, 0, "offset_x_px"))
		{
			outOffsetX = MakeLayoutScalar(uiXml.ReadAttribFlt(texPath, 0, "offset_x_px", 0.0f), true);
		}
		else if (HasAttrib(uiXml, texPath, 0, "draw_offset_x"))
		{
			outOffsetX = MakeLayoutScalar(uiXml.ReadAttribFlt(texPath, 0, "draw_offset_x", 0.0f), true);
		}
		else if (HasAttrib(uiXml, texPath, 0, "offset_x"))
		{
			outOffsetX = MakeLayoutScalar(uiXml.ReadAttribFlt(texPath, 0, "offset_x", 0.0f), false);
		}
		else if (HasAttrib(uiXml, texPath, 0, "x"))
		{
			outOffsetX = MakeLayoutScalar(uiXml.ReadAttribFlt(texPath, 0, "x", 0.0f), false);
		}
		else
		{
			outOffsetX = SCompassLayoutScalar();
		}

		if (HasAttrib(uiXml, texPath, 0, "offset_y_px"))
		{
			outOffsetY = MakeLayoutScalar(uiXml.ReadAttribFlt(texPath, 0, "offset_y_px", 0.0f), true);
		}
		else if (HasAttrib(uiXml, texPath, 0, "draw_offset_y"))
		{
			outOffsetY = MakeLayoutScalar(uiXml.ReadAttribFlt(texPath, 0, "draw_offset_y", 0.0f), true);
		}
		else if (HasAttrib(uiXml, texPath, 0, "offset_y"))
		{
			outOffsetY = MakeLayoutScalar(uiXml.ReadAttribFlt(texPath, 0, "offset_y", 0.0f), false);
		}
		else if (HasAttrib(uiXml, texPath, 0, "y"))
		{
			outOffsetY = MakeLayoutScalar(uiXml.ReadAttribFlt(texPath, 0, "y", 0.0f), false);
		}
		else
		{
			outOffsetY = SCompassLayoutScalar();
		}

		outStretch = uiXml.ReadAttribInt(texPath, 0, "stretch", 1) != 0;
	}

	float ReadCircumferencePx(CUIXml& uiXml, const char* stripPath, float defaultValue)
	{
		return ReadPreferFlt(uiXml, stripPath, 0, "circumference_px", "tex_width", defaultValue, nullptr);
	}

	bool ReadStripLoop(CUIXml& uiXml, const char* stripPath, bool defaultValue)
	{
		return ReadPreferInt(uiXml, stripPath, 0, "loop", "tex_loop", defaultValue ? 1 : 0, nullptr) != 0;
	}

	float ReadHeadingBiasRad(CUIXml& uiXml, const char* stripPath)
	{
		if (!stripPath || !*stripPath)
		{
			return 0.0f;
		}

		float biasDeg = 0.0f;
		if (HasAttrib(uiXml, stripPath, 0, "heading_bias_deg"))
		{
			biasDeg = uiXml.ReadAttribFlt(stripPath, 0, "heading_bias_deg", 0.0f);
		}
		else if (HasAttrib(uiXml, stripPath, 0, "phase_deg"))
		{
			biasDeg = uiXml.ReadAttribFlt(stripPath, 0, "phase_deg", 0.0f);
		}
		else if (HasAttrib(uiXml, stripPath, 0, "heading_bias"))
		{
			biasDeg = uiXml.ReadAttribFlt(stripPath, 0, "heading_bias", 0.0f);
		}
		return deg2rad(biasDeg);
	}

	const char* ResolveDialDrawPath(CUIXml& uiXml, const char* stripPath, string_path& outBuf)
	{
		if (stripPath && *stripPath)
		{
			xr_strconcat(outBuf, stripPath, ":draw:texture");
			if (uiXml.NavigateToNode(outBuf, 0))
			{
				return outBuf;
			}

			xr_strconcat(outBuf, stripPath, ":draw");
			if (uiXml.NavigateToNode(outBuf, 0))
			{
				return outBuf;
			}

			xr_strconcat(outBuf, stripPath, ":texture");
			if (uiXml.NavigateToNode(outBuf, 0))
			{
				return outBuf;
			}
		}
		return stripPath ? stripPath : "";
	}

	void ReadMarkerSize(CUIXml& uiXml, const char* path, int index, SCompassLayoutScalar& outWidth, SCompassLayoutScalar& outHeight)
	{
		if (ReadSizePair(uiXml, path, index, outWidth, outHeight))
		{
			return;
		}
		if (HasAttrib(uiXml, path, index, "width") || HasAttrib(uiXml, path, index, "height"))
		{
			outWidth = ReadLayoutAttrib(uiXml, path, index, "width", nullptr, outWidth.value);
			outHeight = ReadLayoutAttrib(uiXml, path, index, "height", nullptr, outHeight.value);
		}
	}

	SCompassLayoutScalar ReadMarkerOffsetY(CUIXml& uiXml, const char* path, int index, const SCompassLayoutScalar& defaultValue)
	{
		if (HasAttrib(uiXml, path, index, "offset_y_px"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(path, index, "offset_y_px", defaultValue.value), true);
		}
		if (HasAttrib(uiXml, path, index, "offset_y"))
		{
			return MakeLayoutScalar(uiXml.ReadAttribFlt(path, index, "offset_y", defaultValue.value), false);
		}
		return defaultValue;
	}

	bool TryGetCardinalAngleRad(LPCSTR directionNode, float& outAngleRad)
	{
		if (!directionNode || !*directionNode)
		{
			return false;
		}

		if (!xr_strcmp(directionNode, "n"))
		{
			outAngleRad = deg2rad(90.f);
			return true;
		}
		if (!xr_strcmp(directionNode, "e"))
		{
			outAngleRad = deg2rad(0.f);
			return true;
		}
		if (!xr_strcmp(directionNode, "s"))
		{
			outAngleRad = deg2rad(-90.f);
			return true;
		}
		if (!xr_strcmp(directionNode, "w"))
		{
			outAngleRad = deg2rad(180.f);
			return true;
		}
		if (!xr_strcmp(directionNode, "ne"))
		{
			outAngleRad = deg2rad(45.f);
			return true;
		}
		if (!xr_strcmp(directionNode, "se"))
		{
			outAngleRad = deg2rad(-45.f);
			return true;
		}
		if (!xr_strcmp(directionNode, "sw"))
		{
			outAngleRad = deg2rad(-135.f);
			return true;
		}
		if (!xr_strcmp(directionNode, "nw"))
		{
			outAngleRad = deg2rad(135.f);
			return true;
		}
		return false;
	}

	shared_str SanitizeTextureName(const char* rawName)
	{
		if (!rawName || !*rawName)
		{
			return shared_str();
		}

		string256 buf;
		xr_strcpy(buf, rawName);
		if (char* cut = strchr(buf, '<'))
		{
			*cut = 0;
			WarnLegacyOnce("texture_sanitize", "CompassBarXml: sanitized texture name with nested markup");
		}

		u32 len = xr_strlen(buf);
		while (len > 0 && (buf[len - 1] == ' ' || buf[len - 1] == '\t' || buf[len - 1] == '\n' || buf[len - 1] == '\r'))
		{
			buf[--len] = 0;
		}
		u32 start = 0;
		while (buf[start] == ' ' || buf[start] == '\t' || buf[start] == '\n' || buf[start] == '\r')
		{
			++start;
		}
		return shared_str(buf + start);
	}

	shared_str ReadTextureName(
		CUIXml& uiXml,
		const char* nodePath,
		int index,
		const char* defaultName)
	{
		const char* attribTex = uiXml.ReadAttrib(nodePath, index, "texture", nullptr);
		if (attribTex && *attribTex)
		{
			return SanitizeTextureName(attribTex);
		}

		string_path texPath;
		xr_strconcat(texPath, nodePath, ":texture");
		if (uiXml.NavigateToNode(texPath, index))
		{
			shared_str childTex = uiXml.Read(texPath, index, nullptr);
			shared_str sanitized = SanitizeTextureName(childTex.c_str());
			if (sanitized.size())
			{
				return sanitized;
			}
		}

		if (uiXml.NavigateToNode(nodePath, index))
		{
			shared_str selfTex = uiXml.Read(nodePath, index, nullptr);
			shared_str sanitized = SanitizeTextureName(selfTex.c_str());
			if (sanitized.size())
			{
				return sanitized;
			}
		}

		return SanitizeTextureName(defaultName);
	}

	bool ReadStyleSheet(CUIXml& uiXml, SStyleSheet& outStyle)
	{
		outStyle = SStyleSheet();
		const char* stylePath = nullptr;
		if (uiXml.NavigateToNode("compass_bar:style_sheet", 0))
		{
			stylePath = "compass_bar:style_sheet";
		}
		else if (uiXml.NavigateToNode("style_sheet", 0))
		{
			stylePath = "style_sheet";
		}
		else
		{
			return false;
		}

		CUIXmlInit::ReadShadowsNode(uiXml, stylePath, 0, outStyle.shadow);
		if (!outStyle.shadow.enabled)
		{
			string_path shadowPath;
			xr_strconcat(shadowPath, stylePath, ":shadow");
			if (uiXml.NavigateToNode(shadowPath, 0))
			{
				SUITextureShadowParams shadow;
				shadow.thickness = uiXml.ReadAttribFlt(shadowPath, 0, "thickness", 1.0f);
				const int r = uiXml.ReadAttribInt(shadowPath, 0, "r", 0);
				const int g = uiXml.ReadAttribInt(shadowPath, 0, "g", 0);
				const int b = uiXml.ReadAttribInt(shadowPath, 0, "b", 0);
				const int a = uiXml.ReadAttribInt(shadowPath, 0, "a", 160);
				shadow.color = color_argb(a, r, g, b);
				shadow.enabled = (shadow.thickness > 0.0f) && (color_get_A(shadow.color) > 0);
				outStyle.shadow = shadow;
			}
		}

		outStyle.hasShadow = outStyle.shadow.enabled;
		return outStyle.hasShadow;
	}

	const char* ResolveStripPath(CUIXml& uiXml)
	{
		if (uiXml.NavigateToNode("compass_bar:dial", 0))
		{
			return "compass_bar:dial";
		}
		if (uiXml.NavigateToNode("compass_bar:compass_dial", 0))
		{
			return "compass_bar:compass_dial:strip";
		}
		return "compass_bar:strip";
	}

	const char* ResolveCardinalsPath(CUIXml& uiXml, const char* stripPath, string_path& outBuf)
	{
		if (uiXml.NavigateToNode("compass_bar:cardinals", 0))
		{
			return "compass_bar:cardinals";
		}

		xr_strconcat(outBuf, stripPath, ":cardinal_points");
		if (uiXml.NavigateToNode(outBuf, 0))
		{
			return outBuf;
		}
		if (uiXml.NavigateToNode("compass_bar:cardinal_points", 0))
		{
			return "compass_bar:cardinal_points";
		}
		return outBuf;
	}

	namespace
	{
		bool ParseBoolToken(const char* value, bool defaultValue)
		{
			if (!value || !*value)
			{
				return defaultValue;
			}
			if (!_stricmp(value, "true") || !_stricmp(value, "on") || !_stricmp(value, "yes"))
			{
				return true;
			}
			if (!_stricmp(value, "false") || !_stricmp(value, "off") || !_stricmp(value, "no"))
			{
				return false;
			}
			return atoi(value) != 0;
		}

		bool TryReadBoolAttrib(CUIXml& uiXml, const char* path, const char* attrib, bool& outValue)
		{
			if (!HasAttrib(uiXml, path, 0, attrib))
			{
				return false;
			}
			outValue = ParseBoolToken(uiXml.ReadAttrib(path, 0, attrib, nullptr), outValue);
			return true;
		}

		bool TryReadIniBool(LPCSTR section, LPCSTR key, bool& outValue)
		{
			if (!pSettings || !pSettings->section_exist(section) || !pSettings->line_exist(section, key))
			{
				return false;
			}
			outValue = ParseBoolToken(pSettings->r_string(section, key), outValue);
			return true;
		}





	}

	bool ReadLabelSettings(CUIXml& uiXml, const char* cardinalsPath, SCompassLabelSettings& outSettings)
	{
		outSettings = SCompassLabelSettings();
		bool configured = false;

		if (pSettings && pSettings->section_exist("compass"))
		{
			configured |= TryReadIniBool("compass", "show_cardinal", outSettings.showCardinal);
			configured |= TryReadIniBool("compass", "show_degrees", outSettings.showDegrees);
			configured |= TryReadIniBool("compass", "show_intermediate_cardinal", outSettings.showIntermediateCardinal);
			if (pSettings->line_exist("compass", "degree_step"))
			{
				outSettings.degreeStep = CompassLabels::SanitizeDegreeStep(pSettings->r_u32("compass", "degree_step"));
				configured = true;
			}
		}

		const char* paths[2] = { "compass_bar", cardinalsPath };
		for (const char* path : paths)
		{
			if (!path || !*path || !uiXml.NavigateToNode(path, 0))
			{
				continue;
			}

			configured |= TryReadBoolAttrib(uiXml, path, "show_cardinal", outSettings.showCardinal);
			configured |= TryReadBoolAttrib(uiXml, path, "show_degrees", outSettings.showDegrees);
			configured |= TryReadBoolAttrib(uiXml, path, "show_intermediate_cardinal", outSettings.showIntermediateCardinal);
			if (HasAttrib(uiXml, path, 0, "degree_step"))
			{
				outSettings.degreeStep = CompassLabels::SanitizeDegreeStep(
					u32(uiXml.ReadAttribInt(path, 0, "degree_step", int(outSettings.degreeStep))));
				configured = true;
			}
		}

		if (cardinalsPath && *cardinalsPath && uiXml.NavigateToNode(cardinalsPath, 0))
		{
			string_path groupPath;

			xr_strconcat(groupPath, cardinalsPath, ":main");
			if (uiXml.NavigateToNode(groupPath, 0))
			{
				configured = true;
				outSettings.showCardinal = true;
				TryReadBoolAttrib(uiXml, groupPath, "show", outSettings.showCardinal);
			}

			xr_strconcat(groupPath, cardinalsPath, ":intermediate");
			if (uiXml.NavigateToNode(groupPath, 0))
			{
				configured = true;
				outSettings.showIntermediateCardinal = true;
				TryReadBoolAttrib(uiXml, groupPath, "show", outSettings.showIntermediateCardinal);
			}

			xr_strconcat(groupPath, cardinalsPath, ":degrees");
			if (uiXml.NavigateToNode(groupPath, 0))
			{
				configured = true;
				outSettings.showDegrees = true;
				TryReadBoolAttrib(uiXml, groupPath, "show", outSettings.showDegrees);
				if (HasAttrib(uiXml, groupPath, 0, "step"))
				{
					outSettings.degreeStep = CompassLabels::SanitizeDegreeStep(
						u32(uiXml.ReadAttribInt(groupPath, 0, "step", int(outSettings.degreeStep))));
				}
				else if (HasAttrib(uiXml, groupPath, 0, "degree_step"))
				{
					outSettings.degreeStep = CompassLabels::SanitizeDegreeStep(
						u32(uiXml.ReadAttribInt(groupPath, 0, "degree_step", int(outSettings.degreeStep))));
				}
			}
		}

		outSettings.degreeStep = CompassLabels::SanitizeDegreeStep(outSettings.degreeStep);
		return configured;
	}


	bool ReadNodeShow(CUIXml& uiXml, const char* path, bool defaultWhenMissingAttrib)
	{
		if (!path || !*path || !uiXml.NavigateToNode(path, 0))
		{
			return false;
		}
		if (!HasAttrib(uiXml, path, 0, "show"))
		{
			return defaultWhenMissingAttrib;
		}
		return ParseBoolToken(uiXml.ReadAttrib(path, 0, "show", nullptr), defaultWhenMissingAttrib);
	}

	const char* ResolveWidgetDrawPath(CUIXml& uiXml, const char* widgetPath, string_path& outBuf)
	{
		if (!widgetPath || !*widgetPath)
		{
			return "";
		}
		xr_strconcat(outBuf, widgetPath, ":draw");
		if (uiXml.NavigateToNode(outBuf, 0))
		{
			return outBuf;
		}
		return widgetPath;
	}

	const char* ResolveWidgetTexturePath(CUIXml& uiXml, const char* widgetPath, string_path& outBuf)
	{
		if (!widgetPath || !*widgetPath)
		{
			return "";
		}

		xr_strconcat(outBuf, widgetPath, ":draw:texture");
		if (uiXml.NavigateToNode(outBuf, 0))
		{
			return outBuf;
		}
		xr_strconcat(outBuf, widgetPath, ":texture");
		if (uiXml.NavigateToNode(outBuf, 0))
		{
			return outBuf;
		}
		xr_strconcat(outBuf, widgetPath, ":draw");
		if (uiXml.NavigateToNode(outBuf, 0))
		{
			return outBuf;
		}
		return widgetPath;
	}

	void WarnLegacyOnce(const char* featureKey, const char* message)
	{
		if (!featureKey || !*featureKey || !message)
		{
			return;
		}
		shared_str key = featureKey;
		if (s_warnedFeatures.find(key) != s_warnedFeatures.end())
		{
			return;
		}
		s_warnedFeatures[key] = true;
		Msg("! %s", message);
	}
}
