#include "StdAfx.h"
#include "UICompassLabelGenerator.h"

namespace CompassLabelGenerator
{
	namespace
	{
		bool IsMainCardinalNavDeg(float navAngleDeg)
		{
			const float a = CompassLabels::NormalizeNavDeg(navAngleDeg);
			return fis_zero(a) || fis_zero(a - 90.0f) || fis_zero(a - 180.0f) || fis_zero(a - 270.0f);
		}

		void AddMark(
			xr_vector<SCompassLabelDesc>& outMarks,
			float navAngleDeg,
			ECompassLabelKind kind,
			const char* id,
			const char* label)
		{
			SCompassLabelDesc desc;
			desc.navAngleDeg = CompassLabels::NormalizeNavDeg(navAngleDeg);
			desc.angleRad = CompassLabels::NavDegToEngineRad(desc.navAngleDeg);
			desc.kind = kind;
			desc.id = id;
			desc.label = label;
			outMarks.push_back(desc);
		}
	}

	void Generate(const SCompassLabelSettings& settings, xr_vector<SCompassLabelDesc>& outMarks)
	{
		outMarks.clear();

		for (const SCompassCardinalDirection& direction : CompassLabels::kDirections)
		{
			if (direction.intermediate)
			{
				if (!settings.showIntermediateCardinal)
				{
					continue;
				}
			}
			else if (!settings.showCardinal)
			{
				continue;
			}

			AddMark(
				outMarks,
				direction.navAngleDeg,
				direction.intermediate ? ECompassLabelKind::Intermediate : ECompassLabelKind::Cardinal,
				direction.id,
				direction.label);
		}

		if (!settings.showDegrees)
		{
			return;
		}

		const u32 step = CompassLabels::SanitizeDegreeStep(settings.degreeStep);
		string32 buf;
		for (u32 deg = 0; deg < 360; deg += step)
		{
			if (settings.showCardinal && IsMainCardinalNavDeg(float(deg)))
			{
				continue;
			}

			xr_sprintf(buf, "%u°", deg);
			AddMark(outMarks, float(deg), ECompassLabelKind::Degree, nullptr, buf);
		}
	}
}
