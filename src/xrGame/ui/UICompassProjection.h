#pragma once

namespace CompassProjection
{
	inline float AngleDelta(float targetRad, float currentRad)
	{
		return angle_normalize_signed(targetRad - currentRad);
	}

	inline bool AngleToStripX(
		float angleRad,
		float camHeading,
		float fovRad,
		float stripWidth,
		float& outX,
		bool clampToEdges)
	{
		if (fovRad <= 0.0f || stripWidth <= 0.0f)
		{
			return false;
		}

		float delta = AngleDelta(angleRad, camHeading);
		const float halfFov = fovRad * 0.5f;
		if (clampToEdges)
		{
			delta = clampr(delta, -halfFov, halfFov);
		}
		else if (delta < -halfFov || delta > halfFov)
		{
			return false;
		}

		const float halfW = stripWidth * 0.5f;
		outX = -(delta / halfFov) * halfW;
		return true;
	}

	inline bool ProjectToStrip(
		float fovRad,
		float stripWidth,
		float minDistanceSq,
		const Fvector& targetPos,
		const Fvector& actorPos,
		float camHeading,
		float& outX,
		bool clampToEdges)
	{
		if (fovRad <= 0.0f || stripWidth <= 0.0f)
		{
			return false;
		}

		Fvector2 dir;
		dir.set(targetPos.x - actorPos.x, targetPos.z - actorPos.z);
		if (dir.square_magnitude() < minDistanceSq)
		{
			outX = 0.0f;
			return true;
		}

		return AngleToStripX(dir.getH(), camHeading, fovRad, stripWidth, outX, clampToEdges);
	}

	inline float CalculateFovEdgeFade(
		float relX,
		float stripWidth,
		float fadeEdgeLo,
		float fadeEdgeHi,
		float fadeInner,
		float fadeOuter)
	{
		if (stripWidth <= 0.0f)
		{
			return 1.0f;
		}

		const float normalizedX = (relX + stripWidth * 0.5f) / stripWidth;
		if (normalizedX <= fadeEdgeLo || normalizedX >= fadeEdgeHi)
		{
			return 0.0f;
		}
		if (normalizedX < fadeInner)
		{
			const float range = fadeInner - fadeEdgeLo;
			const float t = (range > 0.0f) ? (normalizedX - fadeEdgeLo) / range : 1.0f;
			return t * t;
		}
		if (normalizedX > fadeOuter)
		{
			const float range = fadeEdgeHi - fadeOuter;
			const float t = (range > 0.0f) ? (fadeEdgeHi - normalizedX) / range : 1.0f;
			return t * t;
		}
		return 1.0f;
	}

	inline float ComputeStripUAtlas(
		float heading,
		float atlasCircumference,
		float stripTexWidth,
		float defaultStripTexWidth,
		float widgetW,
		float kx,
		bool textureStretch,
		float baseTexRectWidth,
		bool texLoop,
		float halfCircleRad,
		float twoPiRad)
	{
		const float uvCenter = (heading + halfCircleRad) / twoPiRad;
		const float stripTexW = stripTexWidth > 0.0f ? stripTexWidth : defaultStripTexWidth;
		const float texToAtlas = (stripTexW > 0.0f) ? (atlasCircumference / stripTexW) : 1.0f;

		float winWAtlas = 0.0f;
		if (textureStretch)
		{
			winWAtlas = (kx > 0.0f) ? ((widgetW / kx) * texToAtlas) : 0.0f;
		}
		else
		{
			winWAtlas = baseTexRectWidth;
		}

		if (winWAtlas <= 0.0f || atlasCircumference <= 0.0f)
		{
			return -1.0e9f;
		}

		float uAtlas = uvCenter * atlasCircumference - winWAtlas * 0.5f;
		if (texLoop)
		{
			uAtlas = fmodf(uAtlas, atlasCircumference);
			if (uAtlas < 0.0f)
			{
				uAtlas += atlasCircumference;
			}
		}
		else
		{
			uAtlas = clampr(uAtlas, 0.0f, atlasCircumference - winWAtlas);
		}
		return uAtlas;
	}

	inline float StripWindowWidthAtlas(
		float atlasCircumference,
		float stripTexWidth,
		float defaultStripTexWidth,
		float widgetW,
		float kx,
		bool textureStretch,
		float baseTexRectWidth)
	{
		const float stripTexW = stripTexWidth > 0.0f ? stripTexWidth : defaultStripTexWidth;
		const float texToAtlas = (stripTexW > 0.0f) ? (atlasCircumference / stripTexW) : 1.0f;
		if (textureStretch)
		{
			return (kx > 0.0f) ? ((widgetW / kx) * texToAtlas) : 0.0f;
		}
		return baseTexRectWidth;
	}
}
