#pragma once

#include "../../xrUI/Widgets/UIWindow.h"

class CUIStatic;
class CUIXml;

class CUICompassHazardState final : public CUIWindow
{
	using inherited = CUIWindow;

public:
	enum class EState : u8
	{
		Safe = 0,
		Anomaly,
		Radiation,
		Emission,
		Count
	};

	void InitFromXml(CUIXml& Xml, const char* Path);
	void Update() override;

	EState CurrentState() const
	{
		return State;
	}

private:
	struct SStateStyle
	{
		Fvector Color = {};
		float Level = 0.f;
	};

	EState EvaluateState();
	bool IsEmissionActive();
	void ApplyLayout();

	CUIStatic* Glow = nullptr;
	SStateStyle Styles[u32(EState::Count)];
	EState State = EState::Safe;
	Fvector CurrentColor = {};
	float CurrentLevel = 0.f;
	Fvector2 RelPos = {};
	Fvector2 RelSize = {};
	shared_str EmissionFunction;
	float ScanRadius = 15.f;
	float RadiationThreshold = 0.25f;
	float BlendSpeed = 4.f;
	float EmissionPollInterval = 0.5f;
	float EmissionPollTimer = 0.f;
	bool EmissionCached = false;
};
