#pragma once

#include "UINavigationHudTypes.h"

class CUIMainIngameWnd;

class CUINavigationHudController
{
public:
	explicit CUINavigationHudController(CUIMainIngameWnd& host);

	void SetNavigationMode(ENavigationHudMode mode);
	void SetNavigationModeBool(bool compassBar);
	bool IsCompassBarMode() const;
	ENavigationHudState State() const { return _state; }
	ENavigationHudMode Target() const { return _target; }

	bool ValidateNavigationOwnership(shared_str& outError) const;
	bool RunNavigationOwnershipSmoke(u32 toggleCount = 20);

	bool EnsureCompassBar();
	bool IsCompassBarInitialized() const;
	bool IsCompassBarActive() const;

	void SyncNavigationVisibility();
	void UpdateNavigationHud();
	void DrawNavigationHud();
	void RebindNavigationChildren();

	void PersistNavigationMode(ENavigationHudMode mode);
	void SettleNavigationState(ENavigationHudState state, ENavigationHudMode mode);
	ENavigationHudMode NavigationModeFromState() const;
	bool HasPersistedNavigationMode() const { return s_hasPersistedNavigationMode; }
	ENavigationHudMode PersistedNavigationMode() const { return s_persistedNavigationMode; }

	Frect GetNavigationHostRect() const;

private:
	CUIMainIngameWnd& _host;
	ENavigationHudState _state = ENavigationHudState::Minimap;
	ENavigationHudMode _target = ENavigationHudMode::Minimap;

	static bool s_hasPersistedNavigationMode;
	static ENavigationHudMode s_persistedNavigationMode;
};
