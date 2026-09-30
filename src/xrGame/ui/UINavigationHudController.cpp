#include "StdAfx.h"

#include "UINavigationHudController.h"
#include "UIMainIngameWnd.h"
#include "UICompassBar.h"
#include "UIMotionIcon.h"
#include "UIZoneMap.h"
#include "UINavigationOwnership.h"
#include "../../xrCore/EngineExternal.h"
#include "../../xrEngine/CustomHUD.h"
#include "../Actor.h"
#include "../ActorHelmet.h"
#include "../Level.h"
#include "../map_manager.h"
#include "../game_cl_base.h"
#include "../game_cl_single.h"

bool CUINavigationHudController::s_hasPersistedNavigationMode = false;
ENavigationHudMode CUINavigationHudController::s_persistedNavigationMode = ENavigationHudMode::Minimap;

CUINavigationHudController::CUINavigationHudController(CUIMainIngameWnd& host)
	: _host(host)
{
}

bool CUINavigationHudController::IsCompassBarMode() const
{
	return _state == ENavigationHudState::Compass;
}

void CUINavigationHudController::SetNavigationModeBool(bool compassBar)
{
	SetNavigationMode(compassBar ? ENavigationHudMode::CompassBar : ENavigationHudMode::Minimap);
}

bool CUINavigationHudController::EnsureCompassBar()
{
	if (_host.UICompassBar && _host.UICompassBar->IsInitialized())
	{
		return true;
	}

	if (!_host.UICompassBar)
	{
		_host.UICompassBar = new CUICompassBar();
	}

	_host.UICompassBar->Init();
	return _host.UICompassBar->IsInitialized();
}

bool CUINavigationHudController::IsCompassBarInitialized() const
{
	return _host.UICompassBar && _host.UICompassBar->IsInitialized();
}

bool CUINavigationHudController::IsCompassBarActive() const
{
	return IsCompassBarMode() && IsCompassBarInitialized();
}

Frect CUINavigationHudController::GetNavigationHostRect() const
{
	if (IsCompassBarActive())
	{
		return _host.UICompassBar->GetFrame()->GetWndRect();
	}
	if (_host.UIZoneMap)
	{
		return _host.UIZoneMap->MapFrame().GetWndRect();
	}
	return Frect();
}

void CUINavigationHudController::RebindNavigationChildren()
{
	if (!_host.UIMotionIcon)
	{
		return;
	}

	const bool compass = (_target == ENavigationHudMode::CompassBar) &&
		(_state == ENavigationHudState::Compass ||
			_state == ENavigationHudState::Transitioning) &&
		IsCompassBarInitialized();

	if (compass)
	{
		if (!_host.UIMotionIcon->CompassLayoutFrame())
		{
			_host.UIMotionIcon->SetNavigationPresentation(true);
		}

		CUIWindow* layoutFrame = _host.UIMotionIcon->CompassLayoutFrame();
		if (layoutFrame)
		{
			UINavigationOwnership::ReparentOwned(_host.UICompassBar, layoutFrame);
			UINavigationOwnership::ReparentOwned(layoutFrame, _host.UIMotionIcon);
			_host.UIMotionIcon->ApplyCompassLayout(_host.UICompassBar);
		}
		else if (!_host.UIMotionIcon->IsIndependent())
		{
			_host.UIMotionIcon->ApplyNavigationHost(_host.UICompassBar, GetNavigationHostRect(), true);
		}
	}
	else
	{
		_host.UIMotionIcon->SetNavigationPresentation(false);
		if (_host.UIMotionIcon->IsIndependent())
		{
			UINavigationOwnership::ReparentOwned(&_host, _host.UIMotionIcon);
		}
		else if (_host.UIZoneMap)
		{
			_host.UIMotionIcon->ApplyNavigationHost(&_host.UIZoneMap->MapFrame(), GetNavigationHostRect(), false);
		}
	}

	if (_host.UIPdaOnline)
	{
		if (CUIWindow* parent = _host.UIPdaOnline->GetParent())
		{
			parent->DetachChild(_host.UIPdaOnline);
		}

		if (compass)
		{
			_host.UICompassBar->Background().AttachChild(_host.UIPdaOnline);
		}
		else if (_host.UIZoneMap)
		{
			_host.UIZoneMap->Background().AttachChild(_host.UIPdaOnline);
		}
	}
}

void CUINavigationHudController::PersistNavigationMode(ENavigationHudMode mode)
{
	s_hasPersistedNavigationMode = true;
	s_persistedNavigationMode = mode;
}

void CUINavigationHudController::SettleNavigationState(ENavigationHudState state, ENavigationHudMode mode)
{
	_state = state;
	_target = mode;
}

ENavigationHudMode CUINavigationHudController::NavigationModeFromState() const
{
	if (_state == ENavigationHudState::Compass)
	{
		return ENavigationHudMode::CompassBar;
	}
	return ENavigationHudMode::Minimap;
}

void CUINavigationHudController::SetNavigationMode(ENavigationHudMode mode)
{
	if (_state == ENavigationHudState::Transitioning)
	{
		return;
	}

	if (_state != ENavigationHudState::FailedInit &&
		NavigationModeFromState() == mode &&
		_target == mode)
	{
		PersistNavigationMode(mode);
		return;
	}

	if (!_host.UIMotionIcon || !_host.UIZoneMap)
	{
		return;
	}

	SettleNavigationState(ENavigationHudState::Transitioning, mode);

	if (mode == ENavigationHudMode::CompassBar && !EnsureCompassBar())
	{
		Msg("! CUIMainIngameWnd::SetNavigationMode: compass bar init failed, staying on minimap");
		SettleNavigationState(ENavigationHudState::FailedInit, ENavigationHudMode::Minimap);
		PersistNavigationMode(ENavigationHudMode::Minimap);
		SyncNavigationVisibility();
		RebindNavigationChildren();
		return;
	}

	SettleNavigationState(
		mode == ENavigationHudMode::CompassBar
			? ENavigationHudState::Compass
			: ENavigationHudState::Minimap,
		mode);
	PersistNavigationMode(mode);
	SyncNavigationVisibility();

	if (IsCompassBarActive())
	{
		if (!_host.IsChild(_host.UICompassBar))
		{
			_host.AttachChild(_host.UICompassBar);
		}
		_host.UICompassBar->Reset();
	}
	else
	{
		if (_host.UICompassBar && _host.IsChild(_host.UICompassBar))
		{
			_host.DetachChild(_host.UICompassBar);
		}
		if (_host.UICompassBar)
		{
			_host.UICompassBar->SetHudVisible(false);
		}
		if (_host.UIZoneMap)
		{
			_host.UIZoneMap->SetupCurrentMap();
		}
	}

	RebindNavigationChildren();

	if (_host.UIMotionIcon)
	{
		_host.UIMotionIcon->ResetVisibility();
	}
}

bool CUINavigationHudController::ValidateNavigationOwnership(shared_str& outError) const
{
	outError = nullptr;

	if (_state == ENavigationHudState::Transitioning)
	{
		outError = "navigation state stuck in Transitioning";
		return false;
	}

	if (!_host.UIMotionIcon)
	{
		outError = "UIMotionIcon is null";
		return false;
	}

	if (_host.UIMotionIcon->IsIndependent())
	{
		if (_host.UIMotionIcon->GetParent() != &_host)
		{
			outError = "independent motion icon parent is not main ingame wnd";
			return false;
		}
		if (!_host.UIMotionIcon->IsAutoDelete())
		{
			outError = "independent motion icon is not owned (AutoDelete=false)";
			return false;
		}
		return true;
	}

	if (_state == ENavigationHudState::Compass)
	{
		if (!IsCompassBarInitialized())
		{
			outError = "Compass state without initialized compass bar";
			return false;
		}

		CUIWindow* layoutFrame = _host.UIMotionIcon->CompassLayoutFrame();
		if (layoutFrame)
		{
			if (!UINavigationOwnership::IsOwnedChild(_host.UICompassBar, layoutFrame))
			{
				outError = "layoutFrame is not owned by compass host";
				return false;
			}
			if (!UINavigationOwnership::IsOwnedChild(layoutFrame, _host.UIMotionIcon))
			{
				outError = "motion icon is not owned by layoutFrame";
				return false;
			}
		}
		else if (!UINavigationOwnership::IsOwnedChild(_host.UICompassBar, _host.UIMotionIcon))
		{
			outError = "motion icon is not owned by compass host";
			return false;
		}
		return true;
	}

	if (!_host.UIZoneMap)
	{
		outError = "UIZoneMap is null in minimap path";
		return false;
	}

	if (!UINavigationOwnership::IsOwnedChild(&_host.UIZoneMap->MapFrame(), _host.UIMotionIcon))
	{
		outError = "motion icon is not owned by zone map frame";
		return false;
	}

	return true;
}

bool CUINavigationHudController::RunNavigationOwnershipSmoke(u32 toggleCount)
{
	shared_str error;
	if (!ValidateNavigationOwnership(error))
	{
		Msg("! nav ownership smoke [init]: %s", error.c_str());
		return false;
	}

	const ENavigationHudMode startMode = NavigationModeFromState();
	const u32 cycles = toggleCount ? toggleCount : 20;

	for (u32 i = 0; i < cycles; ++i)
	{
		const ENavigationHudMode next =
			IsCompassBarMode() ? ENavigationHudMode::Minimap : ENavigationHudMode::CompassBar;
		SetNavigationMode(next);

		if (_state == ENavigationHudState::Transitioning)
		{
			Msg("! nav ownership smoke [toggle %u]: stuck Transitioning", i);
			return false;
		}

		if (_state == ENavigationHudState::FailedInit)
		{
			Msg("! nav ownership smoke [toggle %u]: FailedInit", i);
			return false;
		}

		if (!ValidateNavigationOwnership(error))
		{
			Msg("! nav ownership smoke [toggle %u]: %s", i, error.c_str());
			return false;
		}
	}

	const ENavigationHudMode savedMode = NavigationModeFromState();
	const ENavigationHudMode flipped =
		savedMode == ENavigationHudMode::CompassBar
			? ENavigationHudMode::Minimap
			: ENavigationHudMode::CompassBar;

	SetNavigationMode(flipped);
	if (!ValidateNavigationOwnership(error))
	{
		Msg("! nav ownership smoke [pre-restore]: %s", error.c_str());
		return false;
	}

	PersistNavigationMode(savedMode);
	SetNavigationMode(s_persistedNavigationMode);
	if (!ValidateNavigationOwnership(error))
	{
		Msg("! nav ownership smoke [save-load restore]: %s", error.c_str());
		return false;
	}

	if (NavigationModeFromState() != savedMode)
	{
		Msg("! nav ownership smoke [save-load restore]: mode mismatch");
		return false;
	}

	SetNavigationMode(flipped);
	if (!ValidateNavigationOwnership(error))
	{
		Msg("! nav ownership smoke [post-restore toggle]: %s", error.c_str());
		return false;
	}

	SetNavigationMode(startMode);
	if (!ValidateNavigationOwnership(error))
	{
		Msg("! nav ownership smoke [restore start]: %s", error.c_str());
		return false;
	}

	Msg("* nav ownership smoke: OK (toggles=%u, state=%u)", cycles, (u32)_state);
	return true;
}

void CUINavigationHudController::SyncNavigationVisibility()
{
	const bool showNav = psHUD_Flags.test(HUD_MINIMAP);
	const static bool noHUDonMaster = EngineExternal()[EEngineExternalUI::DisableHudRenderingOnMaster];
	CActor* pActor = Level().CurrentViewEntity() ? Level().CurrentViewEntity()->cast_actor() : nullptr;
	const bool renderHUD = noHUDonMaster
		? (g_SingleGameDifficulty < egdVeteran ||
			(pActor && pActor->GetHelmet() && !fis_zero(pActor->GetHelmet()->m_fShowNearestEnemiesDistance)))
		: true;
	const bool navVisible = showNav && renderHUD;

	if (IsCompassBarActive())
	{
		_host.UICompassBar->SetHudVisible(navVisible);
		if (_host.UIZoneMap)
		{
			_host.UIZoneMap->visible = false;
		}
	}
	else if (_host.UIZoneMap)
	{
		if (noHUDonMaster)
		{
			_host.UIZoneMap->disabled = !renderHUD;
		}
		_host.UIZoneMap->visible = navVisible;
	}
}

void CUINavigationHudController::UpdateNavigationHud()
{
	if (!psHUD_Flags.test(HUD_MINIMAP))
	{
		return;
	}

	if (IsCompassBarActive())
	{
		_host.UICompassBar->SetActiveTarget(Level().MapManager().GetActiveTaskCompassLocation());
		_host.UICompassBar->Update();
	}
	else if (_host.UIZoneMap)
	{
		_host.UIZoneMap->Update();
	}
}

void CUINavigationHudController::DrawNavigationHud()
{
	if (!psHUD_Flags.test(HUD_MINIMAP))
	{
		return;
	}

	SyncNavigationVisibility();

	if (!IsCompassBarActive() && _host.UIZoneMap && _host.UIZoneMap->visible)
	{
		_host.UIZoneMap->Render();
	}
}
