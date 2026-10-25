#include "stdafx.h"
#include "xrUITheme.h"

IEditorWnd::~IEditorWnd()
{
	if (StateName != nullptr)
	{
		CEditorWndStates::Get().Unregister(this);
	}
}

void IEditorWnd::ResetBegin()
{

}

void IEditorWnd::ResetEnd()
{

}

void IEditorWnd::Serialize(nlohmann::json& Data, bool IsSaving)
{
	if (IsSaving)
	{
		Data["open"] = bOpen;
	}
	else
	{
		ReadState(Data, "open", bOpen);
	}
}

void IEditorWnd::SetStateName(const char* Name)
{
	VERIFY(StateName == nullptr);
	StateName = Name;
	CEditorWndStates::Get().Register(this);
}

void IEditorWnd::BeginDraw() const
{
	if (!IsFocused && !IsDocked && !IsContextMenu)
	{
		ImGui::SetNextWindowBgAlpha(CUIThemeManager::Get().TransparentUnfocused);
	}
}

void IEditorWnd::EndDraw() const
{
	ImGui::SetNextWindowBgAlpha(CUIThemeManager::Get().TransparentDefault);
}

CEditorWndStates& CEditorWndStates::Get()
{
	static CEditorWndStates Instance;
	return Instance;
}

nlohmann::json& CEditorWndStates::GetEntry(const char* Name)
{
	nlohmann::json& Entry = Data[Name];
	if (Entry.is_boolean())
	{
		// Older configs kept only the open flag
		Entry = nlohmann::json{ { "open", Entry.get<bool>() } };
	}
	else if (!Entry.is_object())
	{
		Entry = nlohmann::json::object();
	}

	return Entry;
}

void CEditorWndStates::Load(const nlohmann::json& NewData)
{
	Data = NewData.is_object() ? NewData : nlohmann::json::object();

	for (IEditorWnd* Wnd : Windows)
	{
		Wnd->Serialize(GetEntry(Wnd->StateName), false);
	}
}

const nlohmann::json& CEditorWndStates::Save()
{
	for (IEditorWnd* Wnd : Windows)
	{
		Wnd->Serialize(GetEntry(Wnd->StateName), true);
	}

	return Data;
}

bool CEditorWndStates::IsOpen(const char* Name, bool Default) const
{
	auto It = Data.find(Name);
	if (It == Data.end())
	{
		return Default;
	}

	if (It->is_boolean())
	{
		return It->get<bool>();
	}

	if (It->is_object())
	{
		auto Open = It->find("open");
		if (Open != It->end() && Open->is_boolean())
		{
			return Open->get<bool>();
		}
	}

	return Default;
}

void CEditorWndStates::Register(IEditorWnd* Wnd)
{
	Windows.push_back(Wnd);
	if (Data.contains(Wnd->StateName))
	{
		Wnd->Serialize(GetEntry(Wnd->StateName), false);
	}
}

void CEditorWndStates::Unregister(IEditorWnd* Wnd)
{
	auto It = std::find(Windows.begin(), Windows.end(), Wnd);
	if (It == Windows.end())
	{
		return;
	}

	Windows.erase(It);

	// The derived part is gone: only the open flag can be kept
	GetEntry(Wnd->StateName)["open"] = Wnd->bOpen;
}
