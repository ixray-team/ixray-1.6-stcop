#pragma once
#include "stdafx.h"
#include <nlohmann/json.hpp>

class XREUI_API IEditorWnd
{
public:
	IEditorWnd() :bOpen(true) {}
	enum FUI
	{
		F_NoDelete=1,
	};
	Flags32 Flags;
	virtual ~IEditorWnd();

	virtual void ResetBegin();
	virtual void ResetEnd();
	bool IsClosed()const { return !bOpen; }

	// Writes the window state to Data or reads it back. The base keeps the open flag
	virtual void Serialize(nlohmann::json& Data, bool IsSaving);

	// Puts the window into the saved editor state under Name and applies the stored state.
	// Call it once the window is fully constructed
	void SetStateName(const char* Name);
	const char* GetStateName() const { return StateName; }

public:
	virtual void Draw() = 0;

	int TabIndex = 0;

protected:
	// Reads Data[Key] into Out when it exists and has a matching type. Never throws
	template<typename T>
	static bool ReadState(const nlohmann::json& Data, const char* Key, T& Out)
	{
		if (!Data.is_object())
		{
			return false;
		}

		auto It = Data.find(Key);
		if (It == Data.end())
		{
			return false;
		}

		if constexpr (std::is_same_v<T, bool>)
		{
			if (!It->is_boolean())
			{
				return false;
			}
			Out = It->get<bool>();
		}
		else if constexpr (std::is_same_v<T, xr_string> || std::is_same_v<T, shared_str>)
		{
			if (!It->is_string())
			{
				return false;
			}
			Out = It->get_ref<const nlohmann::json::string_t&>().c_str();
		}
		else if constexpr (std::is_enum_v<T>)
		{
			if (!It->is_number_integer())
			{
				return false;
			}
			Out = static_cast<T>(It->get<std::underlying_type_t<T>>());
		}
		else
		{
			static_assert(std::is_arithmetic_v<T>, "Unsupported state type");
			if (!It->is_number())
			{
				return false;
			}
			Out = It->get<T>();
		}

		return true;
	}

	bool bOpen;
	bool IsDocked = true;
	bool IsFocused = false;
	bool IsContextMenu = false;

private:
	friend class CEditorWndStates;
	const char* StateName = nullptr;
};

// Saved states of the windows that called IEditorWnd::SetStateName
class XREUI_API CEditorWndStates
{
public:
	static CEditorWndStates& Get();

	// Replaces the stored states and applies them to the live windows
	void Load(const nlohmann::json& Data);
	// Collects the states of the live windows and returns all stored states
	const nlohmann::json& Save();
	// Stored open flag of a window that may not exist yet
	bool IsOpen(const char* Name, bool Default = false) const;

private:
	friend class IEditorWnd;
	void Register(IEditorWnd* Wnd);
	void Unregister(IEditorWnd* Wnd);
	nlohmann::json& GetEntry(const char* Name);

	nlohmann::json Data = nlohmann::json::object();
	xr_vector<IEditorWnd*> Windows;
};
