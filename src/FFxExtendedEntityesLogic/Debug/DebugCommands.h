#pragma once
#include "xrEngine/xr_ioc_cmd.h"

// Registration is header-only: the renderer must not link against xrGame.
class CELSharedDebugCommands final
{
public:
	static void Register()
	{
		static CCommand LogicCommand("monster_logic", EKind::Logic);
		static CCommand DrawCommand("monster_debug_3d_shapes", EKind::Draw);
		static CCommand TextCommand("monster_debug_3d_text", EKind::Text);
		static CCommand StatsCommand("monster_debug_hud", EKind::Stats);
		Console->AddCommand(&LogicCommand);
		Console->AddCommand(&DrawCommand);
		Console->AddCommand(&StatsCommand);
		Console->AddCommand(&TextCommand);
	}
	static void Synchronize();

private:
	enum class EKind
	{
		Logic,
		Draw,
		Text,
		Stats
	};
	struct SControl
	{
		int Mode = 0;
		u32 Revision = 0;
		u32 ResetRevision = 0;
	};
	class CCommand final : public IConsole_Command
	{
	public:
		CCommand(const char* Name, EKind CommandKind) : IConsole_Command(Name), Kind(CommandKind)
		{
			Control.Mode = Kind == EKind::Logic ? -1 : 0;
		}
		void Execute(const char* Arguments) override
		{
			int Mode;
			if (!xr_strcmp(Arguments, "reset") && Kind == EKind::Stats)
			{
				xrCriticalSectionGuard Guard(ControlLock);
				++Control.ResetRevision;
				return;
			}
			if (!xr_strcmp(Arguments, "on") || !xr_strcmp(Arguments, "1"))
			{
				Mode = 1;
			}
			else if (!xr_strcmp(Arguments, "off") || !xr_strcmp(Arguments, "0"))
			{
				Mode = 0;
			}
			else if (!xr_strcmp(Arguments, "config") && Kind == EKind::Logic)
			{
				Mode = -1;
			}
			else
			{
				InvalidSyntax();
				return;
			}
			xrCriticalSectionGuard Guard(ControlLock);
			Control.Mode = Mode;
			++Control.Revision;
		}
		void Status(TStatus& StatusText) override
		{
			const SControl Snapshot = Read();
			xr_strcpy(StatusText, Snapshot.Mode < 0 ? "config" : Snapshot.Mode ? "on"
																			   : "off");
		}
		void Info(TInfo& Information) override
		{
			xr_strcpy(Information, Kind == EKind::Logic ? "on/off/config: включить / выключить / по конфигу" : Kind == EKind::Stats ? "on/off/reset: включить / выключить / сбросить замеры"
																																	: "on/off: включить / выключить");
		}
		SControl Read() const
		{
			xrCriticalSectionGuard Guard(ControlLock);
			return Control;
		}

	private:
		EKind Kind;
		mutable xrCriticalSection ControlLock;
		SControl Control;
	};
	static bool Read(const char* Name, SControl& Control)
	{
		IConsole_Command* Command = Console ? Console->GetCommand(Name) : nullptr;
		if (!Command)
		{
			return false;
		}
		// These names are exclusively registered as CCommand by Register().
		Control = static_cast<const CCommand*>(Command)->Read();
		return true;
	}
};
