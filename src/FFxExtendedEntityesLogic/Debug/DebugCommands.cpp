#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/Debug/DebugCommands.h"
#include "FFxExtendedEntityesLogic/Debug/Debug.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"

// Применяет копии консольных режимов и однократный запрос сброса статистики.
void CELSharedDebugCommands::Synchronize()
{
	static u32 LogicRevision = 0;
	static u32 StatsResetRevision = 0;
	SControl LogicControl;
	if (Read("monster_logic", LogicControl) && (LogicControl.Mode != CELSharedSettings::GetOverride() || LogicControl.Revision != LogicRevision))
	{
		CELSharedSettings::SetEnabled(LogicControl.Mode);
		LogicRevision = LogicControl.Revision;
	}
	SControl DrawControl;
	SControl TextControl;
	if (Read("monsters_debug_3d_shapes", DrawControl) && Read("monsters_debug_3d_text", TextControl))
	{
		CELSharedDebug::Instance().SetDrawEnabled(DrawControl.Mode != 0, TextControl.Mode != 0);
	}
	SControl StatsControl;
	if (Read("monsters_debug_hud", StatsControl))
	{
		if (StatsControl.ResetRevision != StatsResetRevision)
		{
			CELSharedDebug::Instance().Clear();
			StatsResetRevision = StatsControl.ResetRevision;
		}
		CELSharedDebug::Instance().SetStatsEnabled(StatsControl.Mode != 0);
	}
	SControl LogControl;
	if (Read("monsters_debug_log", LogControl))
	{
		CELSharedDebug::Instance().SetCombatLog(LogControl.Mode != 0);
	}
	u32 Mask = 0;
	SControl Detail0;
	if (Read("monsters_debug_animations", Detail0) && Detail0.Mode) { Mask |= 1u << 0; }
	SControl Detail1;
	if (Read("monsters_debug_hearing", Detail1) && Detail1.Mode) { Mask |= 1u << 1; }
	SControl Detail2;
	if (Read("monsters_debug_perception", Detail2) && Detail2.Mode) { Mask |= 1u << 2; }
	SControl Detail3;
	if (Read("monsters_debug_character", Detail3) && Detail3.Mode) { Mask |= 1u << 3; }
	SControl Detail4;
	if (Read("monsters_debug_combat", Detail4) && Detail4.Mode) { Mask |= 1u << 4; }
	SControl Detail5;
	if (Read("monsters_debug_navigation", Detail5) && Detail5.Mode) { Mask |= 1u << 5; }
	CELSharedDebug::Instance().SetDetails(Mask);
}
