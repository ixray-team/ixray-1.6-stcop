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
	if (Read("monster_debug_3d_shapes", DrawControl) && Read("monster_debug_3d_text", TextControl))
	{
		CELSharedDebug::Instance().SetDrawEnabled(DrawControl.Mode != 0, TextControl.Mode != 0);
	}
	SControl StatsControl;
	if (Read("monster_debug_hud", StatsControl))
	{
		if (StatsControl.ResetRevision != StatsResetRevision)
		{
			CELSharedDebug::Instance().Clear();
			StatsResetRevision = StatsControl.ResetRevision;
		}
		CELSharedDebug::Instance().SetStatsEnabled(StatsControl.Mode != 0);
	}
}
