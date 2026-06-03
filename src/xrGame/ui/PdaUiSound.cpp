#include "stdafx.h"
#include "PdaUiSound.h"

#include "PdaConstants.h"
#include "../../xrEngine/device.h"
#include "../../xrUI/xrUIXmlParser.h"

namespace
{
struct SPdaSoundLoadEntry
{
	EPdaUiSound sound;
	const char* xmlNode;
	const char* fallbackNode;
};

struct SPdaSubdialogLoadEntry
{
	EPdaUiSound sound;
	const char* xmlNode;
};

constexpr SPdaSoundLoadEntry s_mainWindowLoadTable[] =
{
	{ EPdaUiSound::Open, PdaUiSoundXml::Open, nullptr },
	{ EPdaUiSound::Close, PdaUiSoundXml::Close, PdaUiSoundXml::KeyClose },
	{ EPdaUiSound::Tab, PdaUiSoundXml::Tab, PdaUiSoundXml::KeyTab },
	{ EPdaUiSound::ListScroll, PdaUiSoundXml::ListScroll, nullptr },
	{ EPdaUiSound::ListSelect, PdaUiSoundXml::ListSelect, nullptr },
	{ EPdaUiSound::FilterToggle, PdaUiSoundXml::FilterToggle, nullptr },
	{ EPdaUiSound::PanelOpen, PdaUiSoundXml::PanelOpen, nullptr },
	{ EPdaUiSound::PanelClose, PdaUiSoundXml::PanelClose, nullptr },
	{ EPdaUiSound::Confirm, PdaUiSoundXml::Confirm, nullptr },
	{ EPdaUiSound::Cancel, PdaUiSoundXml::Cancel, nullptr },
};

constexpr SPdaSubdialogLoadEntry s_subdialogLoadTable[] =
{
	{ EPdaUiSound::ListScroll, PdaUiSoundXml::ListScroll },
	{ EPdaUiSound::ListSelect, PdaUiSoundXml::ListSelect },
	{ EPdaUiSound::FilterToggle, PdaUiSoundXml::FilterToggle },
	{ EPdaUiSound::PanelOpen, PdaUiSoundXml::PanelOpen },
	{ EPdaUiSound::PanelClose, PdaUiSoundXml::PanelClose },
	{ EPdaUiSound::Tab, PdaUiSoundXml::Tab },
	{ EPdaUiSound::Confirm, PdaUiSoundXml::Confirm },
	{ EPdaUiSound::Cancel, PdaUiSoundXml::Cancel },
};
} // namespace

CPdaUiSounds::CPdaUiSounds() = default;

CPdaUiSounds::~CPdaUiSounds()
{
	DestroyAll();
}

void CPdaUiSounds::DestroyAll()
{
	for (u32 i = 0; i < static_cast<u32>(EPdaUiSound::Count); ++i)
	{
		m_sounds[i].destroy();
	}
}

ref_sound& CPdaUiSounds::Slot(EPdaUiSound sound)
{
	return m_sounds[SoundIndex(sound)];
}

void CPdaUiSounds::BuildXmlPath(string512& out, const char* root, const char* child)
{
	xr_strconcat(out, root, ":", child);
}

void CPdaUiSounds::LoadSlot(CUIXml& xml, const char* xmlPath, ref_sound& slot)
{
	slot.destroy();

	if (!xmlPath || !xmlPath[0] || !xml.NavigateToNode(xmlPath, 0))
	{
		return;
	}

	const shared_str soundName = xml.Read(xmlPath, 0, "");
	if (!soundName.size())
	{
		return;
	}

	string_path soundFile;
	if (!FS.exist(soundFile, _game_sounds_, soundName.c_str(), ".ogg"))
	{
		Msg("! PdaUiSound: sound not found [%s.ogg] (xml [%s], file [%s])", soundName.c_str(), xmlPath, xml.m_xml_file_name);
		return;
	}

	::Sound->create(slot, *soundName, st_Effect, sg_SourceType);
}

void CPdaUiSounds::LoadSlotWithFallback(CUIXml& xml, const char* primaryPath, const char* fallbackPath, ref_sound& slot)
{
	LoadSlot(xml, primaryPath, slot);
	if (!slot.handle() && fallbackPath && fallbackPath[0])
	{
		LoadSlot(xml, fallbackPath, slot);
	}
}

void CPdaUiSounds::PlaySlot(ref_sound& slot)
{
	if (slot.handle())
	{
		slot.play(nullptr, sm_2D);
	}
}

bool CPdaUiSounds::CanPlay(EPdaUiSound sound, bool throttled) const
{
	const u32 idx = SoundIndex(sound);
	if (idx >= static_cast<u32>(EPdaUiSound::Count) || m_suppress[idx])
	{
		return false;
	}

	if (!throttled)
	{
		return true;
	}

	const u32 now = Device.dwTimeContinual;
	return (now - m_throttleLast[idx]) >= ThrottleMs;
}

void CPdaUiSounds::Play(EPdaUiSound sound, bool throttled)
{
	const u32 idx = SoundIndex(sound);
	if (idx >= static_cast<u32>(EPdaUiSound::Count) || !CanPlay(sound, throttled))
	{
		return;
	}

	if (throttled)
	{
		m_throttleLast[idx] = Device.dwTimeContinual;
	}

	PlaySlot(m_sounds[idx]);
}

void CPdaUiSounds::PlayPanel(bool opening, bool throttled)
{
	Play(opening ? EPdaUiSound::PanelOpen : EPdaUiSound::PanelClose, throttled);
}

void CPdaUiSounds::PlayFilterToggle(bool throttled)
{
	Play(EPdaUiSound::FilterToggle, throttled);
}

void CPdaUiSounds::SetSuppressSound(EPdaUiSound sound, bool suppress)
{
	const u32 idx = SoundIndex(sound);
	if (idx < static_cast<u32>(EPdaUiSound::Count))
	{
		m_suppress[idx] = suppress;
	}
}

void CPdaUiSounds::LoadMainWindow(CUIXml& xml)
{
	for (const SPdaSoundLoadEntry& entry : s_mainWindowLoadTable)
	{
		LoadSlotWithFallback(xml, entry.xmlNode, entry.fallbackNode, Slot(entry.sound));
	}
}

void CPdaUiSounds::TryLoadSlotAtNode(CUIXml& xml, EPdaUiSound sound, const char* nodeName)
{
	if (Slot(sound).handle() || !nodeName || !nodeName[0])
	{
		return;
	}

	LoadSlot(xml, nodeName, Slot(sound));
}

void CPdaUiSounds::LoadTaskWindow(CUIXml& xml)
{
	for (const SPdaSubdialogLoadEntry& entry : s_subdialogLoadTable)
	{
		TryLoadSlotAtNode(xml, entry.sound, entry.xmlNode);
	}
}

void CPdaUiSounds::LoadLegacyMapZoomIfNeeded(CUIXml& xml, const char* mapWndPath)
{
	if (Slot(EPdaUiSound::MapZoomIn).handle() || Slot(EPdaUiSound::MapZoomOut).handle())
	{
		return;
	}

	string512 path;
	BuildXmlPath(path, mapWndPath, PdaUiSoundXml::MapZoom);
	const shared_str soundName = xml.Read(path, 0, "");
	if (!soundName.size())
	{
		return;
	}

	string_path soundFile;
	if (!FS.exist(soundFile, _game_sounds_, soundName.c_str(), ".ogg"))
	{
		return;
	}

	// HACK: legacy single snd_map_zoom node fills both zoom directions
	::Sound->create(Slot(EPdaUiSound::MapZoomIn), *soundName, st_Effect, sg_SourceType);
	::Sound->create(Slot(EPdaUiSound::MapZoomOut), *soundName, st_Effect, sg_SourceType);
}

void CPdaUiSounds::LoadMapWindow(CUIXml& xml, const char* mapWndPath)
{
	if (!mapWndPath || !mapWndPath[0])
	{
		mapWndPath = PdaConfig::MapSubdialogWindowName;
	}

	string512 path;
	BuildXmlPath(path, mapWndPath, PdaUiSoundXml::MapZoomIn);
	LoadSlot(xml, path, Slot(EPdaUiSound::MapZoomIn));

	BuildXmlPath(path, mapWndPath, PdaUiSoundXml::MapZoomOut);
	LoadSlot(xml, path, Slot(EPdaUiSound::MapZoomOut));

	LoadLegacyMapZoomIfNeeded(xml, mapWndPath);

	BuildXmlPath(path, mapWndPath, PdaUiSoundXml::MapCenter);
	LoadSlot(xml, path, Slot(EPdaUiSound::MapCenter));

	BuildXmlPath(path, mapWndPath, PdaUiSoundXml::MapPan);
	LoadSlot(xml, path, Slot(EPdaUiSound::MapPan));
}

void CPdaUiSounds::TryLoadFromRoot(CUIXml& xml, const char* rootPath, EPdaUiSound sound, const char* nodeName)
{
	if (Slot(sound).handle())
	{
		return;
	}

	string512 path;
	BuildXmlPath(path, rootPath, nodeName);
	LoadSlot(xml, path, Slot(sound));
}

void CPdaUiSounds::LoadSubdialog(CUIXml& xml, const char* rootPath)
{
	if (!rootPath || !rootPath[0])
	{
		return;
	}

	for (const SPdaSubdialogLoadEntry& entry : s_subdialogLoadTable)
	{
		TryLoadFromRoot(xml, rootPath, entry.sound, entry.xmlNode);
	}
}

EPdaUiSound CPdaUiSounds::ResolveMapZoomSound(bool zoomIn)
{
	const EPdaUiSound primary = zoomIn ? EPdaUiSound::MapZoomIn : EPdaUiSound::MapZoomOut;
	const EPdaUiSound fallback = zoomIn ? EPdaUiSound::MapZoomOut : EPdaUiSound::MapZoomIn;

	if (m_sounds[SoundIndex(primary)].handle())
	{
		return primary;
	}

	return fallback;
}

void CPdaUiSounds::PlayMapZoom(bool zoomIn, bool throttled)
{
	Play(ResolveMapZoomSound(zoomIn), throttled);
}
