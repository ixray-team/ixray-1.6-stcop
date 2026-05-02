#pragma once

#include "../../xrSound/Sound.h"

class CUIXml;

namespace PdaUiSoundXml
{
// Main pda.xml
constexpr const char* Open = "snd_open";
constexpr const char* Close = "snd_close";
constexpr const char* Tab = "snd_tab";
constexpr const char* ListScroll = "snd_list_scroll";
constexpr const char* ListSelect = "snd_list_select";
constexpr const char* FilterToggle = "snd_filter_toggle";
constexpr const char* PanelOpen = "snd_panel_open";
constexpr const char* PanelClose = "snd_panel_close";
constexpr const char* Confirm = "snd_confirm";
constexpr const char* Cancel = "snd_cancel";

// Legacy names (pda.xml)
constexpr const char* KeyTab = "snd_key_tab";
constexpr const char* KeyClose = "snd_key_close";

// map_wnd in pda_tasks.xml / pda_map.xml
constexpr const char* MapZoomIn = "snd_map_zoom_in";
constexpr const char* MapZoomOut = "snd_map_zoom_out";
constexpr const char* MapZoom = "snd_map_zoom";
constexpr const char* MapCenter = "snd_map_center";
constexpr const char* MapPan = "snd_map_pan";
} // namespace PdaUiSoundXml

enum class EPdaUiSound : u8
{
	Open = 0,
	Close,
	Tab,
	ListScroll,
	ListSelect,
	FilterToggle,
	PanelOpen,
	PanelClose,
	MapZoomIn,
	MapZoomOut,
	MapCenter,
	MapPan,
	Confirm,
	Cancel,
	Count
};

// PDA UI sounds: load from pda.xml, pda_tasks.xml root, map_wnd, subdialogs; play as 2D effects.
class CPdaUiSounds final
{
public:
	CPdaUiSounds();
	~CPdaUiSounds();

	void LoadMainWindow(CUIXml& xml);
	void LoadTaskWindow(CUIXml& xml);
	void LoadMapWindow(CUIXml& xml, const char* mapWndPath);
	void LoadSubdialog(CUIXml& xml, const char* rootPath);

	void Play(EPdaUiSound sound, bool throttled = false);
	void PlayMapZoom(bool zoomIn, bool throttled = false);
	void PlayPanel(bool opening, bool throttled = false);
	void PlayFilterToggle(bool throttled = false);

	void SetSuppressSound(EPdaUiSound sound, bool suppress);
	void SetSuppressTabSound(bool suppress) { SetSuppressSound(EPdaUiSound::Tab, suppress); }

	void PlayTabSwitch() { Play(EPdaUiSound::Tab); }
	void PlayClose() { Play(EPdaUiSound::Close); }

	void DestroyAll();

private:
	ref_sound	m_sounds[static_cast<u32>(EPdaUiSound::Count)] = {};
	bool		m_suppress[static_cast<u32>(EPdaUiSound::Count)] = {};
	u32			m_throttleLast[static_cast<u32>(EPdaUiSound::Count)] = {};

	static constexpr u32 ThrottleMs = 120;

	static void LoadSlot(CUIXml& xml, const char* xmlPath, ref_sound& slot);
	static void LoadSlotWithFallback(CUIXml& xml, const char* primaryPath, const char* fallbackPath, ref_sound& slot);
	static void PlaySlot(ref_sound& slot);
	static void BuildXmlPath(string512& out, const char* root, const char* child);

	ref_sound& Slot(EPdaUiSound sound);
	u32 SoundIndex(EPdaUiSound sound) const { return static_cast<u32>(sound); }
	bool CanPlay(EPdaUiSound sound, bool throttled) const;
	void TryLoadFromRoot(CUIXml& xml, const char* rootPath, EPdaUiSound sound, const char* nodeName);
	void TryLoadSlotAtNode(CUIXml& xml, EPdaUiSound sound, const char* nodeName);
	void LoadLegacyMapZoomIfNeeded(CUIXml& xml, const char* mapWndPath);
	EPdaUiSound ResolveMapZoomSound(bool zoomIn);
};
