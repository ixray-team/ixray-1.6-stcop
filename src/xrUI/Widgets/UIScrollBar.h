#pragma once
#include "UIWindow.h"
#include "../../xrScripts/script_export_space.h"

class CUI3tButton;
class CUIScrollBox;
class CUIFrameLineWnd;
class CUIStatic;
class CUIXml;

enum class ScrollLayoutMode : u8
{
	Stretch,
	Fixed,
};

enum class ScrollBarPart : u8
{
	Dec,
	Inc,
	Track,
	Thumb,
};

struct ScrollBarProfileConfig
{
	ScrollLayoutMode layoutMode = ScrollLayoutMode::Stretch;
	bool thumbAsButton = false;
	float thickness = 16.0f;
	float holdDelay = 50.0f;
	Ivector2 scrollBoxOffset{};
};

struct ScrollBarPartFlags
{
	bool hasDec = false;
	bool hasInc = false;
	bool hasTrack = false;
	bool hasThumb = false;

	bool anyPart() const
	{
		return hasDec || hasInc || hasTrack || hasThumb;
	}
};

enum class ScrollHitZone : u8
{
	None,
	DecButton,
	IncButton,
	TrackBefore,
	TrackAfter,
};

UI_API int QueryScrollBarProfileLayout_script(const char* profile, bool isHorizontal);

class UI_API CUIScrollBar final :
	public CUIWindow
{
private:
	using ScrollBarBase = CUIWindow;

	CUI3tButton* _decButton = nullptr;
	CUI3tButton* _incButton = nullptr;
	CUIScrollBox* _scrollBox = nullptr;
	CUI3tButton* _fixedThumb = nullptr;
	CUIFrameLineWnd* _frameBackground = nullptr;

	float _holdDelay = 50.0f;
	int _scrollPos = 0;
	int _stepSize = 1;
	int _minPos = 1;
	int _maxPos = 1;
	int _pageSize = 1;
	int _scrollWorkArea = 0;
	bool _enabled = true;
	bool _isHorizontal = false;
	int _mouseState = 0;
	ScrollLayoutMode _layoutMode = ScrollLayoutMode::Stretch;
	Ivector2 _scrollBoxOffset{};
	u32 _lastHoldTimeMs = 0;
	bool _initialized = false;
	ScrollBarProfileConfig _profileConfig{};
	ScrollBarPartFlags _partFlags{};

	bool ScrollInc(bool byScrollbox = false);
	bool ScrollDec(bool byScrollbox = false);
	void UpdateScrollBar();
	void layoutThumbGeometry(CUIWindow* thumb, float boxSz);
	void ClampByViewRect();
	void SetPosScrollFromView(float viewPos, float viewWidth, float viewOffs);
	int PosViewFromScroll(int viewSize, int viewOffs);
	void SetScrollPosClamped(int pos);
	bool IsRelevant() const;

	bool LoadScrollBarXml(CUIXml& xmlDoc, const char* profile);
	static bool ParseProfile(CUIXml& xmlDoc, const char* profile, bool isHorizontal, ScrollBarProfileConfig& out);
	bool ResolvePartPath(CUIXml& xmlDoc, const char* profile, ScrollBarPart part, bool isHorizontal, string_path& outPath) const;
	bool TryInitPart(CUIXml& xmlDoc, const char* nodePath, ScrollBarPart part, const ScrollBarProfileConfig& config);
	void ApplyStaticThumbHack(CUIStatic* tempStatic, CUIWindow* targetWnd);
	void ApplyStaticTrackHack(CUIStatic* tempBackground);
	void RecalcWorkArea(float thickness);
	bool InitStretchLayout(CUIXml& xmlDoc, const char* profile, Fvector2 pos, float length, bool isHorizontal);
	bool InitFixedLayout(CUIXml& xmlDoc, const char* profile, Fvector2 pos, bool isHorizontal);
	bool InitPartsFromProfile(CUIXml& xmlDoc, const char* profile, bool isHorizontal, float incAnchorLength);
	void ResetPartFlags();
	void detachFixedThumbChild();
	void prepareFixedLayoutChildren();
	void PositionIncButton(float anchorLength);
	CUIWindow* GetThumbWindow() const;
	float GetDecSpan() const;
	float GetIncSpan() const;
	float mainBarSpan() const;
	float crossBarSpan() const;
	float thumbViewOffset() const;
	float scrollBoxInset() const;
	void NotifyScrollChanged();
	void HandleThumbMove();
	ScrollHitZone hitTestScrollZone() const;
	bool applyHitZone(ScrollHitZone zone);
	bool handleFixedLayoutMouseAction(CUIWindow* thumb, EUIMessages mouseAction);

public:
	CUIScrollBar();
	~CUIScrollBar();

	void SetEnabled(bool enabled) { _enabled = enabled; if (!_enabled) { Show(_enabled); } }
	bool GetEnabled() const { return _enabled; }

	void Show(bool show) override;
	void Enable(bool enable) override;

	bool InitScrollBar(Fvector2 pos, float length, bool isHorizontal, const char* profile = "default");
	bool InitScrollBar(Fvector2 pos, bool isHorizontal, const char* profile = "pda");

	static bool QueryProfileLayout(const char* profile, bool isHorizontal, ScrollLayoutMode& outMode);
	static bool InitForProfile(CUIScrollBar& bar, Fvector2 pos, float stretchLength, bool isHorizontal, const char* profile);

	bool IsFixedLayout() const { return _layoutMode == ScrollLayoutMode::Fixed; }
	bool IsInitialized() const { return _initialized; }

	void SendMessage(CUIWindow* wnd, s16 msg, void* data) override;
	bool OnMouseAction(float x, float y, EUIMessages mouseAction) override;
	bool OnMouseDown(int mouseBtn) override;
	bool OnMouseDownEx();
	void OnMouseUp(int mouseBtn) override;
	bool OnKeyboardHold(int dik) override;

	void Draw() override;

	void SetWidth(float width) override;
	void SetHeight(float height) override;

	void Reset() override;
	void SyncThumbFromScrollPos();
	void Refresh() { SyncThumbFromScrollPos(); }

	void SetStepSize(int step);
	int GetStepSize() const { return _stepSize; }
	void SetRange(int minPos, int maxPos);
	void GetRange(int& minPos, int& maxPos) const { minPos = _minPos; maxPos = _maxPos; }
	int GetMaxRange() const { return _maxPos; }
	int GetMinRange() const { return _minPos; }
	u32 ScrollSize() const { return std::max(1, _maxPos - _minPos - _pageSize + 1); }

	void SetPageSize(int page) { _pageSize = std::max(0, page); UpdateScrollBar(); }
	int GetPageSize() const { return _pageSize; }

	void SetScrollPos(int pos) { SetScrollPosClamped(pos); UpdateScrollBar(); }
	int GetScrollPos() const { return std::max(_minPos, _scrollPos); }

	void TryScrollInc(bool byScrollbox = false);
	void TryScrollDec(bool byScrollbox = false);

	CUIWindow* ui_cast_window() override { return this; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
