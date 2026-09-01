#pragma once
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIProgressShape.h"

class CUIMotionIcon final : public CUIStatic
{
	typedef CUIStatic inherited;
public:
	enum EState
	{
		stNormal,
		stCrouch,
		stCreep,
		stClimb,
		stRun,
		stSprint,
		stLast
	};
private:
	EState m_current_state;
	xr_map<EState, CUIStatic*> m_states;
	CUIProgressBar* m_power_progress;
	bool m_independent = false;

	CUIProgressShape* m_luminosity_progress_shape;
	CUIProgressShape* m_noise_progress_shape;
	CUIProgressBar* m_luminosity_progress_bar;
	CUIProgressBar* m_noise_progress_bar;

	CUIStatic* _luminosityOverlay;
	CUIStatic* _noiseOverlay;
	u32 _luminosityOverlayBaseColor;
	u32 _noiseOverlayBaseColor;
	float _luminosityNormalized;
	float _noiseNormalized;
	float _luminosityOverlayCur;
	float _noiseOverlayCur;

		struct _npc_visibility{
			u16				id;
			float				value;
			bool operator == (const u16& _id){
				return id == _id;
			}
			bool operator < (const _npc_visibility& m) const
			{
				return (value < m.value);
			}
		};
		xr_vector<_npc_visibility>	m_npc_visibility;
		bool						m_bchanged;
		float						m_luminosity;
		float						m_cur_pos;

		void			EnsureMinimapOverlays(CUIXml& uiXml, Fvector2 const& sz, Fvector2 const& pos);
		void			EnsureCompassLayout(CUIXml& uiXml);
		void			ApplyNavigationPresentation(bool useCompassBar, CUIXml* uiXml = nullptr, Fvector2 const* overlaySize = nullptr, Fvector2 const* overlayPos = nullptr);
		void			SetMinimapOverlayVisibility(bool visible);
		void			SetCompassOverlayVisibility(bool visible);
		void			LoadContextualFadeSettings(CUIXml& uiXml, const char* path, bool& contextualFadeOut);
		void			InitMinimapLuminosityOverlay(CUIXml& uiXml);
		bool						_compassModeActive = false;
		bool						_compassContextualFade = false;
		bool						_minimapContextualFade = false;
		float						_contextualAlpha = 0.f;
		float						_fadeInSpeed = 6.f;
		float						_fadeOutSpeed = 5.f;
		float						_minVisibleAlpha = 0.01f;
		float						_visibilityThreshold = 0.5f;
		CUIStatic*					_compassBackground = nullptr;
		u32							_compassBackgroundBaseColor = 0;

		CUIWindow*					_compassLayoutFrame = nullptr;
		Fvector2					_compassLayoutPos = {};
		Fvector2					_compassLayoutSize = {};
		bool						_compassLayoutRelative = true;
		bool						_compassLayoutAlignCenter = false;

		float UpdateContextualFadeAlpha(float alpha, bool isVisible) const;
		bool IsContextuallyNeeded() const;
		void ApplyCompassContextualAlpha(float alpha);
		void ApplyMinimapLuminosityOverlayAlpha(float contextualAlpha);

public:
	virtual					~CUIMotionIcon		();
							CUIMotionIcon		();
	virtual	void			Update				();
	virtual void			Draw				();
			bool			Init				(Frect const& rect, bool useCompassBar, bool useCompassLayout = false);
			void			ApplyNavigationHost(CUIWindow* attachParent, Frect const& hostRect, bool useCompassBar);
			bool			IsIndependent		() const { return m_independent; }
			CUIWindow*		CompassLayoutFrame	() const;
			void			ApplyCompassLayout	(CUIWindow* compassBar);
			void			SetNavigationPresentation(bool useCompassBar);
			void			ShowState			(EState state);
			void			SetPower			(float Pos);
			void			SetNoise			(float Pos);
			void			SetLuminosity		(float newPos);
			void			SetActorVisibility	(u16 who_id, float value);
			void			ResetVisibility		();

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIStatic* ui_cast_static() { return this; }
};
