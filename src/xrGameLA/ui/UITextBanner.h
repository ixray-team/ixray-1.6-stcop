//=============================================================================
//  Êðàñèâûé òåêñòîâûé áàííåð ñ ìíîæåñòâîì ñòèëåé àíèìàöèè
//=============================================================================

#ifndef UI_TEXT_BANNER_H_
#define UI_TEXT_BANNER_H_

// #pragma once;

#include "../xrUI/Widgets/UIStatic.h"

//-----------------------------------------------------------------------------/
//  Êëàññ ïàðàìåòðîâ ýôôåêòà
//-----------------------------------------------------------------------------/

struct EffectParams
{
	friend class	CUITextBanner;
	float			fPeriod;
	bool			bCyclic;
	bool			bOn;
	int				iAdditionalParam;
	float			fAdditionalParam;
	int				iEffectStage;

	// Constructor
	EffectParams()
		:	fPeriod				(0.0f),
			fTimePassed			(0.0f),
			iEffectStage		(0),
			fAdditionalParam	(0.0f),
			iAdditionalParam	(0),
			bCyclic				(true),
			bOn					(true)
	{}
private:
	float			fTimePassed;
};

//-----------------------------------------------------------------------------/
//  Êëàññ àíèìèðîàííîãî áàííåðà
//-----------------------------------------------------------------------------/

class CUITextBanner
{
public:
	enum TextBannerStyles
	{
		tbsNone		= 0,
		tbsFlicker	= 1,
		tbsFade		= 1 << 1
	};
	// Ctor and Dtor
	CUITextBanner				();
	~CUITextBanner				();

	virtual void	Update		();
	void			Out			(float x, float y, const char *fmt, ...);

	// Óñòàíîâèòü ïàðàìåòðû âèçóàëèçàöèè áàííåðà. Ôëàãè ñì. ïåðå÷èñëåíèå TextBannerStyles
	EffectParams * SetStyleParams(const TextBannerStyles styleName);
	void		ResetAnimation		(const TextBannerStyles styleName);

	// Font
	void		SetFont				(CGameFont *pFont)	{ m_pFont = pFont; }
	CGameFont	*GetFont			() const { return m_pFont; }
	void		SetFontSize			(float sz)	{ fontSize = sz; }
	void		SetFontAlignment	(CGameFont::EAligment al) {aligment = al;}

	// Color
	void		SetTextColor		(u32 cl);
	u32			GetTextColor		();

	// Âêë/âûêë àíèìàöèè
	void		PlayAnimation		()					{ m_bAnimate = true;	}
	void		StopAnimation		()					{ m_bAnimate = false;	}

protected:
	// Ïåðåìåííûå âðåìåíè äëÿ êàæäîãî èç ñòèëåé.
	// Â ïàðå:	first	- êîíòðîëüíûé ïåðèîä ýôôåêòà (çàäàâàåìûé ïîëüçîâàòåëåì)
	//			second	- ïðîøåäøåå âðåìÿ ñ òåêóùåãî àïäåéòà
	typedef xr_map<TextBannerStyles, EffectParams>		StyleParams;
	typedef StyleParams::iterator						StyleParams_it;
	StyleParams											m_StyleParams;

	// Ïðîöåäóðû èçìåíåíèÿ ïàðàìåòðîâ íàäïèñè äëÿ ýôôåêòîâ
	void EffectFade();
	void EffectFlicker();

	// Ôëàã, êîòîðûé îïðåäåëÿåò ñîñòîÿíèå àíèìàöèè
	bool		m_bAnimate;

	// Font
	CGameFont				*m_pFont;
	float					fontSize;
	CGameFont::EAligment	aligment;

	// Letters color
	u32			m_Cl;

};

#endif
