#pragma once

#include "HUDCrosshair.h"
#include "../xrCDB/xr_collide_defs.h"

class CHUDManager;

class CHUDTarget {
private:
	friend class CHUDManager;

private:
	typedef collide::rq_result		rq_result;
	typedef collide::rq_results		rq_results;

private:
	ui_shader		hShader;
	//ref_geom		hGeom;
	float			fuzzyShowInfo;
	rq_result		RQ;
	rq_results		RQR;

private:
	bool			m_bShowCrosshair;
	CHUDCrosshair	HUDCrosshair;

private:
			void	net_Relcase		(CObject* O);

public:
							CHUDTarget	();
							~CHUDTarget	();
	void					CursorOnFrame ();
	void					Render		();
	void					Load		();
	collide::rq_result&		GetRQ		() {return RQ;};
	CHUDCrosshair&			GetHUDCrosshair	() {return HUDCrosshair;}
	void					ShowCrosshair	(bool b);
};
