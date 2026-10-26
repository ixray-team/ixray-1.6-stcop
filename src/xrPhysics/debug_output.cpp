#include "StdAfx.h"
#include "debug_output.h"
#ifdef DEBUG_DRAW

static class DebugOutputEmptyImpl :
	public	IDebugOutput
{
	Flags32 m1;
	Flags32 m2;

	virtual	const	Flags32& ph_dbg_draw_mask()const override
	{
		return m1;
	}

	virtual	const	Flags32& ph_dbg_draw_mask1()const override
	{
		return m2;
	}


	virtual	void DBG_DrawStatBeforeFrameStep() override
	{

	}

	virtual	void DBG_DrawStatAfterFrameStep() override
	{

	}

	virtual	void DBG_OpenCashedDraw() override
	{

	}

	virtual	void DBG_ClosedCashedDraw(u32 remove_time) override
	{

	}

	virtual	void DBG_DrawPHObject(const CPHObject* obj) override
	{

	}

	virtual	void DBG_DrawContact(const dContact& c) override
	{

	}

	virtual	void DBG_DrawTri(CDB::RESULT* T, u32 c) override
	{

	}

	virtual	void DBG_DrawTri(CDB::TRI* T, const Fvector* V_verts, u32 c) override
	{

	}

	virtual	void DBG_DrawLine(const Fvector& p0, const Fvector& p1, u32 c) override
	{

	}

	virtual	void DBG_DrawAABB(const Fvector& center, const Fvector& AABB, u32 c) override
	{

	}

	virtual	void DBG_DrawOBB(const Fmatrix& m, const Fvector h, u32 c) override
	{

	}

	virtual	void DBG_DrawPoint(const Fvector& p, float size, u32 c) override
	{

	}

	virtual	void DBG_DrawMatrix(const Fmatrix& m, float size, u8 a = 255) override
	{

	}

	virtual	void _cdecl DBG_OutText(const char* s, ...) override
	{

	}

	virtual	void DBG_DrawFrameStart() override
	{

	}

	virtual	void PH_DBG_Render() override
	{

	}

	virtual	void PH_DBG_Clear() override
	{

	}

	virtual	const char* PH_DBG_ObjectTrackName() override
	{
		return "none";
	}

	//virtual	bool			draw_frame								()=0;
	u32 tries_num;

	virtual	u32& dbg_tries_num() override
	{
		return tries_num;
	}
	u32 saved_tries_for_active_objects;

	virtual	u32& dbg_saved_tries_for_active_objects() override
	{
		return saved_tries_for_active_objects;
	}
	u32 total_saved_tries;

	virtual	u32& dbg_total_saved_tries() override
	{
		return total_saved_tries;
	}
	u32 reused_queries_per_step;

	virtual	u32& dbg_reused_queries_per_step() override
	{
		return reused_queries_per_step;
	}
	u32 new_queries_per_step;

	virtual	u32& dbg_new_queries_per_step() override
	{
		return new_queries_per_step;
	}
	u32 bodies_num;

	virtual	u32& dbg_bodies_num() override
	{
		return bodies_num;
	}
	u32 joints_num;

	virtual	u32& dbg_joints_num() override
	{
		return joints_num;
	}
	u32 islands_num;

	virtual	u32& dbg_islands_num() override
	{
		return islands_num;
	}
	u32 contacts_num;

	virtual	u32& dbg_contacts_num() override
	{
		return contacts_num;
	}
	float vel_collid_damage_to_display;

	virtual	float		dbg_vel_collid_damage_to_display() override
	{
		return vel_collid_damage_to_display;
	}

	virtual	void DBG_ObjAfterPhDataUpdate(CPHObject* obj) override {}
	virtual	void DBG_ObjBeforePhDataUpdate(CPHObject* obj) override {}
	virtual	void DBG_ObjAfterStep(CPHObject* obj) override {}
	virtual	void DBG_ObjBeforeStep(CPHObject* obj) override {}
	virtual	void DBG_ObjeAfterPhTune(CPHObject* obj) override {}
	virtual	void DBG_ObjBeforePhTune(CPHObject* obj) override {}
	virtual	void DBG_ObjAfterCollision(CPHObject* obj) override {}
	virtual	void DBG_ObjBeforeCollision(CPHObject* obj) override {}


} dbg_output_empty;

IDebugOutput* ph_debug_output = &dbg_output_empty;
#endif