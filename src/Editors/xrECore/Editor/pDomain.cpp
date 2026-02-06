#include "stdafx.h"


#include "D3DUtils.h"
#include "ui_main.h"
#include "../../Editors/Public/PropertiesListHelper.h"
#include "ParticleEffectActions.h"

using namespace PAPI;

PDomain::PDomain(EType et, BOOL ra, u32 color, PDomainEnum t,	
										float inA0,	float inA1,	float inA2,	
								   		float inA3,	float inA4,	float inA5,
										float inA6,	float inA7,	float inA8	)
{
	flags.set(flRenderable,ra);
	e_type = et;
	type = t;
    clr	 = color;
	f[0] = inA0;
	f[1] = inA1;
	f[2] = inA2;    
	f[3] = inA3;
	f[4] = inA4;
	f[5] = inA5;
	f[6] = inA6;
	f[7] = inA7;
	f[8] = inA8;
}

PDomain::PDomain(const PDomain& inDomain)
{
	e_type 	= inDomain.e_type;
    flags	= inDomain.flags;
	type 	= inDomain.type;
    clr	 	= inDomain.clr;
	f[0]	= inDomain.f[0];
	f[1]	= inDomain.f[1];
	f[2]	= inDomain.f[2];
	f[3]	= inDomain.f[3];
	f[4]	= inDomain.f[4];
	f[5]	= inDomain.f[5];
	f[6]	= inDomain.f[6];
	f[7]	= inDomain.f[7];
	f[8]	= inDomain.f[8];
}

PDomain::~PDomain()
{
}

void PDomain::Load(IReader& F)
{
	type		= PDomainEnum(F.r_u32());
	F.r_fvector3(v[0]);
	F.r_fvector3(v[1]);
	F.r_fvector3(v[2]);
}

void PDomain::Load2(CInifile& ini, const shared_str& sect)
{
	type		= PDomainEnum(ini.r_u32(sect,"type"));
	v[0]		= ini.r_fvector3(sect,"v0");
	v[1]		= ini.r_fvector3(sect,"v1");
	v[2]		= ini.r_fvector3(sect,"v2");
}

void PDomain::Save(IWriter& F) const 
{
	F.w_u32		(type);
	F.w_fvector3(v[0]);
	F.w_fvector3(v[1]);
	F.w_fvector3(v[2]);
}

void PDomain::Save2(CInifile& ini, const shared_str& sect) const
{
	ini.w_u32		(sect.c_str(), "type", type);
	ini.w_fvector3	(sect.c_str(), "v0", v[0]);
	ini.w_fvector3	(sect.c_str(), "v1", v[1]);
	ini.w_fvector3	(sect.c_str(), "v2", v[2]);
}

void 	PDomain::Render		(u32 clr, const Fmatrix& parent)
{
	if (!flags.is(flRenderable)) return;
	u32 clr_s = subst_alpha	(clr,0x60);
	u32 clr_w = subst_alpha	(clr,0xff);
    RCache.set_xform_world	(parent);
	switch(type){
    case PDPoint: 	
		EDevice->SetShader	(EDevice->m_WireShader);
    	DU_impl.DrawCross		(v[0], 0.05f,0.05f,0.05f, 0.05f,0.05f,0.05f, clr_w);
    break;
	case PDLine:
		EDevice->SetShader	(EDevice->m_WireShader);
    	DU_impl.DrawCross		(v[0], 0.05f,0.05f,0.05f, 0.05f,0.05f,0.05f, clr_w);
	  	DU_impl.DrawCross		(v[1], 0.05f,0.05f,0.05f, 0.05f,0.05f,0.05f, clr_w);
    	DU_impl.DrawLine 		(v[0], v[1], clr_w);
    break;
    case PDTriangle:
		EDevice->SetShader	(EDevice->m_SelectionShader);
        DU_impl.DrawFace			(v[0], v[1], v[2], clr_s, clr_w, true, true);
    break;
	case PDPlane:{
		EDevice->SetShader	(EDevice->m_SelectionShader);
        Fvector2 sz			= {100.f,100.f};
        DU_impl.DrawPlane		(v[0],v[1],sz,clr_s,clr_w,true,true,true);
    }break;
	case PDBox:
		EDevice->SetShader	(EDevice->m_SelectionShader);
    	DU_impl.DrawAABB			(v[0], v[1], clr_s, clr_w, true, true);
    break;
	case PDSphere:
    	DU_impl.DrawSphere		(parent, v[0], f[4], clr_s, clr_w, true, true);
    	DU_impl.DrawSphere		(parent, v[0], f[3], clr_s, clr_w, true, true);
    break;
	case PDCylinder:{
    	Fvector c,d;
        float h 			= d.sub(v[1],v[0]).magnitude();
        c.add 				(v[0],v[1]).div(2.f);
        if (!fis_zero(h)){
        	d.div			(h);
			DU_impl.DrawCylinder	(parent, c, d, h, f[6], clr_s, clr_w, true, true);
			DU_impl.DrawCylinder	(parent, c, d, h, f[7], clr_s, clr_w, true, true);
        }
    }break;
	case PDCone:{
    	Fvector d;
        float h 			= d.sub(v[1],v[0]).magnitude();
        if (!fis_zero(h)){
            d.div			(h);
            DU_impl.DrawCone		(parent, v[0], d, h, f[6], clr_s, clr_w, true, true);
            DU_impl.DrawCone		(parent, v[0], d, h, f[7], clr_s, clr_w, true, true);
        }
    }break;
	case PDBlob:
		EDevice->SetShader	(EDevice->m_WireShader);
    	DU_impl.DrawCross		(v[0], f[3],f[3],f[3], f[3],f[3],f[3], clr);
    break;
	case PDDisc:
        DU_impl.DrawCylinder		(parent, v[0], v[1], 0.f, f[6], clr_s, clr_w, true, true);
        DU_impl.DrawCylinder		(parent, v[0], v[1], 0.f, f[7], clr_s, clr_w, true, true);
    break;
	case PDRectangle:
        DU_impl.DrawRectangle	(v[0], v[1], v[2], clr_s, clr_w, true, true);
    break;
   }
}

xr_token					domain_token	[ ]={
	{ "Point",				PDPoint		},
	{ "Line",			   	PDLine		},
	{ "Triangle",			PDTriangle	},
	{ "Plane",			   	PDPlane		},
	{ "Box",				PDBox		},
	{ "Sphere",				PDSphere	},
	{ "Cylinder",			PDCylinder	},
	{ "Cone",		   		PDCone		},
	{ "Blob",		   		PDBlob		},
	{ "Disc",		  		PDDisc		},
	{ "Rectangle",	   		PDRectangle	},
	{ 0,					0		   	}
};

void PDomain::OnTypeChange(PropValue* sender)
{               
	ExecCommand				(COMMAND_UPDATE_PROPERTIES);
}

void 	PDomain::FillProp	(PropItemVec& items, LPCSTR pref, u32 clr)
{
    PropValue* V;
    V=PHelper().CreateToken32(items,PrepareKey(pref, "Type"),(u32*)&type,domain_token);
    V->OnChangeEvent.bind	(this,&PDomain::OnTypeChange);
	V->Owner()->prop_color	= TColor(clr);
    
    switch(e_type){
    case vNum:
	    {
		    switch(type){
		    case PDPoint:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Center"), 		&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDLine:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Point 1"), 	&v[0],flt_min,flt_max,0.001f,3); 	V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Point 2"), 	&v[1],flt_min,flt_max,0.001f,3);	V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDTriangle:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Vertex 1"), 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Vertex 2"), 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Vertex 3"), 	&v[2],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDPlane:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Origin"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Normal"), 	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDBox:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Min"),		 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Max"), 	 		&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDSphere:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Center"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Inner"),	&f[3],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat	(items,PrepareKey(pref,"Radius Outer"),	&f[4],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDCylinder:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Point 1"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Point 2"),	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Inner"),	&f[6],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Outer"),	&f[7],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDCone:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Apex"),	 		&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"End Point"),	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Inner"),	&f[6],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Outer"),	&f[7],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDBlob:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Center"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat	(items,PrepareKey(pref,"Radius Outer"),	&f[3],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDDisc:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Center"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Normal"),	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Inner"),	&f[6],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Outer"),	&f[7],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDRectangle:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Origin"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Basis U"),	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Basis V"),	 	&v[2],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    default:
			    {
				    V=PHelper().CreateVector	(items,PrepareKey(pref,"Translate"),	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Rotate"),	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVector	(items,PrepareKey(pref,"Scale"),	 	&v[2],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    }
    		break;
	    }
    case vAngle:
	    {
		    switch(type){
		    case PDPoint:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Center"), 		&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDLine:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Point 1"), 		&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Point 2"), 		&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDTriangle:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Vertex 1"), 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Vertex 2"), 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Vertex 3"), 	&v[2],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDPlane:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Origin"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Normal"), 	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDBox:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Min"),		 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Max"), 	 		&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDSphere:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Center"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat 	(items,PrepareKey(pref,"Radius Inner"),	&f[3],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat 	(items,PrepareKey(pref,"Radius Outer"),	&f[4],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDCylinder:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Point 1"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Point 2"),	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Inner"),	&f[6],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat 	(items,PrepareKey(pref,"Radius Outer"),	&f[7],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDCone:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Apex"),	 		&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"End Point"),	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat 	(items,PrepareKey(pref,"Radius Inner"),	&f[6],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat 	(items,PrepareKey(pref,"Radius Outer"),	&f[7],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDBlob:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Center"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Outer"),	&f[3],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDDisc:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Center"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Normal"),	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Inner"),	&f[6],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat 	(items,PrepareKey(pref,"Radius Outer"),	&f[7],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDRectangle:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Origin"),	 	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Basis U"),	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Basis V"),	 	&v[2],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    default:
			    {
				    V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Translate"),	&v[0],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Rotate"),	 	&v[1],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateAngle3	(items,PrepareKey(pref,"Scale"),	 	&v[2],flt_min,flt_max,0.001f,3);V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    }
    		break;
	    }
    case vColor:
	    {
		    switch(type){
		    case PDPoint:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Center"), 		&v[0]);	V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDLine:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Point 1"), 		&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Point 2"), 		&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDTriangle:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Vertex 1"), 	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Vertex 2"), 	&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Vertex 3"), 	&v[2]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDPlane:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Origin"),	 	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Normal"), 	 	&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDBox:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Min"),		 	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Max"), 	 		&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDSphere:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Center"),	 	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat 	(items,PrepareKey(pref,"Radius Inner"),	&f[3]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Outer"),	&f[4]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDCylinder:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Point 1"),	 	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Point 2"),	 	&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Inner"),	&f[6]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat	(items,PrepareKey(pref,"Radius Outer"),	&f[7]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDCone:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Apex"),	 		&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"End Point"),	&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Inner"),	&f[6]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat  	(items,PrepareKey(pref,"Radius Outer"),	&f[7]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDBlob:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Center"),	 	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat	(items,PrepareKey(pref,"Radius Outer"),	&f[3]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDDisc:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Center"),	 	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Normal"),	 	&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat	(items,PrepareKey(pref,"Radius Inner"),	&f[6]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateFloat	(items,PrepareKey(pref,"Radius Outer"),	&f[7]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    case PDRectangle:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Origin"),	 	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Basis U"),	 	&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Basis V"),	 	&v[2]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    default:
			    {
				    V=PHelper().CreateVColor	(items,PrepareKey(pref,"Translate"),	&v[0]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Rotate"),	 	&v[1]); V->Owner()->prop_color	= TColor(clr);
		    		V=PHelper().CreateVColor	(items,PrepareKey(pref,"Scale"),	 	&v[2]); V->Owner()->prop_color	= TColor(clr);
		    		break;
			    }
		    }
    		break;
	    }
    }
}