#include "stdafx.h"
#include "dxLensFlareRender.h"
#include "../../xrEngine/xr_efflensflare.h"
#include "../../xrEngine/IGame_Persistent.h"

#define MAX_Flares	24
#define FAR_DIST RFrame.Env().far_plane

void dxFlareRender::Copy(IFlareRender &_in)
{
	*this = *(dxFlareRender*)&_in;
}

void dxFlareRender::CreateShader(const char* sh_name, const char* tex_name)
{
	if(tex_name && tex_name[0])
		hShader.create(sh_name, tex_name);
}

void dxFlareRender::DestroyShader()
{
	hShader.destroy();
}

void dxLensFlareRender::Copy(ILensFlareRender &_in)
{
	*this = *(dxLensFlareRender*)&_in;
}

dxLensFlareRender* dxLensFlareRender::Frame = nullptr;

dxLensFlareRender::~dxLensFlareRender()
{
	Frame = Frame == this ? nullptr : Frame;
}

void dxLensFlareRender::Take(CLensFlare& Owner)
{
	Frame = Owner.bRender && Owner.m_Current ? static_cast<dxLensFlareRender*>(&*Owner.m_pRender) : nullptr;

	if (Frame)
	{
		Frame->State = { Owner.LightColor, Owner.vecLight, Owner.vecX, Owner.vecY, Owner.vecDir, Owner.vecAxis, Owner.vecCenter, Owner.fBlend, Owner.fGradientValue, Owner.m_StateBlend, Owner.m_Current };
	}
}

void dxLensFlareRender::Draw(bool bSun, bool bFlares, bool bGradient)
{
	if (Frame)
	{
		Frame->DrawState(bSun, bFlares, bGradient);
	}
}

// For a caller outside the frame's collect
void dxLensFlareRender::Render(CLensFlare &owner, bool bSun, bool bFlares, bool bGradient)
{
	Take(owner);
	Draw(bSun, bFlares, bGradient);
}

void dxLensFlareRender::DrawState(bool bSun, bool bFlares, bool bGradient)
{
	FrameState& owner = State;
	PROF_EVENT("dxLensFlareRender::Render");
	GfxCmdList& Cmd = RImplementation.Imm();
	Fcolor dwLight;
	Fcolor color;
	Fvector vec, vecSx, vecSy;
	Fvector vecDx, vecDy;
	float fDistance = FAR_DIST * 0.75f;
	dwLight.set(owner.LightColor);
	FixedVector<ref_shader, MAX_Flares> _2render;

	u32 VS_Offset;
	struct LITF
	{
		struct
		{
			Fvector p; u32 color; Fvector2 t;
		} buff[4];
	};
	DynamicGeometry& Stream = RImplementation.Dynamic;
	LITF* pv = (LITF*)Stream.Lock(MAX_Flares * 4, EVertexFormat::LIT, VS_Offset);
	Fvector& vecLight = owner.vecLight;
	if (bSun)
	{
		if (owner.m_Current->m_Flags.is(CLensFlareDescriptor::flSource))
		{
			vecSx.mul(owner.vecX, owner.m_Current->m_Source.fRadius*fDistance);
			vecSy.mul(owner.vecY, owner.m_Current->m_Source.fRadius*fDistance);
			if (owner.m_Current->m_Source.ignore_color)
				color.set(1.f,1.f,1.f,1.f);
			else
				color.set(dwLight);
			color.a *= owner.m_StateBlend;
			u32 c = color.get();
			
			*pv =
			{
				vecLight + vecSx - vecSy, c, {0.f, 0.f},
				vecLight + vecSx + vecSy, c, {0.f, 1.f},
				vecLight - vecSx - vecSy, c, {1.f, 0.f},
				vecLight - vecSx + vecSy, c, {1.f, 1.f}
			};
			pv++;
			_2render.push_back(((dxFlareRender*)&*owner.m_Current->m_Source.m_pRender)->hShader);
		}
	}
	if (owner.fBlend>=EPS_L)
	{
		if(bFlares)
		{
			vecDx.normalize (owner.vecAxis);
			vecDy.crossproduct (vecDx, owner.vecDir);
			if (owner.m_Current->m_Flags.is(CLensFlareDescriptor::flFlare))
			{
				for(CLensFlareDescriptor::SFlare& F : owner.m_Current->m_Flares)
				{
					vec.mul(owner.vecAxis, F.fPosition);
					vec.add(owner.vecCenter);
					vecSx.mul(F.fPosition == 1.0f ? owner.vecX : vecDx, F.fRadius*fDistance);//���� ���� � ������ ������ �� �� ����� ��� �������
					vecSy.mul(F.fPosition == 1.0f ? owner.vecY : vecDy, F.fRadius*fDistance);
					color.set(dwLight);
					color.mul_rgba(F.fOpacity*owner.fBlend*owner.m_StateBlend);
					u32 c = color.get();
					*pv =
					{
						vec + vecSx - vecSy, c, {0.f, 0.f},
						vec + vecSx + vecSy, c, {0.f, 1.f},
						vec - vecSx - vecSy, c, {1.f, 0.f},
						vec - vecSx + vecSy, c, {1.f, 1.f}
					};
					pv++;
					_2render.push_back(((dxFlareRender*)&*F.m_pRender)->hShader);
				}
			}
		}
		// gradient
		if (bGradient&&(owner.fGradientValue>=EPS_L))
		{
			if (owner.m_Current->m_Flags.is(CLensFlareDescriptor::flGradient))
			{
				vecSx.mul(owner.vecX, owner.m_Current->m_Gradient.fRadius*owner.fGradientValue*fDistance);
				vecSy.mul(owner.vecY, owner.m_Current->m_Gradient.fRadius*owner.fGradientValue*fDistance);

				color.set(dwLight);
				color.mul_rgba(owner.fGradientValue * owner.m_StateBlend);

				u32 c = color.get();
				*pv =
				{
					
					owner.vecLight + vecSx - vecSy, c, 0, 0,
					owner.vecLight + vecSx + vecSy, c, 0, 1,
					owner.vecLight - vecSx - vecSy, c, 1, 0,
					owner.vecLight - vecSx + vecSy, c, 1, 1
				};
				pv++;

				_2render.push_back(((dxFlareRender*)&*owner.m_Current->m_Gradient.m_pRender)->hShader);
			}
		}
	}
	Stream.Unlock(_2render.size() * 4);

	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetGeometry(hGeom);

	for(u32 i = 0; i < _2render.size(); i++)
	{
		if (_2render[i])
		{
			u32 vBase = i * 4 + VS_Offset;
			Cmd.SetPass(_2render[i]);
			Stream.DrawQuads(Cmd, 1, vBase);
		}
	}
}

void dxLensFlareRender::OnDeviceCreate()
{
	hGeom.create(EGeomSource::Ring);
}

void dxLensFlareRender::OnDeviceDestroy()
{
	hGeom.destroy();
}