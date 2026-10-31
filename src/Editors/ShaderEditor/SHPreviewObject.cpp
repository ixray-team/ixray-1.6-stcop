//---------------------------------------------------------------------------
#include "stdafx.h"
#include "SHPreviewObject.h"
#include "../xrECore/Editor/EditMesh.h"

void CPreviewObject::UpdateClipSpace(const Fmatrix& WVP)
{
	if (!Object)
		return;

	// HACK: inverse of stub_notransform_t screen-space mapping, so stub_notransform shaders render in 3D
	const float W = GfxCmdList::GetTargetWidth();
	const float H = GfxCmdList::GetTargetHeight();

	const bool clip = bNotransform;
	CMatrix* TexGen = clip ? FindTexGenMatrix() : nullptr;
	if (TexGen)
		TexGen->Calculate();

	for (auto it = Object->m_Meshes.begin(); it != Object->m_Meshes.end(); ++it)
	{
		CEditableMesh* M = *it;
		if (!M)
			continue;

		const u32 vc = M->m_Vertices.size();
		if (!vc)
			continue;

		xr_vector<Fvector>& Orig = OrigPos[M];
		if (Orig.empty())
		{
			Orig.resize(vc);
			for (u32 i = 0; i < vc; ++i)
				Orig[i] = M->m_Vertices[i];
		}

		if (clip)
		{
			for (u32 i = 0; i < vc; ++i)
				M->m_Vertices[i] = Orig[i];
			M->GenerateVNormals(nullptr, true);

			for (u32 i = 0; i < vc; ++i)
			{
				Fvector4 c;
				WVP.transform(c, Fvector4(Orig[i].x, Orig[i].y, Orig[i].z, 1.f));
				const float invw = (fabsf(c.w) > EPS_L) ? 1.f / c.w : 0.f;
				const float ndcx = c.x * invw;
				const float ndcy = c.y * invw;
				const float ndcz = c.z * invw;
				M->m_Vertices[i].set((ndcx + 1.f) * 0.5f * W - 0.5f, (1.f - ndcy) * 0.5f * H - 0.5f, ndcz);
			}

			if (TexGen)
			{
				const Fmatrix& View = Device.mView;
				const Fmatrix& Xform = TexGen->xform;
				const bool bCubeRefl = TexGen->dwMode == CMatrix::modeC_refl;

				// FFP texgen emulation: D3DTSS_TCI_CAMERASPACEREFLECTIONVECTOR / D3DTSS_TCI_CAMERASPACENORMAL
				M->m_TexGen = [&](Fvector2& uv, int pindex, const Fvector& N)
				{
					Fvector Pc, Nc, In, T;
					View.transform_tiny(Pc, Orig[pindex]);
					View.transform_dir(Nc, N);
					Nc.normalize_safe();

					if (bCubeRefl)
					{
						Fvector E;
						E.invert(Pc).normalize_safe();
						In.invert(E).mad(Nc, 2.f * E.dotproduct(Nc));
					}
					else
					{
						In = Nc;
					}

					Xform.transform_tiny(T, In);
					uv.set(T.x, T.y);
				};
			}

			M->UnloadRenderBuffers();
			M->GenerateRenderBuffers(true);
			M->m_TexGen = nullptr;
			M->UnloadVNormals();
			bWasClip = true;
		}
		else if (bWasClip)
		{
			for (u32 i = 0; i < vc; ++i)
				M->m_Vertices[i] = Orig[i];
			M->UnloadRenderBuffers();
			M->GenerateRenderBuffers();
			bWasClip = false;
		}
	}
}

CMatrix* CPreviewObject::FindTexGenMatrix() const
{
	if (!Object || Object->m_Surfaces.empty())
		return nullptr;

	ref_shader Sh = Object->m_Surfaces.front()->_Shader();
	if (!Sh || !Sh->E[0] || Sh->E[0]->passes.empty())
		return nullptr;

	ref_matrix_list& List = Sh->E[0]->passes[0]->M;
	if (!List || List->empty())
		return nullptr;

	CMatrix* Mtx = (*List)[0]._get();
	if (Mtx && (Mtx->dwMode == CMatrix::modeC_refl || Mtx->dwMode == CMatrix::modeS_refl))
		return Mtx;

	return nullptr;
}
