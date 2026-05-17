#include "stdafx.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "light_point.h"
#include "xrFace.h"

extern void Jitter_Select	(Fvector2* &Jitter, u32& Jcount);
 
// Освещение
void CDeflector::Light(CDB::COLLIDER* DB, base_lighting* LightsSelected)
{
	// Geometrical bounds
	Fbox bb;	
	bb.invalidate();
 	for (u32 fid = 0; fid < UVpolys.size(); fid++)
	{
		Face* F = UVpolys[fid].owner;
		for (int i = 0; i < 3; i++)
			bb.modify(F->v[i]->P);
	}
	bb.getsphere(Sphere.P, Sphere.R);
	 

	// Convert lights to local form
	LightsSelected->select(inlc_global_data()->L_static(), Sphere.P, Sphere.R);
	lm_layer& lm = layer;

	auto Light = [&](CDB::COLLIDER* DB, base_lighting* LightsSelected)
	{
 		// UV
		RemapUV(0, 0, lm.width, lm.height, lm.width, lm.height, false);
			 
		// Calculate
 		lm.create(lm.width, lm.height);
		L_Direct(DB, LightsSelected);
	};

	// Calculate and fill borders
	Light(DB, LightsSelected);
 
	lm.ApplyBordersFast(0);

	// Compression
 	if (lm.compress_Zero()) return;		// already with borders
    
	u32 BORDER = gCompilerMode.LC_BORDER;;
 	if (layer.width == 1)
	{
		// Horizontal ZERO - vertical line
		lm_layer		T;
		T.create(2 * BORDER, layer.height + 2 * BORDER);

		// Transfer
		for (u32 y = 0; y < T.height; y++)
		{
			int			py = int(y) - BORDER;
			clamp(py, 0, int(layer.height - 1));
			base_color	C = layer.surface[py];
			T.surface[y * 2 + 0] = C;
			T.marker[y * 2 + 0] = 255;
			T.surface[y * 2 + 1] = C;
			T.marker[y * 2 + 1] = 255;
		}

		// Exchange
		T.width = 0;
		T.height = layer.height;
		layer = T;
	}
	else if (layer.height == 1)
	{
		// Vertical ZERO - horizontal line
		lm_layer		T;
		T.create(layer.width + 2 * BORDER, 2 * BORDER);

		// Transfer
		for (u32 x = 0; x < T.width; x++)
		{
			int			px = int(x) - BORDER;
			clamp(px, 0, int(layer.width - 1));
			base_color	C = layer.surface[px];
			T.surface[0 * T.width + x] = C;
			T.marker[0 * T.width + x] = 255;
			T.surface[1 * T.width + x] = C;
			T.marker[1 * T.width + x] = 255;
		}

		// Exchange
		T.width = layer.width;
		T.height = 0;
		layer = T;
	}
	else
	{
		// Generic blit
		lm_layer		lm_old = layer;
		lm_layer		lm_new;
		lm_new.create(lm_old.width + 2 * BORDER, lm_old.height + 2 * BORDER);
		lblit(lm_new, lm_old, BORDER, BORDER, 255 - BORDER);
		layer = lm_new;

 		lm.ApplyBordersFast(0);

		layer.width = lm_old.width;
		layer.height = lm_old.height;
	}
}
 
#include "uv_grid.h"
thread_local UVGridLazy<UVtri> uv_grid_embree;
thread_local xr_vector<JiterPixel> EmbreePacket;

void CDeflector::L_Direct	(CDB::COLLIDER* DB, base_lighting* LightsSelected)
{
	u32 flags = LGetCurrentFlags();
 
 	auto EdgeProcessing = [&](CDB::COLLIDER* DB, base_lighting* LightsSelected, Fvector2& p1, Fvector2& p2, Fvector& v1, Fvector& v2, Fvector& N, float texel_size, Face* skip)
		{
			Fvector		vdir;
			vdir.sub(v2, v1);

			lm_layer& lm = layer;

			Fvector2		size;
			size.x = p2.x - p1.x;
			size.y = p2.y - p1.y;
			int	du = iCeil(std::abs(size.x) / texel_size);
			int	dv = iCeil(std::abs(size.y) / texel_size);
			int steps = std::max(du, dv);
			if (steps <= 0)	return;

			for (int I = 0; I <= steps; I++)
			{
				float	time = float(I) / float(steps);
				Fvector2	uv;
				uv.x = size.x * time + p1.x;
				uv.y = size.y * time + p1.y;
				int	_x = iFloor(uv.x * float(lm.width));
				int _y = iFloor(uv.y * float(lm.height));

				if ((_x < 0) || (_x >= (int)lm.width))	continue;
				if ((_y < 0) || (_y >= (int)lm.height))	continue;
				if (lm.marker[_y * lm.width + _x])		continue;

				// ok - perform lighting
				base_color_c	C;
				Fvector			P;
				P.mad(v1, vdir, time);
				LightPoint(DB, inlc_global_data()->RCAST_Model(), C, P, N, *LightsSelected, flags, skip); //.

				C.mul(.5f);
				lm.surface[_y * lm.width + _x]._set(C);
				lm.marker[_y * lm.width + _x] = 255;
			}
		};

	R_ASSERT	(DB);
	R_ASSERT	(LightsSelected);

	lm_layer&	lm = layer;

	// Setup variables
	Fvector2	dim,half;
	dim.set		(float(lm.width),float(lm.height));
	half.set	(.5f/dim.x,.5f/dim.y);
	
	// Jitter data
	Fvector2	JS;
	JS.set		(.4999f/dim.x, .4999f/dim.y);
	
	u32			Jcount;
	Fvector2*	Jitter;
	Jitter_Select(Jitter, Jcount);

	// вычисляем AABB для каждого треугольника и нормализуем UV
	Fbox2 bounds;
	Bounds_Summary(bounds);
	for (auto& T : UVpolys)
		T.computeAABB(bounds);
	uv_grid_embree.reset();

	// Change to Packet Edition
	if (gCompilerMode.Embree)
	{
		u32 TILE_SIZE = 8;
 		// Lighting itself
 		for (u32 Y = 0; Y < lm.height; Y += TILE_SIZE)
		{
			for (u32 X = 0; X < lm.width; X += TILE_SIZE)
			{
 				EmbreePacket.clear();
				for (auto tX = X; tX < std::min(lm.width, X + TILE_SIZE); tX++)
				for (auto tY = Y; tY < std::min(lm.height, Y + TILE_SIZE); tY++)
				{
 					for (u32 J = 0; J < Jcount; J++)
					{
						// LUMEL space
						Fvector2 P;
						P.x = float(tX) / dim.x + half.x + Jitter[J].x * JS.x;
						P.y = float(tY) / dim.y + half.y + Jitter[J].y * JS.y;

						// World space
						Fvector		wP, wN, B;
						for (auto TRI : uv_grid_embree.query(P.x, P.y, UVpolys))
						{
							if (TRI->isInside(P, B))
							{
								// We found triangle and have barycentric coords
 								GetBarycentricNormalized(TRI->owner, wP, wN, B);
								EmbreePacket.emplace_back().SetDataRays(tY, tX, wP, wN, TRI->owner);
								break;
							}
						}
					}
 				}

				LightPoint_Jitters(EmbreePacket, lc_global_data()->L_static(), LGetCurrentFlags());

				for (auto& WP : EmbreePacket)
				{
					u32 taskID = WP.V * lm.width + WP.U;
					lm.surface[taskID]._add(WP.C);
					lm.samples[taskID] += 1;
				}
			}
		}
 
		for (u32 Y = 0; Y < lm.height; Y += 1)
 		for (u32 X = 0; X < lm.width;  X += 1)
		{
			u32 taskID = Y * lm.width + X;
  			// Apply Colors !
			u32 Samples = lm.samples[taskID];
			if (Samples)
			{
				// Calculate lighting amount
				base_color_c C;
				lm.surface[taskID]._get(C);
				C.scale(Samples);
				C.mul(.5f);
				lm.surface[taskID]._set(C);
				lm.marker[taskID] = 255;
			}
		}
	}
	else
	{
		// Lighting itself
		DB->ray_options(0);
		for (u32 V = 0; V < lm.height; V++)
		{
			for (u32 U = 0; U < lm.width; U++)
			{
				u32				Fcount = 0;
				base_color_c	C;
				for (u32 J = 0; J < Jcount; J++)
				{
					// LUMEL space
					Fvector2 P;
					P.x = float(U) / dim.x + half.x + Jitter[J].x * JS.x;
					P.y = float(V) / dim.y + half.y + Jitter[J].y * JS.y;

					// World space
					Fvector		wP, wN, B;
					for (auto TRI : uv_grid_embree.query(P.x, P.y, UVpolys))
					{
						if (TRI->isInside(P, B))
						{
							// We found triangle and have barycentric coords
							Face* F = TRI->owner;
							GetBarycentricNormalized(F, wP, wN, B);
							LightPoint(DB, inlc_global_data()->RCAST_Model(), C, wP, wN, *LightsSelected, flags, F);
							Fcount += 1;

							break;
						}
					}
				}

				if (Fcount)
				{
					C.scale(Fcount);
					C.mul(.5f);
					lm.surface[V * lm.width + U]._set(C);
					lm.marker[V * lm.width + U] = 255;
				}
				else
				{
					lm.surface[V * lm.width + U]._set(C);	// 0-0-0-0-0
					lm.marker[V * lm.width + U] = 0;
				}
			}
		}

	}
	

	// *** Render Edges
	float texel_size = (1.f/float(std::max(lm.width,lm.height)))/8.f;
	for (u32 t=0; t<UVpolys.size(); t++)
	{
		UVtri&		T	= UVpolys[t];
		Face*		F	= T.owner;
		R_ASSERT	(F);
		EdgeProcessing(DB,LightsSelected, T.uv[0], T.uv[1], F->v[0]->P, F->v[1]->P, F->N, texel_size,F);
		EdgeProcessing(DB,LightsSelected, T.uv[1], T.uv[2], F->v[1]->P, F->v[2]->P, F->N, texel_size,F);
		EdgeProcessing(DB,LightsSelected, T.uv[2], T.uv[0], F->v[2]->P, F->v[0]->P, F->N, texel_size,F);
	}
}
