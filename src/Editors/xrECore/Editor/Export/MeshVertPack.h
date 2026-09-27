#pragma once

template <int MX, int MY, int MZ>
struct MeshVertPackGrid
{
	Fvector Min;
	Fvector Scale;
	Fvector Eps;
	U32Vec Cells[MX + 1][MY + 1][MZ + 1];

	void Init(const Fbox& Bb, int ApxVertices)
	{
		Scale.set(Bb.max.x - Bb.min.x + EPS, Bb.max.y - Bb.min.y + EPS, Bb.max.z - Bb.min.z + EPS);
		Min.set(Bb.min).sub(EPS);
		Eps.set(Scale.x / MX / 2, Scale.y / MY / 2, Scale.z / MZ / 2);
		Eps.x = (Eps.x < EPS_L) ? Eps.x : EPS_L;
		Eps.y = (Eps.y < EPS_L) ? Eps.y : EPS_L;
		Eps.z = (Eps.z < EPS_L) ? Eps.z : EPS_L;

		const int Size = (MX + 1) * (MY + 1) * (MZ + 1);
		const int Average = (ApxVertices / Size) / 2;
		for (int Ix = 0; Ix < MX + 1; ++Ix)
			for (int Iy = 0; Iy < MY + 1; ++Iy)
				for (int Iz = 0; Iz < MZ + 1; ++Iz)
					Cells[Ix][Iy][Iz].reserve(Average);
	}

	template <class Vert, class TGetPos>
	u16 Pack(xr_vector<Vert>& Verts, Vert& V, TGetPos GetPos, bool FailIfFull)
	{
		Fvector& Pos = GetPos(V);
		u32 P = 0xffffffff;

		const u32 Ix = iFloor(float(Pos.x - Min.x) / Scale.x * MX);
		const u32 Iy = iFloor(float(Pos.y - Min.y) / Scale.y * MY);
		const u32 Iz = iFloor(float(Pos.z - Min.z) / Scale.z * MZ);
		R_ASSERT(Ix <= MX && Iy <= MY && Iz <= MZ);

		int SimilarPos = -1;
		{
			U32Vec& Cell = Cells[Ix][Iy][Iz];
			for (u32 Idx : Cell)
			{
				Vert& Src = Verts[Idx];
				if (Src.similar_pos(V))
				{
					if (Src.similar(V))
					{
						P = Idx;
						break;
					}
					SimilarPos = (int)Idx;
				}
			}
		}

		if (P == 0xffffffff)
		{
			if (SimilarPos >= 0)
				Pos.set(GetPos(Verts[SimilarPos]));

			P = Verts.size();
			if (FailIfFull && P >= 0xFFFF)
				return 0xffff;

			Verts.push_back(V);
			Cells[Ix][Iy][Iz].push_back(P);

			const u32 IxE = iFloor(float(Pos.x + Eps.x - Min.x) / Scale.x * MX);
			const u32 IyE = iFloor(float(Pos.y + Eps.y - Min.y) / Scale.y * MY);
			const u32 IzE = iFloor(float(Pos.z + Eps.z - Min.z) / Scale.z * MZ);
			R_ASSERT(IxE <= MX && IyE <= MY && IzE <= MZ);

			if (IxE != Ix)
				Cells[IxE][Iy][Iz].push_back(P);
			if (IyE != Iy)
				Cells[Ix][IyE][Iz].push_back(P);
			if (IzE != Iz)
				Cells[Ix][Iy][IzE].push_back(P);
			if ((IxE != Ix) && (IyE != Iy))
				Cells[IxE][IyE][Iz].push_back(P);
			if ((IxE != Ix) && (IzE != Iz))
				Cells[IxE][Iy][IzE].push_back(P);
			if ((IyE != Iy) && (IzE != Iz))
				Cells[Ix][IyE][IzE].push_back(P);
			if ((IxE != Ix) && (IyE != Iy) && (IzE != Iz))
				Cells[IxE][IyE][IzE].push_back(P);
		}

		VERIFY(P < u16(-1));
		return (u16)P;
	}
};
