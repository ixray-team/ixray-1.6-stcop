#include "common.h"
#include "DX11\tess.h"

void PatchConstantsHS(in InputPatch<p_bumped_new, 3> ip, in uint PatchID : SV_PrimitiveID, out HS_CONSTANT_DATA_OUTPUT Output) {	
	ComputeTessFactor(Output.Edges, Output.Inside);

#ifdef TESS_PN
	float3 N[3] =
	{
		normalize(float3(ip[0].M1.z, ip[0].M2.z, ip[0].M3.z)),
		normalize(float3(ip[1].M1.z, ip[1].M2.z, ip[1].M3.z)),
		normalize(float3(ip[2].M1.z, ip[2].M2.z, ip[2].M3.z))
	};
	
	float3 P[3] = 
	{
		ip[0].position.xyz,
		ip[1].position.xyz,
		ip[2].position.xyz
	};
	
	ComputePNPatch(P, N, Output.patch);
	
	#ifndef TESS_HM
		bool doDiscard = (N[0].z > 0.1f) && (N[1].z > 0.1f) && (N[2].z > 0.1f)
		&& (Output.patch.f3N110.z > 0.1f) && (Output.patch.f3N011.z > 0.1f) && (Output.patch.f3N101.z > 0.1f)
		&& (P[0].z > 5.0f) && (P[1].z > 5.0f) && (P[2].z > 5.0f);
		
		if (doDiscard) {
			Output.Edges[0] = Output.Edges[1] = Output.Edges[2] = Output.Inside = -1;
		}
	#endif
#endif
}

[domain("tri")]
[partitioning("pow2")]
[outputtopology("triangle_ccw")]
[outputcontrolpoints(3)]
[patchconstantfunc("PatchConstantsHS")]

p_bumped_new main(in InputPatch<p_bumped_new, 3> ip, in uint i : SV_OutputControlPointID, in uint PatchID : SV_PrimitiveID) {
	p_bumped_new ouput;
	
	ouput.tcdh = ip[i].tcdh;
	ouput.position = ip[i].position;
	ouput.M1 = ip[i].M1;
	ouput.M2 = ip[i].M2;
	ouput.M3 = ip[i].M3;
	
	ouput.hpos_curr = ip[i].hpos_curr;
	ouput.hpos_old = ip[i].hpos_old;
	
	ouput.hpos = ip[i].hpos;
	
	return ouput;
}


