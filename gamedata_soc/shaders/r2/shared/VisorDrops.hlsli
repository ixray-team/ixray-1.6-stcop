// Procedural visor droplets.
#ifndef VISOR_DROPS_HLSLI
#define VISOR_DROPS_HLSLI

#define _FixedDroplet 0.5
#define _DropletSize 0.5
#define _DropletDensity 1.85
#define _Speed .4

float3 N13(float p) {
	float3 p3 = frac(float3(p,p,p) * float3(.1031,.11369,.13787));
	p3 += dot(p3, p3.yzx + 19.19);
	return frac(float3((p3.x + p3.y)*p3.z, (p3.x+p3.z)*p3.y, (p3.y+p3.z)*p3.x));
}
float N(float t) {
	return frac(sin(t*12345.564)*7658.76);
}
float Saw(float b, float t) {
	return smoothstep(0., b, t)*smoothstep(1., b, t);
}

float2 DropLayer2(float2 uv, float t) {
	float2 UV = uv;
	
	uv.y += t * 0.75;
	float2 a = float2(6, 1);
	float2 grid = a * 2.;
	float2 id = floor(uv * grid);
	
	float colShift = N(id.x); 
	uv.y += colShift;
	
	id = floor(uv * grid);
	float3 n = N13(id.x * 35.2 + id.y * 2376.1);
	float2 st = frac(uv * grid) - float2(0.5, 0);
	
	float x = n.x - 0.5;
	
	float y = UV.y * 20;
	float wiggle = sin(y + sin(y));
	x += wiggle * (0.5 - abs(x)) * (n.z - 0.5);
	x *= _FixedDroplet;
	float ti = frac(t + n.z);
	y = (Saw(0.85, ti) - 0.5) * 0.9 + 0.5;
	float2 p = float2(x, y);
	
	float d = length((st - p) * a.yx);
	
	float mainDrop = smoothstep(_DropletSize, 0, d);
	
	float r = sqrt(smoothstep(1, y, st.y));
	float cd = abs(st.x - x);
	float trail = smoothstep(0.23 * r, 0.15 * r * r, cd);
	float trailFront = smoothstep(-0.02, 0.02, st.y - y);
	trail *= trailFront * r * r;
	
	y = UV.y;
	float trail2 = smoothstep(0.2 * r, 0.0, cd);
	float droplets = max(0, (sin(y * (1 - y) * 120) - st.y)) * trail2 * trailFront*n.z;
	y = frac(y*10.)+(st.y-.5);
	float dd = length(st-float2(x, y));
	droplets = smoothstep(.3, 0., dd);
	float m = mainDrop+droplets*r*trailFront; 
	
	return float2(m, trail);
}

float StaticDrops(float2 uv, float t) {
	uv *= 40.;
	
	float2 id = floor(uv);
	uv = frac(uv)-.5;
	float3 n = N13(id.x * 107.45 + id.y * 3543.654);
	float2 p = (n.xy-.5)*.7;
	float d = length(uv-p);
	
	float fade = Saw(.025, frac(t+n.z));
	float c = smoothstep(.3, 0., d)*frac(n.z*10.)*fade;
	return c;
}

float2 Drops(float2 uv, float t, float l0, float l1, float l2) {
	float s = StaticDrops(uv, t)*l0; 
	float2 m1 = DropLayer2(uv, t)*l1;
	float2 m2 = DropLayer2(uv * _DropletDensity, t) * l2;
	
	float c = max(s, max(m1.x, m2.x));
	c = smoothstep(0.3, 1, c);
	
	return float2(c, max(m1.y * l0, m2.y * l1));
}

float2 DistortUV(float2 tc, float rainAmount, float Time, float2 Aspect) {
	float2 uv = (tc.xy - 0.5) * Aspect + 0.5;
	float staticDrops = smoothstep(.0, 1., rainAmount) * 2.;
	float layer1 = smoothstep(.25, .75, rainAmount);
	float layer2 = smoothstep(.0, .5, rainAmount);

	float t = Time * _Speed; float2 e = float2(0.001, 0.);
	float2 c = Drops(uv, t, staticDrops, layer1, layer2);
	float cx = Drops(uv + e, t, staticDrops, layer1, layer2).x;
	float cy = Drops(uv + e.yx, t, staticDrops, layer1, layer2).x;

	return float2(cx - c.x, cy - c.x) * 0.25f;
}
// Shader Model 3 needs one height evaluation to stay within its instruction limit.
float2 DistortUVSM3(float2 tc, float rainAmount, float Time, float2 Aspect, float2 Resolution)
{
	float2 uv = (tc - 0.5f) * Aspect + 0.5f;
	float staticDrops = smoothstep(0.f, 1.f, rainAmount) * 2.f;
	float layer1 = smoothstep(0.25f, 0.75f, rainAmount);
	float layer2 = smoothstep(0.f, 0.5f, rainAmount);
	float height = Drops(uv, Time * _Speed, staticDrops, layer1, layer2).x;
	return float2(ddx(height), ddy(height)) * Resolution / Aspect * 0.00025f;
}

#endif
