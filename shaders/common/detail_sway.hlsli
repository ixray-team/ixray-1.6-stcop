#ifndef DETAIL_SWAY_HLSLI
#define DETAIL_SWAY_HLSLI

// How a detail stands and moves: its wind and its trample, for every shader that places a detail's vertex. The
// includer supplies SceneGrassWind (common\scene_tables.hlsli), calc_cyclic and calc_xz_wave.

float3x3 QuaternionToMatrix(float4 q)
{
    float xx = q.x * q.x;
    float yy = q.y * q.y;
    float zz = q.z * q.z;
    float xy = q.x * q.y;
    float xz = q.x * q.z;
    float yz = q.y * q.z;
    float wx = q.w * q.x;
    float wy = q.w * q.y;
    float wz = q.w * q.z;
    
    float3x3 m;
    m[0] = float3(1.0 - 2.0 * (yy + zz), 2.0 * (xy + wz), 2.0 * (xz - wy));
    m[1] = float3(2.0 * (xy - wz), 1.0 - 2.0 * (xx + zz), 2.0 * (yz + wx));
    m[2] = float3(2.0 * (xz + wy), 2.0 * (yz - wx), 1.0 - 2.0 * (xx + yy));
    return m;
}

float windHash(float2 p)
{
    float px = p.x * 123.34 + p.y * 456.21;
    px = frac(px);
    float d = px * (px + 45.32);
    float r = frac(d * d + p.x * p.y);
    return r;
}

float2 windGrad(float2 p)
{
    float n = windHash(p) * 6.2831853;
    return float2(cos(n), sin(n));
}

float windNoise(float2 p)
{
    float2 i = floor(p);
    float2 f = frac(p);
    float2 u = f * f * f * (f * (f * 6.0 - 15.0) + 10.0);

    float v00 = dot(windGrad(i), f);
    float v10 = dot(windGrad(i + float2(1, 0)), f - float2(1, 0));
    float v01 = dot(windGrad(i + float2(0, 1)), f - float2(0, 1));
    float v11 = dot(windGrad(i + float2(1, 1)), f - float2(1, 1));

    float a = v00 + u.x * (v10 - v00);
    float b = v01 + u.x * (v11 - v01);
    return (a + u.y * (b - a)) * 0.5 + 0.5;
}

float windFBM(float2 p, float seed)
{
    float val = windNoise(p + seed * 7.31);
    val += windNoise(p * 2.173 + seed * 3.7) * 0.5;
    return val * 0.6667;
}

// Local: the model's vertex; Frac: its sway weight; World: where it stands without wind. Pos and PosOld come
// in as World and leave as where it is this frame and was the frame before
void DetailSway(SceneGrassWind W, float4 Wave, float4 Dir, float4 WaveOld, float4 DirOld, bool IsStill, float3 Local, float Frac, float Scale, float3 Root, float TrampleVisual, float2 TrampleDir, float3 World, inout float3 Pos, inout float3 PosOld)
{
    if (!IsStill)
    {
    float H = Local.y * Scale;
    float windScale = 1.0f - 0.95f * smoothstep(0.02f, 0.15f, TrampleVisual);
    float globalInt = W.wind_global.x;
    
    if (globalInt > 0.001f)
    {
        float t = Wave.w;
        float2 wp = World.xz;
        float hw = saturate(Local.y);
        float h = max(Pos.y - Root.y, 0.0f);
        
        float xzDensity = 0.0f;
        float2 xzDisp = float2(0, 0);
        
        if (W.wind_global.y > 0.5f)
        {
            float c1 = windNoise(wp * W.wind_xz1.x + W.wind_xz1_dir.xy * t * W.wind_xz1.w);
            c1 = saturate((c1 - 0.5f) * (1.0f + W.wind_xz1.z * 2.0f) + 0.5f);
            float2 perp1 = float2(-W.wind_xz1_dir.y, W.wind_xz1_dir.x);
            float2 dir1 = normalize(W.wind_xz1_dir.xy + perp1 * (c1 - 0.5f) * 2.5f);
            float2 d1 = dir1 * c1 * W.wind_xz1.y * W.wind_xz1_dir.z;
            
            float c2 = windNoise(wp * W.wind_xz2.x + W.wind_xz2_dir.xy * t * W.wind_xz2.w + c1 * 0.3f);
            c2 = saturate((c2 - 0.5f) * (1.0f + W.wind_xz2.z * 2.0f) + 0.5f);
            float2 perp2 = float2(-W.wind_xz2_dir.y, W.wind_xz2_dir.x);
            float2 dir2 = normalize(W.wind_xz2_dir.xy + perp2 * (c2 - 0.5f) * 2.5f);
            float2 d2 = dir2 * c2 * W.wind_xz2.y * W.wind_xz2_dir.z;
            
            float c3 = windNoise(wp * W.wind_xz3.x + W.wind_xz3_dir.xy * t * W.wind_xz3.w + c2 * 0.3f);
            c3 = saturate((c3 - 0.5f) * (1.0f + W.wind_xz3.z * 2.0f) + 0.5f);
            float2 perp3 = float2(-W.wind_xz3_dir.y, W.wind_xz3_dir.x);
            float2 dir3 = normalize(W.wind_xz3_dir.xy + perp3 * (c3 - 0.5f) * 2.5f);
            float2 d3 = dir3 * c3 * W.wind_xz3.y * W.wind_xz3_dir.z;
            
            xzDisp = d1 + d2 + d3;
            xzDensity = saturate((c1 * W.wind_xz1.y * W.wind_xz1_dir.z + c2 * W.wind_xz2.y * W.wind_xz2_dir.z + c3 * W.wind_xz3.y * W.wind_xz3_dir.z)
                / max(W.wind_xz1.y * W.wind_xz1_dir.z + W.wind_xz2.y * W.wind_xz2_dir.z + W.wind_xz3.y * W.wind_xz3_dir.z, 0.01f));
        }
        
        float swirlY = 0.0f;
        if (W.wind_global.z > 0.5f)
        {
            float cs = windNoise(wp * W.wind_swirl.x + W.wind_swirl_dir.xy * t * W.wind_swirl.w);
            cs = saturate((cs - 0.5f) * (1.0f + W.wind_swirl.z * 2.0f) + 0.5f);
            swirlY = (cs - 0.5f) * W.wind_swirl.y;
        }
        
        float2 leanDir = normalize(xzDisp + 0.0001f);
        float gust = xzDensity * xzDensity;
        float bend = hw * hw * windScale;
        float leanAmt = bend * gust * globalInt * 1.8f;
        Pos.xz += leanDir * leanAmt * h;
        
        Pos.y += swirlY * globalInt * hw * h * 0.5f;
        Pos.xz += leanDir * swirlY * globalInt * hw * h * 0.3f;
        
            float t_old = WaveOld.w;
            
            float xzDensity_o = 0.0f;
            float2 xzDisp_o = float2(0, 0);
            
            if (W.wind_global.y > 0.5f)
            {
                float c1o = windNoise(wp * W.wind_xz1.x + W.wind_xz1_dir.xy * t_old * W.wind_xz1.w);
                c1o = saturate((c1o - 0.5f) * (1.0f + W.wind_xz1.z * 2.0f) + 0.5f);
                float2 perp1o = float2(-W.wind_xz1_dir.y, W.wind_xz1_dir.x);
                float2 dir1o = normalize(W.wind_xz1_dir.xy + perp1o * (c1o - 0.5f) * 2.5f);
                float2 d1o = dir1o * c1o * W.wind_xz1.y * W.wind_xz1_dir.z;
                
                float c2o = windNoise(wp * W.wind_xz2.x + W.wind_xz2_dir.xy * t_old * W.wind_xz2.w + c1o * 0.3f);
                c2o = saturate((c2o - 0.5f) * (1.0f + W.wind_xz2.z * 2.0f) + 0.5f);
                float2 perp2o = float2(-W.wind_xz2_dir.y, W.wind_xz2_dir.x);
                float2 dir2o = normalize(W.wind_xz2_dir.xy + perp2o * (c2o - 0.5f) * 2.5f);
                float2 d2o = dir2o * c2o * W.wind_xz2.y * W.wind_xz2_dir.z;
                
                float c3o = windNoise(wp * W.wind_xz3.x + W.wind_xz3_dir.xy * t_old * W.wind_xz3.w + c2o * 0.3f);
                c3o = saturate((c3o - 0.5f) * (1.0f + W.wind_xz3.z * 2.0f) + 0.5f);
                float2 perp3o = float2(-W.wind_xz3_dir.y, W.wind_xz3_dir.x);
                float2 dir3o = normalize(W.wind_xz3_dir.xy + perp3o * (c3o - 0.5f) * 2.5f);
                float2 d3o = dir3o * c3o * W.wind_xz3.y * W.wind_xz3_dir.z;
                
                xzDisp_o = d1o + d2o + d3o;
                xzDensity_o = saturate((c1o * W.wind_xz1.y * W.wind_xz1_dir.z + c2o * W.wind_xz2.y * W.wind_xz2_dir.z + c3o * W.wind_xz3.y * W.wind_xz3_dir.z)
                    / max(W.wind_xz1.y * W.wind_xz1_dir.z + W.wind_xz2.y * W.wind_xz2_dir.z + W.wind_xz3.y * W.wind_xz3_dir.z, 0.01f));
            }
            
            float swirlYo = 0.0f;
            if (W.wind_global.z > 0.5f)
            {
                float cso = windNoise(wp * W.wind_swirl.x + W.wind_swirl_dir.xy * t_old * W.wind_swirl.w);
                cso = saturate((cso - 0.5f) * (1.0f + W.wind_swirl.z * 2.0f) + 0.5f);
                swirlYo = (cso - 0.5f) * W.wind_swirl.y;
            }
            
            float2 leanDir_o = normalize(xzDisp_o + 0.0001f);
            float gust_o = xzDensity_o * xzDensity_o;
            float bend_o = hw * hw * windScale;
            float leanAmt_o = bend_o * gust_o * globalInt * 1.8f;
            PosOld.xz += leanDir_o * leanAmt_o * h;
            
            PosOld.y += swirlYo * globalInt * hw * h * 0.5f;
            PosOld.xz += leanDir_o * swirlYo * globalInt * hw * h * 0.3f;
    }
    else
    {
        float dp = calc_cyclic(dot(World, Wave.xyz) + Wave.w);
        float inten = H * dp * windScale;
        Pos.xz += calc_xz_wave(Dir.xz * inten, Frac);
        
            float dp_old = calc_cyclic(dot(World, WaveOld.xyz) + WaveOld.w);
            float inten_old = H * dp_old * windScale;
            PosOld.xz += calc_xz_wave(DirOld.xz * inten_old, Frac);
    }
    }

    if (TrampleVisual > 0.001f)
    {
        float strength = TrampleVisual;
        float2 moveDir = TrampleDir;

        float baseY = Root.y;
        float hAbove = max(Pos.y - baseY, 0.0f);

        float hw = saturate(Local.y);
        float bendT = saturate((hw - 0.15f) / 0.85f);

        float noise = frac(sin(dot(Root.xz, float2(12.9898, 78.233))) * 43758.5453);
        float arcY = saturate(1.0f - bendT * strength * W.trample_params.y);
        arcY = max(arcY, 0.15f + noise * 0.1f * strength);

        Pos.y = baseY + hAbove * arcY;

        float spreadScale = 1.0f + hAbove * bendT * strength * W.trample_params.x * 0.15f;
        float2 bladeOffset = Pos.xz - Root.xz;
        Pos.xz = Root.xz + bladeOffset * spreadScale;

        float lean = hAbove * bendT * strength * W.trample_params.x;
        lean = min(lean, hAbove * 0.5f);
        Pos.xz += moveDir * lean;

            float hOld = max(PosOld.y - baseY, 0.0f);
            PosOld.y = baseY + hOld * arcY;
            float2 bladeOffsetOld = PosOld.xz - Root.xz;
            PosOld.xz = Root.xz + bladeOffsetOld * spreadScale;
            PosOld.xz += moveDir * lean;
    }
}

#endif
