/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Shaders
***************************************************************************************
* Anyone may use, copy, modify and distribute this shader file, changed or
* unchanged, as part of the IX-Ray engine, its games and their modifications.
* It may not be sold on its own.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of this file.
**************************************************************************************/
// The test of a sphere against the scene's depth pyramid, for the two levels of the cull. The shader that includes
// this has the view as `view` (SceneView)

// box: the rectangle of the screen, left bottom right top from -1 to 1 before the view's scale; nearest: the least
// depth of what is in it, in the view. returns true when everything the pyramid holds over it is nearer
bool rect_occluded(Texture2D<float> pyramid, float4 box, float nearest)
{
    box = saturate(box.xwzy * float4(view.hiz.x, view.hiz.y, view.hiz.x, view.hiz.y) * float4(0.5f, -0.5f, 0.5f, -0.5f) + 0.5f);

    // The level where the rectangle is at most four texels wide: its texels are read one by one, five a side at
    // most. Four texels of a level where it is two wide take in up to three times what the rectangle covers, and
    // one far pixel in all that keeps a cluster that nothing shows
    float2 size = (box.zw - box.xy) * view.hiz_size.xy;
    float level = clamp(ceil(log2(max(max(size.x, size.y), 1.0f))) - 2.0f, 0.0f, view.hiz_size.z);
    float2 texels = max(view.hiz_size.xy * exp2(-level), 1.0f);
    int2 low = min(int2(box.xy * texels), int2(texels) - 1);
    int2 high = min(min(int2(box.zw * texels), int2(texels) - 1), low + 4);
    float farthest = 0.0f;

    for (int y = low.y; y <= high.y; y++) {
        for (int x = low.x; x <= high.x; x++) {
            farthest = max(farthest, pyramid.Load(int3(x, y, level)));
        }
    }

    return view.hiz_depth.x + view.hiz_depth.y / nearest > farthest;
}

// returns true when everything the pyramid holds over the sphere is nearer than the sphere
bool sphere_occluded(Texture2D<float> pyramid, float4 bounds)
{
    float3 centre = mul(view.view, float4(bounds.xyz, 1.0f)).xyz;
    float radius = bounds.w;

    if (centre.z - radius <= view.hiz.z) {
        return false;
    }

    // The sphere's rectangle on the screen (Mara and McGuire, 2D polyhedral bounds of a clipped, perspective-projected 3D sphere)
    float3 scaled = centre * radius;
    float depth = centre.z * centre.z - radius * radius;
    float side_x = sqrt(centre.x * centre.x + depth);
    float side_y = sqrt(centre.y * centre.y + depth);
    return rect_occluded(pyramid, float4((side_x * centre.x - scaled.z) / (side_x * centre.z + scaled.x), (side_y * centre.y - scaled.z) / (side_y * centre.z + scaled.y),
        (side_x * centre.x + scaled.z) / (side_x * centre.z - scaled.x), (side_y * centre.y + scaled.z) / (side_y * centre.z - scaled.y)), centre.z - radius);
}

// The same for a box along the world's axes, half its size around its centre: the box of that box along the view's
// axes is what is tested. A sphere around flat or long geometry reaches the eye long before its box does, and what
// reaches the eye cannot be tested at all
bool box_occluded(Texture2D<float> pyramid, float3 centre, float3 half_size)
{
    float3 middle = mul(view.view, float4(centre, 1.0f)).xyz;
    float3 reach = float3(dot(abs(view.view[0].xyz), half_size), dot(abs(view.view[1].xyz), half_size), dot(abs(view.view[2].xyz), half_size));
    float3 low = middle - reach;
    float3 high = middle + reach;

    if (low.z <= view.hiz.z) {
        return false;
    }

    // A side that has crossed the eye's axis is widest at the near end of the box, one that has not at its far end
    return rect_occluded(pyramid, float4(low.x / (low.x < 0.0f ? low.z : high.z), low.y / (low.y < 0.0f ? low.z : high.z), high.x / (high.x > 0.0f ? low.z : high.z), high.y / (high.y > 0.0f ? low.z : high.z)), low.z);
}
