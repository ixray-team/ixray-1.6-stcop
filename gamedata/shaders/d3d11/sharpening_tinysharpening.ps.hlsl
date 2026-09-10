/*
	Copyright (c) 2026 LVutner

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.


	[TinySharpen]
	A shrimple & subtle sharpen effect for ReShade.

	Author: LVutner
	https://github.com/LVutner/reeShaders
	https://www.patreon.com/LVutner
*/

#include "common.hlsli"

uniform float4 screen_res;
float sharpening_intensity;

float4 main(PSInputFullscreen I) : SV_Target
{
	float4 center = s_image.SampleLevel(smp_rtlinear, I.texcoord, 0);
	//center = 1.0; //I don't think you store anything in alpha, so let's overwrite it

	float2 gather_texcoord = I.texcoord + 0.5 * screen_res.zw;

	float2 gather0 = s_image.GatherGreen(smp_rtlinear, gather_texcoord).xz;
	float2 gather1 = s_image.GatherGreen(smp_rtlinear, gather_texcoord, int2(-1, -1)).xz;

	float4 t = float4(gather0.xy, gather1.xy);
	float laplacian = center.y * 4.0 - dot(t, 1.0);

	laplacian *= rcp(1.0 + 30.0 * abs(laplacian));
	laplacian *= sqrt(dot(t, t) + 1e-6); //scale it down, to prevent oversharpening in shadows

	center.xyz *= (sharpening_intensity * laplacian + center.y) / (center.y + 1e-6);
	return center;
}

