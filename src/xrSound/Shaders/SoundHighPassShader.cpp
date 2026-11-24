#include "SoundHighPassShader.h"
#include "SoundShaderUtils.h"

using namespace XRay::Sound::ShaderUtils;

void SoundHighPassShader::Reset()
{
	Filter.Reset();
}

void SoundHighPassShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	Filter.Process(Data, Frames, CutoffHz, Q, SlopeStages(Slope, XRay::Sound::Biquad::Cascade::MaxStages));
}
