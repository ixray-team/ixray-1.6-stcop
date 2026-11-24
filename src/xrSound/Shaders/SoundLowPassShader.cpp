#include "SoundLowPassShader.h"
#include "SoundShaderUtils.h"

using namespace XRay::Sound::ShaderUtils;

void SoundLowPassShader::Reset()
{
	Filter.Reset();
}

void SoundLowPassShader::Process(float** Data, u32 Frames, const SoundShaderContext& Context)
{
	Filter.Process(Data, Frames, CutoffHz, Q, SlopeStages(Slope, XRay::Sound::Biquad::Cascade::MaxStages));
}
