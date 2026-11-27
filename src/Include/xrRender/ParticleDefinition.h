#pragma once

class IReader;
class IWriter;

namespace PS
{
	enum EParticleDefinitionFlags : u32
	{
		dfSprite = 1u << 0,
		dfFramed = 1u << 10,
		dfAnimated = 1u << 11,
		dfRandomFrame = 1u << 12,
		dfRandomPlayback = 1u << 13,
		dfTimeLimit = 1u << 14,
		dfAlignToPath = 1u << 15,
		dfCollision = 1u << 16,
		dfCollisionDel = 1u << 17,
		dfVelocityScale = 1u << 18,
		dfCollisionDyn = 1u << 19,
		dfWorldAlign = 1u << 20,
		dfFaceAlign = 1u << 21,
		dfCulling = 1u << 22,
		dfCullCCW = 1u << 23,
	};

	namespace PE
	{
		enum class Version : u16
		{
			Original = 1,
			Extended,
			MAX,
			Latest = MAX - 1,
		};

		enum class Chunks : u32
		{
			VERSION = 1,
			NAME,
			EFFECTDATA,
			ACTIONLIST,
			FLAGS,
			FRAME,
			SPRITE,
			TIMELIMIT,
			TIMELIMIT2,
			SOURCETEXT_ = 0x20,
			COLLISION,
			VEL_SCALE,
			EDATA = 0x24,
			ALIGN_TO_PATH,
		};
	}
}

class IParticleDefinition
{
public:
	virtual ~IParticleDefinition() = default;
	virtual const char* Name() const = 0;
	virtual void SetName(const char* name) = 0;
	virtual void Save(IWriter& writer) = 0;
	virtual bool LoadOriginal(IReader& reader) = 0;
	virtual void CreateShader() = 0;
	virtual void DestroyShader() = 0;
	virtual int GetMaxParticles() const = 0;
	virtual void SetMaxParticles(int count) = 0;
	virtual float GetTimeLimit() const = 0;
	virtual bool HasTimeLimit() const = 0;
	virtual void SetTimeLimit(float seconds) = 0;
	virtual const shared_str& GetShaderName() const = 0;
	virtual const shared_str& GetTextureName() const = 0;
	virtual void SetSprite(const char* shader, const char* texture) = 0;
	virtual u32 GetCompiledActionBytes() const = 0;
	virtual bool GetSourceRate(float& rate) const = 0;
	virtual bool SetSourceRate(float rate) = 0;
};
