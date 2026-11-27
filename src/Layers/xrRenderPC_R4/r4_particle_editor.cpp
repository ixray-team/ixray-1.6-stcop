#include "stdafx.h"
#include "r4.h"
#include "ParticleEffect.h"
#include "particle_core/particle_holder.h"

bool PS::CPEDef::GetSourceRate(float& rate) const
{
	if (m_Actions.size() < sizeof(u32))
		return false;
	IReader reader(const_cast<CMemoryWriter&>(m_Actions).pointer(), m_Actions.size());
	bool valid = true;
	PAPI::ParticleHolder holder;
	holder.ActionsValid = &valid;
	holder.LoadActions(reader);
	if (!valid || !reader.eof())
		return false;
	for (const PAPI::ParticleAction* action : holder.m_actions)
	{
		if (action != nullptr && action->type == PAPI::PASourceID)
		{
			rate = static_cast<const PAPI::PASource*>(action)->particle_rate;
			return _valid(rate) && rate >= 0.f;
		}
	}
	return false;
}

bool PS::CPEDef::SetSourceRate(float rate)
{
	if (!_valid(rate) || rate < 0.f || rate > 1000000.f)
		return false;
	if (m_Actions.size() == 0)
		m_Actions.w_u32(0);
	IReader reader(m_Actions.pointer(), m_Actions.size());
	bool valid = true;
	PAPI::ParticleHolder holder;
	holder.ActionsValid = &valid;
	holder.LoadActions(reader);
	if (!valid || !reader.eof())
		return false;
	for (PAPI::ParticleAction* action : holder.m_actions)
	{
		if (action != nullptr && action->type == PAPI::PASourceID)
		{
			static_cast<PAPI::PASource*>(action)->particle_rate = rate;
			m_Actions.clear();
			holder.SaveActions(m_Actions);
			return true;
		}
	}

	PAPI::PASource source = {};
	source.type = PAPI::PASourceID;
	source.position = PAPI::pDomain(PAPI::PDPoint);
	source.velocity = PAPI::pDomain(PAPI::PDPoint);
	source.rot = PAPI::pDomain(PAPI::PDPoint);
	source.rot_vel = PAPI::pDomain(PAPI::PDPoint);
	source.size = PAPI::pDomain(PAPI::PDPoint, 1.f, 1.f, 1.f);
	source.color = PAPI::pDomain(PAPI::PDPoint, 1.f, 1.f, 1.f);
	source.random_alpha = false;
	source.alpha = 1.f;
	source.alpha2 = 1.f;
	source.particle_rate = rate;
	source.age = 1.f;
	source.age_sigma = 0.f;
	source.parent_vel.set(0.f, 0.f, 0.f);
	source.parent_motion = 0.f;
	holder.m_actions.push_back(new PAPI::PASource(source));
	m_Actions.clear();
	holder.SaveActions(m_Actions);
	return true;
}

float CRender::particle_source_rate(IRenderVisual* visual) const
{
	const PS::CParticleEffect* effect = dynamic_cast<const PS::CParticleEffect*>(visual);
	if (effect == nullptr)
		return -1.f;
	for (const PAPI::ParticleAction* action : effect->Pholder.m_actions)
	{
		if (action != nullptr && action->type == PAPI::PASourceID)
			return static_cast<const PAPI::PASource*>(action)->particle_rate;
	}
	return -1.f;
}

bool CRender::set_particle_source_rate(IRenderVisual* visual, float rate)
{
	PS::CParticleEffect* effect = dynamic_cast<PS::CParticleEffect*>(visual);
	if (effect == nullptr || effect->m_Def == nullptr || !_valid(rate) || rate < 0.f || rate > 1000000.f)
		return false;
	for (PAPI::ParticleAction* action : effect->Pholder.m_actions)
	{
		if (action == nullptr || action->type != PAPI::PASourceID)
			continue;
		PAPI::PASource* source = static_cast<PAPI::PASource*>(action);
		const float previous = source->particle_rate;
		source->particle_rate = rate;
		if (effect->m_Def->SetSourceRate(rate))
			return true;
		source->particle_rate = previous;
		return false;
	}
	return false;
}

namespace ViewerParticleValidationDetail
{
	class CBoundedReader final : public IReader
	{
	public:
		CBoundedReader(const void* bytes, size_t size) : IReader(const_cast<void*>(bytes), size) {}
		bool Valid = true;

		intptr_t elapsed() const override { return IReader::elapsed(); }
		intptr_t tell() const override { return IReader::tell(); }
		intptr_t length() const override { return IReader::length(); }
		void seek(intptr_t position) override
		{
			if (position < 0 || position > length())
			{
				Valid = false;
				return;
			}
			IReader::seek(position);
		}
		void advance(intptr_t count) override
		{
			if (count < 0 || count > elapsed())
			{
				Valid = false;
				return;
			}
			IReader::advance(count);
		}
		void r(void* destination, intptr_t count) override
		{
			if (count < 0 || count > elapsed())
			{
				Valid = false;
				return;
			}
			if (count != 0)
				Memory.mem_copy(destination, pointer(), count);
			IReader::advance(count);
		}
	};

	bool HasTerminatedString(const void* data, u32 size, u32 offset = 0)
	{
		const u8* bytes = static_cast<const u8*>(data);
		for (u32 index = offset; index < size; ++index)
		{
			if (bytes[index] == 0)
				return true;
		}
		return false;
	}

	bool HasTwoStrings(const void* data, u32 size)
	{
		const u8* bytes = static_cast<const u8*>(data);
		if (size < 3 || bytes[0] == 0)
			return false;
		for (u32 first = 1; first < size; ++first)
		{
			if (bytes[first] != 0)
				continue;
			return first + 1 < size && bytes[first + 1] != 0 && HasTerminatedString(data, size, first + 1);
		}
		return false;
	}

	bool ValidateParticleStream(IReader& source)
	{
		CBoundedReader reader(source.pointer(), source.elapsed());
		Flags32 flags;
		flags.zero();
		bool version = false;
		bool name = false;
		bool effect = false;
		bool actions = false;
		bool sprite = false;
		bool frame = false;
		bool timeLimit = false;
		bool collision = false;
		bool velocity = false;
		bool haveFlags = false;
		while (reader.Valid && !reader.eof())
		{
			if (reader.elapsed() < 8)
				return false;
			const u32 chunk = reader.r_u32();
			const u32 size = reader.r_u32();
			if (size > u32(reader.elapsed()))
				return false;
			CBoundedReader payload(reader.pointer(), size);
			switch (chunk)
			{
			case u32(PS::PE::Chunks::VERSION):
				version = size == 2 && payload.r_u16() == u16(PS::PE::Version::Original);
				break;
			case u32(PS::PE::Chunks::NAME):
				name = size > 1 && size < 256 && HasTerminatedString(payload.pointer(), size);
				break;
			case u32(PS::PE::Chunks::EFFECTDATA):
				effect = size == 4 && payload.r_u32() <= 1000000;
				break;
			case u32(PS::PE::Chunks::ACTIONLIST):
			{
				if (size < 4)
					return false;
				CBoundedReader compiled(payload.pointer(), payload.length());
				bool actionsValid = true;
				PAPI::ParticleHolder holder;
				holder.ActionsValid = &actionsValid;
				holder.LoadActions(compiled);
				actions = actionsValid && compiled.Valid && compiled.eof();
				if (!actions)
					Msg("! particle validation: actions valid=%d reader valid=%d remaining=%lld decoded=%u", actionsValid, compiled.Valid, (long long)compiled.elapsed(), u32(holder.m_actions.size()));
				break;
			}
			case u32(PS::PE::Chunks::FLAGS):
				if (size == sizeof(flags.flags))
				{
					flags.assign(payload.r_u32());
					haveFlags = true;
				}
				else
				{
					return false;
				}
				break;
			case u32(PS::PE::Chunks::SPRITE):
				sprite = HasTwoStrings(payload.pointer(), size);
				break;
			case u32(PS::PE::Chunks::FRAME):
				frame = size == sizeof(PS::SFrame);
				break;
			case u32(PS::PE::Chunks::TIMELIMIT):
				timeLimit = size == sizeof(float);
				break;
			case u32(PS::PE::Chunks::COLLISION):
				collision = size == sizeof(float) * 3;
				break;
			case u32(PS::PE::Chunks::VEL_SCALE):
				velocity = size == sizeof(Fvector);
				break;
			default:
				break;
			}
			if (!payload.Valid)
				return false;
			reader.advance(size);
		}
		const bool valid = reader.Valid && version && name && effect && actions && haveFlags &&
			(!flags.test(PS::CPEDef::dfSprite) || sprite) &&
			(!flags.test(PS::CPEDef::dfFramed) || frame) &&
			(!flags.test(PS::CPEDef::dfTimeLimit) || timeLimit) &&
			(!flags.test(PS::CPEDef::dfCollision) || collision) &&
			(!flags.test(PS::CPEDef::dfVelocityScale) || velocity);
		if (!valid)
			Msg("! particle validation: reader=%d version=%d name=%d effect=%d actions=%d flags=%d sprite=%d frame=%d time=%d collision=%d velocity=%d mask=%x",
				reader.Valid, version, name, effect, actions, haveFlags, sprite, frame, timeLimit, collision, velocity, flags.flags);
		return valid;
	}
}

bool CRender::validate_particle_definition(IReader& reader)
{
	return ViewerParticleValidationDetail::ValidateParticleStream(reader);
}

IParticleDefinition* CRender::create_particle_definition(IReader& reader)
{
	xr_unique_ptr<PS::CPEDef> definition = xr_make_unique<PS::CPEDef>();
	if (!definition->LoadOriginal(reader))
		return nullptr;
	return definition.release();
}

void CRender::destroy_particle_definition(IParticleDefinition*& definition)
{
	PS::CPEDef* particle = static_cast<PS::CPEDef*>(definition);
	xr_delete(particle);
	definition = nullptr;
}

IRenderVisual* CRender::model_CreateParticleDefinition(IParticleDefinition* definition)
{
	return definition != nullptr ? Models->CreatePE(static_cast<PS::CPEDef*>(definition)) : nullptr;
}

IParticleDefinition* CRender::find_particle_definition(const char* name)
{
	return PSLibrary.FindPED(name);
}
