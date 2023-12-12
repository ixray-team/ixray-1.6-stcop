#pragma once

#define PLAYER_ADDICTION_IMPL(name) \
	float V_##name			() const { return name.Variability;	} \
	float V_##name##Power	() const { return name.PowerBoost;	} \
	float V_##name##Health	() const { return name.HealthBoost;	} \
	float name##Critical	() const { return name.Critical;	} \
																  \
	virtual void Change##name(const float value) override;		  \
	virtual float Get##name() const override { return name.Current;} \
	virtual float Get##name##Power() const override {return name.PowerBoost * name.Current;};

#define PLAYER_ADDICTION_BASE(name) \
	virtual void Change##name(const float value) {};        \
	virtual float Get##name() const { return 1.f; }		    \
	virtual float Get##name##Power() const { return 0.f; }; 

struct PlayerAddiction
{
	float Current = 1;

	float Variability = 1;
	float Critical = 0;

	float PowerBoost = 0;
	float HealthBoost = 0;
};