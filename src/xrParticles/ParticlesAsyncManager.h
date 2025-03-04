#pragma once
class CPS_Instance;

class PARTICLES_API CParticlesAsync
{
public:
	static void Play();
	static void Wait();
	
	static void ForceUpdate(CPS_Instance* Obj);
	static bool NeedForceUpdate();

private:
	void UpdateParticle(CPS_Instance* particle) const;
	static void Start();

public:
	CParticlesAsync();

private:
	volatile bool IsStarted = false;
};