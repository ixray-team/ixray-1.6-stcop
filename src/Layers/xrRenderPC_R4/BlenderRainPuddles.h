#pragma once

class BlenderRainPuddles final : public IBlender
{
public:
	BlenderRainPuddles();
	const char* getComment() override { return "Rain puddle reflections"; }
	bool canBeDetailed() override { return false; }
	bool canBeLMAPped() override { return false; }
	void Compile(CBlender_Compile& C) override;
};
