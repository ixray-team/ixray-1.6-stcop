#pragma once

#include "ParticlesObject.h"

struct SElectricCurve
{
	shared_str particleName = "ffx0001\\test\\t_01";
	xr_vector<xr_shared_ptr<CParticlesObject>> particlesElectric;
	xr_vector<Fvector> segmentStarts;
	Fvector initialPos;
	Fvector destinationPos;
	float m_upd_timer = 0.f;
	float m_max_upd_timer = 0.f;

	float animTime = 0.0f;
	float max_anim_time = Random.randF(20.0f, 80.0f);

	float STEP = 0.05f;
	float SEGMENT_LENGTH = 0.5f;           // Длина одного сегмента (метров)
	float ANIMATION_SPEED = Random.randF(0.001f, 0.0015f);        // Скорость анимации (множитель dwTimeDelta)

	float MAX_CURVE_DISTANCE = 5.0f;						// Максимальная длинна дуги
	float PARABOLA_HEIGHT_MULT = Random.randF(0.3f, 0.8f);  // Высота параболы множитель от расстояния
	float MIN_HEIGHT = 0.5f;								// Минимальная высота дуги
	float MAX_HEIGHT = Random.randF(2.0f, 5.0f);            // Максимальная высота дуги

	bool isEnabled = true;
	bool isStopped = true;

	bool IsEnable()
	{
		return isEnabled;
	}

	void Enable()
	{
		isEnabled = true;
	}

	void Disable()
	{
		isEnabled = false;
	}

	ICF Fvector GetArcPoint(float t, const Fvector& initialPos, const Fvector& destinationPos, float dist, float animTime)
	{
		// Параметры гармоник Y (вертикальные колебания)
		float Y_NOISE_AMP1 = 0.75f;
		float Y_NOISE_AMP2 = 0.35f;
		float Y_NOISE_AMP3 = 0.1f;
		float Y_NOISE_FREQ1 = 15.0f;
		float Y_NOISE_FREQ2 = 30.0f;
		float Y_NOISE_FREQ3 = 45.0f;
		float Y_NOISE_INFLUENCE = 0.6f;

		// Параметры хаотичности XZ (боковые отклонения)
		float CHAOS_MULT = 0.5f;
		float CHAOS_AMP1 = 0.5f;
		float CHAOS_AMP2 = 0.25f;
		float CHAOS_FREQ_BASE1 = 25.0f;
		float CHAOS_FREQ_BASE2 = 45.0f;
		float CHAOS_FREQ_VAR = 12.0f;

		Fvector point;
		point.lerp(initialPos, destinationPos, t);

		float parabola = sin(t * 3.14159f);
		float maxHeight = dist * PARABOLA_HEIGHT_MULT;
		if (maxHeight < MIN_HEIGHT) maxHeight = MIN_HEIGHT;
		if (maxHeight > MAX_HEIGHT) maxHeight = MAX_HEIGHT;

		float timeFactor = animTime * 10.0f;
		float yNoise = sin(t * Y_NOISE_FREQ1 + timeFactor) * Y_NOISE_AMP1;
		yNoise += sin(t * Y_NOISE_FREQ2 - timeFactor * 0.8f) * Y_NOISE_AMP2;
		yNoise += sin(t * Y_NOISE_FREQ3 + timeFactor * 1.3f) * Y_NOISE_AMP3;
		yNoise *= parabola;

		point.y += maxHeight * parabola + yNoise * Y_NOISE_INFLUENCE;

		float chaosMult = CHAOS_MULT * parabola;
		float freq1 = CHAOS_FREQ_BASE1 + sin(animTime) * CHAOS_FREQ_VAR;
		float freq2 = CHAOS_FREQ_BASE2 + cos(animTime * 0.8f) * CHAOS_FREQ_VAR;

		point.x += sin(t * freq1 + animTime * 15.0f) * CHAOS_AMP1 * chaosMult;
		point.z += cos(t * freq2 + animTime * 12.0f) * CHAOS_AMP1 * chaosMult;
		point.x += sin(t * 40.0f + animTime * 20.0f) * CHAOS_AMP2 * chaosMult;
		point.z += cos(t * 35.0f - animTime * 15.0f) * CHAOS_AMP2 * chaosMult;

		return point;
	};

	ICF void SolveChainFABRIK(
		xr_vector<Fvector>& points,
		const Fvector& A,
		const Fvector& B,
		float L,
		int S,
		int iterations = 8,
		float epsilon = 0.001f
	)
	{
		VERIFY(points.size() == S + 1);

		float max_len = L * float(S);

		float distAB = A.distance_to(B);

		for (int iter = 0; iter < iterations; ++iter)
		{
			points[S].set(B);

			for (int i = S - 1; i >= 0; --i)
			{
				Fvector dir;
				dir.sub(points[i], points[i + 1]);

				if (dir.square_magnitude() > EPS_S)
					dir.normalize();
				else
					dir.set(0.f, 1.f, 0.f);

				points[i].mad(points[i + 1], dir, L);
			}

			points[0].set(A);

			for (int i = 1; i <= S; ++i)
			{
				Fvector dir;
				dir.sub(points[i], points[i - 1]);

				if (dir.square_magnitude() > EPS_S)
					dir.normalize();
				else
					dir.set(0.f, 1.f, 0.f);

				points[i].mad(points[i - 1], dir, L);
			}

			if (points[S].distance_to(B) <= epsilon)
				break;
		}
	}

	void UpdateMovement() 
	{
		if (!IsEnable())
		{
			if (!isStopped && !particlesElectric.empty())
			{
				for (size_t i = 0; i < particlesElectric.size(); i++)
				{
					if (particlesElectric[i])
					{
						particlesElectric[i]->Stop(false);
					}
				}

				isStopped = true;
			}

			return;
		}

		isStopped = false;

		animTime += Device.dwTimeDelta * ANIMATION_SPEED;
		if (animTime > max_anim_time)
		{
			animTime = 0.0f;
		}

		float dist = initialPos.distance_to(destinationPos);
		if (dist > MAX_CURVE_DISTANCE)
		{
			dist = MAX_CURVE_DISTANCE;
		}
		float coeff = 1.6;

		float chain = (dist * coeff) / SEGMENT_LENGTH;
		chain = std::floor(chain + 1);
		STEP = 1 / chain;

		segmentStarts.clear();

		Fvector currentSpawnPoint = segmentStarts.emplace_back(GetArcPoint(0.0f, initialPos, destinationPos, dist, animTime));
		float currentT = 0.0f;

		while (currentT < 1.0f)
		{
			currentT += STEP;
			if (currentT > 1.0f)
			{
				currentT = 1.0f;
			}

			Fvector currentPoint = GetArcPoint(currentT, initialPos, destinationPos, dist, animTime);
			segmentStarts.push_back(currentPoint);
		}

		for (size_t i = 0; i < segmentStarts.size() - 1; i++)
		{
			Fvector startB;
			startB.set(segmentStarts[i]);

			Fvector endB;
			endB.set(segmentStarts[i + 1]);

			Fvector dirB;
			dirB.sub(endB, startB);
			dirB.normalize();
		}

		size_t neededParticles = segmentStarts.size() - 1;
		SolveChainFABRIK(segmentStarts, initialPos, destinationPos, SEGMENT_LENGTH, neededParticles);

		while (particlesElectric.size() < neededParticles)
		{
			particlesElectric.push_back(Particles::Details::Create(particleName.c_str(), false));
		}

		for (size_t i = 0; i < neededParticles; i++)
		{
			if (particlesElectric[i] != nullptr)
			{
				Fvector start;
				start.set(segmentStarts[i]);

				Fvector end;
				end.set(segmentStarts[i + 1]);

				Fvector dir;
				dir.sub(end, start);
				float length = dir.magnitude();
				dir.normalize();

				Fmatrix XF;
				XF.j.set(dir);
				Fvector::generate_orthonormal_basis(XF.j, XF.k, XF.i);
				XF.c.set(start);
				particlesElectric[i]->SetXFORM(XF);

				if (!particlesElectric[i]->IsPlaying())
				{
					particlesElectric[i]->Play(false);
				}
			}
		}

		for (size_t i = segmentStarts.size() > 0 ? segmentStarts.size() - 1 : 0; i < particlesElectric.size(); i++)
		{
			if (particlesElectric[i])
			{
				particlesElectric[i]->Stop(false);
			}
		}
	}
};

