#pragma once

class CRandom
{
private:
        s32		holdrand;
public:
	CRandom()			: holdrand(1)				{};
	CRandom(s32 _seed)	: holdrand(_seed)			{};

	IC 	void	seed	(s32 val)					{ holdrand=val;	}
	IC 	s32		maxI	()							{ return 32767;	}

    ICN	s32		randI	()							{ return(((holdrand = holdrand * 214013L + 2531011L) >> 16) & 0x7fff); }
	IC 	s32		randI	(size_t max)				{ VERIFY(max);  return randI()% s32(max); }
	IC 	s32		randI	(s32 min, s32 max)			{ return min+randI(max-min); }
	IC 	s32		randIs	(s32 range)					{ return randI(-range,range); }
	IC 	s32		randIs	(s32 range, s32 offs)		{ return offs+randIs(range); }

	IC 	float	maxF	()							{ return 32767.f;	}
	IC 	float	randF	()							{ return float(randI())/maxF();	}
	IC 	float	randF	(float max)					{ return randF()*max; }
	IC 	float	randF	(float min,float max)		{ return min+randF(max-min); }
	IC 	float	randFs	(float range)				{ return randF(-range,range); }
	IC 	float	randFs	(float range, float offs)	{ return offs+randFs(range); }
};

XRCORE_API extern CRandom	Random;

class CFFxRandom {
public:
    CFFxRandom() {
        set_state(static_cast<u32>(std::time(nullptr)), 0);
    };

    CFFxRandom(u32 _seed, u32 _counter) {
        set_state(_seed, _counter);
    };

    ~CFFxRandom() = default;

    static constexpr u32 hash_prime_multiplier_1 = 134775813U;
    static constexpr u32 hash_prime_multiplier_2 = 2246822519U;
    static constexpr u32 hash_prime_multiplier_3 = 3266489917U;
    static constexpr u32 hash_prime_addition = 12345U;
    static constexpr u32 hash_positive_mask = 0x7FFFFFFF;
    static constexpr float inv_2pow31 = 1.0f / static_cast<float>(hash_positive_mask);
    static constexpr int cache_size = 4;
    static constexpr int seed_fill_iterations = 8;

    bool is_valid_counter = true;
    u32 counter = 0;
    u32 seed = 0;
    u32 state = 0;
    u32 cache_index = 0;
    u32 last_values[cache_size] = {
        0, 
        0, 
        0,
        0 
    };

    u32 get_seed()
    {
        return seed;
    }

    u32 get_counter()
    {
        return counter;
    }

    void set_state(u32 _seed, u32 _counter) {
        for (int i = 0; i < cache_size; i++) {
            last_values[i] = 0;
        }

        seed = _seed;
        state = hash(seed);
        cache_index = 0;

        for (int i = 0; i < seed_fill_iterations; i++) {
            generate_raw();
        }

        counter = 0;

        for (u32 i = 0; i < _counter; i++) {
            next_int();
        }
    }

    bool is_counter_valid()
    {
        return is_valid_counter;
    }

    u32 next_int() 
    {
        if (counter >= 4294967295 - 1)
        {
            counter = 0;
            is_valid_counter = false;
        }

        counter++;

        return generate_raw();
    }

    u32 next_int_range(u32 min, u32 max)
    {
        if (min > max) std::swap(min, max);
        u32 range = max - min;
        if (range == 0) return min;

        u32 max_valid = hash_positive_mask - (hash_positive_mask % (range + 1)) - 1;
        u32 val;
        int attempts = 0;

        do {
            val = next_int();
            attempts++;
            if (attempts > 100) {
                break;
            }
        } while (val > max_valid);

        return min + (val % (range + 1));
    }

    float next_float()
    {
        return static_cast<float>(next_int()) * inv_2pow31;
    }

    float next_float_range(float min, float max)
    {
        if (min > max) std::swap(min, max);
        return min + next_float() * (max - min);
    }

    bool next_bool()
    {
        return next_float() < 0.5;
    }

    bool next_bool_probability(float probability = 0.5f)
    {
        return next_float() < probability;
    }

private:
    bool is_recent(u32 val) {
        for (int i = 0; i < cache_size; i++) {
            if (last_values[i] == val) return true;
        }
        return false;
    }

    u32 generate_raw() {
        u16 attempts = 0;

        do {
            state = hash(state);
            attempts++;
            if (attempts > 100) {
                break;
            }
        } while (is_recent(state));

        last_values[cache_index] = state;
        cache_index = (cache_index + 1) % cache_size;

        return state;
    }

    u32 hash(u32 value) {
        u32 x = value * hash_prime_multiplier_1 + hash_prime_addition;
        x ^= x >> 16;
        x *= hash_prime_multiplier_2;
        x ^= x >> 13;
        x *= hash_prime_multiplier_3;
        x ^= x >> 16;

        return x & hash_positive_mask;
    }
};

XRCORE_API extern CFFxRandom FFxRandom;