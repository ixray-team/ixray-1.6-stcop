local tex_base = "water\\water_water"
local tex_nmap = "water\\water_normal"
local tex_dist = "water\\water_dudv"
local tex_caustic = "water\\water_caustic"

local tex_env0 = "$user$sky0"
local tex_env1 = "$user$sky1"

local tex_leaves = "water\\water_foam"

function normal(shader, t_base, t_second, t_detail)

	local wboit = GetShaderOption("ALLOW_WBOIT_TRANSPARENCY")
	
    shader:begin("water", "water")

	:sorting(2, false)
	
	:zb(true, false)
	:distort(true)
	:fog(true)
	
	if wboit then
		shader:blend(true, blend.one, blend.one)
		: iblend(2, true, blend.destcolor, blend.zero)
		: iblend(1, true, blend.srcalpha, blend.invsrcalpha)
		
		log("Water shader " .. t_base .. " WBOIT ON")
	else
		shader:blend(true, blend.srcalpha, blend.invsrcalpha)
		log("Water shader " .. t_base .. " WBOIT OFF")
	end

    shader:dx10texture("s_base", tex_base)

    shader:dx10texture("s_nmap", tex_nmap)

    shader:dx10texture("s_env0", tex_env0)
    shader:dx10texture("s_env1", tex_env1)
	
    shader:dx10texture("s_env", "$user$env")
    shader:dx10texture("s_env_dist", "$user$env_temp")

    shader:dx10texture("env_s0", "$user$env_s0")
    shader:dx10texture("env_s1", "$user$env_s1")

    shader:dx10texture("s_accumulator", "$user$accum")
    shader:dx10texture("s_position", "$user$position")
    shader:dx10texture("s_velocity", "$user$velocity")
    shader:dx10texture("s_image", "$user$generic")

    shader:dx10texture("s_material", "$user$material")

    shader:dx10texture("s_leaves", tex_leaves)
    shader:dx10texture("s_caustic", tex_caustic)
	
    shader:dx10texture("s_smap_sun", "$user$smap_depth_sun")

    shader:dx10sampler("smp_smap")
    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_nofilter")
    shader:dx10sampler("smp_rtlinear")
end

function l_special(shader, t_base, t_second, t_detail)
    shader:begin("water", "waterd")
	
	:blend(true, blend.srcalpha, blend.invsrcalpha)
	:zb(true, false)
	:fog(false)
	:distort(true)

    shader:dx10color_write_enable(true, true, true, false)

    shader:dx10texture("s_base", tex_base)
    shader:dx10texture("s_distort", tex_dist)
    shader:dx10texture("s_position", "$user$position")

    shader:dx10sampler("smp_base")
    shader:dx10sampler("smp_nofilter")
end
