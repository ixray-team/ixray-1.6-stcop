local tex_base = "water\\water_water"
local tex_nmap = "water\\water_normal"
local tex_dist = "water\\water_dudv"
local tex_caustic = "water\\water_caustic"

local tex_env0 = "$user$sky0"
local tex_env1 = "$user$sky1"

local tex_env = "sky\\sky_8_cube"

function normal(shader, t_base, t_second, t_detail)
    shader:begin("water", "water")
	
        :sorting(2, false)
        :blend(true, blend.srcalpha, blend.invsrcalpha)
        :zb(true, false)
        :fog(true)

    shader:dx10texture("s_base",tex_base)
    shader:dx10texture("s_nmap",tex_nmap)

    shader:dx10texture("s_env0",tex_env0)
    shader:dx10texture("s_env1",tex_env1)

    shader:dx10texture("env_s0","$user$env_s0")
    shader:dx10texture("env_s1","$user$env_s1")

    shader:dx10texture("s_image","$user$rt_color")
    shader:dx10texture("s_caustic",tex_caustic)
	
    shader:dx10texture("s_env",tex_env)
	shader:dx10sampler("smp_base")
	shader:dx10sampler("smp_rtlinear")
end