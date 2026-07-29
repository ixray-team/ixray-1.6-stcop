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

    shader:sampler("s_base"):texture(tex_base)
    shader:sampler("s_nmap"):texture(tex_nmap)

    shader:sampler("s_env0"):texture(tex_env0)
    shader:sampler("s_env1"):texture(tex_env1)

    shader:sampler("env_s0"):texture("$user$env_s0")
    shader:sampler("env_s1"):texture("$user$env_s1")

    shader:sampler("s_image"):texture("$user$rt_color")
    shader:sampler("s_caustic"):texture(tex_caustic)
	
    shader:sampler("s_env"):texture(tex_env)
end