local tex_base                = "water\\water_water"
local tex_nmap                = "water\\water_normal"
local tex_dist                = "water\\water_dudv"

function normal(shader, t_base, t_second, t_detail)
  shader:begin          ("water","water")
        : sorting       (2, false)
        : blend         (true,blend.srcalpha,blend.invsrcalpha)
        : zb            (true,false)
        : distort       (true)
        : fog           (false)
  shader:sampler        ("s_base")       :texture  (tex_base)
  shader:sampler        ("s_nmap")       :texture  (tex_nmap)
  shader:sampler        ("s_env0")       :texture  ("$user$sky0")   : clamp()
  shader:sampler        ("s_env1")       :texture  ("$user$sky1")   : clamp()
end

--function l_special(shader, t_base, t_second, t_detail)
--  shader:begin          ("waterd","waterd")
--        : sorting       (2, true)
--        : blend         (true,blend.srcalpha,blend.invsrcalpha)
--        : zb            (true,false)
--        : fog           (false)
--        : distort       (true)
--  shader:sampler        ("s_base")       :texture  (tex_base)
--  shader:sampler        ("s_distort")    :texture  (tex_dist)
--end