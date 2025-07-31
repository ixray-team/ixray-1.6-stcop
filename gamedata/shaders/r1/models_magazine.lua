
function normal		(shader, t_base, t_second, t_detail)
  shader:begin  	("model_env_hq","model_env_hq")
      : fog    		(true)
      : zb     		(true,true)
      : blend   	(true,blend.srcalpha,blend.invsrcalpha)
      : aref    	(true,0)

  shader:sampler	("s_base")      :texture  (t_base)
  shader:sampler  ("s_env")        :texture    ("sky\\sky_cube_weapons") : clamp()

end