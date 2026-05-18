function normal   (shader, t_base, t_second, t_detail)
	shader	:begin		("model_def_lqs","model_compassarrow")
			:fog		(true)
			:blend		(true,blend.srcalpha,blend.invsrcalpha)

	shader:sampler	("s_base"):texture (t_base)
	shader:sampler  ("s_lmap"):texture ("ui\\ui_mono_noise")
end

function l_spot		(shader, t_base, t_second, t_detail)
	r1_lspot 	(shader, t_base, "model_def_spot")
end

function l_point	(shader, t_base, t_second, t_detail)
	r1_lpoint 	(shader, t_base, "model_def_point")
end
