-- without depth test
function l_special(shader, t_base, t_second, t_detail)
    shader:begin("debug_draw", "debug_draw")
        :zb(false, false)
	   
	shader:dx10texture("s_position", "$user$position")
    shader:dx10sampler("smp_nofilter")
end

-- depth test
function normal(shader, t_base, t_second, t_detail)
    shader:begin("debug_draw", "debug_draw_nodepth")
        :zb(true, false)
end