
-- Simple example



-- Variables ( state, HP, MP, current target, etc )



-- Initialize (called at once.)

function init()
	return easymmo_ai.foo(1,2)
	-- return "ok", 1, 2
end

print( init() )
return init()


-- Evaluate (called every eval timing)


-- SEE luerl::load_module



-- Utilitiies ( action reporter, and etc.)


-- Utility - State keeper



