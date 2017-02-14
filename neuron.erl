-module(neuron).

-compile(export_all).

-record(state, {receivers = sets:new(),
		inputs = dict:new(),
		base = 0,
		potential = 0,
		damper = 10,
		timeout = 1,
		threshold = 100}).

init() ->
    spawn(neuron, loop, #State{}).

loop(State) ->
    New_state = maybe_fire(State),
    receive 
	{excite, Origin} -> 
	    loop(excite(New_state, Origin));
	{connect_input, Origin} ->
	    loop(add_input(New_state, Origin));
	{connect_receiver, Origin} ->
	    loop(add_receiver(New_state, Origin))
    after
	State#state.timeout ->
	    loop(dampen(State))
    end.

maybe_fire(State) when State#state.potential > State#state.threshold ->    
    [ R ! {excite, self()} || R <- sets:to_list(State#state.receivers)],
    State#state{potential = State#state.base};

maybe_fire(State) -> 
    State.

excite(State, Origin) ->
    State#state{potential = State#state.potential + dict:fetch(Origin, State#state.inputs)}.

dampen(State) ->
    State#state{potential = State#state.potential - State#state.damper}.

add_input(State, Origin) ->
    State#state{inputs = dict:store(Origin, rand:uniform(State#state.threshold), State#state.inputs)}.

add_receiver(State, Origin) ->
    State#state{receivers = sets:add_element(Origin, State#state.receivers)}.
