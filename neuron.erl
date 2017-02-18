-module(neuron).

-export([init/0]).

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
	    excite(New_state, Origin);
	{connect_input, Origin} ->
	    add_input(New_state, Origin);
	{disconnect_input, Origin} ->
	    remove_input(New_state, Origin);
	{connect_receiver, Origin} ->
	    add_receiver(New_state, Origin);
	{disconnect_receiver, Origin} ->
	    remove_receiver(New_state, Origin);
        {terminate} ->
            State;
	_ ->
	    loop(New_state)
    after State#state.timeout ->
	    dampen(State)
    end.

maybe_fire(State) when State#state.potential > State#state.threshold ->
    [ R ! {excite, self()} || R <- sets:to_list(State#state.receivers)],
    State#state{potential = State#state.base};

maybe_fire(State) ->
    loop(State).

excite(State, Origin) ->
    loop(State#state{
           potential = State#state.potential + dict:fetch(
                                                 Origin,
                                                 State#state.inputs)}).

dampen(State) ->
    loop(State#state{
           potential = State#state.potential - State#state.damper}).

add_input(State, Origin) ->
    loop(State#state{
           inputs = dict:store(
                      Origin,
                      rand:uniform(State#state.threshold),
                      State#state.inputs)}).

remove_input(State, Origin) ->
    loop(State#state{
           inputs = dict:erase(
                      Origin,
                      State#state.inputs)}).

add_receiver(State, Origin) ->
    loop(State#state{
           receivers = sets:add_element(Origin, State#state.receivers)}).

remove_receiver(State, Origin) ->
    loop(State#state{
           receivers = sets:del_element(Origin, State#state.receivers)}).

