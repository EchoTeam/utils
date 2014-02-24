%%% 
%%% Copyright (c) 2008-2014 JackNyfe, Inc. <info@jacknyfe.com>
%%% All rights reserved.
%%%
%%%
%%% When YAWS invokes the out() function of a particular .yaws script, such
%%% .yaws script expects some resources (user UPREFS, domain prefs, etc)
%%% to be available. Sometimes the resources are not available immediately,
%%% but instead must be fetched from some other machine in the cluster.
%%% Since scripts typically require more than a single resource, fetching
%%% them sequentially would introduce a latency multiplier, which is not
%%% desirable. Suppose the script needs resources A, B and C, and we have
%%% fetched these resources in sequence: A, then B, then C. But the .yaws
%%% script may not always want the resource A. Sometimes it is OK with
%%% only resource C present, where A and B are required conditionally, on
%%% rare occasions. Fixing the order of resource fetching is undesirable.
%%%
%%% This smart_fetch module provides the parallel resource acquisition
%%% functionality, see the corresponding functions' descriptions.
-module(smart_fetch_utils).
-export([accessorF/1]).

%% accessorF/1:
%% A caller may specify zero or more named functions which fetch some data
%% from some remote location (typically, dquery:fetch()). These functions
%% are started in parallel and each eventually returns with some value.
%% The obtain/1 returns the "accessor" function, which can get any
%% of this data by its name. If the corresponding fetcher function isn't yet
%% finished, the accessor function will wait for it to finish, otherwise
%% it'll return the data last returned.
%%
%% This is all simple until we consider locking. Sometimes we need to lock
%% the resources so they're not available for others to change before us.
%% We cannot always lock them and invoke accessorF, since dquery:fetch()
%% will try locking it during distributed processing, and lock will fail,
%% resulting in not saving the data on local disk. Same goes for dquery:put().
%% So we need to be able to lock the resource right in the process which
%% does data fetching. However, we will not be able to do destructive updates
%% unless we supply that process with a new data, or a transformation function.
%% This may sound a bit complicated.
%% Furthermore, you can share this accessor function with other processes.
%%
%% Here's the signature:
%%
%% @spec accessorF(FunList) -> AccessorF
%% Type FunList = [{Name, FetcherFun()}]
%% 	Name = term()
%% 	FetcherFun = fun() -> Data
%% 	AccessorF = fun(Name, Operation)
%% 	Operation = get | {replace, DataOut} | forget | fun(DataIn) -> DataOut
%% 	Data = DataIn = DataOut = term()
%% 
accessorF(FunList) ->
	Requester = self(),
	% The whenever_ready option will allow using
	% accessorFunction('ANY', Timeout) -> {Name, DataOut}
	GRef = case lists:member(whenever_ready, FunList) of
		true ->
			% Reference pertaining to the whole operation.
			make_ref();
		false -> undefined
	end,
	Name2Pid = [{Name, spawn(fun() ->
			dataActor(Name, Requester, GRef, Fun) end)}
		|| {Name, Fun} <- FunList, is_function(Fun, 0)],
	case Name2Pid of
	  [] -> fun(_, _) -> throw(no_smart_actors_defined) end;
	  _  -> fun
		('ANY', Timeout) when is_reference(GRef) ->
			receive
				{'ANY', GRef, {Name, Term}} ->
					Requester ! {'ANY', GRef, finished},
					{Name, Term};
				{'ANY', GRef, finished} ->
					throw(no_more_smart_actors)
			after Timeout -> throw(timeout)
			end;
		(Name, Op) -> accessorFunction(Name, Op, Name2Pid)
		end
	end.

accessorFunction(Name, Op, Name2Pid) ->
	% Search for named resource, and let it fail if name does not match
	{value, {_, Pid}} = lists:keysearch(Name, 1, Name2Pid),
	Ref = make_ref(),
	Pid ! {apply, Op, self(), Ref},
	MRef = erlang:monitor(process, Pid),
	Result = receive
		{reply, Ref, Reply} -> Reply;
		{'DOWN', MRef, _, _, _} -> throw({smart_data_actor_dead, Name})
	end,
	erlang:demonitor(MRef),
	Result.

dataActor(Name, ParentPid, GRef, Fun) ->
	InitialResult = Fun(),
	if
	  not is_reference(GRef) -> ok;
	  true -> ParentPid ! {'ANY', GRef, {Name, InitialResult}}
	end,
	MRef = erlang:monitor(process, ParentPid),
	dataActorLoop({MRef, ParentPid}, GRef, InitialResult).

dataActorLoop({MRef, ParentPid} = Arg, GRef, Data) ->
  receive
	% Everyone can get data
	{apply, get, Pid, Ref} ->
		Pid ! {reply, Ref, Data},
		dataActorLoop(Arg, GRef, Data);
	% Only parent can kill us
	{apply, forget, ParentPid, Ref} ->
		ParentPid ! {reply, Ref, ok},
		exit(normal);
	% Only parent can replace data
	{apply, {replace, Fun}, ParentPid, Ref} when is_function(Fun) ->
		NewData = Fun(Data),
		ParentPid ! {reply, Ref, ok},
		dataActorLoop(Arg, GRef, NewData);
	{apply, {replace, NewData}, ParentPid, Ref} ->
		ParentPid ! {reply, Ref, ok},
		dataActorLoop(Arg, GRef, NewData);
	% Everyone can sift data through a function
	{apply, Fun, Pid, Ref} when is_function(Fun) ->
		Pid ! {reply, Ref, Fun(Data)},
		dataActorLoop(Arg, GRef, Data);
	{'DOWN', MRef, process, ParentPid, _Reason} ->
		exit(normal);
	_ -> dataActorLoop(Arg, GRef, Data)
  end.

