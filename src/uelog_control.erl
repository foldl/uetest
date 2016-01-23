%@doc UE log control.
%
% Capture UE logs during tests.
% Two type of logs are supported: protocal stack log and physical layer log.
-module(uelog_control).

-export([start/1, stop/0, new_log/1, close_log/0]).
-export([new_phy/1, close_phy/0, tool_exe/0]).

%@doc Start control. 
start(Arg) -> 
    case whereis(?MODULE) of
        Pid when is_pid(Pid) -> 
            {error, already_started};
        _ -> 
            R = open_tool_port(Arg),
            register(?MODULE, R),
            ok
    end.

%@doc Stop CAT control.
stop() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) -> 
            Pid ! stop,
            unregister(?MODULE);
        _ -> ok
    end.

%@doc Start a new log with a file name.
new_log(Fn) ->
    ?MODULE ! {new_log, Fn}.

%@doc Close current log and start a new one.
close_log() ->
    ?MODULE ! close_log.

%@doc Start a new IML log with a full path file name.
new_phy(Fn) ->
    ?MODULE ! {new_phy, Fn}.

%@doc Close current IML log.
close_phy() ->
    ?MODULE ! close_phy.

%@doc Get executable path of CATStudio
tool_exe() ->
    ?MODULE ! {self(), tool_exe},
    receive 
        {tool_exe, Path} -> {ok, Path}
    after
        3000 -> error
    end.

-define(command_open_new ,          0).
-define(command_close ,             1).
-define(command_dbg_msg       ,     2).
-define(command_shutdown      ,     3).
-define(command_phy_new       ,     4).
-define(command_phy_close     ,     5).
-define(command_get_tool_exe  ,     8).

open_tool_port(_Arg) ->
    ExtPrg = case os:type() of
        {win32, _} ->
            [$" | filename:join(code:priv_dir(uetest), "uelog_port.exe\" -port")];
        OSType ->
            throw(OSType)
    end,
    spawn(fun () ->
            process_flag(trap_exit, true),
            Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
            wait_started(Port),
            loop(Port, [])
    end).

wait_started(Port) ->
    receive
        {Port, {data, [?command_get_tool_exe | Str]}} ->
            put(tool_exe, Str)
    end.

loop(Port, State) ->
    receive
        {Pid, tool_exe} ->
            Pid ! {tool_exe, get(tool_exe)},
            loop(Port, State);
        {Port, {data, [?command_dbg_msg | Str]}} ->
            io:format("UELog dbg: ~s~n", [Str]),
            loop(Port, State);
        {new_log, Fn} ->
            Port ! {self(), {command, [?command_open_new, Fn]}},  
            loop(Port, State);
        close_log ->
            Port ! {self(), {command, [?command_close]}},  
            loop(Port, State);
        {new_phy, Fn} ->
            Port ! {self(), {command, [?command_phy_new, Fn]}},
            loop(Port, State);
        close_phy ->
            Port ! {self(), {command, [?command_phy_close]}},
            loop(Port, State);
        stop ->
            Port ! {self(), {command, [?command_shutdown]}},
            Port ! {self(), close};
        {Port, closed} ->
            ok;
        {'EXIT', _Pid, _Reason} ->
            exit(port_terminated);
        X ->
            io:format("unknown message: ~p~n", [X])
    end.

