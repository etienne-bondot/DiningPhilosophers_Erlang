% eb232 - eb232@kent.ac.uk
% Assessment 3 - CO545 - March 2016

-module(philo).
-compile([export_all]).

-define(PHILOSOPHERS, 5).
-define(RICE, 100).
-define(VERBOSE_LEVEL, 2).

% The Dining Philosophers Problem, originally from Dijkstra, is a classic example
% of concurrent algorithm design and highlights the problem of deadlock
% in systems that synchronise. There are many different descriptions of
% the problem, for this assessment we will consider the scenario described here
% (based on Hoare’s).
% There is a college inhabited by five philosophers who sit at a round table.
% The philosophers alternate between thinking and eating. On the table are five
% forks, each one placed between two of the philosophers. When a philosopher
% is hungry they pick up the two forks immediately adjacent to them (one left
% and one right). If a fork is not available then the philosopher must wait.
% Having finished eating the philosopher returns the forks to the table and goes
% back to thinking.

verbose(Level, FormatString, Parameters) when Level =< ?VERBOSE_LEVEL ->
    io:format(FormatString, Parameters);
verbose(_, _, _) ->
    ok.

get_name(N) when N == 0 -> platon;
get_name(N) when N == 1 -> confucius;
get_name(N) when N == 2 -> socrates;
get_name(N) when N == 3 -> aristote;
get_name(N) when N == 4 -> epicure.

college() ->
    % spawns a report process, five fork processes and five philosopher processes.
    % It will need to pass the PIDs of the forks to the philosophers, and PID of the
    % reporting process to all forks and philosophers.
    random:seed(erlang:timestamp()),
    spawn_report(),
    spawn_pair().

%
% spawn report processes
%

spawn_report() ->
    register(report, spawn(?MODULE, report, [0])).

%
% spawn pair philosopher/right and left fork,
% the first iteration spawn a philosopher and a right and a left fork,
% then for each other iteration, we pass the left fork from the previous
% philosopher to the next (thus beginning his right fork) and spawn only a
% new left fork, and so on until we reach the number PHILOSOPHERS.
% -> fork(Index, status)
% -> philosopher(Index, Name, LeftForkPid, RightForkPid, action)
%

spawn_pair() ->
    RightFork = spawn(?MODULE, fork, [0, on_table]),
    LeftFork = spawn(?MODULE, fork, [1, on_table]),
    Name = get_name(0),
    verbose(2, "Starting ~p~n", [Name]),
    register(Name, spawn(?MODULE, philosopher, [0, Name, LeftFork, RightFork, thinking])),
    spawn_pair(1, LeftFork).

spawn_pair(?PHILOSOPHERS, _) -> ok;

spawn_pair(N, RightFork) ->
    LeftFork = spawn(?MODULE, fork, [N + 1, on_table]),
    Name = get_name(N),
    verbose(2, "Starting ~p~n", [Name]),
    register(Name, spawn(?MODULE, philosopher, [N, Name, LeftFork, RightFork, thinking])),
    spawn_pair(N + 1, LeftFork).

%
% report process
% params: N, the number of rice eaten
%

capitalize([X|Xs]) when X >= $a orelse X =< $z -> [X - 32|Xs].

report(N) ->
    receive
        {philo, Name, eating} ->
            verbose(1, "[~w] ~s: ~s~n", [?RICE - N + 1, capitalize(atom_to_list(Name)), atom_to_list(eating)]),
            report(N + 1);
        {philo, Name, thinking} ->
            verbose(1, "[~w] ~s: ~s~n", [?RICE - N, capitalize(atom_to_list(Name)), atom_to_list(thinking)]),
            report(N);
        {philo, Name, hungry} ->
            verbose(1, "[~w] ~s: ~s~n", [?RICE - N, capitalize(atom_to_list(Name)), atom_to_list(hungry)]),
            report(N);
        {philo, Name, pick_left} ->
            verbose(1, "[~w] ~s: pick the left fork~n", [?RICE - N, capitalize(atom_to_list(Name))]),
            report(N);
        {philo, Name, pick_right} ->
            verbose(1, "[~w] ~s: pick the right fork~n", [?RICE - N, capitalize(atom_to_list(Name))]),
            report(N);
        {philo, Name, shutdown} ->
            verbose(1, "[~w] ~s is full, shutdown...~n", [?RICE - N, capitalize(atom_to_list(Name))]),
            report(N);
        {fork, Pid, on_table} ->
            verbose(1, "~n Fork ~w: on table~n", [Pid]),
            report(N);
        {fork, Pid, in_use} ->
            verbose(1, "~n Fork ~w: in use~n", [Pid]),
            report(N);
        {info, Name, amount_of_rice} ->
            Pid = whereis(Name),
            Pid ! {amount_of_rice, ?RICE - N},
            report(N)
    end.

%
% fork process
%

fork(Pid, Status) ->
    Report = whereis(report),
    receive
        {pick, Source} ->
            case Status =:= on_table of
                true ->
                    Source ! ok,
                    Report ! {fork, Pid, in_use};
                false ->
                    Source ! ko
            end,
            fork(Pid, in_use);
        {drop, Source} ->
            Source ! ok,
            Report ! {fork, Pid, on_table},
            fork(Pid, on_table)
    end.

%
% philosopher process
%

philosopher(Idx, Name, Left, Right, thinking) ->
    Report = whereis(report),
    Report ! {philo, Name, thinking},
    timer:sleep(1000),
    philosopher(Idx, Name, Left, Right, hungry);

% don't forget to drop the forks before being shutdow, because
% there might be still hungry philosophers that wait for forks
philosopher(Idx, Name, Left, Right, hungry) ->
    Report = whereis(report),
    Report ! {philo, Name, hungry},
    case Idx rem 2 =:= 0 of
        true ->
            verbose(2, "~n~s ask for the left fork~n", [Name]),
            check_fork(Name, Left, left),
            verbose(2, "~n~s ask for the right fork~n", [Name]),
            check_fork(Name, Right, right);
        false ->
            verbose(2, "~n~s ask for the right fork~n", [Name]),
            check_fork(Name, Right, right),
            verbose(2, "~n~s ask for the left fork~n", [Name]),
            check_fork(Name, Left, left)
    end,
    verbose(2, "~n~s has the right and left fork~n", [Name]),
    verbose(2, "~n~s check the amount of rice~n", [Name]),
    case check_rice(Name) of
        ok -> philosopher(Idx, Name, Left, Right, eating);
        ko -> drop_fork(Name, Left, Right), shutdown
    end;

philosopher(Idx, Name, Left, Right, eating) ->
    Report = whereis(report),
    Report ! {philo, Name, eating},
    timer:sleep(1000),
    drop_fork(Name, Left, Right),
    philosopher(Idx, Name, Left, Right, thinking).

%
% check if there is still enough rice to eat
% if there is no more rice, return ko and the philosopher shutdown
%

check_rice(Name) ->
    Report = whereis(report),
    Report ! {info, Name, amount_of_rice},
    receive
        {amount_of_rice, N} ->
            case N =< 0 of
                true ->
                    verbose(2, "There is no more rice[~w] for ~s...~n", [N, Name]),
                    Report ! {philo, Name, shutdown},
                    ko;
                false ->
                    verbose(2, "There is still some rice[~w] for ~s...~n", [N, Name]),
                    ok
            end
    end.

%
% check the availability of a fork and loop again if the fork is in use
%

check_fork(Name, Fork, Direction) ->
    Report = whereis(report),
    Fork ! {pick, whereis(Name)},
    receive
        ok ->
            case Direction =:= left of
                true -> Report ! {philo, Name, pick_left};
                false -> Report ! {philo, Name, pick_right}
            end;
        ko -> check_fork(Name, Fork, Direction)
    end.

%
% after eating, the philosopher has to drop both forks on the table
% this allow the other hungry philosophers to take them
%

drop_fork(Name, Left, Right) ->
    Pid = whereis(Name),
    Left ! {drop, Pid},
    receive
        ok -> verbose(2, "~n~s drop the left fork~n", [Name]), ok
    end,
    Right ! {drop, Pid},
    receive
        ok -> verbose(2, "~n~s drop the right fork~n", [Name]), ok
    end.
