% eb232 - eb232@kent.ac.uk
% Assessment 3 - CO545 - March 2016

-module(philo).
-compile([export_all]).

-define(PHILOSOPHERS, 5).
-define(VERBOSE_LEVEL, 1).

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
% Having finished eating the philosopher reCycles the forks to the table and goes
% back to thinking.

verbose(Level, FormatString, Parameters) when Level =< ?VERBOSE_LEVEL ->
    io:format(FormatString, Parameters);
verbose(_, _, _) ->
    ok.

get_name(N) when N == 0 -> platon;
get_name(N) when N == 1 -> ciceron;
get_name(N) when N == 2 -> socrate;
get_name(N) when N == 3 -> seneque;
get_name(N) when N == 4 -> lucrece.

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
    register(report, spawn(?MODULE, report, [])).

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
    register(Name, spawn(?MODULE, philosopher, [0, Name, LeftFork, RightFork, thinking, 0])),
    % the left fork become the right fork for the next philosopher
    % and we keep the right fork to become the left fork of the last philosopher
    spawn_pair(1, LeftFork, RightFork).

spawn_pair(?PHILOSOPHERS, _, _) -> ok;

spawn_pair(N, RightFork, FirstFork) ->
    Name = get_name(N),
    verbose(2, "Starting ~p~n", [Name]),
    case N + 1 =:= ?PHILOSOPHERS of
        % for the last philosopher, the left fork is the right fork of the
        % first philosopher
        true -> LeftFork = FirstFork;
        false -> LeftFork = spawn(?MODULE, fork, [N + 1, on_table])
    end,
    register(Name, spawn(?MODULE, philosopher, [N, Name, LeftFork, RightFork, thinking, 0])),
    % the left fork become the right fork for the next philosopher
    % and we keep the right fork to become the left fork of the last philosopher
    spawn_pair(N + 1, LeftFork, RightFork).

%
% report process
% params: N, the number of rice eaten
%

capitalize([X|Xs]) when X >= $a orelse X =< $z -> [X - 32|Xs].

kill_them_all(?PHILOSOPHERS) -> done;
kill_them_all(N) ->
    Pid = whereis(get_name(N)),
    Pid ! please_kill_yourself_now,
    kill_them_all(N + 1).

report() ->
    receive
        {philo, Name, eating, Cycle} ->
            verbose(1, "[~w] ~s\t: ~s~n", [Cycle, capitalize(atom_to_list(Name)), atom_to_list(eating)]);
        {philo, Name, thinking, Cycle} ->
            verbose(1, "[~w] ~s\t: ~s~n", [Cycle, capitalize(atom_to_list(Name)), atom_to_list(thinking)]);
        {philo, Name, hungry, Cycle} ->
            verbose(1, "[~w] ~s\t: ~s~n", [Cycle, capitalize(atom_to_list(Name)), atom_to_list(hungry)]);
        {philo, Name, pick_left, Cycle} ->
            verbose(1, "[~w] ~s\t: picks up the left fork~n", [Cycle, capitalize(atom_to_list(Name))]);
        {philo, Name, pick_right, Cycle} ->
            verbose(1, "[~w] ~s\t: picks up the right fork~n", [Cycle, capitalize(atom_to_list(Name))]);
        {philo, Name, drop_left, Cycle} ->
            verbose(1, "[~w] ~s\t: puts down the left fork~n", [Cycle, capitalize(atom_to_list(Name))]);
        {philo, Name, drop_right, Cycle} ->
            verbose(1, "[~w] ~s\t: puts down the right fork~n", [Cycle, capitalize(atom_to_list(Name))]);
        {fork, Pid, on_table} ->
            verbose(1, "Fork ~w\t: on table~n", [Pid]);
        {fork, Pid, in_use} ->
            verbose(1, "Fork ~w\t: in use~n", [Pid])
    after 2000 ->
        % in case of fool philosophers
        % ask them to commit suicide with the forks
        verbose(2, "Philosophers are stucked... kill everyone!!!", []),
        kill_them_all(0),
        exit(normal)
    end,
    report().


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

philosopher(Idx, Name, Left, Right, thinking, Cycle) ->
    Report = whereis(report),
    Report ! {philo, Name, thinking, Cycle},
    timer:sleep(random:uniform(1000) + 100),
    philosopher(Idx, Name, Left, Right, hungry, Cycle);

% don't forget to drop the forks before being shutdown, because
% there might be still hungry philosophers that wait for forks
philosopher(Idx, Name, Left, Right, hungry, Cycle) ->
    Report = whereis(report),
    Report ! {philo, Name, hungry, Cycle},

    % this trick avoid to have only one philosopher to eat
    case Idx rem 2 =:= 0 of
        true ->
            verbose(2, "~n~s asks for the left fork~n", [Name]),
            check_fork(Name, Left, left, Cycle),
            verbose(2, "~n~s asks for the right fork~n", [Name]),
            check_fork(Name, Right, right, Cycle);
        false ->
            verbose(2, "~n~s asks for the right fork~n", [Name]),
            check_fork(Name, Right, right, Cycle),
            verbose(2, "~n~s asks for the left fork~n", [Name]),
            check_fork(Name, Left, left, Cycle)
    end,
    verbose(2, "~n~s has the right and left fork~n", [Name]),
    philosopher(Idx, Name, Left, Right, eating, Cycle);

philosopher(Idx, Name, Left, Right, eating, Cycle) ->
    Report = whereis(report),
    Report ! {philo, Name, eating, Cycle},
    timer:sleep(random:uniform(1000) + 100),
    drop_fork(Name, Left, Right, Cycle),
    philosopher(Idx, Name, Left, Right, thinking, Cycle + 1).

philosopher(Name, shutdown) ->
    verbose(2, "~n~s kill himself... :'(~n", [Name]),
    exit(normal).

%
% check the availability of a fork and loop again if the fork is in use
% if stuck in loop for a long time (~2000ms), reporter should tell the philosopher to
% killhimself.
%

check_fork(Name, Fork, Direction, Cycle) ->
    Report = whereis(report),
    Fork ! {pick, whereis(Name)},
    receive
        ok ->
            case Direction =:= left of
                true -> Report ! {philo, Name, pick_left, Cycle};
                false -> Report ! {philo, Name, pick_right, Cycle}
            end;
        ko -> check_fork(Name, Fork, Direction, Cycle);
        please_kill_yourself_now -> philosopher(Name, shutdown)
    end.

%
% after eating, the philosopher has to drop both forks on the table
% this allow the other hungry philosophers to take them
%

drop_fork(Name, Left, Right, Cycle) ->
    Report = whereis(report),
    Pid = whereis(Name),
    Left ! {drop, Pid},
    receive
        ok -> Report ! {philo, Name, drop_left, Cycle}
    end,
    Right ! {drop, Pid},
    receive
        ok -> Report ! {philo, Name, drop_right, Cycle}
    end.


%%
%% CHALLENGE
%%

% 1. Is there any possibility of deadlock in your system? If not, explain why
% this is so. If there is, explain the situation in which deadlock could occur
% (referring to your own implementation).
%
% A deadlock is when there's a cycle in the locking dependencies of two or
% more processes. For instance, in a program :
%
% Foo = spawn(fun() -> receive
%   {From, foo} ->
%     From ! {self(), bar},
%     io:format("foo done~n")
%   end
% end).
% Bar = spawn(fun() -> receive
%   {From, bar} ->
%     From ! {self(), foo},
%     io:format("bar done~n")
%   end
% end).
%
% Because both Foo and Bar are waiting for a message, if there is not an other
% actor that send Foo ! {Pid, foo} or Bar ! {Pid, bar}, both processes will
% be deadlocked.
%
% But there is no such case in this project because there is not two interdependent
% processes that are each waiting for a message from the other, but processes
% which send a message and wait for an answer.
% In the worst case, we could have infinite loop, like if a philosopher check
% for the availability of a fork and the fork allways answer that it is unavailable.
% Thus, the philosopher will ask again and again, but this is not a deadlock
% strictly speaking. And even if this could happened in case of bad implementation,
% the reporter will timeout after 2s without receiving any message and ask all the
% philosophers to exit properly.
%
% 2. Assuming that there is a danger of deadlock in the system, answer one of the
% following questions: (a) How could the deadlock be avoided? (preventing it from
% occuring in the first place)
%
% The only rule to avoid deadlock is that if process A sends a message and waits
% for a response from process B, process B is not allowed, to do a synchronous call
% to process A, as the messages might cross and cause the deadlock.
