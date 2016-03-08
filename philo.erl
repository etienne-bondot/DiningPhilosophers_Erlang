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
% -> philosopher(Index, LeftForkPid, RightForkPid, action)
%

spawn_pair() ->
    RightFork = spawn(?MODULE, fork, [0, on_table]),
    LeftFork = spawn(?MODULE, fork, [1, on_table]),
    Name = get_name(0),
    verbose(2, "Starting ~s~n", [capitalize(atom_to_list(Name))]),
    register(Name, spawn(?MODULE, philosopher, [0, LeftFork, RightFork, thinking, 0])),
    % the left fork become the right fork for the next philosopher
    % and we keep the right fork to become the left fork of the last philosopher
    spawn_pair(1, LeftFork, RightFork).

spawn_pair(?PHILOSOPHERS, _, _) -> ok;

spawn_pair(N, RightFork, FirstFork) ->
    Name = get_name(N),
    verbose(2, "Starting ~s~n", [capitalize(atom_to_list(Name))]),
    case N + 1 =:= ?PHILOSOPHERS of
        % for the last philosopher, the left fork is the right fork of the
        % first philosopher
        true -> LeftFork = FirstFork;
        false -> LeftFork = spawn(?MODULE, fork, [N + 1, on_table])
    end,
    register(Name, spawn(?MODULE, philosopher, [N, LeftFork, RightFork, thinking, 0])),
    % the left fork become the right fork for the next philosopher
    % and we keep the right fork to become the left fork of the last philosopher
    spawn_pair(N + 1, LeftFork, FirstFork).

%
% report process
%

capitalize([X|Xs]) when X >= $a orelse X =< $z -> [X - 32|Xs].

exit_properly(?PHILOSOPHERS) -> done;
exit_properly(N) ->
    get_name(N) ! please_exit,
    exit_properly(N + 1).

report() ->
    receive
        {philo, Idx, eating, Cycle} ->
            verbose(2, "[~w] ~s\t: has the right and left fork~n", [Cycle, capitalize(atom_to_list(get_name(Idx)))]),
            verbose(1, "[~w] ~s\t: ~s~n", [Cycle, capitalize(atom_to_list(get_name(Idx))), atom_to_list(eating)]);
        {philo, Idx, thinking, Cycle} ->
            verbose(1, "[~w] ~s\t: ~s~n", [Cycle, capitalize(atom_to_list(get_name(Idx))), atom_to_list(thinking)]);
        {philo, Idx, hungry, Cycle} ->
            verbose(1, "[~w] ~s\t: ~s~n", [Cycle, capitalize(atom_to_list(get_name(Idx))), atom_to_list(hungry)]);
        {philo, Idx, pick_left, Cycle} ->
            verbose(1, "[~w] ~s\t: picks up the left fork~n", [Cycle, capitalize(atom_to_list(get_name(Idx)))]);
        {philo, Idx, pick_right, Cycle} ->
            verbose(1, "[~w] ~s\t: picks up the right fork~n", [Cycle, capitalize(atom_to_list(get_name(Idx)))]);
        {philo, Idx, drop_left, Cycle} ->
            verbose(1, "[~w] ~s\t: puts down the left fork~n", [Cycle, capitalize(atom_to_list(get_name(Idx)))]);
        {philo, Idx, drop_right, Cycle} ->
            verbose(1, "[~w] ~s\t: puts down the right fork~n", [Cycle, capitalize(atom_to_list(get_name(Idx)))]);
        {fork, Pid, on_table} ->
            verbose(1, "Fork ~w\t: on table~n", [Pid]);
        {fork, Pid, in_use} ->
            verbose(1, "Fork ~w\t: in use~n", [Pid])
    after 2000 ->
        % in case of fool philosophers
        verbose(2, "Philosophers are stucked... program is shutting down.", []),
        exit_properly(0),
        exit(normal)
    end,
    report().


%
% fork process
%

fork(Pid, Status) ->
    receive
        {pick, Source} ->
            case Status =:= on_table of
                true ->
                    Source ! ok,
                    report ! {fork, Pid, in_use};
                false ->
                    Source ! ko
            end,
            fork(Pid, in_use);
        drop ->
            report ! {fork, Pid, on_table},
            fork(Pid, on_table)
    end.

%
% philosopher process
%

philosopher(Idx, Left, Right, thinking, Cycle) ->
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    report ! {philo, Idx, thinking, Cycle},
    timer:sleep(random:uniform(1000 - 100 + 1) + 100),
    philosopher(Idx, Left, Right, hungry, Cycle);

% don't forget to drop the forks before being shutdown, because
% there might be still hungry philosophers that wait for forks
philosopher(Idx, Left, Right, hungry, Cycle) ->
    report ! {philo, Idx, hungry, Cycle},
    % if the philosopher id mod 2 == 0 check left and right,
    % otherwise check right and left
    case Idx rem 2 =:= 0 of
        true ->
            check_fork(Idx, Left, left, Cycle),
            check_fork(Idx, Right, right, Cycle);
        false ->
            check_fork(Idx, Right, right, Cycle),
            check_fork(Idx, Left, left, Cycle)
    end,
    philosopher(Idx, Left, Right, eating, Cycle);

philosopher(Idx, Left, Right, eating, Cycle) ->
    report ! {philo, Idx, eating, Cycle},
    timer:sleep(random:uniform(1000 - 100 + 1) + 100),
    drop_fork(Idx, Left, Right, Cycle),
    philosopher(Idx, Left, Right, thinking, Cycle + 1).

% in case of deadlock
philosopher(Idx, shutdown) ->
    verbose(2, "~n~s exits... :'(~n", [capitalize(atom_to_list(get_name(Idx)))]),
    exit(normal).

%
% check the availability of a fork and loop again if the fork is in use
% if stuck in loop for a long time (~2000ms), reporter should tell the philosopher to
% killhimself.
%

check_fork(Idx, Fork, Direction, Cycle) ->
    Fork ! {pick, get_name(Idx)},
    verbose(2, "~n~s asks for the ~s fork~n", [capitalize(atom_to_list(get_name(Idx))), atom_to_list(Direction)]),
    receive
        ok ->
            case Direction =:= left of
                % if the fork is on table, take the fork
                true -> report ! {philo, Idx, pick_left, Cycle};
                false -> report ! {philo, Idx, pick_right, Cycle}
            end;
        % else ask again until the fork in on table
        ko -> check_fork(Idx, Fork, Direction, Cycle);
        please_exit -> philosopher(Idx, shutdown)
    end.

%
% after eating, the philosopher has to drop both forks on the table
% this allow the other hungry philosophers to take them
%

drop_fork(Idx, Left, Right, Cycle) ->
    Left ! drop,
    report ! {philo, Idx, drop_left, Cycle},
    Right ! drop,
    report ! {philo, Idx, drop_right, Cycle}.

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
% But there is no such case in this project because there are not two interdependent
% processes that are each waiting for a message from the other, but processes
% which send a message and wait for an answer. The design pattern is likely the
% same as a client/server.
% To understand this implementation, when a philosopher is spawn, he gets in parameters
% the two forks pid linked to him, the left and the right. So we have to spawn two first forks
% passed in parameter of the first philosopher and then the left fork become the right fork
% of the next philosopher and a new fork is spawn to be his left fork and so on.
% At the begining we keep the first right fork in memory to set it as the left fork of
% the last philosopher.
% In that way, each philosopher knows his two forks pid and could asks the forks is they are available
% and then wait for a OK/KO answer from the forks. The forks don't send a
% message first but are waiting for a message coming from the philosopher to
% send them a response. And the reporter is just waiting for an incoming message
% from the philosopher without sending response because its main purpose is to
% report what happened, but philosopher's actions don't depend on the reporter.
% In case of infinite loop, which might never occur, and so the reporter does not receive
% any message from the philosopher during 2s, it will timeout and
% send to all philosophers a message telling them to exit properly before
% exiting itself.
%
% 2. Assuming that there is a danger of deadlock in the system, answer one of the
% following questions: (a) How could the deadlock be avoided? (preventing it from
% occurring in the first place)
%
% The only rule to avoid deadlock is that if process A sends a message and waits
% for a response from process B, process B is not allowed, to do a synchronous call
% to process A, as the messages might cross and cause the deadlock.
% But more generally, we could set a timeout, like for the reporter in case of
% deadlock. Them, if a timeout is triggered the process terminates and if
% we have linked the processes the termination will be propagated to them.
