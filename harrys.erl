-module(harrys).
-export([main/0, work/1, work/4]).

-record(node, {name, neighbours = []}).

%% Create a process for every node in a network
create_pids(Topology) ->
  {InitiatorName, Nodes} = Topology,
  NodePids = [{N#node.name, spawn(harrys, work, [N])} || N <- Nodes],
  io:fwrite("NodePids: ~p~n", [NodePids]),
  [ Pid ! NodePids || {_, Pid} <- NodePids],
  {_, InitiatorPid} = lists:keyfind(InitiatorName, 1, NodePids),
  InitiatorPid ! [self()].

work(Node) ->
  receive
    NodePids ->
      work(
        Node,
        zip_neighbours(NodePids, Node#node.neighbours),
        {},
        [])
  end.
work(Node, NodePids, InitialParentPid, SentToPids) ->
  receive
    Token ->
      ParentPid = case InitialParentPid of
        {} -> lists:last(Token);
        _ -> InitialParentPid
      end,

      io:fwrite("~p: Current token: ~p~n", [Node#node.name, Token]),
      io:fwrite("~p: Parent PID: ~p~n", [Node#node.name, ParentPid]),

      UnsentNodePids = [
        {NodeName, Pid} || {NodeName, Pid} <- NodePids,
               not lists:member({NodeName, Pid}, SentToPids),
               Pid /= ParentPid],
      io:fwrite("~p: Unsent PIDs: ~p~n", [Node#node.name, UnsentNodePids]),

      NewToken = (Token ++ [self()]),

      case UnsentNodePids of
        [] -> ParentPid ! NewToken;
        [{NodeName, Pid} | _] ->
          Pid ! NewToken,
          work(Node, NodePids, ParentPid, (SentToPids ++ [{NodeName, Pid}]))
      end
  end.

zip_neighbours(NodePids, Neighbours) ->
  [{NodeName, Pid} ||
   {NodeName, Pid} <- NodePids, lists:member(NodeName, Neighbours)].

%% Get the topology of the network
%% Returns a tuple of the name of the initial node, and a list of nodes
get_topology() -> {
    a,
    [
      #node{name = a, neighbours = [b, c]},
      #node{name = b, neighbours = [a, c]},
      #node{name = c, neighbours = [a, b]}]
 }.

main() ->
  Tuple = get_topology(),
  create_pids(Tuple),
  receive
    FinalToken -> io:fwrite("Final token: ~p~n", [FinalToken])
  end.

