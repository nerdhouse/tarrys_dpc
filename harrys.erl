-module(harrys).
-export([main/0, work/1, work/2]).

-record(node, {name, neighbours = []}).

%% Create a process for every node in a network
create_pids(Topology) ->
  {InitiatorName, Nodes} = Topology,
  NodePids = [{N#node.name, spawn(harrys, work, [N])} || N <- Nodes],
  [ Pid ! NodePids || {_, Pid} <- NodePids],
  {_, InitiatorPid} = lists:keyfind(InitiatorName, 1, NodePids),
  InitiatorPid ! [].

work(Node) ->
  receive
    NodePids -> work(Node, zip_neighbours(NodePids, Node#node.neighbours))
  end.
work(Node, Pids) ->
  receive
    Token -> io:fwrite("~p: Got token: ~p~n", [Node#node.name, Token])
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
  ok.

