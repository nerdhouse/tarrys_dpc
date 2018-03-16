-module(harrys).
-compile(export_all).

-record(node, {name, neighbours = []}).

%% Create a process for every node in a network
create_pids(Topology) ->
  {InitialNodeName, Nodes} = Topology,
  [spawn(harrys, work, [N]) || N <- Nodes].

work(Node) ->
  io:fwrite("~p: I am ~p~n", [self(), Node#node.name]),
  ok.

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
  create_pids(Tuple).

