-module(tarrys).
-export([main/0, work/1, work/4]).

-record(node, {name, neighbours = []}).

%% Create a process for every node in a network
start_network({InitiatorName, Nodes}) ->
  %% Spawn a process for each node and store the PIDs
  NodePids = [{N#node.name, spawn(tarrys, work, [N])} || N <- Nodes],
  %% Let each node know what the PIDs of the other nodes are
  [ Pid ! NodePids || {_, Pid} <- NodePids],
  %% Get the initiator's PID
  {_, InitiatorPid} = lists:keyfind(InitiatorName, 1, NodePids),
  %% Send the initial token to the initial node
  InitiatorPid ! [{root, self()}].

work(Node) ->
  receive
    NodePids ->
      %% Only get the PIDs of the neighbours
      NeighbourPids = [
        {NodeName, Pid} ||
        {NodeName, Pid} <- NodePids,
        lists:member(NodeName, Node#node.neighbours)],
      %% Recieve the node PIDs, and then re-call work with default values
      work(Node, NeighbourPids, {}, [])
  end.
work(Node, NodePids, InitialParentPid, SentToPids) ->
  receive
    Token ->
      %% If the `InitialParentPid` was not set, set to the last PID in `Token`
      {ParentName, ParentPid} = case InitialParentPid of
        {} -> lists:last(Token);
        _ -> InitialParentPid
      end,

      %% io:fwrite("~p: Current token: ~p~n", [Node#node.name, Token]),
      %% io:fwrite("~p: Parent PID: ~p~n", [Node#node.name, ParentPid]),

      %% Get all nodes we haven't sent anything to
      UnsentNodePids = [
        {NodeName, Pid} || {NodeName, Pid} <- NodePids,
               not lists:member({NodeName, Pid}, SentToPids),
               Pid /= ParentPid],
      %% io:fwrite("~p: Unsent PIDs: ~p~n", [Node#node.name, UnsentNodePids]),

      %% Append our name and PID to `Token`
      NewToken = (Token ++ [{Node#node.name, self()}]),

      case UnsentNodePids of
        %% If we have no more nodes to send to, send back to our parent
        [] -> ParentPid ! NewToken;
        _ ->
          %% Select random element from `UnsentNodePids`
          {NodeName, Pid} = lists:nth(
            rand:uniform(length(UnsentNodePids)), UnsentNodePids),
          %% Send the token on to the randomly selected node
          Pid ! NewToken,
          work(
            Node,
            NodePids,
            {ParentName, ParentPid},
            %% Add the node we just sent to to `SentToPids`
            (SentToPids ++ [{NodeName, Pid}]))
      end
  end.

%% Get the topology of the network from `stdin`
%% Returns a tuple of the name of the initial node, and a list of nodes
get_topology() ->
  %% Read the initial node name in from the first line
  [InitialNodeName | _] = string:tokens(io:get_line(""), " \n"),
  {InitialNodeName, get_topology([])}.
get_topology(Nodes) ->
  case io:get_line("") of
    eof -> Nodes;
    NodeStr ->
      %% Parse the line for a node
      [NodeName | Neighbours] = string:tokens(NodeStr, " \n"),
      get_topology(
        [#node{name = NodeName, neighbours = Neighbours} | Nodes])
  end.

main() ->
  Topology = get_topology(),
  start_network(Topology),
  receive
    Token ->
      [_ | FinalToken] = [NodeName || {NodeName, _} <- Token],
      %% io:fwrite("Final token: ~p~n", [FinalToken])
      io:fwrite("~s~n", [string:join(FinalToken, " ")])
  end.

