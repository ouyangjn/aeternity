-module(aest_db_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    node_can_reuse_db_of_other_node/1,
    pre_db_renaming_node_can_reuse_db_of_other_pre_db_renaming_node/1,
    node_cannot_reuse_db_of_pre_db_renaming_node/1
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(STARTUP_TIMEOUT, 20000).
-define(MINING_TIMEOUT,   3000).
-define(GRACEFUL_STOP_TIMEOUT, 60000).

%=== COMMON TEST FUNCTIONS =====================================================

all() ->
    [ {group, current}
    , {group, db_migration}
    ].

groups() ->
    [ {current, [], [node_can_reuse_db_of_other_node]}
    , {db_migration, [sequence],
        [ pre_db_renaming_node_can_reuse_db_of_other_pre_db_renaming_node
        , node_cannot_reuse_db_of_pre_db_renaming_node
        ]}
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

node_can_reuse_db_of_other_node(Cfg) ->
    node_can_reuse_db_of_other_node_(fun node_spec/2, Cfg).

pre_db_renaming_node_can_reuse_db_of_other_pre_db_renaming_node(Cfg) ->
    node_can_reuse_db_of_other_node_(fun pre_db_renaming_node_spec/2, Cfg).

node_can_reuse_db_of_other_node_(NodeSpecFun, Cfg) ->
    DbHostPath = node_db_host_path(node1, Cfg),
    N1 = NodeSpecFun(node1, DbHostPath),
    N2 = NodeSpecFun(node2, DbHostPath),
    aest_nodes:setup_nodes([N1, N2], Cfg),
    start_and_wait_node(node1, ?STARTUP_TIMEOUT, Cfg),
    TargetHeight = 3,
    aest_nodes:wait_for_value({height, TargetHeight}, [node1], TargetHeight * ?MINING_TIMEOUT, Cfg),
    #{hash := BlockHash} = aest_nodes:get_block(node1, TargetHeight),
    aest_nodes:stop_node(node1, ?GRACEFUL_STOP_TIMEOUT, Cfg),
    start_and_wait_node(node2, ?STARTUP_TIMEOUT, Cfg),
    aest_nodes:wait_for_value({height, TargetHeight}, [node2], ?STARTUP_TIMEOUT, Cfg),
    ?assertMatch({ok, 200, _}, get_block_by_hash(node2, BlockHash)),
    ok.

%% Create DB with node name `epoch@localhost`, then check that node with name `aeternity@localhost` cannot start.
node_cannot_reuse_db_of_pre_db_renaming_node(Cfg) ->
    DbHostPath = node_db_host_path(old_node, Cfg),
    OldN = pre_db_renaming_node_spec(old_node, DbHostPath),
    NewN = node_spec(new_node, DbHostPath),
    aest_nodes:setup_nodes([OldN, NewN], Cfg),
    start_and_wait_node(old_node, ?STARTUP_TIMEOUT, Cfg),
    aest_nodes:stop_node(old_node, ?GRACEFUL_STOP_TIMEOUT, Cfg),
    aest_nodes:start_node(new_node, Cfg),
    %% TODO How to assert that node failed to start reporting correct log.
    aest_nodes:stop_node(new_node, ?GRACEFUL_STOP_TIMEOUT, Cfg),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

get_block_by_hash(NodeName, Hash) ->
    aest_nodes:request(NodeName, 'GetKeyBlockByHash', #{hash => Hash}).

start_and_wait_node(NodeName, Timeout, Cfg) ->
    aest_nodes:start_node(NodeName, Cfg),
    aest_nodes:wait_for_value({height, 0}, [NodeName], Timeout, Cfg),
    %% Hardcode expectation that node picks user config
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(NodeName),
    ok.

node_db_host_path(NodeName, Config) ->
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    filename:join(PrivDir, format("~s_db", [NodeName])).

format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

node_spec(Name, DbHostPath) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:local"}, db_path => {DbHostPath, DbGuestPath}}).

pre_db_renaming_node_spec(Name, DbHostPath) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:v1.4.0"}, db_path => {DbHostPath, DbGuestPath}, config_guest_path => "/home/aeternity/.epoch/epoch/epoch.yaml"}).
