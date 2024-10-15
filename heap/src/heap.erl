-module(heap).

-export([insert/2, remove_min/1]).

-type heap_node() :: {integer(), integer(), heap_node() | nil, heap_node() | nil}.



-spec insert(integer(), heap_node()) -> heap_node().
insert(_, _) -> ok.

-spec remove_min(heap_node()) -> heap_node().
remove_min(_) -> ok.


-spec merge_left(heap_node()) -> heap_node().
merge_left(_) -> ok.

-spec merge_right(heap_node()) -> heap_node().
merge_right(_) -> ok.