

#Module eredis_pool_sup#
* [Function Index](#index)
* [Function Details](#functions)


__Behaviours:__ [`supervisor`](supervisor.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_pool-3">create_pool/3</a></td><td>create new pool.</td></tr><tr><td valign="top"><a href="#delete_pool-1">delete_pool/1</a></td><td>delet pool and disconnected to Redis.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="create_pool-3"></a>

###create_pool/3##


<pre>create_pool(PoolName::atom(), Size::integer(), Options::[tuple()]) -&gt; {ok, pid()} | {error, {already_started, pid()}}</pre>
<br></br>


create new pool.<a name="delete_pool-1"></a>

###delete_pool/1##


<pre>delete_pool(PoolName::atom()) -&gt; ok | {error, not_found}</pre>
<br></br>


delet pool and disconnected to Redis.<a name="init-1"></a>

###init/1##


`init(X1) -> any()`

<a name="start_link-0"></a>

###start_link/0##


`start_link() -> any()`

<a name="start_link-2"></a>

###start_link/2##


`start_link(Pools, GlobalOrLocal) -> any()`

