

#Module eredis_pool#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


.



Copyright (c) (C) 2011, Hiroe Shin

__Authors:__ Hiroe Shin ([`shin@mac-hiroe-orz-17.local`](mailto:shin@mac-hiroe-orz-17.local)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_pool-2">create_pool/2</a></td><td>create new pool.</td></tr><tr><td valign="top"><a href="#delete_pool-1">delete_pool/1</a></td><td>delet pool and disconnected to Redis.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td>  
Executes the given command in the specified connection.</td></tr><tr><td valign="top"><a href="#q-3">q/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="create_pool-2"></a>

###create_pool/2##




<pre>create_pool(PoolName::atom(), Size::integer()) -&gt; {ok, pid()} | {error, {already_started, pid()}}</pre>
<br></br>




create new pool.<a name="delete_pool-1"></a>

###delete_pool/1##




<pre>delete_pool(PoolName::atom()) -&gt; ok | {error, not_found}</pre>
<br></br>




delet pool and disconnected to Redis.<a name="q-2"></a>

###q/2##




<pre>q(Client::pid(), Command::iolist()) -&gt; {ok, binary() | [binary()]} | {error, Reason::binary()}</pre>
<br></br>






  
Executes the given command in the specified connection. The  
command must be a valid Redis command and may contain arbitrary  
data which will be converted to binaries. The returned values will  
always be binaries.

WorkerはcheckoutしてPidを得た後にすぐさまcheckinしています。
eredisがnon-blockingなクエリ処理をする仕組みを活かす為です。
<a name="q-3"></a>

###q/3##




<pre>q(Client::pid(), Command::iolist(), Timeout::integer()) -&gt; {ok, binary() | [binary()]} | {error, Reason::binary()}</pre>
<br></br>


<a name="start-0"></a>

###start/0##




`start() -> any()`

<a name="stop-0"></a>

###stop/0##




`stop() -> any()`

