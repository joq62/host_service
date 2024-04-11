%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(host). 
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("host.hrl").
-include("host.rd").


%% API

-export([
	 
	 all_hosts/0,
	 get_hostname/1,
	 get_ip/1,
	 get_ssh/1,
	 get_userid/1,
	 get_passwd/1,
	 get_application_config/1
	 
	 
	]).


-export([
	 all_filenames/0,
	 read_file/1,
	 is_repo_updated/0,
	 update_repo/0,
	 clone/0,
	 delete/0,
	 update/0
	]).

-export([
	 update_repo_dir/1,
	 update_git_path/1
	 
	]).

%% admin




-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		repo_dir,
		git_path
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Retunrs all hostnames that are in repos files   
%% 
%% @end
%%--------------------------------------------------------------------
-spec all_hosts() -> 
	  {ok,HostNamse::string()} | {error,Reason :: term()}.

all_hosts() ->
    gen_server:call(?SERVER,{all_hosts},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Returns the hostname in file FileName   
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_hostname(FileName::string()) -> 
	  {ok,HostName::string()} | {error,Reason :: term()}.

get_hostname(FileName) ->
    gen_server:call(?SERVER,{get,hostname,FileName},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Retunrs all hostnames that are in repos files   
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_ip(FileName::string()) -> 
	  {ok,IpAddr::string()} | {error,Reason :: term()}.

get_ip(FileName) ->
    gen_server:call(?SERVER,{get,ip,FileName},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Retunrs all hostnames that are in repos files   
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_ssh(FileName::string()) -> 
	  {ok,SshPort::integer()} | {error,Reason :: term()}.

get_ssh(FileName) ->
    gen_server:call(?SERVER,{get,ssh_port,FileName},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Retunrs all hostnames that are in repos files   
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_userid(FileName::string()) -> 
	  {ok,UserId::string()} | {error,Reason :: term()}.

get_userid(FileName) ->
    gen_server:call(?SERVER,{get,userid,FileName},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Retunrs all hostnames that are in repos files   
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_passwd(FileName::string()) -> 
	  {ok,Passwd::string()} | {error,Reason :: term()}.

get_passwd(FileName) ->
    gen_server:call(?SERVER,{get,password,FileName},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Retunrs all hostnames that are in repos files   
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_application_config(FileName::string()) -> 
	  {ok,ApplicationConfig::term()} | {error,Reason :: term()}.

get_application_config(FileName) ->
    gen_server:call(?SERVER,{get,application_config,FileName},infinity).




%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%--------------------------------------------------------------------
%% @doc
%% Reads the filenames in the RepoDir   
%% 
%% @end
%%--------------------------------------------------------------------
-spec all_filenames() -> 
	  {ok,FileNames::term()} | {error,Reason :: term()}.

all_filenames() ->
    gen_server:call(?SERVER,{all_filenames},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Reads the filenames in the RepoDir   
%% 
%% @end
%%--------------------------------------------------------------------
-spec read_file(FileName ::string()) -> 
	  {ok,Info::term()} | {error,Reason :: term()}.

read_file(FileName) ->
    gen_server:call(?SERVER,{read_file,FileName},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_repo() -> 
	  ok | {error, Reason :: term()}.
update_repo() ->
    gen_server:call(?SERVER,{update_repo},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Reads the filenames in the RepoDir   
%% 
%% @end
%%--------------------------------------------------------------------
-spec delete() -> 
	  ok | {error,Reason :: term()}.

delete() ->
    gen_server:call(?SERVER,{delete},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone() -> 
	  ok | {error, Reason :: term()}.
clone() ->
    gen_server:call(?SERVER,{clone},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_repo_updated() -> 
	  true | false | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
is_repo_updated() ->
    gen_server:call(?SERVER,{is_repo_updated},infinity).


%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_repo_dir(RepoDir::string()) -> 
	  true | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
update_repo_dir(RepoDir) ->
    gen_server:call(?SERVER,{update_repo_dir,RepoDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%%    
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_git_path(GitPath::string()) -> 
	  true | {error,Reason :: term()}.

% {error,["Inventory doesnt exists, need to clone"]} .
update_git_path(GitPath) ->
    gen_server:call(?SERVER,{update_git_path,GitPath},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update() -> ok | Error::term().
update()-> 
    gen_server:call(?SERVER, {update},infinity).




%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    
    {ok, #state{
	    repo_dir=?RepoDir,
	    git_path=?RepoGit
	  
	    
	   },0}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.




handle_call({all_hosts}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_host:all_hosts(RepoDir) of
	       {ok,R}->
		    {ok,R};
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,AllFileNames}->
		  {ok,AllFileNames};
	      ErrorEvent->
		% io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  ErrorEvent
	  end,
    {reply, Reply,State};


handle_call({get,Type,FileName}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try lib_host:get(Type,FileName,RepoDir) of
	       {ok,R}->
		    {ok,R};
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,AllFileNames}->
		  {ok,AllFileNames};
	      ErrorEvent->
		 io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  ErrorEvent
	  end,
    {reply, Reply,State};

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

handle_call({all_filenames}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try rd:call(git_handler,all_filenames,[RepoDir],5000) of
	       {ok,R}->
		    {ok,R};
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,AllFileNames}->
		  {ok,AllFileNames};
	      ErrorEvent->
		% io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({read_file,FileName}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try rd:call(git_handler,read_file,[RepoDir,FileName],5000) of
	       {ok,R}->
		    {ok,R};
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Info}->
		  {ok,Info};
	      ErrorEvent->
		 ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({update_repo}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try rd:call(git_handler,update_repo,[RepoDir],5000) of 
	       {ok,R}->
		   {ok,R};
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,Info}->
		  {ok,Info};
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};

handle_call({clone}, _From, State) ->
    RepoDir=State#state.repo_dir,
    GitPath=State#state.git_path,
    Result=try rd:call(git_handler,clone,[RepoDir,GitPath],5000) of 
	       ok->
		   ok;
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      ok->
		  ok;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply,State};


    
handle_call({is_repo_updated}, _From, State) ->
    RepoDir=State#state.repo_dir,
    Result=try rd:call(git_handler,is_repo_updated,[RepoDir],5000) of 
	       {ok,R}->
		   {ok,R};
	       Error->
		   Error
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,IsUpdated}->
		  %io:format("IsUpdated ~p~n",[{IsUpdated,?MODULE,?LINE}]),
		   IsUpdated;
	      ErrorEvent->
		  ErrorEvent
	  end,
    {reply, Reply, State};

handle_call({update}, _From, State) ->
%    io:format(" ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    RepoDir=State#state.repo_dir,
    GitPath=State#state.git_path,
    Reply=try lib_host:init(RepoDir,GitPath) of
	      ok->
		  ok;
	      {error,Reason}->
		  {error,Reason}
	  catch
	      Event:Reason:Stacktrace ->
		  {Event,Reason,Stacktrace,?MODULE,?LINE}
	  end,
    spawn(fun()->lib_host:timer_to_call_update(?Interval) end),
    {reply, Reply, State};

handle_call({update_repo_dir,RepoDir}, _From, State) ->
    NewState=State#state{repo_dir=RepoDir},
    Reply=ok,
    {reply, Reply, NewState};

handle_call({update_git_path,GitPath}, _From, State) ->
    NewState=State#state{git_path=GitPath},
    Reply=ok,
    {reply, Reply, NewState};

%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
 %   io:format("timeout ~p~n",[{?MODULE,?LINE}]),
  
    initial_trade_resources(),
    RepoDir=State#state.repo_dir,
    GitPath=State#state.git_path,
    Result=try lib_host:init(RepoDir,GitPath) of
	       ok->
		   ok;
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    spawn(fun()->lib_host:timer_to_call_update(?Interval) end),
    ?LOG_NOTICE("Result init",[Result]),
    ?LOG_NOTICE("Server started ",[?MODULE]),
    {noreply, State};


handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
initial_trade_resources()->
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
    timer:sleep(3000),
    ok.
