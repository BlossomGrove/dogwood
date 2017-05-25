-define(APP_NAME,dogwood).

%% An account that points out the rights for a specific user.
%% Note:
%% - fwds is a list with tuples on the form {SensorIds, FwdAction}, where
%%   + SensorIds is a list with sensor ids
%%   + FwdAction is a tuple, MFA
-record(account,{
	  user,
	  keys,  % Encryption keys used when transporting data
	  fwds,  % Controls how (if) to forward data
	  reads  % Controls how (if) to allow reading data
	  }).

%% A sensor that is able to (somehow) deliver sensor data to us.
-record(sensor,{
	  id,
	  unit,
	  desc,
	  provider,
	  ts
	 }).

-record(msg,{
	  ts,          % Timestamp
	  data,        % Raw data
	  channel,     % Channel where message is posted
	  user,        % User posting the message
	  mentions=[], % (list) User mentions
	  tags=[]      % (list) Tags used
	 }).
