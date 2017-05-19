-define(APP_NAME,dogwood).

%% An account that points out the rights for a specific user.
-record(account,{
	  user,
	  sensors, % List with sensors this account has access to
	  action   % Action on incoming data from accessible sensors
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
