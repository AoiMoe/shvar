-type namespace() :: atom().
-type key() :: any().
-type val() :: any().
-type id(Key) :: Key | shvar:full_id(Key).
-type id() :: id(key()).

-define(COND(Cond, Then, Else), case Cond of true -> Then; false -> Else end).
