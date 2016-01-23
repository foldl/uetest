-define(value(Key, Config), proplists:get_value(Key,Config)).
-define(import_cfg(Key), {Key, ct:get_config(Key)}).

-define(def_common_case(Name), Name(ConfigData) -> uetest_common_cases:Name(ConfigData)).

-define(summary_db_name, "uetest_summary.dets").

-define(chart_class, chart).
