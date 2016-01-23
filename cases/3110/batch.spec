{ct_hooks, [uetest_summary_hook]}.
{auto_compile, false}.

{config, ["batch.config"]}.

{suites, "../../suite/", uetest_batch_SUITE}.

%{groups, "../../suite/", uetest_batch_SUITE, ['9.blind_ho']}.



