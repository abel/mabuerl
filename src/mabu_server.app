{application, mabu_server,
  [
    {description, "Demo mabu server"},
    {vsn, "1.0"},
    {id, "mabu_server"},
    {modules, [mabu_listener, mabu_client]},
    {registered, [mabu_server_sup, mabu_listener_sup, mabu_client_sup]},
    {applications, [kernel, stdlib]},
    %%
    %% mod: Specify the module name to start the application, plus args
    %%
    {mod, {mabu_server_sup, []}},
    {env, []}
  ]
}.
