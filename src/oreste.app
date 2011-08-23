{application, oreste,
 [{description, "oreste"},
  {vsn, "0.1"},
  {modules, [
    oreste,
    oreste_app,
    oreste_sup,
    oreste_deps,
    oreste_resource
  ]},
  {registered, []},
  {mod, {oreste_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
