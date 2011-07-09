{application,etdd_yaws,
             [{id,"ETDD-yaws"},
              {vsn,"0.0.1"},
              {description,"Embedded yaws webserver"},
              {modules,[etdd_yaws_app,etdd_yaws_server,etdd_yaws_sup]},
              {registered,[etdd_yaws_sup,etdd_yaws_server]},
              {applications,[kernel,stdlib,sasl,gs,appmon]},
              {mod,{etdd_yaws_app,[]}},
              {env,[]}]}.
