# oneup_metrics

**High performance metrics app in erlang.**

Usable as is, but still largely UNDER CONSTRUCTION.  

Inspired by and built upon `oneup` app: https://github.com/andytill/oneup

NOTE: Only gauges and counters are available at this time.  Histograms and meters coming soon. 

It is designed to avoid process mailboxes and ets tables when updating metrics in high performance applications, 
at the expense of slightly more complicated design which we try to abstract away as much as possible with `oneup_metrics` module.

The complexity comes from the need to pass a metrics map from parent or supervisor to every process that updates metrics.
Also, only a subset of metrics relevant to the process should be passed to it for performance and memory considerations.  

Then the process ends up with this metrics map subset in it's process dictionary, and that's how the process will be able to update global metrics during its lifetime.
 
**Usage:**

First of all, see common tests for usage examples.

STEP 1. 
`sys.config` is expecting `metrics_config` app variable which is a list of metric names. 
Eventually it will be a list of metric names with metric types, but currently only counters are supported. 

Metric name is a list of atoms, i.e.
```
[total, requests]
[total, responses]
[total, sockets]
[total, tcp, requests],
[remote, tcp, requests],
[remote, tcp, sockets]
[local, tcp, requests],
[local, tcp, sockets]
```

`oneup_metrics` app comes with sample `sys.config` which might change format in the future once different metric types, like meters and histograms are added.

STEP 2. 
In your top-level supervisor, app module, or any other process that will be running along your app or spawned very infrequently, get the global MetricsMap (should be initialized when oneup_metrics app is initialize):

```MetricsMap = oneup_metrics:initial_get_config()```

OR for processes likely to update only a subset of the global metrics

```
Prefix = [local, tcp],
LocalTcpMetrics =  oneup_metrics:initial_get_config(Prefix)```
````

Pass the MetricsMap to any child processes that are spawned very frequently:

spawn(tcp_request, handle, [Request, MetricsMap])  

Inside the process that will be updating metrics call  `oneup_metrics:enable(MetricsMap)`
```
-module(tcp_request).

handle(Request, MetricsMap)->
%% this puts MetricsMap into process dictionary and metrics can now be updated from any method called by this process
oneup_metrics:enable(MetricsMap),  

oneup_metrics:increment([total, tcp, requests]),
case Request of 
{local, ReqBody} -> oneup_metrics:increment([local, tcp, requests]);
{remote, ReqBody} -> oneup_metrics:increment([remote, tcp, requests])
end,
process_request(Request).
```



**BUILD-IN REPORTERS**

It comes with built-in http reporter which is initialized if `http_port` app var is defined.   

If it is, oneup_metric stats are accessible from `localhost:_http_port_/[prefix]` where prefix is any prefix defined in your metrics_config, i.e.
Example 1. 
`curl -s localhost:3339/a`
returns:
```
b.c1.d1.ref1: 0
b.c1.d2.ref2: 0
b.c2.d1.ref3: 0
b.c2.d1.ref4: 0
```

Example 2. 
`curl -s localhost:3339/a/b/c1`
returns:
```
d1.ref1: 0
d1.ref3: 0
d2.ref2: 0
```

Example 3. 
`curl -s localhost:3339`  return all metrics in your config


There is also basic erlang system info reporter that displays the following info:
`processes`
`ports`
`large_mboxes`
`memory` 

currently expecting only one config variable `mbox_threshold` 
probably will be more in the future as the system_info expands

Example 3. 
`curl -s localhost:3339/system_info`
returns:

```Processes: 116
Ports: 18
Large Mboxes:
     init: 0
     erts_code_purger: 0
     <0.2.0>: 0
     <0.3.0>: 0
     <0.5.0>: 0
     .
     .
     .

Memory:
    total: 26434320
    processes: 7312080
    processes_used: 7309696
    system: 19122240
    atom: 429593
    atom_used: 398835
    binary: 56416
    code: 10007912
    ets: 495968
```







