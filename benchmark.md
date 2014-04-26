Both route to same backend that is nginx returning 200 with 'ok'
--

```bash
## Elixir router
$ wrk -t 12 -c200 -d30s http://www.mycoolapp.com:9080
Running 30s test @ http://www.mycoolapp.com:9080
  12 threads and 200 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    38.55ms    8.38ms 212.84ms   95.47%
    Req/Sec   421.55     48.51   608.00     76.67%
  151123 requests in 30.00s, 37.09MB read
  Socket errors: connect 0, read 42, write 0, timeout 0
Requests/sec:   5036.89
Transfer/sec:      1.24MB

$ wrk -t 12 -c20 -d10s http://www.mycoolapp.com:9080
Running 10s test @ http://www.mycoolapp.com:9080
  12 threads and 20 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     2.47ms  612.81us   9.64ms   88.54%
    Req/Sec   429.11     68.88   666.00     63.86%
  48876 requests in 10.00s, 12.00MB read
  Socket errors: connect 0, read 1, write 0, timeout 0
Requests/sec:   4887.08
Transfer/sec:      1.20MB

## Hipache router
$ wrk -t 12 -c20 -d10s http://www.mycoolapp.com:8087
Running 10s test @ http://www.mycoolapp.com:8087
  12 threads and 20 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    21.53ms   12.83ms  34.42ms   57.25%
    Req/Sec    67.82     91.58   562.00     77.26%
  8184 requests in 10.01s, 1.23MB read
  Socket errors: connect 0, read 0, write 0, timeout 24
Requests/sec:    817.94
Transfer/sec:    125.41KB

$ wrk -t 12 -c200 -d30s http://www.mycoolapp.com:8087
Running 30s test @ http://www.mycoolapp.com:8087
  12 threads and 200 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   198.18ms   48.96ms 231.85ms   89.10%
    Req/Sec    20.28     58.27   270.00     88.97%
  8250 requests in 30.01s, 1.24MB read
  Socket errors: connect 0, read 49, write 0, timeout 2547
Requests/sec:    274.89
Transfer/sec:     42.15KB
```


## More connections
```bash
# elixir
$ wrk -t 12 -c500 -d30s http://www.mycoolapp.com:9080
Running 30s test @ http://www.mycoolapp.com:9080
  12 threads and 500 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    98.11ms   24.07ms 513.83ms   93.49%
    Req/Sec   415.30     61.62     0.95k    85.04%
  149051 requests in 30.00s, 36.60MB read
  Socket errors: connect 0, read 396, write 0, timeout 38
Requests/sec:   4967.64
Transfer/sec:      1.22MB

# Hipache
$ wrk -t 12 -c500 -d30s http://www.mycoolapp.com:8087
Running 30s test @ http://www.mycoolapp.com:8087
  12 threads and 500 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    11.13s     4.75s   13.60s    86.15%
    Req/Sec    19.72     54.54   283.00     88.68%
  7811 requests in 30.02s, 1.17MB read
  Socket errors: connect 0, read 348, write 0, timeout 6564
Requests/sec:    260.21
Transfer/sec:     39.90KB
```
