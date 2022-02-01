# streamly-metrics

Collect and send performance metrics from production code.

## How to analyze performance

## Single threaded, non-concurrent programs

Single threaded non-cocnurrent programs are the simplest to analyze. Create a
metrics reporting channel using `newConsoleReporter`. Pass around this channel
to the place where you want to measure the performance. Annotate the function
that you want to measure using benchOnWith. Like this:

```haskell
    chan <- newConsoleReporter
    benchOnWith chan "sum" $ sum [0..1000000]
```

You can create a global channel variable if you do not want to change all the
code to pass around the reporting channel.

```haskell
{-# NOINLINE perfChan #-}
perfChan :: Channel PerfMetrics
perfChan = unsafePerformIO newConsoleReporter

perf = benchOnWith perfChan
```

Then in any module just import `perf` and use it:

```haskell
    perf "sum" $ sum [0..1000000]
```

For lower level APIs see bench and benchWith.

## Issues with concurrent programs

Programs using Haskell threads require a bit more care for correct
benchmarking. The problem is that the performance measurement code collects
metrics from the OS which is not aware of the Haskell threads running on top of
OS threads.

The benchmarking code collects perf metrics from the OS, runs the enclosed code
and collects the metrics again and reports the difference. However, in presence
of Haskell threads the code being measured may block and another Haskell thread
may get scheduled. Thus the perf measurement may include measurements over
other threads as well which may not be what you want.

The following issues may arise:

* The `ProcessCPUTime` metric reports the cpu time of the entire process
  including all the OS threads or Haskell threads in the process. If a
  measurement straddles over Haskell thread yield points, the timing reported
  would include the timing of all the Haskell threads scheduled in between.
* Similarly, the GC statistics would include global timings of the entire
  process, rather than a particular thread.
* The OS's `ThreadCPUTime` metric reports the CPUTime of the OS threads. However,
  the OS threads are started by the Haskell RTS and they can run any Haskell
  threads. Also, after we have taken the initial measurement our thread may be
  scheduled on another thread and our final measurement may be taken on another
  thread, thus the `ThreadCPUTime` would be completely unreliable in this case.

## Measuring round-trip times

When inside a looping handler that is called by some external code we would
like to measure the timing end-to-end from the point where the loop was
started. However, we may not want to change the external library to do so. A
convenient way to do this is to use the special routine provided by this
library. This routine measures the time at a certain point in the program and
ends the measurement when the control flow reaches the same point again. This
way we can measure the round trip timing (using `ThreadCPUTime`) of the loop
without having to annotate the start of the loop. Though the timing of each
iteration is not measured accurately because we are measuring some part of the
previous iteration and rest from the next iteration, but this would average out
over many requests and can still be a very useful metric for the cost of the
loop.
