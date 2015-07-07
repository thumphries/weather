Notes
-----

- Use the `--help` flag until I write up the usage.
- There are quite arbitrary bounds on the numbers generated; standard
  deviation is also quite low. The mean temperature on generated data
  tends to be within a very narrow range. At time of writing, unknown
  stations were far too prominent, flooding the data with Kelvin.
- I'm pretty sure Parsec has some lazy combinators like `many1`, but
  Attoparsec sure doesn't. The input stream kept getting forced. This led
  to the use of Pipes, which ended up being very nifty for the other
  parts of the task. All the components are Pipes producers, and all
  processing jobs are Pipes Pipes.
- Totally disregarded floating point error, so there is probably quite
  a lot of it. Smarter implementation would use some kind of petty
  fusion, (collect like units together first in a strict tuple, apply
  conversions last, minimising the number of additions.)
- Calculating distance on potentially-unordered data seemed like a
  trap. I required the user to estimate a maximum delay, so we can use
  a minheap as cache and flush out more accurate distance
  calculations.
- I know `UTCTime` et al are hella slow, but `thyme` pulls in `lens`,
  and we don't have all day to compile this thing.

Check out the commit history if you'd like to see me stumble through a
couple of representations. Using typeclasses for Length and
Temperature always seemed like the idiomatic way to go, but there was
also the need to have a heterogeneous list or stream of measurements.
I used a GADT to allow a polymorphic constructor, but... couldn't
normalise the data, since the types were buried. Of course, bringing
the types out to the top level would have required either HList or
universally-quantified functions. The concept I'd been reaching for
(and missing) was just `-XExistentialQuantification` with
`RankNTypes`, and using `forall` instead of the GADT did the job.


Task: Weather Observations
--------------------

Build a tool to mine the logs of a weather balloon for important
information.

#### Requirements

There is a weather balloon traversing the globe, periodically taking
observations. At each observation, the balloon records the temperature
and its current location. When possible, the balloon relays this data
back to observation posts on the ground.

A log line returned from the weather balloon looks something like this:

```
2014-12-31T13:44|10,5|243|AU
```

More formally this is:

```
<timestamp>|<location>|<temperature>|<observatory>
```

Where the `timestamp` is `yyyy-MM-ddThh:mm` in UTC.

Where the `location` is a co-ordinate `x,y`. And x, and y are natural numbers in observatory specific units.

Where the `temperature` is an integer representing temperature in observatory specific units.

Where the `observatory` is a code indicating where the measurements were relayed from.

Data from the balloon is of varying quality, so don't make any
assumptions about the quality of the input.

Data from the balloon often comes in large batches, so assume you may
need to deal with data that doesn't fit in memory.

Data from the balloon does not necessarily arrive in order.

Unfortunately, units of measurement are dependent on the
observatory. The following is a lookup table for determining the
correct unit of measure:

| Observatory | Temperature | Distance |
| ----------- | ----------- | -------- |
| AU          | celsius     | km       |
| US          | fahrenheit  | miles    |
| FR          | kelvin      | m        |
| All Others  | kelvin      | km       |

We need a program (or set of programs) that can perform the following
tasks:

 1. Given that it is difficult to obtain real data from the weather
    balloon we would first like to be able to generate a test file of
    representative (at least in form) data for use in simulation and
    testing. This tool should be able to generate at least 500 million
    lines of data for testing your tool. Remember that the data is not
    reliable, so consider including invalid and out of order lines.

 2. Produce statistics of the flight. The program should be able to
    compute any combination of the following on request:

    - The minimum temperature.

    - The maximum temperature.

    - The mean temperature.

    - The number of observations from each observatory.

    - The total distance travelled.

 3. Produce a normalized output of the data, where given desired
    units for temperature and distance, an output file is produced
    containing all observations with the specified output units.
