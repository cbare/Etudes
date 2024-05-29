# Just Log Something

Learning C#. Specifically trying to understand weird behavior of the `Microsoft.Extensions.Logging` library. 

Apparently, rather than write out log lines synchronously, the logging library queues messages to be written and writes them out in a separate thread. This is discussed here:

- [Logging in .Net core console application not working][4]
- [ConsoleLogger Queue optimization can lead to missed log entries #79812][3]

This can cause unexpected and non-deterministic behavior like missing log lines when the program exits and out-of-order messages when mixed with `Console.WriteLine`. This seems to be the result of a Java-esque philosophy of optimizing toward "non-trivial" apps, favoring complexity by default. 🙄


## Commands

```sh
dotnet build
```

```sh
dotnet run --project App\JustLogSomething.csproj --logger "console;verbosity=detailed"
```

```sh
dotnet test --logger "console;verbosity=detailed"
```

* runs in Powershell and WSL.


[1]: https://learn.microsoft.com/en-us/dotnet/core/extensions/logging?tabs=command-line
[2]: https://learn.microsoft.com/en-us/dotnet/core/extensions/console-log-formatter
[3]: https://github.com/dotnet/runtime/issues/79812
[4]: https://stackoverflow.com/questions/51326463/logging-in-net-core-console-application-not-working
