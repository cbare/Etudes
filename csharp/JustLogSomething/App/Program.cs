using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Console;
using System;
using System.Threading.Tasks;

namespace JustLogSomething {

    public class Thing
    {
        private readonly ILogger<Thing> _logger;

        public Thing(ILogger<Thing> logger)
        {
            _logger = logger;
            _logger.LogInformation("Thing constructor");
        }

        public void DoSomething(string argument)
        {
            _logger.LogInformation($"Doing something with {argument}!");
        }
    }

    public class ThingBuilder
    {
        public static Thing buildThing() {

            using ILoggerFactory loggerFactory =
                LoggerFactory.Create(builder =>
                    builder.AddSimpleConsole(options =>
                    {
                        options.IncludeScopes = true;
                        options.SingleLine = true;
                        options.TimestampFormat = "HH:mm:ss ";
                    }));
            ILogger<Thing> logger = loggerFactory.CreateLogger<Thing>();

            return new Thing(logger);
        }
    }

    class Program
    {
        public static void Main(string[] inArgs) {
            Console.WriteLine("Hello, World!");

            var thing = ThingBuilder.buildThing();
            thing.DoSomething("flapdoodle");

            System.Threading.Thread.Sleep(1000);
        }
    }

}
