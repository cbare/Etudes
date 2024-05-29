using Microsoft.Extensions.Logging;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using JustLogSomething;

namespace JustLogSomething.Tests
{
    [TestClass()]
    public class LoggingTest
    {
        private readonly Thing _thing = ThingBuilder.buildThing();

        [TestMethod("First test")]
        public void FirstTest()
        {
            _thing.DoSomething("test 1");
        }

        [TestMethod("A Bogus test")]
        public void BogusTest()
        {
            _thing.DoSomething("test 2");
        }
    }
}
