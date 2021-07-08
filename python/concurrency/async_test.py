"""
"""
import asyncio
import logging
import time

from contexttimer import Timer

async def lazy_worker(i):
    logging.info("Lazy worker %d taking a snooze...", i)
    time.sleep(1)
    logging.info("Lazy worker %d done.", i)


async def main():
    with Timer() as timer:
        n = 10
        logging.info("Create %d coroutines.", n)
        coroutines = [lazy_worker(i) for i in range(n)]
        await asyncio.gather(*coroutines)

    print(timer.elapsed)


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s: %(message)s",
        level=logging.INFO,
        datefmt="%H:%M:%S"
    )
    asyncio.run(main())
