"""
"""
import logging
import threading
import time

from contexttimer import Timer

def lazy_worker(i):
    logging.info("Lazy worker %d taking a snooze...", i)
    time.sleep(1)
    logging.info("Lazy worker %d done.", i)


def main():
    with Timer() as timer:
        n = 10
        logging.info("Create %d threads.", n)
        threads = [
            threading.Thread(target=lazy_worker, args=(i,))
            for i in range(n)
        ]

        for t in threads:
            t.start()
        
        for t in threads:
            t.join()

    print(timer.elapsed)


if __name__ == "__main__":
    logging.basicConfig(
        format="%(asctime)s: %(message)s",
        level=logging.INFO,
    )
    main()

