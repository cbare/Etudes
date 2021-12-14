"""
Batch processor template: An application of the template method pattern to
batch processing.

I got myself into a tangle trying to factor out common code in a small batch
processing program. This is a toy version to experiment with.

The `batch_processor_template` function performs timing and error handling,
sourcing work items from an `Iterable` batch and delegating the actual
processing to a Processor with four methods: validate, get_data, process, and
write_result. We could make an ABC for Processors with several implementations
for doing different kinds of work.

Note that the batch `Iterable` and the Processor must be a matched pair,
producing and consuming compatible type. Maybe that's a sign they should be
combined. Otherwise, I'm only assuming that items have an `id` and a decent
`__repr__`.
"""
import dataclasses
import logging
import random
import sys
import traceback
import uuid
from collections import namedtuple
from typing import List

from contexttimer import Timer


def configure_logging(level=logging.INFO):
    logger = logging.getLogger("example")
    handler = logging.StreamHandler(sys.stdout)
    formatter = logging.Formatter('%(asctime)s %(levelname)-8s %(message)s')
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    logger.setLevel(level)
    return logger


logger = configure_logging()


def _generate_id():
    return uuid.uuid4().hex[20:]


def _randomly_fail(p, ex, msg):
    """
    Raise an exception with probability p.
    """
    if random.uniform(0,1) < p:
        raise ex(msg)


@dataclasses.dataclass
class ProcessingStats:
    total_processing_time: float = 0.0
    n_succeeded: int = 0
    n_failed: int = 0
    batch_id: str = None
    items: List = dataclasses.field(default_factory=list)


WhatsitDetails = namedtuple("WhatsitDetails", "id")
WhatsitResult = namedtuple("WhatsitResult", "id status")


def whatsit_batch(n):
    """
    Generate objects that describe work to be done.
    """
    for _ in range(n):
        yield WhatsitDetails(id=_generate_id())


class WhatsitProcessor:
    """
    A processor for Whatsits that sometimes fails.
    """
    def validate(self, whatsit_detail):
        _randomly_fail(p=0.05, ex=ValueError, msg=f'Invalid whatsit: {whatsit_detail.id}!')

    def get_data(self, whatsit_detail):
        _randomly_fail(p=0.05, ex=OSError, msg=f'"{whatsit_detail.id}.whatsit" not found!')
        return whatsit_detail

    def process(self, whatsit):
        _randomly_fail(p=0.05, ex=RuntimeError, msg=f"Processing {whatsit.id} borked!")
        return WhatsitResult(whatsit.id, "ok")

    def write_result(self, result):
        _randomly_fail(p=0.05, ex=OSError, msg=f"Writing {result.id}.result failed!")
        logger.info(f"Writing whatsit_{result.id}.result")


def batch_processor_template(processor, batch, on_error_continue=True):
    """
    Process a batch of items while collecting stats and logging failures.
    """
    stats = ProcessingStats()
    try:
        with Timer() as total_t:
            for item in batch:
                try:
                    logger.info(f"Processing {item}")
                    processor.validate(item)

                    data = processor.get_data(item)
                    result = processor.process(data)
                    processor.write_result(result)

                    stats.n_succeeded += 1
                    stats.items.append({
                        "id": item.id,
                        "status": "ok",
                    })
                except (RuntimeError, OSError, ValueError) as e:
                    logger.error(f"Error processing %s %s\n%s",
                        item, e, traceback.format_exc(limit=None, chain=True))

                    stats.n_failed += 1
                    stats.items.append({
                        "id": item.id,
                        "status": f"{e.__class__.__name__}",
                    })

                    if not on_error_continue:
                        raise e
    finally:
        stats.total_processing_time = total_t.elapsed
        logger.info(f"{stats!r}")


if __name__ == "__main__":
    batch_processor_template(
        processor=WhatsitProcessor(),
        batch=whatsit_batch(20),
    )
