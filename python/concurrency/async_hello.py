import asyncio

async def upper_cased(value: str) -> str:
    await asyncio.sleep(1)
    return value.upper()

coroutines = [
    upper_cased("h"),
    upper_cased("e"),
    upper_cased("l"),
    upper_cased("l"),
    upper_cased("o"),
    upper_cased(" "),
    upper_cased("w"),
    upper_cased("o"),
    upper_cased("r"),
    upper_cased("l"),
    upper_cased("d"),
]

async def main():
    print("".join(await asyncio.gather(*coroutines)))

if __name__ == '__main__':
    asyncio.run(main())
