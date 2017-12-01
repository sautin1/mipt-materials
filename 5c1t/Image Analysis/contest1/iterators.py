import itertools


def roundrobin(*iterables):
    """"roundrobin('ABC', 'D', 'EF') --> A D E B F C
    Itertools recipe.

    """
    # Recipe credited to George Sakkis
    num_active = len(iterables)
    nexts = itertools.cycle(iter(it).__next__ for it in iterables)
    while num_active:
        try:
            for next in nexts:
                yield next()
        except StopIteration:
            # Remove the iterator we just exhausted from the cycle.
            num_active -= 1
            nexts = itertools.cycle(itertools.islice(nexts, num_active))


def repeatfunc(func, times=None, *args):
    """Repeat calls to func with specified arguments.

    Itertools recipe.
    Example:  repeatfunc(random.random)
    """
    if times is None:
        return itertools.starmap(func, itertools.repeat(args))
    return itertools.starmap(func, itertools.repeat(args, times))


def repeat_infinitely(func, *args):
    return itertools.chain.from_iterable(repeatfunc(func, None, *args))
