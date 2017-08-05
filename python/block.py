#!/usr/bin/env python3

# Copied with minor modifications from:

# https://medium.com/crypto-currently/lets-build-the-tiniest-blockchain-e70965a248b

from datetime import datetime
from hashlib import sha256
from itertools import islice

from toolz import iterate, take, drop


class Block:

    def __init__(self, index, timestamp, data, previous_hash):
        self.index = index
        self.timestamp = timestamp
        self.data = data
        self.previous_hash = previous_hash
        self.hash = self.hash_block()

    def hash_block(self):
        sha = sha256()
        sha.update(''.join(str(getattr(self, a))
                           for a in ['index',
                                     'timestamp',
                                     'data',
                                     'previous_hash']).encode('utf-8'))
        return sha.hexdigest()

    @classmethod
    def create_genesis_block(cls):
        return cls(0, datetime.now(), "Genesis Block", "0")

    def next(self, data=None):
        index = self.index + 1
        return self.__class__(index,
                              self.timestamp,
                              data or "Hey! I'm block %d" % index,
                              self.previous_hash)


def main(n=20):

    for b in islice(iterate(lambda b: b.next(),
                            Block.create_genesis_block()),
                    1, n + 1):
        print('Block #{} has been added to the blockchain!'.format(b.index))
        print('Hash: {}\n'.format(b.hash))


if __name__ == '__main__':
    main()
