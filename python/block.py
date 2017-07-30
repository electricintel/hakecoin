#!/usr/bin/env python3

# Copied with minor modifications from:

# https://medium.com/crypto-currently/lets-build-the-tiniest-blockchain-e70965a248b

from datetime import datetime

from hashlib import sha256


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
    blockchain = [Block.create_genesis_block()]
    previous_block = blockchain[0]

    for i in range(n):
        block_to_add = previous_block.next()
        blockchain.append(block_to_add)
        previous_block = block_to_add
        print('Block #{} has been added to the blockchain!'.format(
            blockchain[-1].index))
        print('Hash: {}\n'.format(blockchain[-1].hash))

if __name__ == '__main__':
    main()
