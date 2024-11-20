#! /usr/bin/env python

from __future__ import print_function
import sys
from optparse import OptionParser
import random
import math

def random_seed(seed):
    try:
        random.seed(seed, version=1)
    except:
        random.seed(seed)
    return

def working_set_replacement(memory, ref, page_age, current_time, delta, cachesize, new_page):
    victim_candidates = []
    for page in memory:
        if current_time - page_age.get(page, 0) > delta:
            victim_candidates.append(page)
    
    if victim_candidates:
        victim = min(victim_candidates, key=lambda p: page_age.get(p, 0))
        memory.remove(victim)
        del ref[victim]
        del page_age[victim]
    else:
        victim = memory.pop(0)
        del ref[victim]
        del page_age[victim]
    
    memory.append(new_page)
    ref[new_page] = 1
    page_age[new_page] = current_time
    
    return memory, ref, page_age

def main():
    parser = OptionParser()
    parser.add_option('-a', '--addresses', default='-1', 
                      help='a set of comma-separated pages to access; -1 means randomly generate',  
                      action='store', type='string', dest='addresses')
    parser.add_option('-f', '--addressfile', default='',   
                      help='a file with a bunch of addresses in it',                                
                      action='store', type='string', dest='addressfile')
    parser.add_option('-n', '--numaddrs', default='10',    
                      help='if -a (--addresses) is -1, this is the number of addrs to generate',    
                      action='store', type='string', dest='numaddrs')
    parser.add_option('-p', '--policy', default='FIFO',    
                      help='replacement policy: FIFO, LRU, OPT, UNOPT, RAND, CLOCK, WORKINGSET',    
                      action='store', type='string', dest='policy')
    parser.add_option('-b', '--clockbits', default=2,      
                      help='for CLOCK policy, how many clock bits to use',                          
                      action='store', type='int', dest='clockbits')
    parser.add_option('-C', '--cachesize', default='3',    
                      help='size of the page cache, in pages',                                      
                      action='store', type='string', dest='cachesize')
    parser.add_option('-m', '--maxpage', default='10',     
                      help='if randomly generating page accesses, this is the max page number',     
                      action='store', type='string', dest='maxpage')
    parser.add_option('-s', '--seed', default='0',         
                      help='random number seed',                                                    
                      action='store', type='string', dest='seed')
    parser.add_option('-N', '--notrace', default=False,    
                      help='do not print out a detailed trace',                                     
                      action='store_true', dest='notrace')
    parser.add_option('-c', '--compute', default=False,    
                      help='compute answers for me',                                                
                      action='store_true', dest='solve')
    parser.add_option('-d', '--delta', default='5',        
                      help='working set time window',                                               
                      action='store', type='int', dest='delta')

    (options, args) = parser.parse_args()

    print('ARG addresses', options.addresses)
    print('ARG addressfile', options.addressfile)
    print('ARG numaddrs', options.numaddrs)
    print('ARG policy', options.policy)
    print('ARG clockbits', options.clockbits)
    print('ARG cachesize', options.cachesize)
    print('ARG maxpage', options.maxpage)
    print('ARG seed', options.seed)
    print('ARG notrace', options.notrace)
    print('ARG delta', options.delta)
    print('')

    addresses   = str(options.addresses)
    addressFile = str(options.addressfile)
    numaddrs    = int(options.numaddrs)
    cachesize   = int(options.cachesize)
    seed        = int(options.seed)
    maxpage     = int(options.maxpage)
    policy      = str(options.policy)
    notrace     = options.notrace
    clockbits   = int(options.clockbits)
    delta       = int(options.delta)

    random_seed(seed)

    addrList = []
    if addressFile != '':
        fd = open(addressFile)
        for line in fd:
            addrList.append(int(line))
        fd.close()
    else:
        if addresses == '-1':
            for i in range(0,numaddrs):
                n = int(maxpage * random.random())
                addrList.append(n)
        else:
            addrList = [int(x) for x in addresses.split(',')]

    if options.solve == False:
        print(f'Assuming a replacement policy of {policy}, and a cache of size {cachesize} pages,')
        print('figure out whether each of the following page references hit or miss')
        print('in the page cache.\n')

        for n in addrList:
            print(f'Access: {n}  Hit/Miss?  State of Memory?')
        print('')

    else:
        if notrace == False:
            print('Solving...\n')

        count = 0
        memory = []
        hits = 0
        miss = 0
        ref = {}
        page_age = {}
        current_time = 0
        
        # need to generate addresses
        addrIndex = 0
        for nStr in addrList:
            # first, lookup
            n = int(nStr)
            try:
                idx = memory.index(n)
                hits += 1
            except:
                idx = -1
                miss += 1

            victim = -1        
            if idx == -1:
                # Miss, replace if cache is full
                if policy == 'FIFO' or policy == 'LRU':
                    if count >= cachesize:
                        victim = memory.pop(0)
                    count += 1
                    memory.append(n)
                
                elif policy == 'CLOCK':
                    if len(memory) >= cachesize:
                        done = False
                        while not done:
                            if ref.get(memory[0], 0) == 0:
                                victim = memory.pop(0)
                                done = True
                            else:
                                ref[memory[0]] -= 1
                                memory.append(memory.pop(0))
                    memory.append(n)
                    ref[n] = clockbits
                
                elif policy == 'RAND':
                    if len(memory) >= cachesize:
                        victim = random.choice(memory)
                        memory.remove(victim)
                    memory.append(n)
                
                elif policy == 'MRU':
                    if len(memory) >= cachesize:
                        victim = memory.pop(-1)
                    memory.append(n)
                
                elif policy == 'OPT':
                    if len(memory) >= cachesize:
                        maxReplace = -1
                        replaceIdx = -1
                        for pageIndex in range(len(memory)):
                            page = memory[pageIndex]
                            whenReferenced = len(addrList)
                            for futureIdx in range(addrIndex+1, len(addrList)):
                                if page == int(addrList[futureIdx]):
                                    whenReferenced = futureIdx
                                    break
                            if whenReferenced >= maxReplace:
                                replaceIdx = pageIndex
                                maxReplace = whenReferenced
                        victim = memory.pop(replaceIdx)
                    memory.append(n)
                
                elif policy == 'UNOPT':
                    if len(memory) >= cachesize:
                        minReplace = len(addrList) + 1
                        replaceIdx = -1
                        for pageIndex in range(len(memory)):
                            page = memory[pageIndex]
                            whenReferenced = len(addrList)
                            for futureIdx in range(addrIndex+1, len(addrList)):
                                if page == int(addrList[futureIdx]):
                                    whenReferenced = futureIdx
                                    break
                            if whenReferenced < minReplace:
                                replaceIdx = pageIndex
                                minReplace = whenReferenced
                        victim = memory.pop(replaceIdx)
                    memory.append(n)
                
                elif policy == 'WORKINGSET':
                    if len(memory) >= cachesize:
                        memory, ref, page_age = working_set_replacement(
                            memory, ref, page_age, current_time, delta, cachesize, n
                        )
                    else:
                        memory.append(n)
                        ref[n] = 1
                        page_age[n] = current_time
                
                else:
                    print(f'Policy {policy} is not yet implemented')
                    exit(1)

            # update reference count / age
            if policy in ['LRU', 'WORKINGSET']:
                page_age[n] = current_time
            
            if policy in ['CLOCK', 'WORKINGSET']:
                ref[n] = ref.get(n, 0) + 1
                if ref[n] > clockbits:
                    ref[n] = clockbits

            if notrace == False:
                print(f'Access: {n} {"HIT" if idx != -1 else "MISS"} Memory: {memory}')

            current_time += 1
            addrIndex += 1
        
        print('')
        print(f'FINALSTATS hits {hits}   misses {miss}   hitrate {(hits/(hits+miss))*100:.2f}')
        print('')

if __name__ == "__main__":
    main()