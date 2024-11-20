import subprocess
import csv

def run_test(policy, cache_size, num_pages=20, max_page=10, delta=5, seed=42):
    cmd = [
        'python', 'paging-policy.py',
        '-p', policy,
        '-C', str(cache_size),
        '-n', str(num_pages),
        '-m', str(max_page),
        '-s', str(seed),
        '-d', str(delta),
        '-c',
        '--notrace'
    ]
    
    result = subprocess.run(cmd, capture_output=True, text=True)
    output = result.stdout
    
    # Extract hit rate from output
    for line in output.split('\n'):
        if 'FINALSTATS' in line:
            parts = line.split()
            hits = int(parts[2])
            misses = int(parts[4])
            hit_rate = float(parts[6])
            return hits, misses, hit_rate
    
    return 0, 0, 0

# Test 1: Compare algorithms with different cache sizes
with open('cache_size_comparison.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['Cache_Size', 'Policy', 'Hit_Rate'])
    
    for cache_size in [2, 3, 4, 5, 6]:
        for policy in ['WORKINGSET', 'LRU', 'FIFO', 'CLOCK']:
            _, _, hit_rate = run_test(policy, cache_size)
            writer.writerow([cache_size, policy, hit_rate])

# Test 2: Impact of delta on WORKINGSET
with open('delta_impact.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['Delta', 'Hit_Rate'])
    
    for delta in [2, 3, 4, 5, 6, 7, 8, 9, 10]:
        _, _, hit_rate = run_test('WORKINGSET', 4, delta=delta)
        writer.writerow([delta, hit_rate])

# Test 3: Behavior with different working set sizes (max_page)
with open('workingset_size.csv', 'w', newline='') as f:
    writer = csv.writer(f)
    writer.writerow(['Max_Page', 'Policy', 'Hit_Rate'])
    
    for max_page in [5, 10, 15, 20, 25]:
        for policy in ['WORKINGSET', 'LRU', 'FIFO', 'CLOCK']:
            _, _, hit_rate = run_test(policy, 4, max_page=max_page)
            writer.writerow([max_page, policy, hit_rate])