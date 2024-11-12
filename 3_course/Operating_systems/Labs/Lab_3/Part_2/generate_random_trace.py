import random
import os

def generate_random_trace(num_pages, num_accesses):
    """
    Generates a random page access trace.

    Args:
        num_pages (int): Maximum page number.
        num_accesses (int): Number of page accesses to generate.

    Returns:
        list: List of page numbers in the access trace.
    """
    trace = []
    for _ in range(num_accesses):
        trace.append(random.randint(0, num_pages - 1))
    return trace

if __name__ == "__main__":
    num_pages = 100
    num_accesses = 1000
    trace = generate_random_trace(num_pages, num_accesses)

    if not os.path.exists('traces'):
        os.makedirs('traces')

    with open("traces/random_trace.txt", "w") as f:
        for page in trace:
            f.write(str(page) + "\n")
    print(f"Random trace generated and saved to 'random_trace.txt'.")