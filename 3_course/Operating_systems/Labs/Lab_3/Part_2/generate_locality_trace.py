import random
import os

def generate_locality_trace(num_pages, num_accesses, locality_factor):
    """
    Generates a page access trace with locality.

    Args:
        num_pages (int): Maximum page number.
        num_accesses (int): Number of page accesses to generate.
        locality_factor (float): Factor controlling the degree of locality (0.0 to 1.0).

    Returns:
        list: List of page numbers in the access trace.
    """
    trace = []
    current_page = random.randint(0, num_pages - 1)
    trace.append(current_page)

    for _ in range(num_accesses - 1):
        if random.random() < locality_factor:
            # Locality-based access
            current_page = (current_page + random.randint(-1, 1)) % num_pages
        else:
            # Random access
            current_page = random.randint(0, num_pages - 1)
        trace.append(current_page)

    return trace

if __name__ == "__main__":
    num_pages = 100
    num_accesses = 1000
    locality_factor = 0.8
    trace = generate_locality_trace(num_pages, num_accesses, locality_factor)

    if not os.path.exists('traces'):
        os.makedirs('traces')

    with open("traces/locality_trace.txt", "w") as f:
        for page in trace:
            f.write(str(page) + "\n")
    print(f"Locality-based trace generated and saved to 'locality_trace.txt'.")