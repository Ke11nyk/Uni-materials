import os
from subprocess import Popen, PIPE

def run_simulations():
    # Визначте шлях до директорії, де зберігаються згенеровані файли
    data_dir = "traces"

    # Визначте список політик, які потрібно протестувати
    policies = ["FIFO", "LRU", "OPT", "RAND", "CLOCK"]

    # Визначте список файлів трасування, які потрібно використати
    trace_files = ["locality_trace.txt"]

    # Створіть файл для запису результатів
    if not os.path.exists("results"):
        os.makedirs("results")

    results_file = "results/locality_results.txt"

    # Виконайте симуляцію для кожної політики та кожного файлу трасування
    with open(results_file, "w") as f:
        for policy in policies:
            f.write(f"Simulating policy: {policy}\n")
            for trace_file in trace_files:
                trace_path = os.path.join(data_dir, trace_file)
                f.write(f"Using trace file: {trace_file}\n")
                finalstats = run_paging_policy_simulation(policy, trace_path)
                f.write(f"{finalstats}\n\n")

def run_paging_policy_simulation(policy, trace_path):
    # Викликаємо paging-policy.py зі спеціальними аргументами
    cmd = ["python", "paging-policy.py", "-p", policy, "-f", trace_path, "-c"]
    with Popen(cmd, stdout=PIPE, stderr=PIPE, universal_newlines=True) as proc:
        output, err = proc.communicate()
        if proc.returncode == 0:
            # Знайти рядок з FINALSTATS
            for line in output.splitlines():
                if line.startswith("FINALSTATS"):
                    return line
        else:
            return f"Error running simulation: {err}"

if __name__ == "__main__":
    run_simulations()