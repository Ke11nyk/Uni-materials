import pandas as pd
import matplotlib.pyplot as plt
try:
    import seaborn as sns
    HAS_SEABORN = True
except ImportError:
    HAS_SEABORN = False

# Function for reading and processing data
def load_and_process_data():
    # Read data from CSV files
    try:
        cache_size_data = pd.read_csv('cache_size_comparison.csv')
        delta_data = pd.read_csv('delta_impact.csv')
        workingset_data = pd.read_csv('workingset_size.csv')
        
        return cache_size_data, delta_data, workingset_data
    except FileNotFoundError as e:
        print(f"Error loading data: {e}")
        return None, None, None

# Function for creating visualizations
def create_visualizations(cache_size_data, delta_data, workingset_data):
    # Set style
    if HAS_SEABORN:
        sns.set_theme(style="whitegrid")
    else:
        plt.style.use('default')
    
    # Create figure with three subplots
    fig = plt.figure(figsize=(15, 15))
    
    # 1. Cache size impact plot
    ax1 = fig.add_subplot(311)
    for policy in cache_size_data['Policy'].unique():
        policy_data = cache_size_data[cache_size_data['Policy'] == policy]
        ax1.plot(policy_data['Cache_Size'], policy_data['Hit_Rate'], 
                marker='o', label=policy, linewidth=2)
    
    ax1.set_title('Cache Size Impact on Hit Rate', pad=20, fontsize=12)
    ax1.set_xlabel('Cache Size (pages)', fontsize=10)
    ax1.set_ylabel('Hit Rate (%)', fontsize=10)
    ax1.legend(fontsize=10)
    ax1.grid(True, alpha=0.3)
    
    # 2. Delta impact plot
    ax2 = fig.add_subplot(312)
    ax2.plot(delta_data['Delta'], delta_data['Hit_Rate'], 
             marker='o', color='blue', linewidth=2)
    ax2.set_title('Impact of Delta on Working Set Performance', pad=20, fontsize=12)
    ax2.set_xlabel('Delta Value', fontsize=10)
    ax2.set_ylabel('Hit Rate (%)', fontsize=10)
    ax2.grid(True, alpha=0.3)
    
    # 3. Working set size impact plot
    ax3 = fig.add_subplot(313)
    for policy in workingset_data['Policy'].unique():
        policy_data = workingset_data[workingset_data['Policy'] == policy]
        ax3.plot(policy_data['Max_Page'], policy_data['Hit_Rate'], 
                marker='o', label=policy, linewidth=2)
    
    ax3.set_title('Working Set Size Impact on Different Policies', pad=20, fontsize=12)
    ax3.set_xlabel('Maximum Page Number', fontsize=10)
    ax3.set_ylabel('Hit Rate (%)', fontsize=10)
    ax3.legend(fontsize=10)
    ax3.grid(True, alpha=0.3)
    
    # Adjust layout
    plt.tight_layout(pad=3.0)
    
    # Save graphs
    plt.savefig('cache_performance_analysis.png', dpi=300, bbox_inches='tight')
    print("Graphs saved as 'cache_performance_analysis.png'")
    
    # Print statistical data
    print("\nStatistical Summary:")
    print("-------------------")
    
    # Cache size statistics
    print("\nCache Size Analysis:")
    best_cache_size = cache_size_data.loc[cache_size_data['Hit_Rate'].idxmax()]
    print(f"Best performing configuration:")
    print(f"Cache Size: {best_cache_size['Cache_Size']} pages")
    print(f"Policy: {best_cache_size['Policy']}")
    print(f"Hit Rate: {best_cache_size['Hit_Rate']:.2f}%")
    
    # Delta statistics
    print("\nDelta Analysis:")
    best_delta = delta_data.loc[delta_data['Hit_Rate'].idxmax()]
    print(f"Optimal Delta: {best_delta['Delta']}")
    print(f"Best Hit Rate: {best_delta['Hit_Rate']:.2f}%")
    
    # Working set statistics
    print("\nWorking Set Analysis:")
    avg_performance = workingset_data.groupby('Policy')['Hit_Rate'].mean()
    print("Average Hit Rate by Policy:")
    for policy, rate in avg_performance.items():
        print(f"{policy}: {rate:.2f}%")

def main():
    # Load data
    cache_size_data, delta_data, workingset_data = load_and_process_data()
    
    if cache_size_data is not None:
        # Create visualizations
        create_visualizations(cache_size_data, delta_data, workingset_data)
    else:
        print("Unable to create visualizations due to missing data.")

if __name__ == "__main__":
    main()