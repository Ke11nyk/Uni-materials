import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings('ignore')

# ============================================================
# ЗАВДАННЯ 1. ПІДГОТОВКА НАБОРУ ДАНИХ
# ============================================================

# 1.1 Завантаження набору даних
print("=" * 60)
print("ЗАВДАННЯ 1. ПІДГОТОВКА НАБОРУ ДАНИХ")
print("=" * 60)

df = pd.read_csv('./niaaa_apparent_per_capita_consumption_1977_2023.csv')
print("\n1.1 Перші 5 рядків DataFrame:")
print(df.head())
print(f"\nРозмір датасету: {df.shape[0]} рядків, {df.shape[1]} колонок")
print(f"Колонки: {list(df.columns)}")

# 1.2 Видалення зайвих стовпців (залишаємо 7 найбільш корисних)
print("\n1.2 Очищення — видалення зайвих стовпців:")
cols_to_keep = [
    'state', 'state_name', 'year',
    'ethanol_beer_gallons_per_capita',
    'ethanol_wine_gallons_per_capita',
    'ethanol_spirit_gallons_per_capita',
    'ethanol_all_drinks_gallons_per_capita'
]
df = df[cols_to_keep]
print(f"Залишені колонки: {list(df.columns)}")
print(df.head())

# 1.3 Обробка пропущених значень
print("\n1.3 Обробка пропущених значень:")
print(f"Пропущені значення до очищення:\n{df.isnull().sum()}")
# Ключовий стовпець — ethanol_all_drinks_gallons_per_capita
df = df.dropna(subset=['ethanol_all_drinks_gallons_per_capita'])
print(f"\nРядків після видалення NaN у ключовому стовпці: {len(df)}")

# 1.4 Форматування даних
print("\n1.4 Форматування даних:")
print(f"Тип 'year' до: {df['year'].dtype}")
df['year'] = df['year'].astype('int32')
print(f"Тип 'year' після: {df['year'].dtype}")
print(f"\nТип 'state' до: {df['state'].dtype}")
df['state'] = df['state'].astype('category')
print(f"Тип 'state' після: {df['state'].dtype}")
print(f"\nТип 'state_name' до: {df['state_name'].dtype}")
df['state_name'] = df['state_name'].astype('category')
print(f"Тип 'state_name' після: {df['state_name'].dtype}")

# ============================================================
# ЗАВДАННЯ 2. ДОСЛІДЖЕННЯ НАБОРУ ДАНИХ
# ============================================================
print("\n" + "=" * 60)
print("ЗАВДАННЯ 2. ДОСЛІДЖЕННЯ НАБОРУ ДАНИХ")
print("=" * 60)

# 2.1 Загальна кількість рядків
print(f"\n2.1 Загальна кількість записів після очищення: {len(df)}")

# 2.2 Робота з числовими показниками
print("\n2.2 Робота з числовими показниками:")
threshold = 2.5
high_consumption = df[df['ethanol_all_drinks_gallons_per_capita'] > threshold]
print(f"Кількість записів із загальним споживанням > {threshold}: {len(high_consumption)}")

avg_beer = high_consumption['ethanol_beer_gallons_per_capita'].mean()
print(f"Середнє споживання пива для цієї вибірки: {avg_beer:.4f} галонів на душу населення")

top10_ids = df.nlargest(10, 'ethanol_all_drinks_gallons_per_capita')[['state_name', 'year', 'ethanol_all_drinks_gallons_per_capita']]
print(f"\nТоп-10 записів з найбільшим загальним споживанням алкоголю:")
print(top10_ids.to_string(index=True))

# 2.3 Дослідження категорій та тексту
print("\n2.3 Дослідження категорій та тексту:")

# Умова 1: точний збіг — стан New York
cond1 = df['state_name'] == 'New York'
count_cond1 = cond1.sum()
print(f"Кількість записів для New York (точний збіг): {count_cond1}")

# Умова 2: частковий збіг — state_name містить 'New'
cond2 = df['state_name'].str.contains('New', na=False)
df_partial = df[cond2]
count_cond2 = cond2.sum()
print(f"Кількість записів де state_name містить 'New': {count_cond2}")

both = (cond1 & cond2).sum()
print(f"Записів, що задовольняють обидві умови: {both}")

fraction_cond1_not_cond2 = (cond1 & ~cond2).sum() / len(df)
print(f"Частка записів: умова 1, але НЕ умова 2: {fraction_cond1_not_cond2:.4f} ({fraction_cond1_not_cond2*100:.2f}%)")

neither = (~cond1 & ~cond2).sum()
print(f"Записів, що не задовольняють ні 1, ні 2 умову: {neither}")

# 2.4 Дослідження числових діапазонів та зрізів даних
print("\n2.4 Дослідження числових діапазонів та зрізів даних:")

# Конкретне значення: year == 2000
count_year_2000 = (df['year'] == 2000).sum()
print(f"Кількість записів для 2000 року: {count_year_2000}")

# Діапазон: year від 1990 до 2000 включно
count_range = ((df['year'] >= 1990) & (df['year'] <= 2000)).sum()
print(f"Кількість записів за 1990–2000: {count_range}")

print(f"Чи кількість записів у першій підмножині > другій? {count_year_2000 > count_range}")

# 2.5 Комбіновані фільтри та оцінки
print("\n2.5 Комбіновані фільтри та оцінки:")

# "Значущі" записи — загальне споживання > 1.5 (вище мінімального поточного)
significant = df[df['ethanol_all_drinks_gallons_per_capita'] > 1.5]
print(f"Кількість 'значущих' записів (споживання > 1.5): {len(significant)}")

top5_significant = significant.nlargest(5, 'ethanol_all_drinks_gallons_per_capita')[
    ['state_name', 'year', 'ethanol_all_drinks_gallons_per_capita']
]
print(f"\nТоп-5 записів із значущої вибірки:")
print(top5_significant.to_string(index=False))

# Медіана для топ-10 за споживанням вина
top10_wine = df.nlargest(10, 'ethanol_wine_gallons_per_capita')
median_spirit_top10 = top10_wine['ethanol_spirit_gallons_per_capita'].median()
print(f"\nМедіана споживання спиртного для топ-10 за вином: {median_spirit_top10:.4f}")

# 2.6 Порівняння груп
print("\n2.6 Порівняння груп:")

df_nevada = df[df['state_name'] == 'Nevada']
df_utah = df[df['state_name'] == 'Utah']

comparison = pd.DataFrame({
    'category_name': ['Nevada', 'Utah'],
    'total_records': [len(df_nevada), len(df_utah)]
})
comparison['average_value'] = [
    df_nevada['ethanol_all_drinks_gallons_per_capita'].mean(),
    df_utah['ethanol_all_drinks_gallons_per_capita'].mean()
]
print("Порівняння Nevada vs Utah:")
print(comparison.to_string(index=False))

# 2.7 Комплексні завдання
print("\n2.7 Комплексні завдання:")

# Складний фільтр: AND, OR, NOT
# Записи де: (рік після 2000 І споживання пива > 1.0) АБО (споживання вина > 0.5) І НЕ (state == 'utah')
complex_filter = df[
    ((df['year'] > 2000) & (df['ethanol_beer_gallons_per_capita'] > 1.0)) |
    (df['ethanol_wine_gallons_per_capita'] > 0.5) &
    ~(df['state'] == 'utah')
]
print(f"Кількість записів після складного фільтру (AND + OR + NOT): {len(complex_filter)}")

# Побудова графіку — середнє споживання по роках
print("\nПобудова графіку...")
yearly_avg = df.groupby('year')['ethanol_all_drinks_gallons_per_capita'].mean()

fig, ax = plt.subplots(figsize=(12, 6))
ax.plot(yearly_avg.index, yearly_avg.values, color='steelblue', linewidth=2, marker='o', markersize=4)
ax.set_title('Середнє споживання алкоголю на душу населення (США, 1977–2023)', fontsize=14, fontweight='bold')
ax.set_xlabel('Рік', fontsize=12)
ax.set_ylabel('Галонів чистого спирту на душу населення', fontsize=12)
ax.grid(True, alpha=0.3)
ax.axhline(y=yearly_avg.mean(), color='red', linestyle='--', alpha=0.7, label=f'Середнє за весь час: {yearly_avg.mean():.2f}')
ax.legend()
plt.tight_layout()
plt.savefig('alcohol_trend.png', dpi=150, bbox_inches='tight')
print("Графік збережено!")

print("\n" + "=" * 60)
print("Аналіз завершено успішно!")
print("=" * 60)