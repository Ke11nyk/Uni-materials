#!/usr/bin/env python3
"""
Соніфікація:
Температура → Висота + Тембр (холодна=Synth, тепла=Piano)
Опади → Висота + Щільність нот (більше дощу = частіші удари)
Вітер → Гучність + Панорама (stereo ефект)
"""

import struct
import json
import urllib.request
import urllib.error
from datetime import datetime, timedelta
import sys


class MIDIWriter:
    """Клас для створення MIDI-файлів"""

    def __init__(self):
        self.tempo = 100
        self.ticks_per_beat = 480

    def _write_variable_length(self, value):
        """Записує число у форматі variable-length quantity (VLQ)"""
        result = bytearray()
        result.append(value & 0x7F)
        value >>= 7

        while value > 0:
            result.insert(0, (value & 0x7F) | 0x80)
            value >>= 7

        return bytes(result)

    def _calculate_tempo_value(self):
        """Конвертує BPM в мікросекунди на чверть ноту"""
        return int(60000000 / self.tempo)

    def create_header_chunk(self, num_tracks):
        """Створює MIDI header chunk"""
        header = b'MThd'
        length = struct.pack('>I', 6)
        format_type = struct.pack('>H', 1)
        num_tracks_bytes = struct.pack('>H', num_tracks)
        ticks = struct.pack('>H', self.ticks_per_beat)

        return header + length + format_type + num_tracks_bytes + ticks

    def create_track_chunk(self, events):
        """Створює MIDI track chunk"""
        track_data = bytearray()

        for event in events:
            delta_time = self._write_variable_length(event['delta'])
            track_data.extend(delta_time)
            track_data.extend(event['data'])

        track_data.extend(self._write_variable_length(0))
        track_data.extend(b'\xFF\x2F\x00')

        header = b'MTrk'
        length = struct.pack('>I', len(track_data))

        return header + length + bytes(track_data)

    def note_on(self, channel, note, velocity):
        status = 0x90 | (channel & 0x0F)
        return bytes([status, note & 0x7F, velocity & 0x7F])

    def note_off(self, channel, note, velocity=64):
        status = 0x80 | (channel & 0x0F)
        return bytes([status, note & 0x7F, velocity & 0x7F])

    def program_change(self, channel, program):
        status = 0xC0 | (channel & 0x0F)
        return bytes([status, program & 0x7F])

    def control_change(self, channel, controller, value):
        """MIDI Control Change (для панорами тощо)"""
        status = 0xB0 | (channel & 0x0F)
        return bytes([status, controller & 0x7F, value & 0x7F])

    def tempo_event(self):
        tempo_value = self._calculate_tempo_value()
        return bytes([0xFF, 0x51, 0x03]) + struct.pack('>I', tempo_value)[1:]

    def track_name_event(self, name):
        name_bytes = name.encode('utf-8')
        length = self._write_variable_length(len(name_bytes))
        return bytes([0xFF, 0x03]) + length + name_bytes

    def text_event(self, text):
        text_bytes = text.encode('utf-8')
        length = self._write_variable_length(len(text_bytes))
        return bytes([0xFF, 0x01]) + length + text_bytes

    def save(self, filename, tracks_data):
        """Зберігає MIDI-файл"""
        with open(filename, 'wb') as f:
            f.write(self.create_header_chunk(len(tracks_data)))
            for track in tracks_data:
                f.write(self.create_track_chunk(track))


class WeatherAPI:
    """Клас для роботи з Open-Meteo API"""

    BASE_URL = "https://api.open-meteo.com/v1/forecast"

    @staticmethod
    def get_weather(latitude, longitude, days=7):
        """Отримує погодні дані з Open-Meteo API"""
        params = {
            'latitude': latitude,
            'longitude': longitude,
            'daily': 'temperature_2m_max,temperature_2m_min,precipitation_sum,windspeed_10m_max',
            'timezone': 'auto',
            'forecast_days': days
        }

        url = f"{WeatherAPI.BASE_URL}?"
        url += "&".join([f"{key}={value}" for key, value in params.items()])

        try:
            print(f"Отримання даних з API...")
            print(f"Координати: {latitude}, {longitude}")

            with urllib.request.urlopen(url, timeout=10) as response:
                data = json.loads(response.read().decode('utf-8'))

            print(f"Дані успішно отримано!\n")
            return WeatherAPI._parse_weather_data(data)

        except Exception as e:
            print(f"Не вдалося отримати дані з API: {e}")
            print(f"Використання демо-даних...\n")
            return WeatherAPI.get_demo_weather(days)

    @staticmethod
    def get_demo_weather(days=7):
        """Генерація демонстраційних погодних даних"""
        import random

        weather_list = []
        base_date = datetime.now()

        for i in range(days):
            date = base_date + timedelta(days=i)
            temp_avg = 8 + random.uniform(-5, 7) + (i * 0.5)
            weather_list.append({
                'date': date.strftime('%Y-%m-%d'),
                'temp_max': temp_avg + random.uniform(2, 5),
                'temp_min': temp_avg - random.uniform(2, 5),
                'temp_avg': temp_avg,
                'precipitation': random.uniform(0, 15) if random.random() > 0.4 else 0,
                'wind_speed': random.uniform(3, 25)
            })

        return weather_list

    @staticmethod
    def _parse_weather_data(data):
        """Парсинг відповіді API"""
        daily = data['daily']

        weather_list = []
        for i in range(len(daily['time'])):
            weather_list.append({
                'date': daily['time'][i],
                'temp_max': daily['temperature_2m_max'][i],
                'temp_min': daily['temperature_2m_min'][i],
                'temp_avg': (daily['temperature_2m_max'][i] + daily['temperature_2m_min'][i]) / 2,
                'precipitation': daily['precipitation_sum'][i],
                'wind_speed': daily['windspeed_10m_max'][i]
            })

        return weather_list


def map_value(value, from_min, from_max, to_min, to_max):
    """Відображення значення з одного діапазону на інший"""
    if from_max == from_min:
        return to_min
    normalized = (value - from_min) / (from_max - from_min)
    return int(to_min + normalized * (to_max - to_min))


def create_enhanced_weather_sonification(weather_data, filename="weather_enhanced.mid", location="Unknown"):
    """
    ТЕМПЕРАТУРА:
       - Висота ноти: холодна (низькі ноти) → тепла (високі ноти)
       - Тембр: холодна погода = Crystal Synth, тепла = Piano
       - Канал 0

    ОПАДИ:
       - Висота нот: менше опадів = низькі, більше = високі
       - Щільність: більше дощу = частіші удари (швидка арпеджіо)
       - Інструмент: Marimba (перкусія для ефекту краплин)
       - Канал 1

    ВІТЕР:
       - Гучність: слабкий вітер = тихо, сильний = гучно
       - Панорама (pan): вітер "рухається" ліво-право
       - Інструмент: Pad (довгі ноти)
       - Канал 2
    """

    if not weather_data:
        print("Немає даних для соніфікації")
        return

    print("=" * 70)
    print(f"{'СОНІФІКАЦІЯ ПОГОДНИХ ДАНИХ':^70}")
    print(f"{'Локація: ' + location:^70}")
    print("=" * 70 + "\n")

    midi = MIDIWriter()
    midi.tempo = 100

    # Знаходимо діапазони
    temps = [d['temp_avg'] for d in weather_data]
    precipitations = [d['precipitation'] for d in weather_data]
    wind_speeds = [d['wind_speed'] for d in weather_data]

    temp_min, temp_max = min(temps), max(temps)
    precip_min, precip_max = min(precipitations), max(precipitations)
    wind_min, wind_max = min(wind_speeds), max(wind_speeds)

    # Середня температура для вибору тембру
    temp_avg_all = sum(temps) / len(temps)

    # === ТРЕК 1: ТЕМПЕРАТУРА ===
    track1_events = []
    track1_events.append({'delta': 0, 'data': midi.track_name_event(f"Temperature ({location})")})
    track1_events.append({'delta': 0, 'data': midi.tempo_event()})

    # === ТРЕК 2: ОПАДИ ===
    track2_events = []
    track2_events.append({'delta': 0, 'data': midi.track_name_event("Precipitation (Rain)")})
    track2_events.append({'delta': 0, 'data': midi.program_change(1, 12)})  # Marimba

    # === ТРЕК 3: ВІТЕР ===
    track3_events = []
    track3_events.append({'delta': 0, 'data': midi.track_name_event("Wind (with Pan)")})
    track3_events.append({'delta': 0, 'data': midi.program_change(2, 88)})  # New Age Pad

    print(f"{'День':<13} {'Температура':<18} {'Опади':<14} {'Вітер':<15}")
    print("-" * 70)

    for i, data in enumerate(weather_data):
        # ========================================
        # ТЕМПЕРАТУРА: Висота + Тембр
        # ========================================
        pitch_temp = map_value(data['temp_avg'], temp_min, temp_max, 55, 84)  # G3-C6

        # Вибір інструменту залежно від температури
        if data['temp_avg'] < 10:
            instrument_temp = 80  # Square Wave (холодний електронний звук)
            timbre = "холодний"
        elif data['temp_avg'] < 20:
            instrument_temp = 0  # Acoustic Piano (нейтральний)
            timbre = "помірний"
        else:
            instrument_temp = 11  # Vibraphone (теплий м'який звук)
            timbre = "теплий"

        # Додаємо зміну інструменту для першої ноти
        if i == 0:
            track1_events.append({'delta': 0, 'data': midi.program_change(0, instrument_temp)})
        else:
            # Змінюємо інструмент якщо температура сильно змінилася
            prev_temp = weather_data[i - 1]['temp_avg']
            if (prev_temp < 10 and data['temp_avg'] >= 10) or \
                    (prev_temp < 20 and data['temp_avg'] >= 20) or \
                    (prev_temp >= 10 and data['temp_avg'] < 10) or \
                    (prev_temp >= 20 and data['temp_avg'] < 20):
                track1_events.append({'delta': 0, 'data': midi.program_change(0, instrument_temp)})

        # Додаємо ноту температури
        delta = 0 if i == 0 else midi.ticks_per_beat * 2
        duration = midi.ticks_per_beat
        track1_events.append({'delta': delta, 'data': midi.note_on(0, pitch_temp, 85)})
        track1_events.append({'delta': duration, 'data': midi.note_off(0, pitch_temp)})

        # ========================================
        # ОПАДИ: Висота + Щільність нот
        # ========================================
        if data['precipitation'] > 0:
            pitch_rain = map_value(data['precipitation'], precip_min, precip_max, 60, 84)  # C4-C6

            # Щільність краплин: більше дощу = більше нот
            num_drops = 1 + int((data['precipitation'] / (precip_max if precip_max > 0 else 1)) * 5)  # 1-6 нот
            drop_interval = midi.ticks_per_beat * 2 // num_drops  # Розподіляємо по часу

            delta_base = 0 if i == 0 else midi.ticks_per_beat * 2

            for drop in range(num_drops):
                # Варіюємо висоту кожної краплі трохи (+-2 півтони)
                pitch_variation = pitch_rain + (drop % 3) - 1
                pitch_variation = max(48, min(96, pitch_variation))

                delta = delta_base if drop == 0 else drop_interval
                track2_events.append({'delta': delta, 'data': midi.note_on(1, pitch_variation, 65)})
                track2_events.append({'delta': midi.ticks_per_beat // 4, 'data': midi.note_off(1, pitch_variation)})
                delta_base = 0
        else:
            # Пауза якщо немає опадів
            if i > 0:
                # Просто пропускаємо час
                pass

        # ========================================
        # ВІТЕР: Гучність + Панорама
        # ========================================
        volume_wind = map_value(data['wind_speed'], wind_min, wind_max, 30, 120)
        volume_wind = min(127, volume_wind)

        # Панорама (CC 10): вітер "рухається" з ліва на право
        # 0 = лівий канал, 64 = центр, 127 = правий канал
        pan_value = int((i / len(weather_data)) * 127)  # Поступово рухається

        pitch_wind = map_value(data['wind_speed'], wind_min, wind_max, 40, 65)  # E2-F4

        delta = 0 if i == 0 else midi.ticks_per_beat * 2

        # Спочатку встановлюємо панораму
        track3_events.append({'delta': delta, 'data': midi.control_change(2, 10, pan_value)})

        # Потім грає ноту
        track3_events.append({'delta': 0, 'data': midi.note_on(2, pitch_wind, volume_wind)})
        track3_events.append({'delta': midi.ticks_per_beat * 2, 'data': midi.note_off(2, pitch_wind)})

        # ========================================
        # Виведення інформації
        # ========================================
        try:
            date_str = datetime.strptime(data['date'], '%Y-%m-%d').strftime('%d.%m (%a)')
        except:
            date_str = data['date'][:10]

        drops_info = f"{num_drops} краплі" if data['precipitation'] > 0 else "без дощу"
        pan_info = "←ліво" if pan_value < 42 else ("центр" if pan_value < 85 else "право→")

        print(f"{date_str:<13} {data['temp_avg']:5.1f}°C ({timbre:<8}) "
              f"{data['precipitation']:5.1f}mm ({drops_info:<9}) "
              f"{data['wind_speed']:5.1f} km/h ({pan_info})")

    # Фінальний акорд (C Major)
    final_time = midi.ticks_per_beat * 2
    for offset in [0, 4, 7]:
        track1_events.append({'delta': final_time if offset == 0 else 0,
                              'data': midi.note_on(0, 60 + offset, 95)})
    for offset in [0, 4, 7]:
        track1_events.append({'delta': midi.ticks_per_beat * 4 if offset == 0 else 0,
                              'data': midi.note_off(0, 60 + offset)})

    # Зберігаємо
    midi.save(filename, [track1_events, track2_events, track3_events])

    print("-" * 70)
    print(f"MIDI-файл створено: {filename}")
    print(f"\nСтатистика соніфікації:")
    print(f"   Температура: {temp_min:.1f}°C - {temp_max:.1f}°C")
    print(f"      └─ Тембр змінюється: холодний → помірний → теплий")
    print(f"   Опади: {precip_min:.1f}mm - {precip_max:.1f}mm")
    print(f"      └─ Щільність краплин: 1-6 нот залежно від інтенсивності")
    print(f"   Вітер: {wind_min:.1f}km/h - {wind_max:.1f}km/h")
    print(f"      └─ Панорама: звук рухається в стерео полі")
    print(f"   Треків: 3 (кожен з унікальними параметрами)")
    print(f"   Днів: {len(weather_data)}")

    import os
    if os.path.exists(filename):
        print(f"   Розмір: {os.path.getsize(filename)} байт\n")


def main():
    """Головна функція"""

    print("\n" + "╔" + "═" * 64 + "╗")
    print("║" + " " * 23 + "СОНІФІКАЦІЯ ПОГОДИ" + " " * 23 + "║")
    print("║" + " " * 9 + "Унікальні MIDI параметри для кожного показника" + " " * 9 + "║")
    print("╚" + "═" * 64 + "╝\n")

    # Перевіряємо аргументи
    if '--demo' in sys.argv:
        print("Режим демонстрації з тестовими даними\n")
        weather_data = WeatherAPI.get_demo_weather(7)
        create_enhanced_weather_sonification(weather_data, "weather_enhanced_demo.mid", "Demo Location")
        return

    if '--lat' in sys.argv and '--lon' in sys.argv:
        try:
            lat_idx = sys.argv.index('--lat')
            lon_idx = sys.argv.index('--lon')
            latitude = float(sys.argv[lat_idx + 1])
            longitude = float(sys.argv[lon_idx + 1])

            location = sys.argv[sys.argv.index('--name') + 1] if '--name' in sys.argv else f"{latitude}, {longitude}"
            days = int(sys.argv[sys.argv.index('--days') + 1]) if '--days' in sys.argv else 7
            days = max(1, min(16, days))

            weather_data = WeatherAPI.get_weather(latitude, longitude, days)
            filename = f"weather_enhanced_{location.replace(' ', '_').replace(',', '')}.mid"
            create_enhanced_weather_sonification(weather_data, filename, location)
            return
        except (ValueError, IndexError) as e:
            print(f"Помилка: {e}")
            return

    # Інтерактивний режим
    cities = {
        '1': (50.4501, 30.5234, "Київ, Україна"),
        '2': (49.8397, 24.0297, "Львів, Україна"),
        '3': (48.4647, 35.0462, "Дніпро, Україна"),
        '4': (46.4825, 30.7233, "Одеса, Україна"),
        '5': (51.5074, -0.1278, "Лондон, Велика Британія"),
        '6': (40.7128, -74.0060, "Нью-Йорк, США"),
        '7': (35.6762, 139.6503, "Токіо, Японія"),
        '8': (48.8566, 2.3522, "Париж, Франція"),
    }

    print("Оберіть місто або введіть власні координати:\n")
    for key, (lat, lon, name) in cities.items():
        print(f"  {key}. {name}")
    print(f"  9. Власні координати")
    print(f"  0. Демо-дані\n")

    choice = input("Ваш вибір (0-9): ").strip()

    if choice == '0':
        print("\nВикористовую демо-дані\n")
        weather_data = WeatherAPI.get_demo_weather(7)
        create_enhanced_weather_sonification(weather_data, "weather_enhanced_demo.mid", "Demo Location")
        return
    elif choice in cities:
        latitude, longitude, location = cities[choice]
    elif choice == '9':
        try:
            latitude = float(input("Введіть широту (наприклад, 50.4501): "))
            longitude = float(input("Введіть довготу (наприклад, 30.5234): "))
            location = input("Назва локації: ").strip() or "Custom Location"
        except ValueError:
            print("Невірний формат!")
            return
    else:
        print("Невірний вибір!")
        return

    try:
        days = int(input("\nКількість днів прогнозу (1-16, за замовчуванням 7): ").strip() or "7")
        days = max(1, min(16, days))
    except ValueError:
        days = 7

    print()
    weather_data = WeatherAPI.get_weather(latitude, longitude, days)

    if weather_data:
        filename = f"weather_enhanced_{location.replace(' ', '_').replace(',', '')}.mid"
        create_enhanced_weather_sonification(weather_data, filename, location)
        print(f"\nГотово! Відкрийте {filename} у MIDI-програвачі!")


if __name__ == "__main__":
    main()