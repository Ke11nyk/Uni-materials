/**
 * index.js
 * Точка входу — запускає всі тести послідовно.
 *
 * Запуск:
 *   node index.js                    → headless (без вікна)
 *   HEADLESS=false node index.js     → з вікном браузера (можна спостерігати)
 *   HEADLESS=false SLOW=1 node index.js → з вікном + уповільнення між кроками
 *
 * Запуск окремої групи тестів:
 *   SUITE=catalog node index.js
 *   SUITE=search  node index.js
 *   SUITE=cart    node index.js
 *   SUITE=reg     node index.js
 *   SUITE=auth    node index.js
 *   SUITE=contact node index.js
 */

const { test, printSummary } = require('./utils/runner');

// ─── Тест-функції ──────────────────────────────────────────────────────────
const { T01_1_1, T01_1_2, T01_1_3, T01_1_5 } = require('./tests/catalog.test');
const { T02_2_1, T02_2_2, T02_2_3, T02_2_4 } = require('./tests/search.test');
const { T03_3_1, T03_3_4, T03_3_5, T04_4_4 } = require('./tests/cart.test');
const { T05_5_3, T05_5_4, T05_5_7, T05_5_8 } = require('./tests/registration.test');
const { T06_6_2, T06_6_3, T06_6_4 }          = require('./tests/auth.test');
const { T09_9_1, T09_9_2, T09_9_3, T09_9_4 } = require('./tests/contact.test');

// ──────────────────────────────────────────────────────────────────────────
async function main() {
  const isHeadless = process.env.HEADLESS !== 'false';
  const suite      = process.env.SUITE || 'all';

  console.log('='.repeat(70));
  console.log(' Selenium JS (POM) — Зелені Янголи (landshaft.info)');
  console.log(' Студент: Вербицький Артем, ІПС-41, Лаба 3');
  console.log(`  Режим: ${isHeadless ? '🤖 headless (без вікна)' : '👁️  UI (вікно браузера відкрите)'}`);
  console.log(`  Набір: ${suite}`);
  console.log('='.repeat(70));

  const start = Date.now();

  // ─── Визначаємо які тести запускати ───────────────────────────────────
  const all = suite === 'all';

  // T01 — Каталог
  if (all || suite === 'catalog') {
    await test('1.1', 'Головна сторінка завантажується',           T01_1_1);
    await test('1.2', 'Навігація до категорії /uk/42-conifers',    T01_1_2);
    await test('1.3', 'Картка товару — прямий URL',                T01_1_3);
    await test('1.5', 'Неіснуюча сторінка — 404/редирект',        T01_1_5);
  }

  // T02 — Пошук
  if (all || suite === 'search') {
    await test('2.1', "Пошук «туя» — результати відображаються",  T02_2_1);
    await test('2.2', 'Порожній пошуковий запит — заблокований',  T02_2_2);
    await test('2.4', 'XSS: <b>xsstest</b> у полі пошуку',        T02_2_3);
    await test('2.6', "SQL-ін'єкція у полі пошуку",               T02_2_4);
  }

  // T03 & T04 — Кошик
  if (all || suite === 'cart') {
    await test('3.1', 'Додавання товару до кошика',                T03_3_1);
    await test('3.4', 'Кількість = 0 відхиляється',               T03_3_4);
    await test('3.5', "Від'ємна кількість -1 коригується",        T03_3_5);
    await test('4.4', 'Порожній кошик — повідомлення на /uk/order', T04_4_4);
  }

  // T05 — Реєстрація
  if (all || suite === 'reg') {
    await test('5.3', 'Некоректний email на кроці 1 — валідація', T05_5_3);
    await test('5.4', 'Короткий пароль (3 символи) на кроці 2',   T05_5_4);
    await test('5.7', 'Порожня форма кроку 1 — не надсилається',  T05_5_7);
    await test('5.8', "XSS у полі ім'я на кроці 2",               T05_5_8);
  }

  // T06 — Авторизація
  if (all || suite === 'auth') {
    await test('6.2', 'Невірний пароль — повідомлення є',          T06_6_2);
    await test('6.3', 'Порожній email — форма заблокована',        T06_6_3);
    await test('6.4', "«Забули пароль?» → /uk/password-recovery", T06_6_4);
  }

  // T09 — Форма контактів
  if (all || suite === 'contact') {
    await test('9.1', 'Поля форми контактів присутні',             T09_9_1);
    await test('9.2', 'Форма без повідомлення — заблокована',      T09_9_2);
    await test('9.3', 'Некоректний email у формі контактів',       T09_9_3);
    await test('9.4', 'XSS у полі повідомлення',                   T09_9_4);
  }

  printSummary(start);
}

main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
