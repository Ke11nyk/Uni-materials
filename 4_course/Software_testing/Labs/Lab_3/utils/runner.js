/**
 * utils/runner.js
 * Мінімальний тест-ранер: збирає результати, виводить підсумок.
 *
 * Змінні середовища:
 *   HEADLESS=false  → браузер з UI
 *   SLOW=1          → пауза 2 с між тестами (зручно для спостереження)
 */

const { buildDriver, maskWebDriver } = require('./driver');

const results = [];

/** Клас-маркер для пропуску тесту */
class SkipTest extends Error {
  constructor(reason) {
    super(reason);
    this.name = 'SkipTest';
  }
}

/** Пропустити тест з вказаною причиною */
function skip(reason) { throw new SkipTest(reason); }

/** Проста перевірка умови */
function assert(condition, message) {
  if (!condition) throw new Error(`AssertionError: ${message}`);
}

/**
 * Запустити один тест у ізольованому WebDriver.
 * @param {string}   id   - ідентифікатор (напр. '1.1')
 * @param {string}   desc - опис тесту
 * @param {Function} fn   - async (driver) => void
 */
async function test(id, desc, fn) {
  const isHeadless = process.env.HEADLESS !== 'false';
  const isSlow     = process.env.SLOW === '1';

  let driver;
  try {
    driver = buildDriver();
    await maskWebDriver(driver);
    await fn(driver);
    results.push({ id, desc, status: 'PASS', note: '' });
    console.log(`[PASS ${id}] ${desc}`);
  } catch (err) {
    if (err.name === 'SkipTest') {
      results.push({ id, desc, status: 'SKIP', note: err.message });
      console.log(`[SKIP ${id}] ${desc} — ${err.message}`);
    } else {
      results.push({ id, desc, status: 'FAIL', note: err.message });
      console.log(`[FAIL ${id}] ${desc}`);
      console.log(`       ${err.message}`);
    }
  } finally {
    // У UI-режимі + SLOW=1: пауза перед закриттям браузера
    if (!isHeadless && isSlow) {
      await new Promise(r => setTimeout(r, 2000));
    }
    if (driver) await driver.quit();
  }
}

/** Вивести підсумок і завершити процес */
function printSummary(startTime) {
  const elapsed = ((Date.now() - startTime) / 1000).toFixed(1);
  const pass  = results.filter(r => r.status === 'PASS').length;
  const skip  = results.filter(r => r.status === 'SKIP').length;
  const fail  = results.filter(r => r.status === 'FAIL').length;

  console.log('\n' + '='.repeat(70));
  console.log(` Ran ${results.length} tests in ${elapsed}s`);
  console.log(` PASS: ${pass}  |  SKIP: ${skip}  |  FAIL: ${fail}`);
  console.log('='.repeat(70));

  if (fail > 0) {
    console.log('\nFailed tests:');
    results.filter(r => r.status === 'FAIL').forEach(r => {
      console.log(`  [${r.id}] ${r.desc}`);
      console.log(`       ${r.note}`);
    });
    process.exit(1);
  }
}

module.exports = { test, skip, assert, printSummary, SkipTest };
