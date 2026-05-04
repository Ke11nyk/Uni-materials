/**
 * utils/driver.js
 * Фабрика WebDriver — створення та налаштування браузера.
 *
 * Режим запуску керується змінною середовища HEADLESS:
 *   HEADLESS=false node index.js   → браузер з UI (можна спостерігати)
 *   HEADLESS=true  node index.js   → без UI (для CI/CD)
 *   node index.js                  → без UI (за замовчуванням)
 */

const { Builder } = require('selenium-webdriver');
const chrome      = require('selenium-webdriver/chrome');

/**
 * Створює та повертає налаштований Chrome WebDriver.
 *
 * @param {boolean|null} headless
 *   true  — без вікна браузера (CI-режим)
 *   false — з вікном браузера (для спостереження за тестами)
 *   null  — читає зі змінної середовища HEADLESS (default: true)
 * @returns {WebDriver}
 */
function buildDriver(headless = null) {
  // Визначаємо режим: аргумент → env → default true
  if (headless === null) {
    headless = process.env.HEADLESS !== 'false';
  }

  const opts = new chrome.Options();

  if (headless) {
    opts.addArguments('--headless=new');
  } else {
    // UI-режим: встановлюємо розмір і позицію вікна
    opts.addArguments('--window-size=1400,900');
    opts.addArguments('--window-position=100,50');
  }

  opts.addArguments(
    '--no-sandbox',
    '--disable-dev-shm-usage',
    '--lang=uk-UA',
  );

  // Маскуємо автоматизацію для обходу бот-захисту PrestaShop
  opts.excludeSwitches(['enable-automation']);
  opts.addArguments('--disable-blink-features=AutomationControlled');

  const driver = new Builder()
    .forBrowser('chrome')
    .setChromeOptions(opts)
    .build();

  return driver;
}

/**
 * Ін'єктує скрипт для приховування ознаки webdriver.
 * Викликати одразу після buildDriver().
 * @param {WebDriver} driver
 */
async function maskWebDriver(driver) {
  await driver.executeScript(
    "Object.defineProperty(navigator,'webdriver',{get:()=>undefined})"
  );
}

module.exports = { buildDriver, maskWebDriver };
