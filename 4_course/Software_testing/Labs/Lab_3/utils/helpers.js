/**
 * utils/helpers.js
 * Допоміжні функції: очікування, пошук елементів, перевірки.
 */

const { By } = require('selenium-webdriver');

/** Затримка на ms мілісекунд */
const sleep = ms => new Promise(r => setTimeout(r, ms));

/**
 * Повертає перший видимий елемент із масиву CSS-селекторів, або null.
 * @param {WebDriver} driver
 * @param {string[]} cssSelectors
 * @returns {WebElement|null}
 */
async function findVisible(driver, cssSelectors) {
  for (const sel of cssSelectors) {
    try {
      const el = await driver.findElement(By.css(sel));
      if (await el.isDisplayed()) return el;
    } catch (_) { /* пропустити — елемент не знайдено */ }
  }
  return null;
}

/**
 * Повертає перший видимий елемент із масиву XPath-виразів, або null.
 * @param {WebDriver} driver
 * @param {string[]} xpathList
 * @returns {WebElement|null}
 */
async function findVisibleXPath(driver, xpathList) {
  for (const xp of xpathList) {
    try {
      const el = await driver.findElement(By.xpath(xp));
      if (await el.isDisplayed()) return el;
    } catch (_) { /* пропустити */ }
  }
  return null;
}

/**
 * Перевіряє, чи містить page source хоча б одне з ключових слів.
 * @param {WebDriver} driver
 * @param {...string} keywords
 * @returns {boolean}
 */
async function pageContains(driver, ...keywords) {
  const src = (await driver.getPageSource()).toLowerCase();
  return keywords.some(kw => src.includes(kw.toLowerCase()));
}

/**
 * Перевіряє validity.valid поля через JS.
 * @param {WebDriver} driver
 * @param {WebElement} el
 * @returns {boolean}
 */
async function isFieldInvalid(driver, el) {
  return driver.executeScript(
    'return arguments[0].validity && arguments[0].validity.valid === false;', el
  );
}

module.exports = { sleep, findVisible, findVisibleXPath, pageContains, isFieldInvalid };
