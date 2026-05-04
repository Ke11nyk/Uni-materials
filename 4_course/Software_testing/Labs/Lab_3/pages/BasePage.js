/**
 * pages/BasePage.js
 * Базовий клас для всіх Page Object-ів.
 * Містить спільні методи навігації та очікування.
 */

const { By, Key }    = require('selenium-webdriver');
const { sleep, findVisible, findVisibleXPath, pageContains } = require('../utils/helpers');
const { BASE_URL }   = require('../utils/config');

class BasePage {
  /**
   * @param {WebDriver} driver
   */
  constructor(driver) {
    this.driver = driver;
  }

  /** Перейти на вказану URL-адресу */
  async goto(url) {
    await this.driver.get(url);
    await sleep(2000);
  }

  /** Перейти за шляхом відносно BASE_URL */
  async open(path = '') {
    await this.goto(BASE_URL + path);
  }

  /** Відкрити головну сторінку */
  async openHome() {
    await this.open('');
  }

  /** Поточна URL */
  async currentUrl() {
    return this.driver.getCurrentUrl();
  }

  /** Заголовок сторінки */
  async title() {
    return this.driver.getTitle();
  }

  /** Чи містить page source хоча б одне ключове слово */
  async contains(...keywords) {
    return pageContains(this.driver, ...keywords);
  }

  /** Знайти перший видимий елемент за CSS-селекторами */
  async find(cssSelectors) {
    return findVisible(this.driver, cssSelectors);
  }

  /** Знайти перший видимий елемент за XPath-виразами */
  async findXPath(xpathList) {
    return findVisibleXPath(this.driver, xpathList);
  }

  /** Виконати JS-скрипт */
  async js(script, ...args) {
    return this.driver.executeScript(script, ...args);
  }

  /** Зробити елемент видимим через JS */
  async makeVisible(el) {
    await this.js(
      "arguments[0].style.display='block';" +
      "arguments[0].style.visibility='visible';" +
      "arguments[0].style.opacity='1';",
      el
    );
  }

  /** Перевірити HTML5-валідність поля */
  async isInvalid(el) {
    return this.js(
      'return arguments[0].validity && arguments[0].validity.valid === false;',
      el
    );
  }
}

module.exports = BasePage;
