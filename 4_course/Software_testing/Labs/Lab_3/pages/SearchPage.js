/**
 * pages/SearchPage.js
 * Page Object для функціональності пошуку.
 */

const { By, Key }  = require('selenium-webdriver');
const BasePage     = require('./BasePage');
const { sleep }    = require('../utils/helpers');

class SearchPage extends BasePage {

  /**
   * Знайти поле пошуку на головній сторінці.
   * PrestaShop: поле може бути приховане — відкривається через toggle-кнопку.
   * @returns {WebElement|null}
   */
  async getSearchInput() {
    // Спроба натиснути іконку пошуку (toggle)
    const toggleSels = [
      'button.search-toggle',
      '.search-widget button',
      '#search_widget button',
    ];
    for (const sel of toggleSels) {
      try {
        const el = await this.driver.findElement(By.css(sel));
        if (await el.isDisplayed()) { await el.click(); await sleep(600); break; }
      } catch (_) {}
    }

    // Стандартні PrestaShop-селектори поля пошуку
    const fieldSels = [
      'input#search_query_top',
      "input[name='search_query']",
      "input[name='s']",
      'input[type="search"]',
      '.search-widget input',
      '#searchbox input',
    ];

    for (const sel of fieldSels) {
      try {
        const el = await this.driver.findElement(By.css(sel));
        await this.makeVisible(el);
        return el;
      } catch (_) {}
    }
    return null;
  }

  /**
   * Виконати пошук за запитом.
   * @param {string} query
   */
  async search(query) {
    const inp = await this.getSearchInput();
    if (!inp) return null;
    await inp.clear();
    await inp.sendKeys(query, Key.RETURN);
    await sleep(3000);
    return inp;
  }

  /**
   * Надіслати порожній пошук (Enter без тексту).
   * @returns {{ beforeUrl: string, afterUrl: string, input: WebElement|null }}
   */
  async submitEmptySearch() {
    const inp = await this.getSearchInput();
    if (!inp) return { beforeUrl: null, afterUrl: null, input: null };
    await inp.clear();
    const beforeUrl = await this.currentUrl();
    await inp.sendKeys(Key.RETURN);
    await sleep(2000);
    const afterUrl = await this.currentUrl();
    return { beforeUrl, afterUrl, input: inp };
  }

  /**
   * Шукати та перевіряти наявність XSS-тегу в результатах.
   * @param {string} payload
   * @returns {{ rendered: boolean }}
   */
  async searchXss(payload) {
    const inp = await this.getSearchInput();
    if (!inp) return { rendered: false };
    await inp.clear();
    await inp.sendKeys(payload, Key.RETURN);
    await sleep(2000);
    try {
      const bold = await this.driver.findElement(
        By.xpath("//b[normalize-space()='xsstest']")
      );
      return { rendered: await bold.isDisplayed() };
    } catch (_) {
      return { rendered: false };
    }
  }

  /**
   * Перевірити SQL-ін'єкцію через поле пошуку.
   * @returns {{ hasSqlError: boolean }}
   */
  async searchSqlInjection() {
    const inp = await this.getSearchInput();
    if (!inp) return { hasSqlError: false };
    await inp.clear();
    await inp.sendKeys("' OR 1=1 --", Key.RETURN);
    await sleep(2000);
    const src = (await this.driver.getPageSource()).toLowerCase();
    const hasSqlError = ['sql syntax', 'mysql_', 'pg_query', 'ora-0', 'db error', 'stack trace']
      .some(kw => src.includes(kw));
    return { hasSqlError };
  }
}

module.exports = SearchPage;
