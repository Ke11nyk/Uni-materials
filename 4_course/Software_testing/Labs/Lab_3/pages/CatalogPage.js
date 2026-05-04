/**
 * pages/CatalogPage.js
 * Page Object для каталогу та карток товарів.
 */

const { By }      = require('selenium-webdriver');
const BasePage    = require('./BasePage');
const { sleep }   = require('../utils/helpers');
const { BASE_URL, KNOWN_PRODUCTS, CATEGORIES } = require('../utils/config');

class CatalogPage extends BasePage {

  /** Відкрити категорію хвойних */
  async openConifers() {
    await this.open('42-conifers');
    await sleep(2000);
  }

  /** Відкрити сторінку конкретного товару за відомим URL */
  async openProductByUrl(path) {
    await this.open(path);
    await sleep(3000);
  }

  /**
   * Відкрити будь-яку доступну картку товару.
   * Спочатку пробує прямі URL, потім пошук, потім категорії.
   * Після відкриття чекає появи кнопки «Купити» або поля кількості.
   * @returns {boolean} true якщо сторінка товару повністю завантажилась
   */
  async openAnyProduct() {
    // Метод 1: прямий URL відомого товару
    for (const path of KNOWN_PRODUCTS) {
      await this.open(path);
      await sleep(4000); // збільшено: бот-захист PrestaShop
      if (await this._hasProductControls()) return true;
    }

    // Метод 2: через сторінку результатів пошуку
    await this.open('search?controller=search&search_query=%D1%82%D1%83%D1%8F');
    await sleep(3000);
    const card = await this.find([
      'article.product-miniature a.thumbnail',
      '.product-miniature h2 a',
      'a.product-thumbnail',
    ]);
    if (card) {
      await card.click();
      await sleep(4000);
      if (await this._hasProductControls()) return true;
    }

    // Метод 3: перебір категорій
    for (const url of Object.values(CATEGORIES)) {
      await this.driver.get(url);
      await sleep(3000);
      const c = await this.find([
        'article.product-miniature a.thumbnail',
        '.product-miniature h2 a',
        'a.product-thumbnail',
      ]);
      if (c) {
        await c.click();
        await sleep(4000);
        if (await this._hasProductControls()) return true;
      }
    }

    return false;
  }

  /**
   * Перевірити, чи є на сторінці елементи управління товаром
   * (кнопка «Купити» або поле кількості) — через JS, не через isDisplayed().
   * Обхід бот-захисту: елементи можуть бути в DOM але не «видимі» для Selenium.
   */
  async _hasProductControls() {
    return this.js(`
      return !!(
        document.querySelector('button#add-to-cart-or-refresh') ||
        document.querySelector('button.add-to-cart') ||
        document.querySelector('input#quantity_wanted') ||
        document.querySelector('.product-add-to-cart')
      );
    `);
  }

  /** Перевірити, чи відкрита сторінка товару (за текстом) */
  async _isProductPage() {
    return this.contains('add-to-cart', 'quantity_wanted', 'купити', 'buy');
  }

  /**
   * Кнопка «Додати до кошика».
   * Шукає спочатку через CSS, потім через JS (обхід бот-захисту).
   */
  async getAddToCartButton() {
    // Спроба 1: звичайний findVisible
    const btn = await this.find([
      'button#add-to-cart-or-refresh',
      'button.add-to-cart',
      "button[data-button-action='add-to-cart']",
      '.product-add-to-cart button[type="submit"]',
      '.product-add-to-cart button',
    ]);
    if (btn) return btn;

    // Спроба 2: через JS — ігнорує видимість (обхід бот-захисту)
    const selectors = [
      'button#add-to-cart-or-refresh',
      'button.add-to-cart',
      "button[data-button-action='add-to-cart']",
      '.product-add-to-cart button',
    ];
    for (const sel of selectors) {
      try {
        const el = await this.driver.findElement(By.css(sel));
        // Скролимо до елемента та робимо його видимим через JS
        await this.js('arguments[0].scrollIntoView(true);', el);
        await sleep(500);
        return el;
      } catch (_) {}
    }
    return null;
  }

  /**
   * Поле кількості товару.
   * Шукає через CSS, потім через JS.
   */
  async getQuantityField() {
    const qty = await this.find([
      'input#quantity_wanted',
      "input[name='qty']",
      '.product-quantity input[type="number"]',
      '.product-quantity input',
    ]);
    if (qty) return qty;

    // Спроба через JS
    for (const sel of ['input#quantity_wanted', "input[name='qty']", '.product-quantity input']) {
      try {
        const el = await this.driver.findElement(By.css(sel));
        await this.js('arguments[0].scrollIntoView(true);', el);
        await sleep(300);
        return el;
      } catch (_) {}
    }
    return null;
  }

  /** Отримати усі посилання головного меню (видимі) */
  async getMenuLinks() {
    const all = await this.driver.findElements(
      By.css('#top-menu a, ul.top-menu a, .main-menu a, header a')
    );
    const result = [];
    for (const el of all) {
      try {
        if (await el.isDisplayed()) {
          const href = await el.getAttribute('href');
          if (href && href.includes('/uk/') && href !== BASE_URL &&
              !href.includes('authentication')) {
            result.push({ el, href });
          }
        }
      } catch (_) {}
    }
    return result;
  }

  /** Знайти та натиснути перший пункт меню */
  async clickFirstMenuCategory() {
    const links = await this.getMenuLinks();
    if (!links.length) return false;
    await links[0].el.click();
    await sleep(2000);
    return true;
  }

  /** Лічильник кошика у шапці */
  async getCartCount() {
    const sels = [
      '.cart-products-count',
      '#_desktop_cart .cart-products-count',
      '.blockcart .cart-products-count',
    ];
    for (const sel of sels) {
      try {
        const el = await this.driver.findElement(By.css(sel));
        const txt = (await el.getText()).trim().replace(/[()]/g, '');
        if (/^\d+$/.test(txt)) return parseInt(txt, 10);
      } catch (_) {}
    }
    return null;
  }
}

module.exports = CatalogPage;
