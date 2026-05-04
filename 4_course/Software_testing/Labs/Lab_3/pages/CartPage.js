/**
 * pages/CartPage.js
 * Page Object для сторінки кошика.
 */

const BasePage  = require('./BasePage');
const { sleep } = require('../utils/helpers');

class CartPage extends BasePage {

  /** Відкрити сторінку кошика напряму */
  async open() {
    await this.goto(require('../utils/config').BASE_URL + 'order');
    await sleep(2000);
  }

  /** Перевірити, чи є повідомлення про порожній кошик */
  async isEmptyCartMessageVisible() {
    return this.contains('порожній', 'пустий', 'empty', 'no items', 'немає товарів');
  }

  /**
   * Закрити модальне вікно PrestaShop після додавання товару (якщо з'явилось).
   */
  async closeModal() {
    try {
      const close = await this.find([
        '.modal .close',
        'button.close[data-dismiss="modal"]',
        '.cart-modal .close',
      ]);
      if (close) await close.click();
    } catch (_) {}
  }
}

module.exports = CartPage;
