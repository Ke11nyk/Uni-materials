/**
 * tests/cart.test.js
 * T03 & T04 — Тести кошика.
 */

const { skip }   = require('../utils/runner');
const { sleep }  = require('../utils/helpers');
const CatalogPage = require('../pages/CatalogPage');
const CartPage    = require('../pages/CartPage');

async function T03_3_1(driver) {
  // 3.1 Додавання товару до кошика
  const catalog = new CatalogPage(driver);
  if (!(await catalog.openAnyProduct())) skip('Сторінку товару не вдалось відкрити');

  const btn = await catalog.getAddToCartButton();
  if (!btn) skip("Кнопка «Додати до кошика» не знайдена");

  const before = await catalog.getCartCount();
  await btn.click();
  await sleep(2000);

  const cart = new CartPage(driver);
  await cart.closeModal();

  const after = await catalog.getCartCount();
  if (before !== null && after !== null && after > before) {
    console.log(`       Додано: ${before} → ${after}`);
  } else {
    console.log(`       Кнопка натиснута (до=${before}, після=${after})`);
  }
}

async function T03_3_4(driver) {
  // 3.4 Кількість = 0 — має відхилятись
  const catalog = new CatalogPage(driver);
  if (!(await catalog.openAnyProduct())) skip('Сторінку товару не вдалось відкрити');

  const qty = await catalog.getQuantityField();
  if (!qty) skip('Поле кількості не знайдено');

  await driver.executeScript("arguments[0].value='';", qty);
  await qty.sendKeys('0');

  const btn = await catalog.getAddToCartButton();
  if (btn) await btn.click();
  await sleep(2000);

  const src = (await driver.getPageSource()).toLowerCase();
  const hasError = ['мінімум', 'minimum', 'error', 'least 1', 'invalid quantity'].some(k => src.includes(k));
  const val = await qty.getAttribute('value');
  const corrected = /^\d+$/.test(val) && parseInt(val) >= 1;

  if (hasError || corrected) {
    console.log(`       Кількість 0 відхилена або скоригована → '${val}'`);
  } else {
    console.log(`[DEFECT 3.4] Товар прийнято з кількістю 0! Поле: '${val}'`);
  }
}

async function T03_3_5(driver) {
  // 3.5 Від'ємна кількість -1 — коригується або відхиляється
  const catalog = new CatalogPage(driver);
  if (!(await catalog.openAnyProduct())) skip('Сторінку товару не вдалось відкрити');

  const qty = await catalog.getQuantityField();
  if (!qty) skip('Поле кількості не знайдено');

  await driver.executeScript("arguments[0].value='';", qty);
  await qty.sendKeys('-1');

  const btn = await catalog.getAddToCartButton();
  if (btn) await btn.click();
  await sleep(2000);

  const val = await qty.getAttribute('value');
  if (/^-?\d+$/.test(val)) {
    const v = parseInt(val);
    if (v >= 1) {
      console.log(`       Від'ємне скориговано до ${v}`);
    } else {
      console.log(`[DEFECT 3.5] Поле містить від'ємне значення: ${v}`);
    }
  } else {
    console.log(`       Значення після -1: '${val}'`);
  }
}

async function T04_4_4(driver) {
  // 4.4 Порожній кошик — повідомлення на /uk/order
  const cart = new CartPage(driver);
  await cart.open();
  const hasMsg = await cart.isEmptyCartMessageVisible();
  const url = await cart.currentUrl();
  if (hasMsg) {
    console.log(`       Порожній кошик: ${url}`);
  } else {
    console.log(`       INFO — повідомлення не знайдено: ${url}`);
  }
}

module.exports = { T03_3_1, T03_3_4, T03_3_5, T04_4_4 };
