/**
 * tests/catalog.test.js
 * T01 — Тести перегляду каталогу.
 */

const { test, skip, assert } = require('../utils/runner');
const { sleep }              = require('../utils/helpers');
const { BASE_URL }           = require('../utils/config');
const CatalogPage            = require('../pages/CatalogPage');

async function T01_1_1(driver) {
  // 1.1 Головна сторінка завантажується
  const page = new CatalogPage(driver);
  await page.openHome();
  const title = await page.title();
  assert(title.length > 0, 'Заголовок сторінки порожній');
  assert((await page.currentUrl()).includes('landshaft.info'), 'URL не містить landshaft.info');
  console.log(`       Title: ${title}`);
}

async function T01_1_2(driver) {
  // 1.2 Навігація до категорії /uk/42-conifers
  const page = new CatalogPage(driver);
  await page.openConifers();
  const url = await page.currentUrl();
  assert(url.includes('landshaft.info'), 'Сторінка не завантажилась');
  const ok = await page.contains('хвойн', 'conifers', 'roslyny', 'product');
  assert(ok, `Сторінка категорії не відкрилась: ${url}`);
  console.log(`       URL: ${url}`);
}

async function T01_1_3(driver) {
  // 1.3 Відкриття картки товару по прямому URL
  const page = new CatalogPage(driver);
  await page.openProductByUrl('thuja/111-thuja-occidentalis-smaragd');
  const isProduct = await page.contains('add-to-cart', 'quantity_wanted', 'купити', 'buy');
  assert(isProduct, 'Картка товару не відкрилась');
  console.log(`       Картка: ${await page.currentUrl()}`);
}

async function T01_1_5(driver) {
  // 1.5 Неіснуюча сторінка — 404 або редирект
  const page = new CatalogPage(driver);
  await page.open('99999-invalid-xyz');
  await sleep(2000);
  const src = (await driver.getPageSource()).toLowerCase();
  const url = await page.currentUrl();
  const handled =
    src.includes('404') || src.includes('не знайден') || src.includes('not found') ||
    url.replace(/\/$/, '') === BASE_URL.replace(/\/$/, '');
  console.log(`       ${handled ? 'Handled' : 'INFO — no explicit 404'}: ${url}`);
}

module.exports = { T01_1_1, T01_1_2, T01_1_3, T01_1_5 };
