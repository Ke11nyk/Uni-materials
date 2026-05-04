/**
 * tests/search.test.js
 * T02 — Тести функціональності пошуку.
 */

const { skip, assert } = require('../utils/runner');
const SearchPage       = require('../pages/SearchPage');

async function T02_2_1(driver) {
  // 2.1 Пошук «туя» — результати відображаються
  const page = new SearchPage(driver);
  await page.openHome();
  await page.search('туя');
  const url = await page.currentUrl();
  const ok  = await page.contains('туя', 'search');
  assert(ok || url.includes('search'), 'Результати пошуку не відображаються');
  console.log(`       URL: ${url}`);
}

async function T02_2_2(driver) {
  // 2.2 Порожній пошуковий запит — має блокуватись
  const page = new SearchPage(driver);
  await page.openHome();
  const { beforeUrl, afterUrl, input } = await page.submitEmptySearch();
  if (!input) skip('Поле пошуку не знайдено');
  const src     = (await driver.getPageSource()).toLowerCase();
  const blocked = beforeUrl === afterUrl ||
    ['введіть', 'обов', 'required', 'мінімум'].some(kw => src.includes(kw));
  if (blocked) {
    console.log('       Порожній пошук заблокований');
  } else {
    console.log(`[DEFECT 2.2] Порожній пошук не заблокований! URL: ${afterUrl}`);
  }
}

async function T02_2_3(driver) {
  // 2.4 XSS: <b>xsstest</b> у полі пошуку
  const page = new SearchPage(driver);
  await page.openHome();
  const inp = await page.getSearchInput();
  if (!inp) skip('Поле пошуку не знайдено');
  const { rendered } = await page.searchXss('<b>xsstest</b>');
  if (rendered) {
    console.log('[DEFECT 2.4] XSS УРАЗЛИВІСТЬ: тег <b> рендерується!');
  } else {
    console.log('       XSS-рядок екранований коректно');
  }
}

async function T02_2_4(driver) {
  // 2.6 SQL-ін'єкція у полі пошуку
  const page = new SearchPage(driver);
  await page.openHome();
  const inp = await page.getSearchInput();
  if (!inp) skip('Поле пошуку не знайдено');
  const { hasSqlError } = await page.searchSqlInjection();
  if (hasSqlError) {
    console.log('[DEFECT 2.6] SQL-помилка у відповіді сервера!');
  } else {
    console.log("       SQL-ін'єкція не спричинила витоку");
  }
}

module.exports = { T02_2_1, T02_2_2, T02_2_3, T02_2_4 };
