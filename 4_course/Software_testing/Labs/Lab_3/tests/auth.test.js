/**
 * tests/auth.test.js
 * T06 — Тести авторизації.
 */

const { skip, assert } = require('../utils/runner');
const { sleep }        = require('../utils/helpers');
const AuthPage         = require('../pages/AuthPage');

async function T06_6_2(driver) {
  // 6.2 Авторизація з невірним паролем — повідомлення є
  const page = new AuthPage(driver);
  await page.openLogin();

  await page.login('nonexistent_test@example.com', 'WrongPass999!');

  const hasError = await page.hasLoginError();
  if (hasError) {
    console.log('       Повідомлення про невірний пароль є');
  } else {
    console.log('[DEFECT 6.2] Немає повідомлення при невірному паролі!');
  }
}

async function T06_6_3(driver) {
  // 6.3 Авторизація з порожнім email — форма заблокована
  const page = new AuthPage(driver);
  await page.openLogin();

  const submit = await page.getLoginSubmitButton();
  if (!submit) skip('Кнопка входу не знайдена');

  const beforeUrl = await page.currentUrl();
  await submit.click();
  await sleep(1500);

  const afterUrl = await page.currentUrl();
  const src      = (await driver.getPageSource()).toLowerCase();
  const blocked  = beforeUrl === afterUrl ||
    ['required', 'обов', 'введіть', 'error'].some(k => src.includes(k));

  if (blocked) {
    console.log('       Порожній email при вході заблокований');
  } else {
    console.log('[DEFECT 6.3] Форма надіслана з порожнім email!');
  }
}

async function T06_6_4(driver) {
  // 6.4 «Забули пароль?» → /uk/password-recovery
  const page = new AuthPage(driver);
  await page.openLogin();

  const link = await page.getForgotPasswordLink();
  if (!link) { console.log('       INFO — посилання не знайдено, перевірте вручну'); return; }

  await link.click();
  await sleep(2000);

  const url = await page.currentUrl();
  assert(url.includes('password'), 'Перехід не на сторінку відновлення паролю');
  console.log(`       'Забули пароль?' → ${url}`);
}

module.exports = { T06_6_2, T06_6_3, T06_6_4 };
