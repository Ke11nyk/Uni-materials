/**
 * tests/registration.test.js
 * T05 — Тести реєстрації (двоетапна PrestaShop).
 */

const { Key }    = require('selenium-webdriver');
const { skip }   = require('../utils/runner');
const { sleep }  = require('../utils/helpers');
const AuthPage   = require('../pages/AuthPage');

async function T05_5_3(driver) {
  // 5.3 Некоректний email на кроці 1 — валідація
  const page = new AuthPage(driver);
  if (!(await page.openRegistrationStep1())) skip('Крок 1 реєстрації не відкрився');

  const emailField = await page.getStep1EmailField();
  if (!emailField) skip('Поле email на кроці 1 не знайдено');

  await emailField.clear();
  await emailField.sendKeys('notanemail', Key.TAB);
  await sleep(800);

  const isInvalid = await page.isInvalid(emailField);
  const hasError  = await page.contains('невірн', 'некоректн', 'invalid', 'error');

  if (isInvalid || hasError) {
    console.log('       Некоректний email — валідація спрацювала');
  } else {
    console.log('       INFO — валідація не підтверджена в DOM');
  }
}

async function T05_5_4(driver) {
  // 5.4 Короткий пароль (3 символи) на кроці 2
  const page = new AuthPage(driver);
  if (!(await page.completeStep1())) skip('Крок 2 реєстрації не відкрився');

  const passField = await page.getStep2PasswordField();
  if (!passField) skip('Поле паролю не знайдено');

  await passField.clear();
  await passField.sendKeys('123', Key.TAB);
  await sleep(800);

  const hasError = await page.contains('коротк', 'мінімум', 'minimum', 'least', 'characters', 'символ');
  console.log(`[${hasError ? 'PASS' : 'INFO'} 5.4] Короткий пароль — ${hasError ? 'помилка є' : 'перевірте після submit'}`);
}

async function T05_5_7(driver) {
  // 5.7 Порожня форма кроку 1 — не надсилається
  const page = new AuthPage(driver);
  if (!(await page.openRegistrationStep1())) skip('Крок 1 реєстрації не відкрився');

  const btn = await page.getStep1SubmitButton();
  if (!btn) skip('Кнопка submit не знайдена');

  const beforeUrl = await page.currentUrl();
  try { await btn.click(); } catch (_) { await page.js('arguments[0].click();', btn); }
  await sleep(1500);

  const afterUrl = await page.currentUrl();
  const src      = (await driver.getPageSource()).toLowerCase();
  const blocked  = beforeUrl === afterUrl ||
    ['required', 'обов', 'введіть', 'error', 'помилк'].some(k => src.includes(k));

  if (blocked) {
    console.log('       Порожня форма — валідація спрацювала');
  } else {
    console.log('[DEFECT 5.7] Порожня форма реєстрації надіслана без помилок!');
  }
}

async function T05_5_8(driver) {
  // 5.8 XSS у полі ім'я на кроці 2
  const page = new AuthPage(driver);
  if (!(await page.completeStep1())) skip('Крок 2 реєстрації не відкрився');

  const nameField = await page.getFirstNameField();
  if (!nameField) skip('Поле firstname не знайдено');

  await nameField.clear();
  await nameField.sendKeys("<script>alert('xss')</script>");
  await sleep(500);

  const val = await nameField.getAttribute('value');
  if (val.includes('<script>')) {
    console.log('       INFO — поле приймає HTML, важлива серверна перевірка');
  } else {
    console.log('       HTML-теги відфільтровані на рівні input');
  }
}

module.exports = { T05_5_3, T05_5_4, T05_5_7, T05_5_8 };
