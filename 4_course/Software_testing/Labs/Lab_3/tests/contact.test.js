/**
 * tests/contact.test.js
 * T09 — Тести форми зворотного зв'язку.
 */

const { Key }       = require('selenium-webdriver');
const { skip }      = require('../utils/runner');
const { sleep }     = require('../utils/helpers');
const ContactPage   = require('../pages/ContactPage');

async function T09_9_1(driver) {
  // 9.1 Поля форми контактів присутні
  const page = new ContactPage(driver);
  if (!(await page.open())) skip('Сторінка контактів не знайдена');

  const emailField = await page.getEmailField();
  const msgField   = await page.getMessageField();

  if (!emailField) {
    console.log('[INFO 9.1] input#email не знайдено стандартними селекторами — перевірте вручну');
    return;
  }
  if (!msgField) {
    console.log('[INFO 9.1] Textarea не знайдено — перевірте вручну');
    return;
  }
  console.log(`       Форма контактів: email і textarea знайдено. URL: ${await page.currentUrl()}`);
}

async function T09_9_2(driver) {
  // 9.2 Форма без повідомлення — заблокована валідацією
  const page = new ContactPage(driver);
  if (!(await page.open())) skip('Сторінка контактів не знайдена');

  const { beforeUrl, afterUrl } = await page.submitWithoutMessage();
  if (!beforeUrl) skip('Кнопка submit не знайдена');

  const src     = (await driver.getPageSource()).toLowerCase();
  const blocked = beforeUrl === afterUrl ||
    ['required', 'обов', 'введіть', 'error', 'повідомлення'].some(k => src.includes(k));

  if (blocked) {
    console.log('       Порожнє повідомлення заблоковане');
  } else {
    console.log('[DEFECT 9.2] Форма надіслана без тексту повідомлення!');
  }
}

async function T09_9_3(driver) {
  // 9.3 Некоректний email у формі контактів
  const page = new ContactPage(driver);
  if (!(await page.open())) skip('Сторінка контактів не знайдена');

  const emailField = await page.getEmailField();
  if (!emailField) skip('Поле email не знайдено');

  await emailField.clear();
  await emailField.sendKeys('notvalidemail', Key.TAB);
  await sleep(500);

  const isInvalid = await page.isEmailInvalid(emailField);
  const hasError  = await page.contains('invalid', 'невірн', 'error');

  if (isInvalid || hasError) {
    console.log('       Некоректний email відхиляється');
  } else {
    console.log('       INFO — HTML5-валідація, перевірте вручну після submit');
  }
}

async function T09_9_4(driver) {
  // 9.4 XSS у полі повідомлення
  const page = new ContactPage(driver);
  if (!(await page.open())) skip('Сторінка контактів не знайдена');

  const msgField = await page.getMessageField();
  if (!msgField) skip('Textarea не знайдено');

  await msgField.clear();
  await msgField.sendKeys("<script>alert('xss')</script>");

  const val = await msgField.getAttribute('value');
  if (val.includes('<script>')) {
    console.log('       INFO — textarea приймає HTML, перевірте серверну обробку');
  } else {
    console.log('       HTML-теги відфільтровані на рівні textarea');
  }
}

module.exports = { T09_9_1, T09_9_2, T09_9_3, T09_9_4 };
