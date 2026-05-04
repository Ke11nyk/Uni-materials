/**
 * pages/ContactPage.js
 * Page Object для форми зворотного зв'язку (/uk/contact-us).
 *
 * Landshaft.info використовує нестандартний PrestaShop-модуль contactform.
 * Поле email: може мати будь-який id/name — шукаємо по type=email, type=text,
 * data-validate=isEmail або label з текстом «email».
 * Textarea: шукаємо будь-який видимий textarea на сторінці.
 */

const { By, Key }  = require('selenium-webdriver');
const BasePage     = require('./BasePage');
const { sleep }    = require('../utils/helpers');
const { BASE_URL } = require('../utils/config');

class ContactPage extends BasePage {

  /**
   * Відкрити сторінку контактів.
   * Пробує кілька можливих URL.
   */
  async open() {
    for (const path of ['contact-us', 'contact', 'contacts', 'contactus']) {
      await this.goto(BASE_URL + path);
      if (await this.contains('контакт', 'form', 'message', 'повідомлення', 'contact')) {
        return true;
      }
    }
    return false;
  }

  /**
   * Поле email у формі контактів.
   * Перебирає максимально широкий набір селекторів:
   * стандартні PrestaShop → type=email → data-validate → будь-який input поруч з label Email.
   */
  async getEmailField() {
    // Спроба 1: стандартні PrestaShop-атрибути
    const byAttr = await this.find([
      'input#email',
      "input[name='from']",
      "input[type='email']",
      "input[name='email']",
      "input[data-validate='isEmail']",
      "input[id*='email' i]",
      "input[name*='email' i]",
      "input[placeholder*='email' i]",
      "input[placeholder*='пошта' i]",
      "input[placeholder*='e-mail' i]",
    ]);
    if (byAttr) return byAttr;

    // Спроба 2: перший видимий input[type=text] на сторінці контактів
    // (деякі PrestaShop-модулі використовують type=text для email-поля)
    const textInputs = await this.driver.findElements(By.css("input[type='text']"));
    for (const el of textInputs) {
      try {
        if (await el.isDisplayed()) return el;
      } catch (_) {}
    }

    return null;
  }

  /**
   * Textarea повідомлення.
   * Шукає будь-який видимий textarea на сторінці.
   */
  async getMessageField() {
    // Спроба 1: специфічні атрибути
    const byAttr = await this.find([
      'textarea#message',
      "textarea[name='message']",
      "textarea[id*='message' i]",
      "textarea[name*='message' i]",
      "textarea[placeholder*='повідомлення' i]",
      "textarea[placeholder*='message' i]",
    ]);
    if (byAttr) return byAttr;

    // Спроба 2: перший видимий textarea на сторінці
    const textareas = await this.driver.findElements(By.css('textarea'));
    for (const el of textareas) {
      try {
        if (await el.isDisplayed()) return el;
      } catch (_) {}
    }

    return null;
  }

  /** Кнопка надсилання форми */
  async getSubmitButton() {
    return (
      await this.find([
        "button[type='submit']",
        "input[type='submit']",
        'button#submitMessage',
        "button[id*='submit' i]",
        "button[class*='submit' i]",
      ]) ||
      await this.findXPath([
        "//button[contains(.,'Надіслати') or contains(.,'Відправити') or contains(.,'Submit') or contains(.,'Send')]",
      ])
    );
  }

  /** Перевірити HTML5-валідність поля email */
  async isEmailInvalid(emailEl) {
    return this.isInvalid(emailEl);
  }

  /**
   * Надіслати форму з лише email (без повідомлення).
   */
  async submitWithoutMessage(email = 'test@example.com') {
    const emailField = await this.getEmailField();
    if (emailField) {
      await emailField.clear();
      await emailField.sendKeys(email);
    }
    const submit = await this.getSubmitButton();
    if (!submit) return { beforeUrl: null, afterUrl: null };
    const beforeUrl = await this.currentUrl();
    await submit.click();
    await sleep(2000);
    return { beforeUrl, afterUrl: await this.currentUrl() };
  }
}

module.exports = ContactPage;
