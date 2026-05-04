/**
 * pages/AuthPage.js
 * Page Object для авторизації та реєстрації (PrestaShop двоетапна).
 *
 * Реєстрація:
 *   Крок 1: /uk/authentication#account-creation — поле email + «СТВОРИТИ АКАУНТ»
 *   Крок 2: після submit — повна форма (ім'я, пароль тощо)
 *
 * Особливість: PrestaShop-тема landshaft.info використовує нестандартні id
 * для полів кроку 2, тому шукаємо через JS по всіх input[type=text] та input[type=password].
 */

const { By, Key }  = require('selenium-webdriver');
const BasePage     = require('./BasePage');
const { sleep }    = require('../utils/helpers');
const { BASE_URL } = require('../utils/config');

class AuthPage extends BasePage {

  // ─── Авторизація (Вхід) ───────────────────────────────────────────────

  /** Відкрити сторінку авторизації */
  async openLogin() {
    await this.goto(BASE_URL + 'authentication');
    await sleep(2000);
  }

  /** Поле email у формі входу */
  async getLoginEmailField() {
    return this.find([
      'input#field-email',
      "form#login-form input[name='email']",
      "input[name='email']",
    ]);
  }

  /** Поле пароля у формі входу */
  async getLoginPasswordField() {
    return this.find([
      'input#field-password',
      "form#login-form input[name='password']",
      "input[type='password']",
    ]);
  }

  /** Кнопка «Увійти» */
  async getLoginSubmitButton() {
    return this.find([
      'button#submit-login',
      "button[data-link-action='sign-in']",
      "form#login-form button[type='submit']",
      "button[type='submit']",
    ]);
  }

  /** Спроба входу з вказаними даними */
  async login(email, password) {
    const emailField = await this.getLoginEmailField();
    const passField  = await this.getLoginPasswordField();
    if (!emailField || !passField) return false;
    await emailField.clear();
    await emailField.sendKeys(email);
    await passField.clear();
    await passField.sendKeys(password);
    const btn = await this.getLoginSubmitButton();
    if (btn) await btn.click();
    await sleep(2000);
    return true;
  }

  /** Перевірити наявність повідомлення про помилку входу */
  async hasLoginError() {
    return this.contains('невірн', 'помилк', 'incorrect', 'invalid', 'authentication failed', 'error');
  }

  /** Знайти посилання «Забули пароль?» */
  async getForgotPasswordLink() {
    return (
      await this.find(["a.forgot-password", "a[href*='password-recovery']", "a[href*='password']"]) ||
      await this.findXPath(["//a[contains(.,'Забули') or contains(.,'Forgot')]"])
    );
  }

  // ─── Реєстрація — Крок 1 ─────────────────────────────────────────────

  /** Відкрити крок 1 реєстрації */
  async openRegistrationStep1() {
    await this.goto(BASE_URL + 'authentication#account-creation');
    await sleep(2000);
    return this.contains('створити акаунт', 'account-creation', 'create');
  }

  /** Поле email на кроці 1 */
  async getStep1EmailField() {
    return this.find([
      "#account-creation input[type='email']",
      "#account-creation input[name='email']",
      "input[name='email']",
      "input[type='email']",
    ]);
  }

  /** Кнопка «СТВОРИТИ АКАУНТ» (крок 1) */
  async getStep1SubmitButton() {
    return (
      await this.find([
        "button[data-link-action='display-register-form']",
        "button.no-account-btn",
        '#account-creation button[type="submit"]',
        '#account-creation button',
      ]) ||
      await this.findXPath([
        "//button[contains(.,'СТВОРИТИ') or contains(.,'Створити')]",
      ])
    );
  }

  /**
   * Виконати крок 1 реєстрації (ввести email і натиснути кнопку).
   * @param {string} email
   * @returns {boolean} true якщо крок 2 відкрився
   */
  async completeStep1(email = 'sel_test_laba3@example.com') {
    if (!(await this.openRegistrationStep1())) return false;
    const emailField = await this.getStep1EmailField();
    if (!emailField) return false;
    await emailField.clear();
    await emailField.sendKeys(email);
    const btn = await this.getStep1SubmitButton();
    if (btn) { await btn.click(); await sleep(2500); }
    return this.contains('firstname', 'field-firstname', 'пароль', 'password', 'ім\'я', 'name');
  }

  // ─── Реєстрація — Крок 2 ─────────────────────────────────────────────

  /**
   * Поле ім'я (firstname) на кроці 2.
   * Landshaft.info використовує нестандартні атрибути — шукаємо широко:
   * за id, name, placeholder, а також перший видимий input[type=text].
   */
  async getFirstNameField() {
    // Спроба 1: стандартні селектори
    const bySelectors = await this.find([
      'input#field-firstname',
      "input[name='firstname']",
      "input[name='first_name']",
      "input[placeholder*='ім']",
      "input[placeholder*='name' i]",
      "input[placeholder*='first' i]",
    ]);
    if (bySelectors) return bySelectors;

    // Спроба 2: перший видимий input[type=text] у формі реєстрації
    // (PrestaShop-теми часто мають лише тип без name/id)
    const textInputs = await this.driver.findElements(
      By.css("form#customer-form input[type='text'], form input[type='text']")
    );
    for (const el of textInputs) {
      try {
        if (await el.isDisplayed()) return el;
      } catch (_) {}
    }

    // Спроба 3: через JS — перший text-input на сторінці що не є email
    try {
      const el = await this.driver.findElement(
        By.css("input[type='text']:not([type='email']):not([type='hidden'])")
      );
      if (el) return el;
    } catch (_) {}

    return null;
  }

  /** Поле пароля на кроці 2 */
  async getStep2PasswordField() {
    return this.find([
      'input#field-password',
      "form#customer-form input[name='password']",
      "input[type='password']",
    ]);
  }

  /** Кнопка «Зареєструватися» (крок 2) */
  async getStep2SubmitButton() {
    return this.find([
      "button[data-link-action='save-customer']",
      "form#customer-form button[type='submit']",
      "button.form-control-submit",
      "button[type='submit']",
    ]);
  }
}

module.exports = AuthPage;
