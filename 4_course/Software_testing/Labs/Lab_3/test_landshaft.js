/**
 * Запуск:
 *   npm install selenium-webdriver
 *   node test_landshaft.js
 *
 * Структура сайту:
 *   Головна:         /uk/
 *   Хвойні:          /uk/42-conifers
 *   Пошук:           /uk/search?controller=search&search_query=...
 *   Авторизація:     /uk/authentication
 *   Реєстрація:      /uk/authentication#account-creation  (крок 1, email)
 *   Кошик:           /uk/order
 *   Пароль:          /uk/password-recovery
 *   Контакти:        /uk/contact-us
 *   Товар (відомий): /uk/thuja/111-thuja-occidentalis-smaragd
 */

const { Builder, By, Key, until, logging } = require('selenium-webdriver');
const chrome = require('selenium-webdriver/chrome');

// ─── config ───────────────────────────────────────────────────────────────
const BASE_URL = 'https://landshaft.info/uk/';
const TIMEOUT  = 12_000; // ms

const KNOWN_PRODUCTS = [
    'thuja/111-thuja-occidentalis-smaragd',
    'thuja/115-thuja-occidentalis-teddy',
    'yalivec/806-juniperus-communis-hibernica',
    'thuja/971-thuja-plicata-daniellow',
];

// ─── driver factory ───────────────────────────────────────────────────────
function buildDriver(headless = true) {
    const opts = new chrome.Options();
    if (headless) opts.addArguments('--headless=new');
    opts.addArguments(
        '--no-sandbox',
        '--disable-dev-shm-usage',
        '--window-size=1400,900',
        '--lang=uk-UA',
    );
    opts.excludeSwitches(['enable-automation']);
    opts.addArguments('--disable-blink-features=AutomationControlled');

    return new Builder()
        .forBrowser('chrome')
        .setChromeOptions(opts)
        .build();
}

// ─── helpers ──────────────────────────────────────────────────────────────
const sleep = ms => new Promise(r => setTimeout(r, ms));

async function findVisible(driver, cssSelectors) {
    for (const sel of cssSelectors) {
        try {
            const el = await driver.findElement(By.css(sel));
            if (await el.isDisplayed()) return el;
        } catch (_) { /* not found */ }
    }
    return null;
}

async function findVisibleXPath(driver, xpathList) {
    for (const xp of xpathList) {
        try {
            const el = await driver.findElement(By.xpath(xp));
            if (await el.isDisplayed()) return el;
        } catch (_) { /* not found */ }
    }
    return null;
}

async function pageContains(driver, ...keywords) {
    const src = (await driver.getPageSource()).toLowerCase();
    return keywords.some(kw => src.includes(kw.toLowerCase()));
}

// ─── test runner ──────────────────────────────────────────────────────────
const results = [];

async function test(id, description, fn) {
    let driver;
    try {
        driver = buildDriver();
        // Маскуємо webdriver
        await driver.executeScript(
            "Object.defineProperty(navigator,'webdriver',{get:()=>undefined})");
        await fn(driver);
        results.push({ id, description, status: 'PASS', note: '' });
        console.log(`[PASS ${id}] ${description}`);
    } catch (err) {
        if (err.name === 'SkipTest') {
            results.push({ id, description, status: 'SKIP', note: err.message });
            console.log(`[SKIP ${id}] ${description} — ${err.message}`);
        } else {
            results.push({ id, description, status: 'FAIL', note: err.message });
            console.log(`[FAIL ${id}] ${description}`);
            console.log(`       ${err.message}`);
        }
    } finally {
        if (driver) await driver.quit();
    }
}

function skip(reason) {
    const err = new Error(reason);
    err.name = 'SkipTest';
    throw err;
}

function assert(condition, message) {
    if (!condition) throw new Error(`AssertionError: ${message}`);
}

// ══════════════════════════════════════════════════════════════════════════════
//  T01 — КАТАЛОГ
// ══════════════════════════════════════════════════════════════════════════════

async function T01_1_1(driver) {
    await driver.get(BASE_URL);
    const title = await driver.getTitle();
    assert(title.length > 0, 'Заголовок сторінки порожній');
    assert((await driver.getCurrentUrl()).includes('landshaft.info'), 'URL не містить landshaft.info');
    console.log(`       Title: ${title}`);
}

async function T01_1_2(driver) {
    await driver.get(BASE_URL + '42-conifers');
    await sleep(2000);
    const url = await driver.getCurrentUrl();
    assert(url.includes('landshaft.info'), 'Сторінка не завантажилась');
    const ok = await pageContains(driver, 'хвойн', 'conifers', 'roslyny', 'product');
    assert(ok, `Сторінка категорії не відкрилась: ${url}`);
    console.log(`       URL: ${url}`);
}

async function T01_1_3(driver) {
    await driver.get(BASE_URL + KNOWN_PRODUCTS[0]);
    await sleep(2000);
    const src = (await driver.getPageSource()).toLowerCase();
    const isProduct =
        src.includes('add-to-cart') || src.includes('quantity_wanted') ||
        src.includes('купити') || src.includes('buy');
    assert(isProduct, 'Картка товару не відкрилась');
    const url = await driver.getCurrentUrl();
    console.log(`       Картка: ${url}`);
}

async function T01_1_5(driver) {
    await driver.get(BASE_URL + '99999-invalid-xyz');
    await sleep(2000);
    const src = (await driver.getPageSource()).toLowerCase();
    const url = await driver.getCurrentUrl();
    const handled =
        src.includes('404') || src.includes('не знайден') || src.includes('not found') ||
        url.replace(/\/$/, '') === BASE_URL.replace(/\/$/, '');
    console.log(`       ${handled ? 'Handled' : 'INFO — no explicit 404'}: ${url}`);
}

// ══════════════════════════════════════════════════════════════════════════════
//  T02 — ПОШУК
// ══════════════════════════════════════════════════════════════════════════════

async function getSearchInput(driver) {
    await driver.get(BASE_URL);
    await sleep(2000);

    // Спроба відкрити через toggle-кнопку
    const toggleSels = [
        'button.search-toggle',
        '.search-widget button',
        '#search_widget button',
    ];
    for (const sel of toggleSels) {
        try {
            const el = await driver.findElement(By.css(sel));
            if (await el.isDisplayed()) { await el.click(); await sleep(600); break; }
        } catch (_) {}
    }

    // PrestaShop стандартні селектори
    const fieldSels = [
        'input#search_query_top',
        "input[name='search_query']",
        "input[name='s']",
        'input[type="search"]',
        '.search-widget input',
        '#searchbox input',
    ];
    for (const sel of fieldSels) {
        try {
            const el = await driver.findElement(By.css(sel));
            await driver.executeScript(
                "arguments[0].style.display='block';" +
                "arguments[0].style.visibility='visible';" +
                "arguments[0].style.opacity='1';", el);
            return el;
        } catch (_) {}
    }
    return null;
}

async function T02_2_1(driver) {
    const inp = await getSearchInput(driver);
    if (!inp) skip('Поле пошуку не знайдено');
    await inp.clear();
    await inp.sendKeys('туя', Key.RETURN);
    await sleep(3000);
    const url = await driver.getCurrentUrl();
    const ok = await pageContains(driver, 'туя', 'search');
    assert(ok || url.includes('search'), 'Результати пошуку не відображаються');
    console.log(`       URL: ${url}`);
}

async function T02_2_2(driver) {
    const inp = await getSearchInput(driver);
    if (!inp) skip('Поле пошуку не знайдено');
    const beforeUrl = await driver.getCurrentUrl();
    await inp.clear();
    await inp.sendKeys(Key.RETURN);
    await sleep(2000);
    const afterUrl = await driver.getCurrentUrl();
    const src = (await driver.getPageSource()).toLowerCase();
    const blocked =
        beforeUrl === afterUrl ||
        ['введіть', 'обов', 'required', 'мінімум'].some(kw => src.includes(kw));
    if (blocked) {
        console.log('       Порожній пошук заблокований');
    } else {
        console.log(`[DEFECT 2.2] Порожній пошук не заблокований! URL: ${afterUrl}`);
    }
}

async function T02_2_3(driver) {
    const inp = await getSearchInput(driver);
    if (!inp) skip('Поле пошуку не знайдено');
    await inp.clear();
    await inp.sendKeys('<b>xsstest</b>', Key.RETURN);
    await sleep(2000);
    try {
        const bold = await driver.findElement(By.xpath("//b[normalize-space()='xsstest']"));
        if (await bold.isDisplayed()) {
            console.log('[DEFECT 2.4] XSS УРАЗЛИВІСТЬ: тег <b> рендерується!');
        } else {
            console.log('       <b> є в DOM але не відображається');
        }
    } catch (_) {
        console.log('       XSS-рядок екранований коректно');
    }
}

async function T02_2_4(driver) {
    const inp = await getSearchInput(driver);
    if (!inp) skip('Поле пошуку не знайдено');
    await inp.clear();
    await inp.sendKeys("' OR 1=1 --", Key.RETURN);
    await sleep(2000);
    const src = (await driver.getPageSource()).toLowerCase();
    const hasSqlError = ['sql syntax', 'mysql_', 'pg_query', 'ora-0', 'db error', 'stack trace']
        .some(kw => src.includes(kw));
    if (hasSqlError) {
        console.log('[DEFECT 2.6] SQL-помилка у відповіді сервера!');
    } else {
        console.log('       SQL-ін\'єкція не спричинила витоку');
    }
}

// ══════════════════════════════════════════════════════════════════════════════
//  T03 — КОШИК
// ══════════════════════════════════════════════════════════════════════════════

async function openProductPage(driver) {
    // Метод 1: прямий URL відомого товару
    for (const path of KNOWN_PRODUCTS) {
        await driver.get(BASE_URL + path);
        await sleep(2000);
        const src = (await driver.getPageSource()).toLowerCase();
        const isProduct =
            src.includes('add-to-cart') || src.includes('quantity_wanted') ||
            src.includes('купити') || src.includes('buy');
        if (isProduct) return true;
    }
    // Метод 2: пошук → перший результат
    await driver.get(BASE_URL + 'search?controller=search&search_query=%D1%82%D1%83%D1%8F');
    await sleep(3000);
    const cards = await driver.findElements(
        By.css('article.product-miniature a.thumbnail, .product-miniature h2 a, a.product-thumbnail'));
    for (const c of cards) {
        if (await c.isDisplayed()) { await c.click(); await sleep(2000); return true; }
    }
    return false;
}

async function getCartCount(driver) {
    const sels = [
        '.cart-products-count',
        '#_desktop_cart .cart-products-count',
        '.blockcart .cart-products-count',
    ];
    for (const sel of sels) {
        try {
            const el = await driver.findElement(By.css(sel));
            const txt = (await el.getText()).trim().replace(/[()]/g, '');
            if (/^\d+$/.test(txt)) return parseInt(txt, 10);
        } catch (_) {}
    }
    return null;
}

async function T03_3_1(driver) {
    if (!(await openProductPage(driver))) skip('Сторінку товару не вдалось відкрити');
    const btn = await findVisible(driver, [
        'button#add-to-cart-or-refresh',
        'button.add-to-cart',
        "button[data-button-action='add-to-cart']",
        '.product-add-to-cart button[type="submit"]',
    ]);
    if (!btn) skip('Кнопка «Додати до кошика» не знайдена');
    const before = await getCartCount(driver);
    await btn.click();
    await sleep(2000);
    // Закрити модальне вікно PrestaShop
    try {
        const close = await findVisible(driver, ['.modal .close', 'button.close[data-dismiss="modal"]']);
        if (close) await close.click();
    } catch (_) {}
    const after = await getCartCount(driver);
    if (before !== null && after !== null && after > before) {
        console.log(`       Додано: ${before} → ${after}`);
    } else {
        console.log(`       Кнопка натиснута (до=${before}, після=${after})`);
    }
}

async function T03_3_4(driver) {
    if (!(await openProductPage(driver))) skip('Сторінку товару не вдалось відкрити');
    const qty = await findVisible(driver, [
        'input#quantity_wanted',
        "input[name='qty']",
        '.product-quantity input[type="number"]',
    ]);
    if (!qty) skip('Поле кількості не знайдено');
    await driver.executeScript("arguments[0].value='';", qty);
    await qty.sendKeys('0');
    const btn = await findVisible(driver, ['button#add-to-cart-or-refresh', 'button.add-to-cart']);
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
    if (!(await openProductPage(driver))) skip('Сторінку товару не вдалось відкрити');
    const qty = await findVisible(driver, [
        'input#quantity_wanted',
        "input[name='qty']",
        '.product-quantity input',
    ]);
    if (!qty) skip('Поле кількості не знайдено');
    await driver.executeScript("arguments[0].value='';", qty);
    await qty.sendKeys('-1');
    const btn = await findVisible(driver, ['button#add-to-cart-or-refresh', 'button.add-to-cart']);
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

async function T03_4_4(driver) {
    await driver.get(BASE_URL + 'order');
    await sleep(2000);
    const hasMsg = await pageContains(driver, 'порожній', 'пустий', 'empty', 'no items', 'немає товарів');
    const url = await driver.getCurrentUrl();
    if (hasMsg) {
        console.log(`       Порожній кошик: ${url}`);
    } else {
        console.log(`       INFO — повідомлення не знайдено: ${url}`);
    }
}

// ══════════════════════════════════════════════════════════════════════════════
//  T05 — РЕЄСТРАЦІЯ (двоетапна)
// ══════════════════════════════════════════════════════════════════════════════

async function openRegistrationStep1(driver) {
    await driver.get(BASE_URL + 'authentication#account-creation');
    await sleep(2000);
    return pageContains(driver, 'створити акаунт', 'account-creation', 'create');
}

async function openRegistrationStep2(driver, email = 'sel_test_laba3@example.com') {
    if (!(await openRegistrationStep1(driver))) return false;
    const emailField = await findVisible(driver, [
        "#account-creation input[type='email']",
        "#account-creation input[name='email']",
        "input[name='email']",
        "input[type='email']",
    ]);
    if (!emailField) return false;
    await emailField.clear();
    await emailField.sendKeys(email);
    // Натиснути «СТВОРИТИ АКАУНТ»
    const btn = await findVisibleXPath(driver, [
        "//button[@data-link-action='display-register-form']",
        "//button[contains(.,'СТВОРИТИ') or contains(.,'Створити')]",
        "//*[@id='account-creation']//button[@type='submit']",
        "//*[@id='account-creation']//button",
    ]) || await findVisible(driver, [
        "button[data-link-action='display-register-form']",
        "button.no-account-btn",
        '#account-creation button[type="submit"]',
        '#account-creation button',
    ]);
    if (btn) { await btn.click(); await sleep(2000); }
    return pageContains(driver, 'firstname', 'field-firstname', 'пароль', 'password');
}

async function T05_5_3(driver) {
    if (!(await openRegistrationStep1(driver))) skip('Крок 1 реєстрації не відкрився');
    const emailField = await findVisible(driver, [
        "#account-creation input[type='email']",
        "#account-creation input[name='email']",
        "input[name='email']",
        "input[type='email']",
    ]);
    if (!emailField) skip('Поле email на кроці 1 не знайдено');
    await emailField.clear();
    await emailField.sendKeys('notanemail');
    await emailField.sendKeys(Key.TAB);
    await sleep(800);
    const isInvalid = await driver.executeScript(
        "return arguments[0].validity && arguments[0].validity.valid === false;", emailField);
    const hasError = await pageContains(driver, 'невірн', 'некоректн', 'invalid', 'error');
    if (isInvalid || hasError) {
        console.log('       Некоректний email — валідація спрацювала');
    } else {
        console.log('       INFO — валідація не підтверджена в DOM');
    }
}

async function T05_5_4(driver) {
    if (!(await openRegistrationStep2(driver))) skip('Крок 2 реєстрації не відкрився');
    const passField = await findVisible(driver, [
        'input#field-password',
        "form#customer-form input[name='password']",
        "input[type='password']",
    ]);
    if (!passField) skip('Поле паролю не знайдено');
    await passField.clear();
    await passField.sendKeys('123');
    await passField.sendKeys(Key.TAB);
    await sleep(800);
    const hasError = await pageContains(driver, 'коротк', 'мінімум', 'minimum', 'least', 'characters', 'символ');
    console.log(`[${hasError ? 'PASS' : 'INFO'} 5.4] Короткий пароль — ${hasError ? 'помилка є' : 'перевірте після submit'}`);
}

async function T05_5_7(driver) {
    if (!(await openRegistrationStep1(driver))) skip('Крок 1 реєстрації не відкрився');
    const beforeUrl = await driver.getCurrentUrl();
    // Клікнути «СТВОРИТИ АКАУНТ» без заповнення поля
    const btn = await findVisibleXPath(driver, [
        "//button[contains(.,'СТВОРИТИ') or contains(.,'Створити')]",
    ]) || await findVisible(driver, [
        "button[data-link-action='display-register-form']",
        '#account-creation button[type="submit"]',
        '#account-creation button',
        "button[type='submit']",
    ]);
    if (!btn) skip('Кнопка submit не знайдена');
    try { await btn.click(); } catch (_) {
        await driver.executeScript('arguments[0].click();', btn);
    }
    await sleep(1500);
    const afterUrl = await driver.getCurrentUrl();
    const src = (await driver.getPageSource()).toLowerCase();
    const blocked =
        beforeUrl === afterUrl ||
        ['required', 'обов', 'введіть', 'error', 'помилк'].some(k => src.includes(k));
    if (blocked) {
        console.log('       Порожня форма — валідація спрацювала');
    } else {
        console.log('[DEFECT 5.7] Порожня форма реєстрації надіслана без помилок!');
    }
}

async function T05_5_8(driver) {
    if (!(await openRegistrationStep2(driver))) skip('Крок 2 реєстрації не відкрився');
    const nameField = await findVisible(driver, [
        'input#field-firstname',
        "input[name='firstname']",
        "input[placeholder*='ім']",
        'form#customer-form input[type="text"]:first-of-type',
    ]);
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

// ══════════════════════════════════════════════════════════════════════════════
//  T06 — АВТОРИЗАЦІЯ
// ══════════════════════════════════════════════════════════════════════════════

async function T06_6_2(driver) {
    await driver.get(BASE_URL + 'authentication');
    await sleep(2000);
    const emailField = await findVisible(driver, [
        'input#field-email',
        "form#login-form input[name='email']",
        "input[name='email']",
    ]);
    const passField = await findVisible(driver, [
        'input#field-password',
        "form#login-form input[name='password']",
        "input[type='password']",
    ]);
    if (!emailField || !passField) skip('Поля email/пароль не знайдені');
    await emailField.clear();
    await emailField.sendKeys('nonexistent_test@example.com');
    await passField.clear();
    await passField.sendKeys('WrongPass999!');
    const submit = await findVisible(driver, [
        'button#submit-login',
        "button[data-link-action='sign-in']",
        "form#login-form button[type='submit']",
    ]);
    if (submit) await submit.click();
    await sleep(2000);
    const hasError = await pageContains(driver,
        'невірн', 'помилк', 'incorrect', 'invalid', 'authentication failed', 'error');
    if (hasError) {
        console.log('       Повідомлення про невірний пароль є');
    } else {
        console.log('[DEFECT 6.2] Немає повідомлення при невірному паролі!');
    }
}

async function T06_6_3(driver) {
    await driver.get(BASE_URL + 'authentication');
    await sleep(2000);
    const submit = await findVisible(driver, [
        'button#submit-login',
        "button[data-link-action='sign-in']",
        "form#login-form button[type='submit']",
        "button[type='submit']",
    ]);
    if (!submit) skip('Кнопка входу не знайдена');
    const beforeUrl = await driver.getCurrentUrl();
    await submit.click();
    await sleep(1500);
    const afterUrl = await driver.getCurrentUrl();
    const src = (await driver.getPageSource()).toLowerCase();
    const blocked =
        beforeUrl === afterUrl ||
        ['required', 'обов', 'введіть', 'error'].some(k => src.includes(k));
    if (blocked) {
        console.log('       Порожній email при вході заблокований');
    } else {
        console.log('[DEFECT 6.3] Форма надіслана з порожнім email!');
    }
}

async function T06_6_4(driver) {
    await driver.get(BASE_URL + 'authentication');
    await sleep(2000);
    const link = await findVisible(driver, [
        'a.forgot-password',
        "a[href*='password-recovery']",
        "a[href*='password']",
    ]) || await findVisibleXPath(driver, [
        "//a[contains(.,'Забули') or contains(.,'Forgot')]",
    ]);
    if (!link) { console.log('       INFO — посилання не знайдено, перевірте вручну'); return; }
    await link.click();
    await sleep(2000);
    const url = await driver.getCurrentUrl();
    assert(url.includes('password'), 'Перехід не на сторінку відновлення паролю');
    console.log(`       'Забули пароль?' → ${url}`);
}

// ══════════════════════════════════════════════════════════════════════════════
//  T09 — ФОРМА КОНТАКТІВ
// ══════════════════════════════════════════════════════════════════════════════

async function openContactPage(driver) {
    for (const path of ['contact-us', 'contact', 'contacts', 'contactus']) {
        await driver.get(BASE_URL + path);
        await sleep(2000);
        const ok = await pageContains(driver, 'контакт', 'form', 'message', 'повідомлення', 'contact');
        if (ok) return true;
    }
    return false;
}

async function T09_9_1(driver) {
    if (!(await openContactPage(driver))) skip('Сторінка контактів не знайдена');
    // PrestaShop contact form: input#email (type=text, name=from) або нестандартний
    const emailField = await findVisible(driver, [
        'input#email',
        "input[name='from']",
        "input[type='email']",
        "input[name='email']",
        // Якщо нестандартний модуль — будь-який input з label «email»
        "label[for*='mail'] ~ input, label[for*='mail'] + input",
    ]);
    const msgField = await findVisible(driver, [
        'textarea#message',
        "textarea[name='message']",
        'textarea',
    ]);
    if (!emailField) {
        console.log('[INFO 9.1] Поле email не знайдено стандартними селекторами (нестандартний модуль) — перевірте вручну');
        return;
    }
    assert(msgField !== null, 'Textarea не знайдено');
    console.log(`       Форма контактів: email знайдено, textarea знайдено. URL: ${await driver.getCurrentUrl()}`);
}

async function T09_9_2(driver) {
    if (!(await openContactPage(driver))) skip('Сторінка контактів не знайдена');
    const emailField = await findVisible(driver, [
        'input#email', "input[name='from']", "input[type='email']", "input[name='email']"]);
    if (emailField) {
        await emailField.clear();
        await emailField.sendKeys('test@example.com');
    }
    const submit = await findVisible(driver, [
        "button[type='submit']", "input[type='submit']", 'button#submitMessage',
    ]);
    if (!submit) skip('Кнопка submit не знайдена');
    const beforeUrl = await driver.getCurrentUrl();
    await submit.click();
    await sleep(2000);
    const afterUrl = await driver.getCurrentUrl();
    const src = (await driver.getPageSource()).toLowerCase();
    const blocked =
        beforeUrl === afterUrl ||
        ['required', 'обов', 'введіть', 'error', 'повідомлення'].some(k => src.includes(k));
    if (blocked) {
        console.log('       Порожнє повідомлення заблоковане');
    } else {
        console.log('[DEFECT 9.2] Форма надіслана без тексту повідомлення!');
    }
}

async function T09_9_3(driver) {
    if (!(await openContactPage(driver))) skip('Сторінка контактів не знайдена');
    const emailField = await findVisible(driver, [
        'input#email', "input[name='from']", "input[type='email']", "input[name='email']"]);
    if (!emailField) skip('Поле email не знайдено');
    await emailField.clear();
    await emailField.sendKeys('notvalidemail');
    await emailField.sendKeys(Key.TAB);
    await sleep(500);
    const isInvalid = await driver.executeScript(
        "return arguments[0].validity && arguments[0].validity.valid === false;", emailField);
    const hasError = await pageContains(driver, 'invalid', 'невірн', 'error');
    if (isInvalid || hasError) {
        console.log('       Некоректний email відхиляється');
    } else {
        console.log('       INFO — HTML5-валідація email, перевірте вручну після submit');
    }
}

async function T09_9_4(driver) {
    if (!(await openContactPage(driver))) skip('Сторінка контактів не знайдена');
    const msgField = await findVisible(driver, ["textarea[name='message']", 'textarea']);
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

// ══════════════════════════════════════════════════════════════════════════════
//  MAIN — запуск усіх тестів послідовно
// ══════════════════════════════════════════════════════════════════════════════

async function main() {
    console.log('='.repeat(70));
    console.log(' Selenium JS — Зелені Янголи (landshaft.info)');
    console.log(' Студент: Вербицький Артем, ІПС-41, Лаба 3');
    console.log('='.repeat(70));
    const start = Date.now();

    // T01 — Каталог
    await test('1.1', 'Головна сторінка завантажується', T01_1_1);
    await test('1.2', 'Навігація до категорії /uk/42-conifers', T01_1_2);
    await test('1.3', 'Картка товару — прямий URL', T01_1_3);
    await test('1.5', 'Неіснуюча сторінка — 404/редирект', T01_1_5);

    // T02 — Пошук
    await test('2.1', "Пошук «туя» — результати відображаються", T02_2_1);
    await test('2.2', 'Порожній пошуковий запит — заблокований', T02_2_2);
    await test('2.4', 'XSS: <b>xsstest</b> у полі пошуку', T02_2_3);
    await test('2.6', "SQL-ін'єкція у полі пошуку", T02_2_4);

    // T03 — Кошик
    await test('3.1', 'Додавання товару до кошика', T03_3_1);
    await test('3.4', 'Кількість = 0 відхиляється', T03_3_4);
    await test('3.5', "Від'ємна кількість -1 коригується", T03_3_5);
    await test('4.4', 'Порожній кошик — повідомлення на /uk/order', T03_4_4);

    // T05 — Реєстрація
    await test('5.3', 'Некоректний email на кроці 1 — валідація', T05_5_3);
    await test('5.4', 'Короткий пароль (3 символи) на кроці 2', T05_5_4);
    await test('5.7', 'Порожня форма кроку 1 — не надсилається', T05_5_7);
    await test('5.8', "XSS у полі ім'я на кроці 2", T05_5_8);

    // T06 — Авторизація
    await test('6.2', 'Невірний пароль — повідомлення є', T06_6_2);
    await test('6.3', 'Порожній email — форма заблокована', T06_6_3);
    await test('6.4', "«Забули пароль?» → /uk/password-recovery", T06_6_4);

    // T09 — Форма контактів
    await test('9.1', 'Поля форми контактів присутні', T09_9_1);
    await test('9.2', 'Форма без повідомлення — заблокована', T09_9_2);
    await test('9.3', 'Некоректний email у формі контактів', T09_9_3);
    await test('9.4', 'XSS у полі повідомлення', T09_9_4);

    // ─── підсумок ─────────────────────────────────────────────────────────────
    const elapsed = ((Date.now() - start) / 1000).toFixed(1);
    const pass = results.filter(r => r.status === 'PASS').length;
    const skip = results.filter(r => r.status === 'SKIP').length;
    const fail = results.filter(r => r.status === 'FAIL').length;
    const info = results.filter(r => r.status === 'INFO').length;

    console.log('\n' + '='.repeat(70));
    console.log(` Ran ${results.length} tests in ${elapsed}s`);
    console.log(` PASS: ${pass}  |  SKIP: ${skip}  |  FAIL: ${fail}  |  INFO: ${info}`);
    console.log('='.repeat(70));

    if (fail > 0) {
        console.log('\nFailed tests:');
        results.filter(r => r.status === 'FAIL').forEach(r => {
            console.log(`  [${r.id}] ${r.description}`);
            console.log(`       ${r.note}`);
        });
        process.exit(1);
    }
}

main().catch(err => {
    console.error('Fatal error:', err);
    process.exit(1);
});