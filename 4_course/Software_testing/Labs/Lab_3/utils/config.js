/**
 * utils/config.js
 * Глобальна конфігурація — URL, таймаути, відомі адреси товарів.
 */

const BASE_URL = 'https://landshaft.info/uk/';

const TIMEOUT = 12_000;

/** Прямі URL конкретних товарів (встановлено дослідним шляхом через Google) */
const KNOWN_PRODUCTS = [
  'thuja/111-thuja-occidentalis-smaragd',
  'thuja/115-thuja-occidentalis-teddy',
  'yalivec/806-juniperus-communis-hibernica',
  'thuja/971-thuja-plicata-daniellow',
];

/** Категорії з товарами */
const CATEGORIES = {
  conifers  : BASE_URL + '42-conifers',
  foliar    : BASE_URL + '53-foliar',
  trees     : BASE_URL + '55-trees',
  stamb     : BASE_URL + '1328-roslyny-na-shtambi',
};

module.exports = { BASE_URL, TIMEOUT, KNOWN_PRODUCTS, CATEGORIES };
