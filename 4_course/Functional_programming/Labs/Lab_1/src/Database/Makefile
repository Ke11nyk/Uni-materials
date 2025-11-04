# Makefile для проекту Faculty Newspaper

.PHONY: help build run clean install-deps init-db reset-db test lint format

# Кольори для виводу
GREEN  := \033[0;32m
YELLOW := \033[0;33m
NC     := \033[0m # No Color

help: ## Показати це повідомлення
	@echo "$(GREEN)Faculty Newspaper - Доступні команди:$(NC)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(YELLOW)%-15s$(NC) %s\n", $$1, $$2}'
	@echo ""

build: ## Збудувати проект
	@echo "$(GREEN)Збірка проекту...$(NC)"
	cabal build

run: ## Запустити програму
	@echo "$(GREEN)Запуск програми...$(NC)"
	cabal run faculty-newspaper

clean: ## Очистити зібрані файли
	@echo "$(GREEN)Очищення...$(NC)"
	cabal clean
	rm -rf dist-newstyle/

install-deps: ## Встановити залежності
	@echo "$(GREEN)Встановлення залежностей...$(NC)"
	cabal update
	cabal install --only-dependencies

init-db: ## Ініціалізувати базу даних
	@echo "$(GREEN)Ініціалізація бази даних...$(NC)"
	@read -p "MySQL пароль root: " password; \
	mysql -u root -p$$password < database_init.sql
	@echo "$(GREEN)База даних успішно ініціалізована!$(NC)"

reset-db: ## Видалити та створити базу даних заново
	@echo "$(YELLOW)УВАГА: Це видалить всі дані!$(NC)"
	@read -p "Продовжити? (yes/no): " confirm; \
	if [ "$$confirm" = "yes" ]; then \
		read -p "MySQL пароль root: " password; \
		mysql -u root -p$$password < database_init.sql; \
		echo "$(GREEN)База даних перестворена!$(NC)"; \
	else \
		echo "Скасовано"; \
	fi

test: build ## Запустити тести (заглушка для майбутнього)
	@echo "$(YELLOW)Тести поки не реалізовані$(NC)"

lint: ## Перевірити код (hlint)
	@echo "$(GREEN)Перевірка коду...$(NC)"
	@if command -v hlint >/dev/null 2>&1; then \
		hlint src/; \
	else \
		echo "$(YELLOW)hlint не встановлено. Встановіть: cabal install hlint$(NC)"; \
	fi

format: ## Форматувати код (stylish-haskell)
	@echo "$(GREEN)Форматування коду...$(NC)"
	@if command -v stylish-haskell >/dev/null 2>&1; then \
		find src/ -name "*.hs" -exec stylish-haskell -i {} \; ; \
		echo "$(GREEN)Код відформатовано!$(NC)"; \
	else \
		echo "$(YELLOW)stylish-haskell не встановлено. Встановіть: cabal install stylish-haskell$(NC)"; \
	fi

quick: build run ## Швидкий запуск (збірка + запуск)

setup: install-deps init-db build ## Повне налаштування проекту
	@echo "$(GREEN)Проект повністю налаштовано!$(NC)"

check: ## Перевірити структуру проекту
	@echo "$(GREEN)Перевірка структури...$(NC)"
	@echo "Каталоги:"
	@ls -la src/
	@echo ""
	@echo "Модулі:"
	@find src/ -name "*.hs" -type f
	@echo ""
	@echo "База даних:"
	@ls -lh database_init.sql
	@echo ""
	@echo "$(GREEN)Перевірка завершена!$(NC)"

docs: ## Відкрити документацію
	@echo "$(GREEN)Відкриття документації...$(NC)"
	@if [ -f README.md ]; then \
		cat README.md; \
	else \
		echo "$(YELLOW)README.md не знайдено$(NC)"; \
	fi

db-backup: ## Створити резервну копію БД
	@echo "$(GREEN)Створення резервної копії...$(NC)"
	@read -p "MySQL пароль root: " password; \
	mysqldump -u root -p$$password faculty_newspaper > backup_$(shell date +%Y%m%d_%H%M%S).sql
	@echo "$(GREEN)Резервна копія створена!$(NC)"

db-status: ## Показати статус бази даних
	@echo "$(GREEN)Статус бази даних:$(NC)"
	@read -p "MySQL пароль root: " password; \
	mysql -u root -p$$password -e "USE faculty_newspaper; \
		SELECT 'Авторів' as Тип, COUNT(*) as Кількість FROM authors \
		UNION ALL SELECT 'Статей', COUNT(*) FROM articles \
		UNION ALL SELECT 'Опубліковано', COUNT(*) FROM articles WHERE is_published = TRUE \
		UNION ALL SELECT 'Читачів', COUNT(*) FROM readers \
		UNION ALL SELECT 'Коментарів', COUNT(*) FROM comments \
		UNION ALL SELECT 'Категорій', COUNT(*) FROM categories \
		UNION ALL SELECT 'Тегів', COUNT(*) FROM tags \
		UNION ALL SELECT 'Матеріалів', COUNT(*) FROM graphic_materials;"

ghci: ## Запустити GHCi з модулями проекту
	@echo "$(GREEN)Запуск GHCi...$(NC)"
	cabal repl

watch: ## Автоматична перезбірка при змінах (потребує ghcid)
	@if command -v ghcid >/dev/null 2>&1; then \
		ghcid --command="cabal repl" --test=":main"; \
	else \
		echo "$(YELLOW)ghcid не встановлено. Встановіть: cabal install ghcid$(NC)"; \
	fi

info: ## Показати інформацію про проект
	@echo "$(GREEN)Faculty Newspaper v2.0$(NC)"
	@echo ""
	@echo "Структура:"
	@tree -L 2 -I 'dist*' || ls -R
	@echo ""
	@echo "Залежності:"
	@cabal list --installed | grep -E 'mysql-simple|text|time|bytestring' || echo "Виконайте: make install-deps"
	@echo ""
	@echo "GHC версія:"
	@ghc --version

all: clean install-deps build ## Повна перебудова
	@echo "$(GREEN)Проект перебудовано!$(NC)"