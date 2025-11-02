.PHONY: all build run clean help

all: build

build:
	@echo "Компіляція з -threaded прапорцем..."
	ghc -O2 -threaded -rtsopts DiscreteLog.hs -o discrete-log

run: build
	@echo "Запуск з 4 потоками..."
	./discrete-log +RTS -N4 -s

run-8:
	@echo "Запуск з 8 потоками..."
	./discrete-log +RTS -N8 -s

run-custom:
	@echo "Запуск з користувацькими параметрами..."
	@echo "Використання: ./discrete-log g h p +RTS -N<потоки>"
	./discrete-log $(ARGS) +RTS -N$(THREADS)

benchmark:
	@echo "Бенчмарк з різною кількістю потоків..."
	@echo "\n=== 1 потік ==="
	./discrete-log +RTS -N1 -s
	@echo "\n=== 2 потоки ==="
	./discrete-log +RTS -N2 -s
	@echo "\n=== 4 потоки ==="
	./discrete-log +RTS -N4 -s
	@echo "\n=== 8 потоків ==="
	./discrete-log +RTS -N8 -s

clean:
	@echo "Очищення..."
	rm -f *.hi *.o discrete-log

help:
	@echo "Доступні команди:"
	@echo "  make build      - Компіляція програми"
	@echo "  make run        - Запуск з 4 потоками"
	@echo "  make run-8      - Запуск з 8 потоками"
	@echo "  make benchmark  - Бенчмарк з різною кількістю потоків"
	@echo "  make clean      - Очищення скомпільованих файлів"
	@echo ""
	@echo "Користувацький запуск:"
	@echo "  make run-custom ARGS='g h p' THREADS=4"
	@echo ""
	@echo "Приклади:"
	@echo "  make run-custom ARGS='2 5 23' THREADS=4"
	@echo "  make run-custom ARGS='5 3125 65537' THREADS=8"
