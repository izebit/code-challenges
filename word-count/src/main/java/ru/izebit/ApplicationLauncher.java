package ru.izebit;

import lombok.*;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.AbstractMap.SimpleEntry;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public class ApplicationLauncher {

    @SneakyThrows
    public static void main(String[] args) {
        System.out.println(
                new ApplicationLauncher()
                        .count(Parameters.parseArguments(args))
        );
    }

    @Builder
    private static class Parameters {
        private Collection<CounterType> counterTypes;
        @Builder.Default
        private Charset charset = StandardCharsets.UTF_8;
        private String filePath;
        @Builder.Default
        private InputType inputType = InputType.CONSOLE;


        public static Parameters parseArguments(String[] args) {
            var filePath = getFilePath(args);
            final InputType inputType;
            if (filePath.isPresent())
                inputType = InputType.FILE;
            else
                inputType = InputType.CONSOLE;

            return Parameters.builder()
                    .counterTypes(getCounterTypes(args))
                    .filePath(filePath.orElse(null))
                    .inputType(inputType)
                    .build();
        }

        private static List<CounterType> getCounterTypes(String[] args) {
            var counterTypesArgs = Arrays.stream(args)
                    .filter(e -> e.startsWith("-"))
                    .toList();
            if (counterTypesArgs.isEmpty())
                return List.of(CounterType.LINES, CounterType.WORDS, CounterType.BYTES);
            else if (counterTypesArgs.size() == 1) {
                var counterTypesArg = counterTypesArgs.getFirst();
                var counterTypes = counterTypesArg
                        .chars()
                        .skip(1)
                        .mapToObj(e -> CounterType.getType((char) e))
                        .toList();
                if (counterTypes.isEmpty())
                    throw new IllegalArgumentException("there are no valid passed counter types");

                return counterTypes;
            } else
                throw new IllegalArgumentException("there must be only one parameter with counter types");
        }

        private static Optional<String> getFilePath(String[] args) {
            return Arrays.stream(args)
                    .filter(e -> !e.startsWith("-"))
                    .toList()
                    .stream()
                    .findFirst();
        }
    }

    @Builder
    public static class Result {
        private final List<Entry<CounterType, Long>> result;
        private final String filePath;

        @Override
        public String toString() {
            return result
                    .stream()
                    .map(e -> String.format("%8s", e.getValue()))
                    .collect(Collectors.joining("", "", filePath == null ? "" : " " + filePath));
        }
    }

    private Result count(Parameters parameters) {
        var counters = List.of(
                new SymbolsCounter(),
                new WordsCounter(),
                new LinesCounter(),
                new BytesCounter(parameters.charset)
        );

        var targetCounters = handle(parameters, counters)
                .stream()
                .collect(Collectors.toMap(Counter::getType, Function.identity()));

        var result = parameters.counterTypes
                .stream()
                .map(targetCounters::get)
                .map(e -> (Entry<CounterType, Long>) new SimpleEntry<>(e.getType(), e.getResult()))
                .collect(Collectors.toList());

        return Result.builder()
                .result(result)
                .filePath(parameters.filePath)
                .build();
    }

    @SneakyThrows
    private static Collection<Counter> handle(final Parameters parameters,
                                              final Collection<Counter> counters) {
        val inputStream = parameters.inputType == InputType.CONSOLE
                ? new InputStreamReader(System.in)
                : new FileReader(parameters.filePath, parameters.charset);


        var targetCounters = counters
                .stream()
                .filter(e -> parameters.counterTypes.contains(e.getType()))
                .collect(Collectors.toList());

        try (val reader = new BufferedReader(inputStream)) {
            int c;
            while ((c = reader.read()) != -1) {
                final var character = (char) c;
                targetCounters.forEach(counter -> counter.apply(character));
            }
        }

        return targetCounters;
    }


    private interface Counter {
        long getResult();

        void apply(char chunk);

        CounterType getType();
    }


    @RequiredArgsConstructor
    private static class WordsCounter implements Counter {
        @Getter
        private final CounterType type = CounterType.WORDS;

        private long result = 0;
        private boolean isWord = false;

        @Override
        public long getResult() {
            if (isWord)
                result++;

            return result;
        }

        @Override
        public void apply(char chunk) {
            if (Character.isWhitespace(chunk)) {
                if (isWord)
                    result++;
                isWord = false;
            } else
                isWord = true;
        }
    }

    @Getter
    private static class LinesCounter implements Counter {
        private final CounterType type = CounterType.LINES;

        private long result = 0;

        @Override
        public void apply(char chunk) {
            if (chunk == '\n')
                result++;
        }
    }

    @RequiredArgsConstructor
    private static class BytesCounter implements Counter {
        @Getter
        private final CounterType type = CounterType.BYTES;
        private final Charset charset;

        @Getter
        private long result = 0;

        @Override
        public void apply(char chunk) {
            var countOfBytes = String.valueOf(chunk)
                    .getBytes(charset)
                    .length;
            result += countOfBytes;
        }
    }

    @Getter
    private static class SymbolsCounter implements Counter {
        private long result = 0;
        private final CounterType type = CounterType.SYMBOLS;

        @Override
        public void apply(char chunk) {
            result++;
        }
    }

    @AllArgsConstructor
    private enum CounterType {
        BYTES('c'),
        SYMBOLS('m'),
        WORDS('w'),
        LINES('l');

        private final char parameterName;

        public static CounterType getType(char p) {
            return Arrays
                    .stream(CounterType.values()).filter(e -> e.parameterName == p)
                    .findFirst()
                    .orElseThrow(() -> new IllegalArgumentException("unknown counter type: " + p));
        }
    }

    private enum InputType {
        CONSOLE,
        FILE
    }
}
