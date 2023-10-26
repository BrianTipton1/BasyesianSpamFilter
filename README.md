# Bayesian Spam Filter

## Building
```bash
make
```

## Help Text
```bash
Usage: BayesianSpamFilter [OPTIONS] [PATH]

Options:
  -r, --report           Generate a report on the data processed (requires pdflatex).
  -h, --help             Show this help message and exit.

Arguments:
  PATH                   The path to the dataset file containing the text message data to be processed.
                         Default: $PWD/SMSSpamCollection.txt

Description:
  This CLI application utilizes Bayesian filtering techniques to categorize
  text messages as spam or ham (not-spam). It processes text message data
  from the specified dataset file or from $PWD/SMSSpamCollection.txt if no
  path is given.

  The --report flag can be used to generate a report detailing the
  classification results, including information such as the number
  of messages processed, the number classified as spam, and the overall
  accuracy of the filter. The report is generated using the pdflatex command.

  Significant results will be output to the terminal screen with or without the --report flag.

Examples:
  # Process text message data from the default dataset file and generate a report
  $ BayesianSpamFilter --report

  # Process text message data from the specified dataset file
  $ BayesianSpamFilter /path/to/SMSSpamCollection.txt

  # Process text message data from the specified dataset file and generate a report
  $ BayesianSpamFilter --report /path/to/SMSSpamCollection.txt
```

## ChatGPT
- [Generating Help Text](https://chat.openai.com/share/d9d6ec82-0536-40c5-89f8-783489a633c8)